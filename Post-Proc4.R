install.packages('fansi')
install.packages("backports")
install.packages("devtools")
install.packages("sf")
install.packages("magick")
library(devtools)
install_github('JGCRI/rgcam', build_vignettes=TRUE, force = TRUE)
install.packages("rgdal")
install.packages("tmap")

library('tmap')
install_github('JGCRI/metis', build_vignettes=TRUE, force = TRUE)



library(tibble)
library(rgdal)
library('rgcam')
library(ggplot2)
library(dplyr)
library(tidyr)
library('metis')
library(reshape)
library(data.table)

# Load single file produced from RDM experiment.
base_dir <- c('C:/Users/Juan Manuel Rincon R/OneDrive - Universidad de los andes/Modelacion Energetica/RDM Analisis R/' )
output_file <- c('05282020.proj')
prj_path <- paste0(base_dir, output_file)
prj <- loadProject(prj_path)



names(prj)
# Create empty data frame
queries <- listQueries(prj)

# Loop through all data outputs, rename scenario as regular scenario name, and create new "experiment" column with
# corresponding experiment number
for(experiment in names(prj)){
  for(query in queries){
    exp <- prj[[experiment]]
    qry <- exp[[query]]
    qry <- qry %>%
      mutate(scenario = gsub('DDP_Delayed_EndPt', 'DelayedEndPt', scenario)) %>% 
      mutate(scenario = gsub('DDP_Delayed_CumEmiss', 'DelayedCumEmiss', scenario)) %>% 
      mutate(experiment=substring(scenario, regexpr("_", scenario) + 1, nchar(scenario))) %>%
      mutate(old_scen_name=scenario) %>% 
      mutate(scenario=substring(scenario, 0, regexpr("_", scenario)-1))
    prj[[experiment]][[query]] <- qry  # replace query result for this query and experiment with modified dataframe
  }
}
#prj_copy <- prj  # Stores the original prj, before we cut extra stuff out of it.

num_runs <- 720
short_list <- c()
scens <- c("DDP","DDP_Delayed_CumEmiss","DDP_Delayed_EndPt")
for(scen in scens){
  for(run in seq(1:num_runs)){
    if(paste0(scen, "_", run-1) %in% names(prj)){
      short_list <- append(short_list, paste0(scen, "_", run-1))
    }
  }
}
prj <- prj[short_list]  # Eliminate any extra/unnecessary runs stored in the .dat file from PIC.


#Eliminate unneeded queries
queries_relevant <- queries  # c('CO2 emissions by sector (no bio)')
# Create a single dataframe that stores all all experiments under a single query.
reorg_prj <- vector("list", length(queries_relevant))
names(reorg_prj) <- queries_relevant


reorg_prj <- list()
for(query in queries_relevant){
  reorg_temp <- list()
  for(experiment in names(prj)){
    reorg_temp[[experiment]] <- prj[[experiment]][[query]]
  }
  reorg_prj[[query]] <- rbindlist(reorg_temp)
  print(paste0("completing query:", query))
}


# Save the new file so it can then be imported in the way Metis expects
saveRDS(reorg_prj, file='C:/Users/Juan Manuel Rincon R/OneDrive - Universidad de los andes/Modelacion Energetica/RDM Analisis R/05282020_Reorg.dat')

########CO2 emissions by GCAM######################
CO2<-reorg_prj$`CO2 emissions by sector (no bio)`%>% mutate(Metric="CO2 Emissions",Units="MTCO2") %>% group_by(scenario, region, experiment, old_scen_name, Units,year, Metric) %>%summarize(value=(44/12)*sum(value))
Cumulative(CO2)
########GHG (CO2eq)######################
## GWP file with SAR, AR4 and AR5 values
GWP<-read.csv('C:/Users/Juan Manuel Rincon R/OneDrive - Universidad de los andes/Modelacion Energetica/RDM Analisis R/ReferenceTables/GWP.csv', header = TRUE, sep = ",", dec = ".")

################adding nonCO2 Emissions ###########
Non1<-reorg_prj$`nonCO2 emissions by sector`
Non2<-reorg_prj$`nonCO2 emissions by resource production`
names(Non2)[3]<-"sector"
NonCO2emiss<-rbind(Non1,Non2)
rm(Non1,Non2)

######NONCO2 emissions##
nonco2<-NonCO2emiss%>% left_join(GWP, by="ghg") %>% 
  mutate(Metric="Nonco2 Emissions",Units="MTCO2e",value=value*AR5all)%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%summarize(value=sum(value)) 
Cumulative(nonco2)
#####GHG Emissions 
GHG<-rbind(nonco2,CO2)%>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  summarize(value=sum(value)) %>% mutate(Units="MTCO2e",Metric="GHG Emissions")
##########N2O emissions##########
N2O<-NonCO2emiss%>%  filter(ghg %in% c("N2O","N2O_AGR","N2O_AWB"))%>% group_by(scenario, region, experiment, old_scen_name, year) %>%
  summarize(value=sum(value)*265)  %>% mutate(Metric="N2O Emissions",Units="MTCO2e")
Cumulative(N2O)
##########CH4 emissions##########
CH4<-NonCO2emiss%>%  filter(ghg %in% c("CH4","CH4_AGR","CH4_AWB"))%>% group_by(scenario, region, experiment, old_scen_name, year) %>%
  summarize(value=sum(value)*28)  %>% mutate(Metric="CH4 Emissions",Units="MTCO2e")
Cumulative(CH4)

############Agriculture GHG emissions############

CO2eqAg<-NonCO2emiss%>%filter(sector %in% c("FiberCrop","MiscCrop","OilCrop","OtherGrain","PalmFruit","Corn","Rice","Root_Tuber","SugarCrop","UnmanagedLand","Wheat","biomass","FodderGrass","FodderHerb"))%>% left_join(GWP, by="ghg") %>% 
  mutate(Metric="Ag. GHG Emissions",Units="MTCO2e",value=value*AR5all)%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%summarize(value=sum(value)) 
Cumulative(CO2eqAg)

############Agriculture(livestock) GHG emissions############

CO2eqLs<-NonCO2emiss%>%filter(sector %in% c("Beef","Dairy","Pork","Poultry","SheepGoat"))%>% left_join(GWP, by="ghg") %>% 
  mutate(Metric="Livestock GHG Emissions",Units="MTCO2e",value=value*AR5all)%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%summarize(value=sum(value)) 
Cumulative(CO2eqLs)

############HouseHolding GHG emissions############

CO2eqHh<-NonCO2emiss%>%filter(sector %in% c("resid cooling","resid heating","resid others"))%>% left_join(GWP, by="ghg") %>% 
  mutate(Metric="Households GHG Emissions",Units="MTCO2e",value=value*AR5all)%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%summarize(value=sum(value)) 
Cumulative(CO2eqHh)

############Passenger GHG emissions############

CO2eqPass<-NonCO2emiss%>%filter(sector %in% c("trn_pass","trn_pass_road","trn_pass_road_LDV","trn_pass_road_LDV_2W","trn_pass_road_LDV_4W","trn_pass_road_bus"))%>% left_join(GWP, by="ghg") %>% 
  mutate(Metric="Passenger GHG Emissions",Units="MTCO2e",value=value*AR5all)%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%summarize(value=sum(value)) 
Cumulative(CO2eqPass)
############Commercial GHG emissions############

CO2eqComm<-NonCO2emiss%>%filter(sector %in% c("backup_electricity","comm cooling","comm heating","comm others"))%>% left_join(GWP, by="ghg") %>% 
  mutate(Metric="Commercial GHG Emissions",Units="MTCO2e",value=value*AR5all)%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%summarize(value=sum(value)) 
Cumulative(CO2eqComm)

############Industry of Energy GHG emissions############

CO2eqIE<-NonCO2emiss%>%filter(sector %in% c("csp_backup","district heat","industrial energy use","process heat cement","regional biomassOil","regional corn for ethanol","regional sugar for ethanol"))%>% left_join(GWP, by="ghg") %>% 
  mutate(Metric="Industry of E. GHG Emissions",Units="MTCO2e",value=value*AR5all)%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%summarize(value=sum(value)) 
Cumulative(CO2eqIE)

############Industry Process GHG emissions############

CO2eqIP<-NonCO2emiss%>%filter(sector %in% c("N fertilizer","cement","industrial feedstocks","industrial processes"))%>% left_join(GWP, by="ghg") %>% 
  mutate(Metric="I. Process GHG Emissions",Units="MTCO2e",value=value*AR5all)%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%summarize(value=sum(value)) 
Cumulative(CO2eqIP)

############Power GHG emissions############

CO2eqPower<-NonCO2emiss%>%filter(sector %in% c("elec_biomass (IGCC CCS)","elec_biomass (IGCC)","elec_biomass (conv CCS)","elec_biomass (conv)","elec_coal (IGCC CCS)","elec_coal (IGCC)","elec_coal (conv pul CCS)","elec_coal (conv pul)","elec_gas (CC CCS)","elec_gas (CC)","elec_gas (steam/CT)","elec_refined liquids (CC CCS)","elec_refined liquids (CC)","elec_refined liquids (steam/CT)","electricity","electricity_net_ownuse","gas pipeline"))%>% left_join(GWP, by="ghg") %>% 
  mutate(Metric="Power GHG Emissions",Units="MTCO2e",value=value*AR5all)%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%summarize(value=sum(value)) 
Cumulative(CO2eqPower)

############Fugitive GHG emissions############

CO2eqFug<-NonCO2emiss%>%filter(sector %in% c("unconventional oil production","coal","crude oil","natural gas"))%>% left_join(GWP, by="ghg") %>% 
  mutate(Metric="Fugitive GHG Emissions",Units="MTCO2e",value=value*AR5all)%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%summarize(value=sum(value)) 
Cumulative(CO2eqFug)

############Refining GHG emissions############

CO2eqRef<-NonCO2emiss%>%filter(sector %in% c("H2 central production","H2 forecourt production","gas processing","refining"))%>% left_join(GWP, by="ghg") %>% 
  mutate(Metric="Refining GHG Emissions",Units="MTCO2e",value=value*AR5all)%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%summarize(value=sum(value)) 
Cumulative(CO2eqRef)

############Waste GHG emissions############

CO2eqWst<-NonCO2emiss%>%filter(sector %in% c("urban processes"))%>% left_join(GWP, by="ghg") %>% 
  mutate(Metric="Waste GHG Emissions",Units="MTCO2e",value=value*AR5all)%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%summarize(value=sum(value)) 
Cumulative(CO2eqWst)

############Bunker(trn_int) GHG emissions############

CO2eqBk<-NonCO2emiss%>%filter(sector %in% c("trn_aviation_intl","trn_shipping_intl"))%>% left_join(GWP, by="ghg") %>% 
  mutate(Metric="Int. Transp. GHG Emissions",Units="MTCO2e",value=value*AR5all)%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%summarize(value=sum(value)) 
Cumulative(CO2eqBk)

############Freight GHG emissions############

CO2eqFrght<-NonCO2emiss%>%filter(sector %in% c("trn_freight","trn_freight_road"))%>% left_join(GWP, by="ghg") %>% 
  mutate(Metric="Freight Transp. GHG Emissions",Units="MTCO2e",value=value*AR5all)%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year,Metric) %>%summarize(value=sum(value)) 
CO2eqFrght$value<-CO2eqFrght$value+(CO2eqBk$value*0.5)
Cumulative(CO2eqFrght)

############Agriculture+Livestock GHG emissions############
CO2eqAgri<-CO2eqAg
CO2eqAgri$Metric<-"Agriculture(Ag+Ls) GHG Emissions"
CO2eqAgri$value<-CO2eqLs$value+CO2eqAg$value
############FFI GHG emissions############
CO2eqFFI<-rbind(as_tibble(CO2eqFrght),as_tibble(CO2eqFug),as_tibble(CO2eqIE),as_tibble(CO2eqIP),as_tibble(CO2eqPass),as_tibble(CO2eqPower),as_tibble(CO2eqRef))
CO2eqFFI<-CO2eqFFI%>%  group_by(scenario, region, experiment, old_scen_name,Units, year) %>%
  summarize(value=sum(value))
CO2eqFFI<-add_column(CO2eqFFI,Metric="FFI GHG Emissions")

############Others GHG emissions############
CO2eqOther<-CO2eqAg
CO2eqOther$Metric<-"Non-Ag-FFI. GHG Emissions"
CO2eqOther$value<-GHG$value-CO2eqAg$value-CO2eqFFI$value

######NOx emissions##

NOx<-reorg_prj$`nonCO2 emissions by sector`%>%  filter(ghg %in% c("NOx_AGR","NOx_AWB","NOx")) %>%
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>%  summarize(value=sum(value))%>% mutate(Metric="NOx Emissions")
Cumulative(NOx)

######SO2 emissions##

SO2<-reorg_prj$`nonCO2 emissions by sector` %>%  filter(ghg %in% c("SO2_3","SO2_3_AWB")) %>%
  group_by(scenario, region, experiment, old_scen_name,Units, year) %>%  summarize(value=sum(value))%>% mutate(Metric="SO2 Emissions")
Cumulative(SO2)
############Land use change (LUC) emissions############

LUC<-reorg_prj$`Land Use Change Emission (future)` %>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  summarize(value=(44/12)*sum(value)) %>% mutate(Metric="LUC Emissions",Units="MTCO2")

#######land Allocation######
biomassland<-reorg_prj$`aggregated land allocation`%>%  filter(landleaf %in% c("biomass"),year %in% c(2010:2050)) %>%
  group_by(scenario, region, experiment, old_scen_name,Units, year) %>%
  summarize(value=sum(value))%>% mutate(Metric="Biomass lands")

cropland<-reorg_prj$`aggregated land allocation`%>%  filter(landleaf %in% c("crops"),year %in% c(2010:2050)) %>%
  group_by(scenario, region, experiment, old_scen_name,Units, year) %>%
  summarize(value=sum(value))%>% mutate(Metric="Crop lands")

biomasscropland<-reorg_prj$`aggregated land allocation`%>%  filter(landleaf %in% c("biomass","crops"),year %in% c(2010:2050)) %>%
  group_by(scenario, region, experiment, old_scen_name,Units, year) %>%
  summarize(value=sum(value))%>% mutate(Metric="Agricultural lands")

#********############Total Water Demand###############
waterconsump<-reorg_prj$`water withdrawals by water mapping source`%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year) %>%
  summarize(value=sum(value))%>%  filter(region %in% c("Colombia"))%>%mutate(Metric="Water Demand")

#**********#######prices ###########



price <- list()
price1 <- list()
for(experiment in names(prj)){
  price1[[experiment]] <- prj[[experiment]]$`prices of all markets` %>%group_by(scenario, region, experiment, old_scen_name,Units, year,market) %>%
    filter(market %in% c("globalcrude oil","ColombiaBeef","globalRice","globalWheat"))
  print(paste0("completing exp:", experiment))
  }
price<- rbindlist(price1)





#**********#######Oil prices ###########

oilprice2015<-price%>%filter(market %in% c("globalcrude oil"),year %in% c("2015"))%>%
  group_by(scenario, region, experiment, old_scen_name) %>%summarize(value=sum(value))

oilprice<-price%>%filter(market %in% c("globalcrude oil"),year %in% c(2010:2050))%>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%summarize(value=sum(value))%>%left_join(oilprice2015, by=c("scenario","region","experiment","old_scen_name"))%>%
  mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",Metric="Oil price",value.x=NULL,value.y=NULL)

#**********#######Beef prices ###########
beefprice2015<-price%>%filter(market %in% c("ColombiaBeef"),year %in% c("2015"))%>%
  group_by(scenario, region, experiment, old_scen_name) %>%summarize(value=sum(value))

beefprice<-price%>%filter(market %in% c("ColombiaBeef"),year %in% c(2010:2050))%>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%summarize(value=sum(value))%>%left_join(beefprice2015, by=c("scenario","region","experiment","old_scen_name"))%>%
  mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",Metric="Beff price",value.x=NULL,value.y=NULL)

#**********#######Rice prices ###########
riceprice2015<-price%>%filter(market %in% c("globalRice"),year %in% c("2015"))%>%
  group_by(scenario, region, experiment, old_scen_name) %>%summarize(value=sum(value))

riceprice<-price%>%filter(market %in% c("globalRice"),year %in% c(2010:2050))%>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%summarize(value=sum(value))%>%left_join(riceprice2015, by=c("scenario","region","experiment","old_scen_name"))%>%
  mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",Metric="Rice price",value.x=NULL,value.y=NULL)

#**********#######Wheat prices ###########
wheatprice2015<-price%>%filter(market %in% c("globalWheat"),year %in% c("2015"))%>%
  group_by(scenario, region, experiment, old_scen_name) %>%summarize(value=sum(value))

wheatprice<-price%>%filter(market %in% c("globalWheat"),year %in% c(2010:2050))%>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%summarize(value=sum(value))%>%left_join(wheatprice2015, by=c("scenario","region","experiment","old_scen_name"))%>%
  mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",Metric="Wheat price",value.x=NULL,value.y=NULL)

###############Final Energy###################
FinalEnergy<-reorg_prj$`total final energy by aggregate sector` %>%
  group_by(scenario, region, experiment, old_scen_name,Units, year) %>%
  summarize(value=sum(value))
FinalEnergy<-add_column(FinalEnergy,Metric="Final Energy")
####################  Electricity prices#############
ElecPrice<-reorg_prj$`elec prices by sector`%>%  filter(year %in% c(1990,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2065,2080,2095))

elecPrice<-ElecPrice%>%  filter(fuel %in% c("elect_td_bld")) %>%
  group_by(scenario, region, experiment, old_scen_name, year) 
bldEnergy<-reorg_prj$`building final energy by fuel` %>%
  group_by(scenario, region, experiment, old_scen_name,input, year) %>%
  filter(input %in% c("elect_td_bld"))

trnPrice<-ElecPrice %>%  filter(fuel %in% c("elect_td_trn"))%>%
  group_by(scenario, region, experiment, old_scen_name,  year) 
trnEnergy<-reorg_prj$`transport final energy by fuel` %>%
  group_by(scenario, region, experiment, old_scen_name,input, year) %>%
  filter(input %in% c("elect_td_trn"))

indPrice<-ElecPrice%>%  filter(fuel %in% c("elect_td_ind")) %>%
  group_by(scenario, region, experiment, old_scen_name, year) 
indEnergy<-reorg_prj$`industry final energy by fuel` %>%
  group_by(scenario, region, experiment, old_scen_name,input,  year) %>%
  filter(input %in% c("elect_td_ind"))
elecPrice$value<-(elecPrice$value*bldEnergy$value*(10**3))+(trnPrice$value*trnEnergy$value*(10**3))+(indEnergy$value*indPrice$value*(10**3))

price2015<-elecPrice%>%  filter(year %in% c("2015"))%>%  group_by(scenario, region, experiment, old_scen_name) %>%
  summarize(value=sum(value))

elecPrice<-elecPrice%>%left_join(price2015, by=c("scenario","region","experiment","old_scen_name"))%>%
  mutate(value=((value.x-value.y)/value.y)+1,Units="2015 reference",Metric="Elec. price",value.x=NULL,value.y=NULL,fuel=NULL)

###########Building Traditional Biomass#####

TotalBuild<-reorg_prj$`building final energy by fuel` %>%
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>% summarize(value=sum(value))
TradBio<-reorg_prj$`building final energy by fuel`%>%filter(input=='traditional biomass')
TradBio$value<-((TradBio$value/TotalBuild$value)*100)
names(TradBio)[3]<-"Metric"
TradBio$Metric<-"Building Trad. Biomass"
TradBio$Units<- "%"


#####VKT
load_factors <- read.csv('C:/Users/Juan Manuel Rincon R/OneDrive - Universidad de los andes/Modelacion Energetica/RDM Analisis R/L254.StubTranTechLoadFactor.csv', skip=1) %>% filter(region == "Colombia")  
##load_factors%>%rename(sector=supplysector, subsector = tranSubsector, technology = stub.technology)
names(load_factors)[2]<-"sector"
names(load_factors)[3]<-"subsector"
names(load_factors)[4]<-"technology"

transbytech<-reorg_prj$`transport service output by tech`%>% filter(Units=='million pass-km')
VKT<-left_join(transbytech,load_factors, by=c("region","sector", "subsector", "technology", "year")) %>% mutate(vkt = value/loadFactor)
VKT$Units<- 'million vehicle-km'
VKT[is.na(VKT)] <- 0
EVVKT<-VKT %>%filter(technology %in% c("Electric","FCEV"))%>% 
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>% summarize(value=sum(vkt))
TotVKT<-VKT %>%  group_by(scenario, region, experiment, old_scen_name, Units, year) %>% summarize(value=sum(vkt))%>% mutate(Metric = "EV VKT2.0")
TotVKT$Units<-"%"
TotVKT$value<-(EVVKT$value/TotVKT$value)*100
#####number of EV
vkt_veh_yr <- read.csv('C:/Users/Juan Manuel Rincon R/OneDrive - Universidad de los andes/Modelacion Energetica/RDM Analisis R/UCD_trn_data_CORE.csv', skip=7) %>% filter(UCD_region == "Latin America",unit=="vkt/veh/yr",mode=="LDV_4W")
names(vkt_veh_yr)[4]<-'subsector'
vkt_veh_yr2<-melt(vkt_veh_yr, id.vars=c("UCD_region", "UCD_sector", "mode" , "subsector", "UCD_technology", "UCD_fuel","variable","unit" ),variable.name = c("year"))
vkt_veh_yr2$year<-as.numeric(substring(vkt_veh_yr2$year,2))

reorg_prj$`transport service output by tech` %>%  filter(sector == "trn_pass_road_LDV_4W",technology=="FCEV")%>%
  left_join(load_factors, by = c("region","sector", "subsector", "technology", "year")) %>%  mutate(vehicles = (value / loadFactor / 14000)*(10**3),Metric = "Cumulative Elec. vehicles",Units="Thous #")  %>%
  group_by(scenario, experiment, old_scen_name, region, Metric, year, Units) %>% summarise(value = sum(vehicles))%>%  ungroup() -> Evehicles

Evehicles$value<- ave(Evehicles$value*5,Evehicles$old_scen_name,FUN=cumsum)

#********############renewables percentage of power generation. ###############
PowGen<-reorg_prj$`Electricity generation by aggregate technology`%>%
  group_by(scenario, region, experiment, old_scen_name,Units, year) %>%
  summarize(value=sum(value))
PowGenRenew<-reorg_prj$`Electricity generation by aggregate technology`%>%
  filter(technology %in% c("Biomass","Geothermal","Hydro","Solar","Wind","Biomass w/CCS"))%>%
  group_by(scenario, region,experiment, old_scen_name, year) %>%
  summarize(value=sum(value))%>% mutate(Metric = "Renewable Percentage",Units="%")
PowGenRenew$value<-(PowGenRenew$value/PowGen$value)*100
####Now in GWh
PowGenRenewGW<-reorg_prj$`Electricity generation by aggregate technology`%>%
  filter(technology %in% c("Biomass","Geothermal","Hydro","Solar","Wind","Biomass w/CCS"))%>%
  group_by(scenario, region,experiment, old_scen_name, year) %>%
  summarize(value=sum(value)*2.777*(10**2))%>% mutate(Metric = "Renewable Power generation",Units="Thous GWh")

#********############intermitent percentage of power generation. ###############
PowGenRenewInt<-reorg_prj$`Electricity generation by aggregate technology`%>%
  filter(technology %in% c("Solar","Wind"))%>%
  group_by(scenario, region,experiment, old_scen_name, year) %>%
  summarize(value=sum(value))%>% mutate(Metric = "Wind and Solar Percentage",Units="%")
PowGenRenewInt$value=(PowGenRenewInt$value/PowGen$value)*100
####Now in GWh
PowGenRenewIntGW<-reorg_prj$`Electricity generation by aggregate technology`%>%
  filter(technology %in% c("Solar","Wind"))%>%
  group_by(scenario, region,experiment, old_scen_name, year) %>%
  summarize(value=sum(value)*2.777*(10**2))%>% mutate(Metric = "Wind and Solar Power generation",Units="Thous GWh")
#********############Hydro percentage of power generation. ###############
PowGenHydro<-reorg_prj$`Electricity generation by aggregate technology`%>%
  filter(technology %in% c("Hydro"))%>%
  group_by(scenario, region,experiment, old_scen_name, year) %>%
  summarize(value=sum(value))%>% mutate(Metric = "Hydro Percentage",Units="%")
PowGenHydro$value=(PowGenHydro$value/PowGen$value)*100
####Now in GWh
PowGenHydroGW<-reorg_prj$`Electricity generation by aggregate technology`%>%
  filter(technology %in% c("Hydro"))%>%
  group_by(scenario, region,experiment, old_scen_name, year) %>%
  summarize(value=sum(value)*2.777*(10**2))%>% mutate(Metric = "Hydro Power generation",Units="Thous GWh")

#********############Bus percentage of passenger demand. ###############
#******************************************************
########Passenger demand#######
traindemand<-reorg_prj$`transport service output by mode` %>% filter(mode %in% c("HSR","Passenger Rail")) %>%
  group_by(scenario, region, experiment, old_scen_name, Units, year) %>%
  summarize(value=sum(value))
cycledemand<-reorg_prj$`transport service output by mode` %>%
  group_by(scenario, region, experiment, old_scen_name,sector,mode, Units, year) %>%
  summarize(value=sum(value))%>%  filter(mode %in% c("Cycle"))
airdemand<-reorg_prj$`transport service output by mode` %>%
  group_by(scenario, region, experiment, old_scen_name,sector,mode, Units, year) %>%
  summarize(value=sum(value))%>%  filter(mode %in% c("Domestic Aviation"))
walkdemand<-reorg_prj$`transport service output by mode` %>%
  group_by(scenario, region, experiment, old_scen_name,sector,mode, Units, year) %>%
  summarize(value=sum(value))%>%  filter(mode %in% c("Walk"))
busdemand<-reorg_prj$`transport service output by mode` %>%filter(mode %in% c("Bus"))%>%
  group_by(scenario, region, experiment, old_scen_name, year) %>%
  summarize(value=sum(value))%>%  mutate(Metric="Bus passenger demand", Units ="%")
cardemand<-reorg_prj$`transport service output by mode` %>%
  group_by(scenario, region, experiment, old_scen_name,sector,mode, Units, year) %>%
  summarize(value=sum(value))%>%  filter(mode %in% c("LDV"))
#*****########Bus percentage########
passdemand<-cardemand$value+traindemand$value+busdemand$value+airdemand$value+cycledemand$value+walkdemand$value
busdemand$value<-(busdemand$value/passdemand)*100

Metrics<-rbind(as_tibble(PowGenHydroGW),as_tibble(PowGenRenewGW),as_tibble(PowGenRenewIntGW),as_tibble(PowGenHydro),as_tibble(PowGenRenew),as_tibble(PowGenRenewInt),
               as_tibble(Evehicles),as_tibble(TotVKT),as_tibble(CO2),as_tibble(GHG),
               as_tibble(CO2eqAgri),as_tibble(CO2eqFFI),as_tibble(CO2eqAg),as_tibble(CO2eqBk),as_tibble(CO2eqComm),as_tibble(CO2eqFrght),
               as_tibble(CO2eqFug),as_tibble(CO2eqHh),as_tibble(CO2eqIE),as_tibble(CO2eqIP),as_tibble(CO2eqLs),as_tibble(CO2eqPass),
               as_tibble(CO2eqPower),as_tibble(CO2eqRef),as_tibble(CO2eqWst),as_tibble(CO2eqFFI),as_tibble(LUC),as_tibble(NOx),as_tibble(SO2),
               as_tibble(CO2eqOther),as_tibble(oilprice),as_tibble(waterconsump),as_tibble(biomasscropland),as_tibble(biomassland),
               as_tibble(cropland),as_tibble(FinalEnergy),as_tibble(elecPrice),as_tibble(beefprice),as_tibble(riceprice),as_tibble(wheatprice),
               as_tibble(TradBio),as_tibble(busdemand),as_tibble(nonco2),as_tibble(N2O),as_tibble(CH4))
####Cumulative Plots
Metrics<-rbind(as_tibble(cCO2),as_tibble(cCO2eqAg),as_tibble(cCO2eqBk),as_tibble(cCO2eqComm),
               as_tibble(cCO2eqFrght),as_tibble(cCO2eqFug),as_tibble(cCO2eqHh),as_tibble(cCO2eqIE),as_tibble(cCO2eqIP),as_tibble(cCO2eqLs),
               as_tibble(cCO2eqPass),as_tibble(cCO2eqPower),as_tibble(cCO2eqRef),as_tibble(cCO2eqWst),as_tibble(cNOx),as_tibble(cSO2),as_tibble(cCH4),
               as_tibble(cnonco2),as_tibble(cN2O))


#Metrics<-rbind(as_tibble(CO2))
               
for (i in seq(length(unique(Metrics$Metric)))) {
  M<-Metrics%>% filter(Metric %in% c(unique(Metrics$Metric)[i]))
  plot_df <- M%>%filter(scenario %in% c("DDP","DelayedCumEmiss","DelayedEndPt"))
  fig_path <- c(paste('C:/Users/Juan Manuel Rincon R/OneDrive - Universidad de los andes/Modelacion Energetica/RDM Analisis R/3DDPScenariosv1/Cumulative/',unique(Metrics$Metric)[i],'.png',sep = ""))
  y_lbl <- paste(M$Metric[1],' (',M$Units[1],')',sep = "")
  x_lbl <- 'Time'
  title <- ''
  x_min <- 2015
  x_max <- 2050
  plot_scens <- c("DDP","DelayedCumEmiss","DelayedEndPt")#"Reference",
  line_plot(plot_df, fig_path, plot_scens, y_lbl=y_lbl, x_lbl=x_lbl, title=title, x_min=x_min, x_max=x_max)
}
# Function to make Cumulative queries
Cumulative<-function(Query){
  h<- Query%>%filter(year %in%(2020:2050))%>% mutate(Metric=paste("Cumulative" ,unique(Query$Metric)))
  h$value<-ave(h$value*5,h$old_scen_name,FUN=cumsum)
  assign(paste("c",deparse(substitute(Query)),sep = ""), h,envir = globalenv())
  
}


               # Function to make plots
line_plot <- function(plot_df, fig_path, plot_scens, y_lbl=NULL,
                      x_lbl=NULL, y_max=NULL, y_min=NULL,
                      all_same_color=1, title=NULL, legend_on=TRUE,
                      plot_var=NULL, x_min=NULL, x_max=NULL){
  # ggplot2 Theme
  z_theme <<- theme_bw() +
    theme(
      text =                element_text(family = NULL, face = "plain",colour = "black", size = 10 ,hjust = 0.5,
                                         vjust = 0.5, angle = 0, lineheight = 0.9)
      , axis.text.x =       element_text(size=8)
      , axis.text.y =       element_text(size=8)
      ,axis.title.x =       element_text(vjust = -1, margin=margin(t=1,unit="line"))
      ,axis.title.y =       element_text(angle = 90, vjust = 2, margin=margin(r=1,unit="line"))
      ,legend.key =         element_blank()
      ,legend.key.size =    unit(1.5, 'lines')
      ,legend.text =        element_text(size = 8, colour = "black")
      ,legend.title =       element_text(size = rel(1.2), face = NULL, hjust = 0, colour = "black")
      ,strip.background =   element_rect(fill = NA, colour = "black")
      ,plot.margin =        unit(c(1, 1, 1, 1), "lines")
      ,plot.title=          element_text(face="bold", hjust=0.2, vjust = -4, margin = margin(b=20), size=8)
      ,legend.position =    c(0.95, 0.95)
    )
  
  p <- ggplot()
  ctr <- 0
  color_list <- c('dodgerblue3','black','gray')  #  #de2d26, #fc9272
  for(plot_scen in plot_scens){
    ctr <- ctr+1
    plot_df_filt <- plot_df %>% filter(scenario==plot_scen)
    p <- p + geom_line(size=0.5, color=color_list[ctr],
                       data=plot_df_filt, mapping = aes(x = year, y = value, group=experiment))  # colour=scenario
    
  }
  
  p <- p + xlab(x_lbl) + ylab(y_lbl)
  if(!is.null(y_min)){
    p<-p + scale_y_continuous(limits=c(y_min - 0.1*abs(y_min), 1.1*y_max))
  }
  if(!is.null(x_min)){
    p<-p + scale_x_continuous(limits=c(x_min, x_max))
  }
  if(legend_on==TRUE){
    p <- p + guides(color=legend_on)
  }
  p <- p + ggtitle(title)
  p <- p + z_theme
  p
  ggsave(fig_path, dpi=900, width=2.5, height=2.5, units="in")
}
