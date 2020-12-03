library(devtools)
library('tmap')
library(tibble)
library(rgdal)
library('rgcam')
library(ggplot2)
library(dplyr)
library(tidyr)
library('metis')
library(reshape)
library(data.table)

qry <- "prices of all markets.proj"
prj_path <- paste0('C:/Users/Juan Manuel Rincon R/Desktop/Colombia-RDM-Analysis/query_proj_06_22_2020/', qry)
prj <- loadProject(prj_path)
price<- prj$data$`prices of all markets`%>%group_by(scenario, region, experiment, old_scen_name,Units, year,market) %>%
  filter(market %in% c("ColombiaBeef","ColombiaC2F6","ColombiaCF4","ColombiaCH4","ColombiaCH4_AGR","ColombiaCH4_AWB",
                       "ColombiaCO2","ColombiaDDGS and feedcakes","ColombiaDairy","ColombiaExports_Meat","ColombiaExports_Meat-tfe",
                       "ColombiaExports_fertilizer","ColombiaExports_fertilizer-tfe","ColombiaFeedCrops","ColombiaFodderGrass",
                       "ColombiaFodderHerb_Residue","ColombiaFoodDemand_Crops","ColombiaFoodDemand_Crops-tfe","ColombiaFoodDemand_Meat",
                       "ColombiaFoodDemand_Meat-tfe","ColombiaGHG_tax","ColombiaH2 central production","ColombiaH2 distribution",
                       "ColombiaH2 enduse","ColombiaH2 forecourt production","ColombiaHFC125","ColombiaHFC134a","ColombiaHFC143a",
                       "ColombiaHFC152a","ColombiaHFC227ea","ColombiaHFC23","ColombiaHFC236fa","ColombiaHFC245fa","ColombiaHFC32",
                       "ColombiaHFC365mfc","ColombiaMagdalena_water consumption","ColombiaMagdalena_water withdrawals","ColombiaN fertilizer",
                       "ColombiaN2O","ColombiaN2O_AGR","ColombiaN2O_AWB","ColombiaNonFoodDemand_Crops","ColombiaNonFoodDemand_Crops-tfe",
                       "ColombiaNonFoodDemand_Forest","ColombiaNonFoodDemand_Forest-tfe","ColombiaNonFoodDemand_Meat",
                       "ColombiaNonFoodDemand_Meat-tfe","ColombiaPasture","ColombiaPasture_FodderGrass","ColombiaPork",
                       "ColombiaPoultry","ColombiaSF6","ColombiaScavenging_Other","ColombiaSheepGoat","ColombiaUnmanagedLand",
                       "Colombiabackup_electricity","Colombiabiomass","Colombiabiophysical water consumption","Colombiacarbon-storage",
                       "Colombiacement","Colombiacement-tfe","Colombiacomm cooling","Colombiacomm heating","Colombiacomm others",
                       "Colombiacomm-internal-gains-trial-market-trial-supply","Colombiacsp_backup","Colombiadelivered biomass",
                       "Colombiadelivered coal","Colombiadelivered gas","Colombiadesalination","Colombiadistributed_solar",
                       "Colombiaelec_CSP","Colombiaelec_CSP-fixed-output","Colombiaelec_CSP_storage","Colombiaelec_CSP_storage-fixed-output",
                       "Colombiaelec_Gen_III","Colombiaelec_Gen_III-fixed-output","Colombiaelec_Gen_II_LWR","Colombiaelec_Gen_II_LWR-fixed-output",
                       "Colombiaelec_biomass (IGCC CCS)","Colombiaelec_biomass (IGCC CCS)-fixed-output","Colombiaelec_biomass (IGCC)",
                       "Colombiaelec_biomass (IGCC)-fixed-output","Colombiaelec_biomass (conv CCS)","Colombiaelec_biomass (conv CCS)-fixed-output",
                       "Colombiaelec_biomass (conv)","Colombiaelec_biomass (conv)-fixed-output","Colombiaelec_coal (IGCC CCS)",
                       "Colombiaelec_coal (IGCC CCS)-fixed-output","Colombiaelec_coal (IGCC)","Colombiaelec_coal (IGCC)-fixed-output",
                       "Colombiaelec_coal (conv pul CCS)","Colombiaelec_coal (conv pul CCS)-fixed-output","Colombiaelec_coal (conv pul)",
                       "Colombiaelec_coal (conv pul)-fixed-output","Colombiaelec_gas (CC CCS)","Colombiaelec_gas (CC CCS)-fixed-output",
                       "Colombiaelec_gas (CC)","Colombiaelec_gas (CC)-fixed-output","Colombiaelec_gas (steam/CT)",
                       "Colombiaelec_gas (steam/CT)-fixed-output","Colombiaelec_geothermal","Colombiaelec_geothermal-fixed-output",
                       "Colombiaelec_refined liquids (CC CCS)","Colombiaelec_refined liquids (CC CCS)-fixed-output","Colombiaelec_refined liquids (CC)",
                       "Colombiaelec_refined liquids (CC)-fixed-output","Colombiaelec_refined liquids (steam/CT)",
                       "Colombiaelec_refined liquids (steam/CT)-fixed-output","Colombiaelect_td_bld","Colombiaelect_td_ind","Colombiaelect_td_trn",
                       "Colombiaelectricity","ColombiaelectricityDemand_int","Colombiaelectricity_net_ownuse","Colombiagas pipeline",
                       "Colombiagas processing","Colombiageothermal","Colombiaindustrial energy use","Colombiaindustrial feedstocks",
                       "Colombiaindustrial processes","Colombiaindustry","Colombiaindustry-tfe","Colombiamunicipal water","Colombiamunicipal water-tfe","ColombianuclearFuelGenII","ColombianuclearFuelGenIII","Colombiaonshore carbon-storage","Colombiaonshore wind resource","Colombiaprocess heat cement","Colombiarefined liquids enduse","Colombiarefined liquids industrial","Colombiarefining","Colombiaregional biomass","Colombiaregional biomassOil","Colombiaregional coal","Colombiaregional corn for ethanol","Colombiaregional natural gas","Colombiaregional oil","Colombiaregional sugar for ethanol","Colombiaresid cooling","Colombiaresid heating","Colombiaresid others","Colombiaresid-internal-gains-trial-market-trial-supply","Colombiaseawater","Colombiasolar-trial-supply","Colombiatotal biomass","Colombiatraditional biomass","Colombiatrn_aviation_intl","Colombiatrn_aviation_intl-tfe","Colombiatrn_freight","Colombiatrn_freight-tfe","Colombiatrn_freight_road","Colombiatrn_pass","Colombiatrn_pass-tfe","Colombiatrn_pass_road","Colombiatrn_pass_road_LDV","Colombiatrn_pass_road_LDV_2W","Colombiatrn_pass_road_LDV_4W","Colombiatrn_shipping_intl","Colombiatrn_shipping_intl-tfe","Colombiaunconventional oil","Colombiaunconventional oil production","Colombiaurban processes","Colombiaurban processes-tfe","Colombiawater_td_an_C","Colombiawater_td_an_W","Colombiawater_td_dom_C","Colombiawater_td_dom_W","Colombiawater_td_elec_C","Colombiawater_td_elec_W","Colombiawater_td_ind_C","Colombiawater_td_ind_W","Colombiawater_td_irr_AmazonR_C","Colombiawater_td_irr_AmazonR_W","Colombiawater_td_irr_ColEcuaCst_C","Colombiawater_td_irr_ColEcuaCst_W","Colombiawater_td_irr_MagdalenaR_C","Colombiawater_td_irr_MagdalenaR_W","Colombiawater_td_irr_OrinocoR_C","Colombiawater_td_irr_OrinocoR_W","Colombiawater_td_irr_SAmerCstN_C","Colombiawater_td_irr_SAmerCstN_W","Colombiawater_td_pri_C","Colombiawater_td_pri_W","Colombiawholesale gas","Colombiawind-trial-supply","globalCorn","globalFiberCrop","globalFodderHerb","globalForest","globalMiscCrop","globalOilCrop","globalOtherGrain","globalOtherMeat_Fish","globalPalmFruit","globalResidue","globalRice","globalRoot_Tuber","globalScavenging_Other_Rsrc","globalSugarCrop","globalWheat","globalcoal","globalcrude oil","globalglobal solar resource","globallimestone","globalmisc emissions sources","globalnatural gas","globaloffshore carbon-storage","globaloil-credits","globaluranium"))
prj$data$`prices of all markets`<-price
saveProject(prj,"C:/Users/Juan Manuel Rincon R/Desktop/Colombia-RDM-Analysis/prices of all markets.proj")
