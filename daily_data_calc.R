###daily data calculations
##Simulation start and end_date is required to summarized all shutoff scenarios
#so loading this dataframe which has all of these information
end_date<- read.csv("D:/Coordinate_shutdown/Scenario_end_date.csv") %>% filter(str_detect(simulation,"^Scenario"))

##Read optimal (full irrigation) daily data
daily_data<- fread("D:/Coordinate_shutdown/Results of model run/NEW_Recalibrated/After_Recalibration/optimal/daily_result.dat", 
                   fill = TRUE, nrows = Inf) %>% 
  rename("irrig" = "Irrigation cum(timestep)") %>% 
  rename("DOY"= "DOY ") %>% 
  dplyr::select(c("Year","DOY" ,"irrig", "region","crop","site")) %>% 
  filter(crop %in% crp_all) %>% #Filtering out by list of crops for the analysis
  filter(DOY %in% c(55:241)) %>% #Filtering out irrigation events wthing this time frame
  filter(irrig > 0) %>% ##We need only rows where irrigation events happened 
  mutate(simulation= "optimal") %>% 
  left_join(crops_grids %>% dplyr::select("crop","site","region","Irrigation","ML_Fraction","WM_ML_factor")) %>% 
  mutate(irrig1= irrig*ML_Fraction) %>% 
  left_join(optimal_ir %>% dplyr::select("Year","region","crop","site","harvest_date")) %>% 
  mutate(harvest_date= as.numeric(substr(harvest_date, 5,7)))


###run function to findout irrigation requirements for each simulation timeframe
scenarios<- unique(end_date$simulation)
path<- "D:/Coordinate_shutdown/Results of model run/NEW_Recalibrated/After_Recalibration/Scheduled/data_daily/"
irrigation_demand<- Shutoff_irrig(scenarios,path)

##Required modification to account harvest date irrigation shutoff periods
#in real field, farmers usually stop appyling irrigation around a week prior to the harvest date, but model doesn't, therefore, we have accounted it here coinciding the harvest date, we put the irrigation value zero if the harvest date withing first five days of the shutoff period.
irrigation_demand1<- irrigation_demand %>% left_join(end_date %>% dplyr::select(c("simulation","start_date"))) %>% 
  filter(!region %in% c("Yakima","Naches","Wenatchee")) %>% 
  mutate(target_harvest_date= start_date+5, #adding column to check the harvest date reality
         irrig2= ifelse(harvest_date>= start_date & harvest_date <= target_harvest_date,0,irrig1)) %>% 
  group_by(Year,site,crop,region,simulation,target_harvest_date) %>% 
  summarise(irrigation_demand_mm = sum(irrig2))

# Ensure subfolders exist; if not, create them
# if (!dir.exists("data/CropSyst_output")) {
#   dir.create("data/CropSyst_output", recursive = TRUE)
# }

#write the daily summarised data file for further use
#write.csv(irrigation_demand1, "data/CorpSyst_output/combined_daily.csv")
