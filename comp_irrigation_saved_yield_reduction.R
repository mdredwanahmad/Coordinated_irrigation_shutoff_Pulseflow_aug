library(tidyverse)
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(Metrics)
#########
#load moisture content data to convert dry matter to wet matter
Moisture_content<- read.csv("data/Moisture_content.csv")
###Irrigation Efficiency##
Irrig_eff<- read.csv("data/Irrigation efficiency.csv")

##Grain and Hay Crops used in the simulation
#Grain crops
crp_y<- c("Corn_grain", "Spring_wheat", "Sweet_Corn", "Winter_wheat", "Alfalfa_Seed",  
          "Barley_Spring","Canola","Oats","Onion","Triticale","Yellow_Mustard")
#Hay crops
bmass<- c("Alfalfa_Hay","Barley_Hay", "Grass_Hay", "Sudangrass","Triticale_Hay",
          "Oats_hay", "Rye", "Timothy")


###Crop_grids_acreages information for the simulation sourced from WSDA 2020 CDL
crops_grids<- read.csv("data/crops_grids_Newfile.csv") %>% 
  left_join(Irrig_eff %>% dplyr::select("Irrigation","Efficiency","ML_Fraction")) %>% 
  left_join(Moisture_content %>% dplyr::select("crop","WM_ML_factor")) %>% 
  mutate(lat= formatC(lat, digits = 5, format = "f"),
         long= formatC(long, digits = 5, format = "f")) %>% filter(region %in% c("WallaWalla","Okanogan","Methow"))



##Crop_price data from USDA_NASS_QUICK_STATS (quickstats.nass.usda.gov/#FFECA65E-FAAD-354D-B567-4FF1308F78BD)
####Optimal Price plot has been created after DR_15
#Sweet_corn price has been replaced with 2019-2022 mean(34.2,51.3,35.07,45.46)=34.2, this data got from USDA Washington Outlook , recommemnded by Dr. Mike (pdf)
Crop_price<- fread("data/Price_m_for_all_new_V2.csv") 
crop_price1<- Crop_price %>% 
  dplyr::select(c("crop","Mean_price_2020_2023","Mean_Price_of_all_time_1979_2023")) %>% 
  rename("mean_Price"="Mean_price_2020_2023", "mean_price_bef"="Mean_Price_of_all_time_1979_2023")


##Selling units for simulated crops according to NASS STATS 
bushels<-c("Barley_Spring","Corn_grain","Oats","Spring_wheat","Winter_wheat","Alfalfa_Seed","Triticale")
tons<- c("Alfalfa_Hay","Grass_Hay","Oats_hay","Timothy","Barley_Hay","Sudangrass","Triticale_Hay","Rye")
cwt<- c("Canola","Yellow_Mustard","Sweet_Corn","Onion")



###Load optimal dataset with all crops year and grids combination
optimal<- fread("data/CorpSyst_output/optimal/result.dat", fill = TRUE, nrows = Inf) %>%
  rename("Date" = "YYYY-MM-DD(DOY)") %>% 
  separate(Date, into= c("Year", "Month","Day"), sep="-") %>%
  mutate(simulation= "optimal")%>% 
  dplyr::select(c("Year","planting_date","harvest_date", "yield", "used_biomass", "irrig", "region", "crop", "site","simulation"))
annual_crops<- filter(optimal, crop %in% crp_y) ##Filter out grain crops tocombine both yield in a column
annual_crops$yield_kgha<- annual_crops$yield
Hay_crops<- filter(optimal, crop %in% bmass) ##Filter out hay crops tocombine both yield in a column
Hay_crops$yield_kgha<- Hay_crops$used_biomass
##Combine both again into one
optimal<- merge(Hay_crops,annual_crops, all.x = TRUE, all.y = TRUE,sort = TRUE) %>% 
  left_join(crops_grids %>% dplyr::select("crop","site","region","Irrigation","ML_Fraction","WM_ML_factor")) %>% 
  mutate(irrig= irrig*ML_Fraction,
         WM_yield_kgha= yield_kgha*WM_ML_factor) %>% filter(irrig>0) %>% na.omit()


##rearranging and Manipulating data
optimal_ir<- optimal%>% 
  mutate(Year= as.numeric(Year),
         irrig_optimal_mm= irrig,
         WM_yield_optimal_kgha=WM_yield_kgha)


####Joining dataframe to get the acreages information####
optimal_ir1<- optimal_ir %>% 
  dplyr::select(Year,crop,planting_date,harvest_date,yield_kgha,WM_yield_kgha,irrig,region,site,simulation,irrig_optimal_mm, WM_yield_optimal_kgha,Irrigation,ML_Fraction,WM_ML_factor) %>% 
  left_join(crops_grids %>%  dplyr::select(crop, Grid_Number, site,Acres,CropArea,Irrigation, region, lat, long,WR_DOC_ID)) %>% 
  mutate(area_ha_opt= Acres*0.405,
         total_WM_yield_optimal_kg= WM_yield_kgha*area_ha_opt,
         total_irrig_mm_opt= irrig*area_ha_opt,
         Crop_site_region= paste0(crop,"_",site,"_",region,"_",Year),
         harvest_date= as.numeric(substr(harvest_date, 5,7))) %>% na.omit() %>% 
  mutate(sold_unit_opt=ifelse(crop %in% bushels,(total_WM_yield_optimal_kg/25.40),
                              ifelse(crop %in% tons, (total_WM_yield_optimal_kg*0.001),
                                     ifelse(crop %in% cwt,(total_WM_yield_optimal_kg*0.019),0)))) %>% 
  left_join(crop_price1) %>% 
  mutate(Total_economic_return_opt=sold_unit_opt*mean_Price) %>% distinct() %>% filter(!crop %in% c("Alfalfa_Seed","Rye"))



##################################
##### Fifteen Days shutdown  #####
##################################

##Counting folders
filesfolder<- list.files("data/CorpSyst_output/DR15/")
#specifying path to load dataset
path<- "data/CorpSyst_output/DR15/"

###Reading all data files for the 15 days shutdown scenarios 
fifteendays_sc <- read_data(path,filesfolder) %>%
  left_join(crops_grids %>% dplyr::select("crop","site","region","Irrigation","ML_Fraction","WM_ML_factor")) %>% 
  mutate(irrig= irrig*ML_Fraction,
         WM_yield_kgha= yield_kgha*WM_ML_factor) %>% filter(irrig>0)

##Combining optimal dataframe with fifteendays_sc
fifteendays_sc<- rbind(fifteendays_sc,optimal) %>% 
  mutate(Year= as.numeric(Year),
         Crop_site_region= paste0(crop,"_",site,"_",region,"_",Year)) %>% 
  filter(Crop_site_region %in% optimal_ir1$Crop_site_region) %>% distinct()


##Percentage Yield and irrigation calculation and weighting with the area
##Weightening/converting/shaping all variables into required using function written in the "req_functions.R" script
fifteendays_sc_t <- Compute_irrigation_n_yield_loss(fifteendays_sc) %>% na.omit() 

##We can get irrigation demand fro each shutoff period by substracting irrigation demand of optimal and shutoff irrigation demand from seasonal CropSyst output file, and "fifteendays_sc_t" dataset does that. However, to account harvest and shutoff time conincdence  we need to calculate from daily file , which has been done in "daily_data_calc.R"  script, but the daily data is too large in size, so reading the summarised dataset here again. 
irrigation_demand1<- read.csv("data/CorpSyst_output/Processed_CropSyst_output.csv")
#removing the missing value and setting the yield percentage as 100 if its slightly higher , and if the percentage yield is higher (should be less than 1% total dataset)
fifteendays_sc_t1<- fifteendays_sc_t %>%   
  na.omit() %>% 
  mutate(Percentage_yield= ifelse(Percentage_yield>100,100,Percentage_yield),
         streamflow_aug_acrft= ifelse(streamflow_aug_acrft<0.01,0,streamflow_aug_acrft),
         WM_yield_loss= ifelse(WM_yield_loss<0,0,WM_yield_loss),
         Revenue_loss= ifelse(Revenue_loss<0,0,Revenue_loss),
         Revenue_loss= ifelse(Revenue_loss<1,0,Revenue_loss),
         Cost_acre_ft= (Revenue_loss/streamflow_aug_acrft),
         Cost_acre= (Revenue_loss/Acres),
         Crop_site_region= paste0(crop,"_",site,"_",region)) %>% 
  left_join(irrigation_demand1) %>% 
  mutate(harvest_date1= as.numeric(substr(harvest_date, 5,7)),
         irrigation_demand_mm= ifelse(is.na(irrigation_demand_mm), 0,irrigation_demand_mm), #replacing NA with zero, see explanation in below
         streamflow_aug_acrft= irrigation_demand_mm*0.00328084*Acres,
         streamflow_aug_ft3= (irrigation_demand_mm*0.00328084*area_ft2),
         streamflow_aug_cfs= (streamflow_aug_ft3/(3600*24*15)),
         Cost_acre_ft= (Revenue_loss/streamflow_aug_acrft),
         Cost_acre= (Revenue_loss/Acres))

##NA value has been replaced with zero in the column "irrigation_demand_mm", this colun came from daily summarized dataset "irrigation_demand1", There are some years, crops, and simulation time frame, when there is no irrigation events.  So when we filtered out irrigation events only higher than zeero in daily file, some of these combination dropped from the dataset, and while joining this with the seasonal calculated file "fifteendays_sc_t1" it returns NA value, but if we calculate from seasonal file, when there is no irrigation in a shutoff periods, we gets a zero insted of NA, so replaced the NA value with zero. 


###Filter out required datset to create box plots
fifteendays_sc_stn<- fifteendays_sc_t1 %>% filter(site %in% all_sites,simulation != "optimal") %>% 
  mutate(crop= ifelse(crop=="Oats_hay","Oats_Hay",ifelse(crop=="Barley_Spring","Barley_Grain",ifelse(crop=="Spring_wheat", "Spring Wheat", ifelse(crop=="Winter_wheat","Winter Wheat",ifelse(crop=="Corn_grain","Corn Grain", ifelse(crop=="Alfalfa_Hay", "Alfalfa Hay",crop)))))),
         crop= factor(crop, ordered = TRUE, levels= c("Grass_Hay","Oats_Hay","Timothy","Barley_Hay","Sudangrass","Alfalfa Hay","Triticale_Hay","Rye","Oats","Canola","Yellow_Mustard","Spring Wheat","Winter_Wheat","Triticale","Sweet_Corn","Barley_Grain","Corn Grain","Alfalfa_Seed","Onion"))) %>% filter(!crop %in% c("Timothy","Rye","Alfalfa_Seed")) %>% 
  mutate(yield_loss_acre = WM_yield_loss/Acres,
         Production_loss_acre = (sold_unit_opt-sold_unit_shut)/Acres,  #based on sold unit
         Economic_return_ac = Total_economic_return_shut/Acres,
         Revenue_loss_acre = Revenue_loss/Acres,
         Production_acre= sold_unit_shut/Acres) %>% filter(crop %in% c("Alfalfa Hay","Corn Grain","Spring Wheat","Onion")) %>% 
  mutate(simulation_abb= case_when(simulation == "Scenario01" ~ "S1",
                                   simulation == "Scenario02" ~ "S2",
                                   simulation == "Scenario03" ~ "S3",
                                   simulation == "Scenario04" ~ "S4",
                                   simulation == "Scenario05" ~ "S5",
                                   simulation == "Scenario06" ~ "S6",
                                   simulation == "Scenario07" ~ "S7",
                                   simulation == "Scenario08" ~ "S8"))




###Create Plots for Figure 04

# Ensure Figure folder exist
if (!dir.exists("Figure")) {
dir.create("Figure", recursive = TRUE)
}

##Filtering out data again to remove if any yield reduction or irrigation saved lower than 0 or 40, also cost_acre is usually lower than 1000, except some outliers, so removing this
#Alfalfa<- fifteendays_sc_stn %>% filter(crop==crops[i]) %>% filter(Per_irrrig_saved>= 0 & Per_irrrig_saved <= 40)
crop_sp_data<- fifteendays_sc_stn %>% filter(Per_yield_reduction>=0 & Per_yield_reduction<=40)

median_values <- crop_sp_data %>% 
  group_by(crop, simulation_abb) %>%
  summarize(Cost_acre = median(Cost_acre, na.rm = TRUE),
            Per_irrrig_saved= median(Per_irrrig_saved, na.rm=T),
            Per_yield_reduction= median(Per_yield_reduction, na.rm=T), 
            streamflow_aug_mm = median(streamflow_aug_mm, na.rm=T))

##Create required plots
P1<- ggplot(crop_sp_data, aes(x=simulation_abb,y= streamflow_aug_mm,fill=simulation_abb))+ 
  geom_boxplot(outlier.shape = NA)+ facet_wrap(~crop, ncol = 4, nrow = 1)+
  stat_boxplot(geom='errorbar')+
  geom_line(median_values, mapping=aes(x = simulation_abb, y = streamflow_aug_mm, group=1), 
            size = 0.75)+
  scale_fill_brewer(palette= "Paired")+
  scale_y_continuous(" ",limits=c(0,250), breaks=seq(0,250, by=50))+ #there are no outliers above this range in irrigation demand
  labs(x= "", y= " ", title = paste0(" "))+theme_bw()+ 
  theme(plot.title = element_text(size = 20),axis.title = element_blank(), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.position = "none",axis.text.y.right = element_blank(),
        strip.text = element_text(size=22,color = "black"))
##save figure to Figure folder
ggsave(P1, file="Figure/streamflow_aug_mm.png", width = 25, height = 12,dpi = 300, units = "cm")



P1<- ggplot(crop_sp_data, aes(x=simulation_abb,y= Per_yield_reduction,fill=simulation_abb))+ 
  geom_boxplot(outlier.shape = NA)+ facet_wrap(~crop, ncol = 4, nrow = 1)+
  stat_boxplot(geom='errorbar')+
  geom_line(median_values, mapping=aes(x = simulation_abb, y = Per_yield_reduction, group=1), 
            size = 0.75)+
  scale_fill_brewer(palette= "Paired")+
  scale_y_continuous(" ",limits=c(0,40), breaks=seq(0,40, by=10))+ #there are no outliers above this range in irrigation demand
  labs(x= "", y= " ", title = paste0(" "))+theme_bw()+ 
  theme(plot.title = element_text(size = 20),axis.title = element_blank(), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.position = "none",axis.text.y.right = element_blank(),
        strip.text = element_text(size=22,color = "black"))
##Save the plot
ggsave(P1, file="Figure/Per_yield_reduction.png", width = 25, height = 12,dpi = 300, units = "cm")

### Create panel (c)
crop_sp_data<- fifteendays_sc_stn %>% filter(Cost_acre<=1000) 

median_values <- crop_sp_data %>% 
  group_by(crop, simulation_abb) %>%
  summarize(Cost_acre = median(Cost_acre, na.rm = TRUE),
            Per_irrrig_saved= median(Per_irrrig_saved, na.rm=T),
            Per_yield_reduction= median(Per_yield_reduction, na.rm=T), 
            streamflow_aug_mm = median(streamflow_aug_mm, na.rm=T))

P1<- ggplot(crop_sp_data, aes(x=simulation_abb,y= Cost_acre,fill=simulation_abb))+ #Cost_acre
  geom_boxplot(outlier.shape = NA)+ facet_wrap(~crop, ncol = 4, nrow = 1)+
  stat_boxplot(geom='errorbar')+
  geom_line(median_values, mapping=aes(x = simulation_abb, y = Cost_acre, group=1), 
            size = 0.75)+
  scale_fill_brewer(palette= "Paired")+
  scale_y_continuous(" ",limits=c(0,1000), breaks=seq(0,1000, by=200))+ #there are no outliers above this range in irrigation demand
  labs(x= "", y= " ", title = paste0(" "))+theme_bw()+ 
  theme(plot.title = element_text(size = 20),axis.title = element_blank(), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.position = "none",axis.text.y.right = element_blank(),
        strip.text = element_text(size=22,color = "black"))
##Save the plot
ggsave(P1, file="Figure/Cost_acre.png", width = 25, height = 12,dpi = 300, units = "cm")


