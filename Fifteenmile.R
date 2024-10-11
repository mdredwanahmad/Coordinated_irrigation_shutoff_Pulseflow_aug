library(tidyverse)
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(Metrics)


###Fifteenmile crops grids##
crops_grids_fifteenmile<- read.csv("data/fifteenmile_crops_selected_V1.csv") %>% 
  rename("crop"="Crop_Name","WR_DOC_ID"="WR_DOC_ID..Dummy.","CropArea"="CropAcres") %>% 
  left_join(Irrig_eff %>% dplyr::select("Irrigation","Efficiency","ML_Fraction")) %>%
  left_join(Moisture_content %>% dplyr::select("crop","WM_ML_factor")) %>% 
  mutate(region= "Fifteenmile")


###Optimal_fifteenmile
###Optimal Irrigation###
opt_fiftn_mile<- fread("data/CorpSyst_output/Fifteenmile/optimal/result.dat", fill = TRUE, nrows = Inf) %>%
  rename("Date" = "YYYY-MM-DD(DOY)") %>% 
  separate(Date, into= c("Year", "Month","Day"), sep="-") %>%
  mutate(simulation= "optimal")%>% 
  dplyr::select(c("Year","planting_date","harvest_date", "yield", "used_biomass", "irrig", "region", "crop", "site","simulation"))
annual_crops<- filter(opt_fiftn_mile, crop %in% crp_y)
annual_crops$yield_kgha<- annual_crops$yield
Hay_crops<- filter(opt_fiftn_mile, crop %in% bmass)
Hay_crops$yield_kgha<- Hay_crops$used_biomass
opt_fiftn_mile<- merge(Hay_crops,annual_crops, all.x = TRUE, all.y = TRUE,sort = TRUE) %>% 
  left_join(crops_grids_fifteenmile %>% dplyr::select("crop","site","region","Irrigation","ML_Fraction","WM_ML_factor")) %>% 
  mutate(irrig= irrig*ML_Fraction,
         WM_yield_kgha= yield_kgha*WM_ML_factor)


##rearranging and Manipulating data
opt_fiftn_mile_ir<- opt_fiftn_mile%>% 
  mutate(Year= as.numeric(Year),
         irrig_optimal_mm= irrig,
         DM_yield_optimal_kgha= yield_kgha,
         WM_yield_optimal_kgha=WM_yield_kgha)


####Joining dataframe to get the acreages information####
opt_fiftn_mile_ir1<- opt_fiftn_mile_ir %>% 
  dplyr::select(Year,planting_date,harvest_date,crop,yield_kgha,WM_yield_kgha,irrig,region,site,simulation,irrig_optimal_mm, DM_yield_optimal_kgha,WM_yield_optimal_kgha,Irrigation,ML_Fraction,WM_ML_factor) %>% 
  left_join(crops_grids_fifteenmile %>%  dplyr::select(crop, Grid_Number, site,Acres,CropArea,Irrigation, region,HUC10_NAME, lat, long,WR_DOC_ID)) %>% 
  mutate(area_ha_opt= Acres*0.405,
         total_WM_yield_optimal_kg= WM_yield_kgha*area_ha_opt,
         total_irrig_mm_opt= irrig*area_ha_opt,0,
         Crop_site_region= paste0(crop,"_",site,"_",region),
         harvest_date= as.numeric(substr(harvest_date, 5,7))) %>% na.omit() %>% 
  mutate(sold_unit_opt=ifelse(crop %in% bushels,(total_WM_yield_optimal_kg/25.40),
                              ifelse(crop %in% tons, (total_WM_yield_optimal_kg*0.001),
                                     ifelse(crop %in% cwt,(total_WM_yield_optimal_kg*0.019),0)))) %>% 
  left_join(crop_price1) %>% 
  mutate(Total_economic_return_opt=sold_unit_opt*mean_Price) %>% filter(!crop %in% c("Alfalfa_Seed",'Rye'))




##################################
##### Fifteen Days shutdown  #####
##################################

##Counting folders
filesfolder<- list.files("data/CorpSyst_output/Fifteenmile/15_days/")
path<- "data/CorpSyst_output/Fifteenmile/15_days/"

###Reading all data files for the shutdown scenarios (oneweek/10/15 days)
fifteendays_sc_15mile <- read_data(path,filesfolder) %>%
  left_join(crops_grids_fifteenmile %>% dplyr::select("crop","site","region","Irrigation","ML_Fraction","WM_ML_factor")) %>% 
  mutate(irrig= irrig*ML_Fraction,
         WM_yield_kgha= yield_kgha*WM_ML_factor) %>% na.omit() 

##Combining optimal dataframe with fifteendays_sc
fifteendays_sc_15mile<- rbind(fifteendays_sc_15mile,opt_fiftn_mile) %>% 
  mutate(Year= as.numeric(Year),
         Crop_site_region= paste0(crop,"_",site,"_",region)) %>% 
  filter(Crop_site_region %in% opt_fiftn_mile_ir1$Crop_site_region) #%>% filter(Year==2015)



##Percentage Yield and irrigation calculation and weighting with the area
##Weightening/converting/shaping all variables into required using function written in the "req_functions.R" script
fifteendays_sc_15mile_t <- Compute_irrigation_n_yield_loss_15_mile(fifteendays_sc_15mile) %>% na.omit() 

#removing the missing value and setting the yield percentage as 100 if its slightly higher , and if the percentage yield is higher (should be less than 1% total dataset)
fifteendays_sc_15mile_t1<- fifteendays_sc_15mile_t %>%   
  na.omit() %>% 
  mutate(Percentage_yield= ifelse(Percentage_yield>100,100,Percentage_yield),
         streamflow_aug_acrft= ifelse(streamflow_aug_acrft<0.01,0,streamflow_aug_acrft),
         WM_yield_loss= ifelse(WM_yield_loss<0,0,WM_yield_loss),
         Revenue_loss= ifelse(Revenue_loss<0,0,Revenue_loss),
         Revenue_loss= ifelse(Revenue_loss<1,0,Revenue_loss),
         cost_acrft = (Revenue_loss/streamflow_aug_acrft),
         cost_acr = (Revenue_loss/Acres),
         Crop_site_region= paste0(crop,"_",site,"_",region)) %>% filter(simulation != "optimal", !crop %in% c("Alfalfa_Seed","Rye"))

#####Calculating marginal cost########

##Create dataframe
#if any harvest date is a withing first five days of irrigation shutoof, farmers in reality will shuttoff irrigation automatically so to account these we are filtering out these rows which have harvest within first 5 days of shutoff
data_marginal_15mile<- fifteendays_sc_15mile_t1 %>%  #filter(site %in% Contributing_grids) %>% 
  dplyr::select(c("region","Year","crop","planting_date","harvest_date","site","lat","long","simulation","Acres",
                  "streamflow_aug_acrft","streamflow_aug_cfs","Percentage_irrig",
                  "Per_irrrig_saved","Percentage_yield","Per_yield_reduction" ,"Revenue_loss","cost_acrft","cost_acr")) %>% 
  mutate(harvest_date=as.numeric(substr(harvest_date, 5,7))) %>% filter(harvest_date>180)


###Marginal cost calculation to create table 04 and figure 09
###Run the function 
marginal_cost_15mile<- marginal_cost_calculation(data_marginal_15mile)


##Calculate cost to create table
#https://www.rrnw.org/wp-content/uploads/2016Barter.pdf 
#2.5 cfs = (2.5*24*3600*15)/43560 = 74.38 ac-ft (option 1)
#1.39 cfs = (1.39*24*3600*15)/43560 = 41.36 ac-ft (option 2)
#total 74.38+41.36= 115.74 ac-ft (option 1+2)
data1<- marginal_cost_15mile %>% filter(Year==2015,cum_streamflow<=131)
cost<- 17.87*115.74/sum(data1$Acres) ##Need to adjust exact acres
print(cost)


###Create Supply Curve########### Figure 09
#Filter out supply curve data for 2015
data1<- marginal_cost_15mile %>% filter(Year %in% c(2015))
#Fiter out the amount required to meet in Fifteenmile creek in 2015
option2_1<- data1 %>% filter(cum_streamflow<131)
x21<- 115.74
y21<- max(option2_1$cost_acrft)


P1<- ggplot()+geom_line(data1 , mapping=aes(x=cum_streamflow , y=cost_acrft),color="#33A02C",size=1.5)+
  geom_point(option2_1,mapping=aes(x=x21, y=y21),color="#FF7F00", size=6)+
  theme_bw()+scale_y_continuous("Marginal cost ($/ac-ft)",limits=c(0,1000), breaks=seq(0,1000,200))+
  scale_x_continuous("Streamflow augmentation (ac-ft)",limits=c(0,800), breaks=seq(0,800,100))+
  labs(x= "Streamflow augmentation (ac-ft)", y= "Marginal cost ($/ac-ft)", title = paste0())+theme_bw()+ 
  theme(plot.title = element_text(size = 15),axis.title = element_text(size = 22,color = "black"), 
        strip.text = element_text(size = 20,face="bold"),
        axis.text.x = element_text(size = 22,color = "black",angle = 0, hjust=0.5),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.text = element_text(size=16, face="bold", color="black"), legend.title = element_blank(),legend.position = "right")

ggsave(P1, file="Figure/Fifteen_2015.png", width = 20, height = 15,dpi = 300, units = "cm")
