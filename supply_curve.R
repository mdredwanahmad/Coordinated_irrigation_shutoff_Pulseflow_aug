library(tidyverse)
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(Metrics)
##All irrigated crops grids of my study area
ALLCRPGRIDS<- read.csv("data/ALLCRPGRIDS.csv")[,c(2:14)]
ALLCRPGRIDS<- ALLCRPGRIDS %>% filter(!WRIA %in% c("Lower Yakima","Upper Yakima","Naches","Wenatchee"))

##Water right (WRTS) data which has overlayed with WSDA 2020 and WRIA file
Water_right<- read.csv("data/WSDA2020_PriorityDate_Final.csv") %>% 
  rename("FID_WSDA"="OBJECTID_1","lat"="join_lat", "long"="join_lon","crop"="Crop_Name","PriorityDate"="PriorityDa","WRIA"="WRIA_NM") %>% 
  mutate(LONG= gsub("-","",long),
         LAT= paste(lat,"N",sep = ""),
         LONG= paste(LONG,"W", sep = ""),
         site= paste(LAT,LONG,sep="")) %>% 
  dplyr::select(c("FID_WSDA","crop","PriorityDate","WRDocID","FieldAcres","Lat","Long","IntersectA","lat","long","site" ,"WRIA")) %>% 
  mutate(lat= formatC(lat, digits = 5, format = "f"),
         long= formatC(long, digits = 5, format = "f"))

#Filter out "-" pattern in priority date
Water_rightV1<- filter(Water_right, grepl("-",PriorityDate))
#Filter out "/" pattern in priority date
Water_rightV2<- filter(Water_right,!PriorityDate %in% Water_rightV1$PriorityDate)
#make "-" pattern into "/"
Water_rightV1<- filter(Water_right, grepl("-",PriorityDate)) %>% 
  separate(PriorityDate, into = c("Year","Month","Day"),sep = "-") %>% 
  mutate(Month= as.numeric(Month),
         Day=as.numeric(Day),
         PriorityDate= paste0(Month,"/",Day,"/",Year)) %>% 
  dplyr::select(c("FID_WSDA","crop","PriorityDate","WRDocID","FieldAcres","Lat","Long","IntersectA","lat","long","site" ,"WRIA"))

##merge both back inyo one
Water_right<- merge(Water_rightV1,Water_rightV2, all.x = TRUE, all.y = TRUE,sort = TRUE)

########
##Crops_grids_including_Field_ID_WSDA
crops_grids_FID_WSDA<- read.csv("data/crops_grids_FID_ID.csv") %>% 
  rename("crop"= "Crop_Name", "Acres_PF"="Acres") %>% 
  dplyr::select(c("FID_WSDA","crop","Acres_PF","lat","long","WRIA_ID1","WRIA","Grid_Number","region","Long","Lat","site")) %>% 
  mutate(lat= formatC(lat, digits = 5, format = "f"),
         long= formatC(long, digits = 5, format = "f"))

##Join both crops_grids file to get fields merged
## crops_grids data from "loading_data_and_boxplots.R" script
crops_grids_FID_WSDA1<- left_join(crops_grids_FID_WSDA,crops_grids) %>% 
  rename("LAT"="Lat","LONG"="Long") %>% 
  filter(WRIA %in% Water_right$WRIA)

##Join crops grids with priority date
crops_grids_V1<- left_join(crops_grids_FID_WSDA1,Water_right) %>% na.omit() %>% 
  dplyr::select(c("FID_WSDA","crop","Acres_PF","Acres","lat","long","Grid_Number","site","region","Crop_site_region","Irrigation","PriorityDate","WRDocID","Lat","Long")) %>% #"FieldAcres",
  rename("Field_lat"="Lat","Field_long"="Long")
##Calculate field_acres fraction of total grid acres
crops_grids_V1<- crops_grids_V1 %>% 
  mutate(fraction_acres= Acres_PF/Acres)


Interruptable_water_right<- read.csv("data/interruptibles.csv")
non_interruptable<- filter(crops_grids_V1, !WRDocID %in% Interruptable_water_right$WR_Doc_ID)

####################NON_INTERRUPTIBLE#####
##Removing interruptible fields by dis-aggregating data set into fields levels and again aggregating into grid level
## fifteendays_sc_t1 data from "loading_data_and_boxplots.R" script
NON_Interruptible <- inner_join(fifteendays_sc_t1 %>% dplyr::select("Year","crop","WM_yield_kgha","irrig","simulation","site","lat","long","region","Acres","irrig_optimal_mm","WM_yield_optimal_kgha","WM_yield_loss","total_WM_yield_kg","Total_economic_return_shut","Revenue_loss","sold_unit_opt","sold_unit_shut","streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft","streamflow_aug_cfs","Crop_site_region","Percentage_irrig","Per_irrrig_saved","Percentage_yield","Per_yield_reduction","target_harvest_date","harvest_date1"),non_interruptable %>% dplyr::select(c("crop","Acres_PF","Acres","fraction_acres","lat","long","site","Grid_Number","region","Crop_site_region","PriorityDate","WRDocID","Field_lat","Field_long"))) %>% 
  mutate(streamflow_aug_acft_fld=streamflow_aug_acrft*fraction_acres,
         streamflow_aug_cfs_fld= streamflow_aug_cfs*fraction_acres,
         streamflow_aug_mm_fld= streamflow_aug_mm*fraction_acres,
         streamflow_aug_ft_fld= streamflow_aug_ft*fraction_acres,
         yield_gain_field= total_WM_yield_kg*fraction_acres,
         yield_loss_fld= WM_yield_loss*fraction_acres,
         Revenue_loss_fld=Revenue_loss*fraction_acres,
         Total_economic_return_shut= Total_economic_return_shut*fraction_acres,
         cost_acft_fld= Revenue_loss_fld/streamflow_aug_acft_fld,
         cost_acre_fld= Revenue_loss_fld/Acres_PF,
         Per_yield_reduction_fld=Per_yield_reduction*fraction_acres) %>% 
  group_by(crop,region,site,lat,long,Grid_Number,Year, simulation,harvest_date1,target_harvest_date) %>% 
  summarise(Acres_PF= sum(Acres_PF, na.rm=T),
            Acres=mean(Acres, na.rm=T),
            streamflow_aug_mm = sum(streamflow_aug_mm_fld, na.rm=T),
            streamflow_aug_ft = sum(streamflow_aug_ft_fld, na.rm=T),
            streamflow_aug_acrft= sum(streamflow_aug_acft_fld, na.rm=T),
            streamflow_aug_cfs= sum(streamflow_aug_cfs_fld, na.rm=T),
            Total_Yield= sum(yield_gain_field, na.rm=T),
            WM_yield_loss= sum(yield_loss_fld, na.rm=T),
            Revenue_loss= sum(Revenue_loss_fld,na.rm=T),
            Total_economic_return_shut= sum(Total_economic_return_shut, na.rm=T),
            cost_acrft= Revenue_loss/streamflow_aug_acrft,
            cost_acr= Revenue_loss/Acres) %>% filter(simulation !="optimal")

##Calculate gridwise values using the function and then calculate streamflow augmentation using "Flow_AC1.R"
##Fifteendays
grid_non_interruptible<- grid_level_analysis(NON_Interruptible)


####Calculate marginal cost for each gage locations and create data set for supply curve####
##NFT_Dayton dataset
NFT_D_C<- NON_Interruptible %>% filter(site %in% TR_Dayton_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres","streamflow_aug_mm",
                  "streamflow_aug_ft","streamflow_aug_acrft","Revenue_loss",
                  "cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>%  
  filter(!simulation=="optimal") %>% mutate(SITENAME= "TR_Dayton")

##Calculate marginal cost using marginal cost calculation function from "req_functions.R"
marginal_nfd<- marginal_cost_calculation(NFT_D_C) %>% 
  mutate(SITENAME= "N.F. Touchet R. abv Dayton")

##Detour Rd ##WallaWalla Outlet
Detour_Rd_C<- NON_Interruptible %>% filter(site %in% WR_Detour_Rd_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres","streamflow_aug_mm",
                  "streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  filter(!simulation=="optimal1") %>% mutate(SITENAME= "WR_Detour_Rd")

##Calculate marginal cost using marginal cost calculation function from "req_functions.R"
marginal_Detour<- marginal_cost_calculation(Detour_Rd_C) %>% 
  mutate(SITENAME= "Walla Walla R.  E. Detour Rd.")

##Touchet_R_bolls
Touchet_R_C<- NON_Interruptible %>% filter(site %in% TR_Bolles_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres",
                  "streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  filter(!simulation=="optimal1") %>% mutate(SITENAME= "TR_Bolles")

#Calculate marginal cost using marginal cost calculation function from "req_functions.R"
marginal_Touchet<- marginal_cost_calculation(Touchet_R_C) %>% 
  mutate(SITENAME= "Touchet R. at Bolles")


##Okanogan River at Tonasket
Okn_Tonasket_R_C<- NON_Interruptible %>% filter(site %in% Tonasket_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres",
                  "streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  filter(!simulation=="optimal1") %>% mutate(SITENAME= "Tonasket")
##Marginal cost calculate by cost per ac-ft
marginal_Tonasket<- marginal_cost_calculation(Okn_Tonasket_R_C) %>% 
  mutate(SITENAME= "OKANOGAN RIVER NEAR TONASKET, WA")


##Similkameen River Okanogan
Okn_Similkameen_R_C<- NON_Interruptible %>% filter(site %in% Similkameen_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres",
                  "streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  filter(!simulation=="optimal1") %>% mutate(SITENAME= "Similkameen")
##Marginal cost calculate by cost per ac-ft
marginal_similkameen<- marginal_cost_calculation(Okn_Similkameen_R_C) %>% 
  mutate(SITENAME= "SIMILKAMEEN RIVER NEAR NIGHTHAWK, WA")


##Okanogan River at Malott ##Okanogan Outlet
Okn_Malott_R_C<- NON_Interruptible %>% filter(site %in% Malott_grids$site) %>% #df_tmp_malott$site1
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres",
                  "streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  filter(!simulation=="optimal1") %>% mutate(SITENAME= "Malott")

##Marginal cost calculate by cost per ac-ft
marginal_malott<- marginal_cost_calculation(Okn_Malott_R_C) %>% 
  mutate(SITENAME= "OKANOGAN RIVER AT MALOTT, WA")




##Methow River at Pateros ##Methow Outlet
Mtw_Pateros_R_C<- NON_Interruptible %>% filter(site %in% Pateros_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres",
                  "streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  filter(!simulation=="optimal1") %>% mutate(SITENAME= "Pateros")

##Marginal cost calculate by cost per ac-ft
marginal_pateros<- marginal_cost_calculation(Mtw_Pateros_R_C) %>% 
  mutate(SITENAME= "METHOW RIVER NEAR PATEROS, WA")

##Methow_River_at Twisp
Mtw_Twisp_R_C<- NON_Interruptible %>% filter(site %in% MR_Twisp_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres",
                  "streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  filter(!simulation=="optimal1") %>% mutate(SITENAME= "MR_Twisp")
##Marginal cost calculate by cost per ac-ft
marginal_twisp<- marginal_cost_calculation(Mtw_Twisp_R_C) %>% 
  mutate(SITENAME= "METHOW RIVER AT TWISP, WA")


##Twisp_River_Near Twisp
Twisp_Twisp_R_C<- NON_Interruptible%>% filter(site %in% TRN_Twisp_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres",
                  "streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  filter(!simulation=="optimal1") %>% mutate(SITENAME= "TRN_Twisp")
##Marginal cost calculate by cost per ac-ft
marginal_twispntwisp<- marginal_cost_calculation(Twisp_Twisp_R_C) %>% 
  mutate(SITENAME= "TWISP RIVER NEAR TWISP, WA")

####List all sites supply_curve data and combined them together
list_all_supply_curve_data<- list(marginal_nfd,marginal_Detour,marginal_Touchet,marginal_Tonasket,marginal_malott,marginal_pateros,marginal_twisp,marginal_twispntwisp,marginal_similkameen)

##Combined_dataframe and renaiming station name to make it shorten
combined_sites_suuply<- do.call(rbind,list_all_supply_curve_data) %>% 
  mutate(SITENAME=ifelse(SITENAME=="METHOW RIVER AT TWISP, WA","MR_Twisp",ifelse(SITENAME=="METHOW RIVER NEAR PATEROS, WA","Pateros",ifelse(SITENAME=="TWISP RIVER NEAR TWISP, WA","TRN_Twisp",ifelse(SITENAME=="OKANOGAN RIVER AT MALOTT, WA","Malott",ifelse(SITENAME=="OKANOGAN RIVER AT OROVILLE, WA","Oroville",ifelse(SITENAME=="OKANOGAN RIVER NEAR TONASKET, WA","Tonasket",ifelse(SITENAME=="SIMILKAMEEN RIVER NEAR NIGHTHAWK, WA","Similkameen",ifelse(SITENAME=="N.F. Touchet R. abv Dayton","TR_Dayton",ifelse(SITENAME=="Touchet R. at Bolles","TR_Bolles",ifelse(SITENAME=="Walla Walla R.  E. Detour Rd.","WR_Detour_Rd",0)))))))))))




###Create Supply Curve Figure 05
##2015 taken as an example year
##Panel a (Okanogan River at Malott)
site_sp<- combined_sites_suuply %>% 
  filter(SITENAME=="Malott") 
site_sp1<- site_sp %>% 
  filter(Year==2015)  

###Filter data frame for specific timeframe curve
#mayH2
mayH2<- filter(site_sp1,simulation=="Scenario02")
#julH1
julH1<- filter(site_sp1,simulation=="Scenario05")
#augH2
augH2<- filter(site_sp1,simulation=="Scenario08")

P1<- ggplot()+
  geom_line(mayH2 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="Scenario02"),lwd= 1.5)+
  geom_line(julH1 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="Scenario05"),lwd= 1.5)+
  geom_line(augH2 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="Scenario08"),lwd= 1.5)+
  theme_bw()+ scale_y_continuous("Marginal cost ($/ac-ft)",limits=c(0,1000), breaks=seq(0,1000,200))+
  scale_x_continuous("Streamflow augmentation (ac-ft)",limits=c(0,1500), breaks=seq(0,1500,300))+
  scale_color_manual(values = c(
    "Scenario02" = "#1F78B4",  # Blue
    "Scenario05" = "#FB9A99",  # Dark Blue
    "Scenario08" = "#FF7F00"   # Black
  ))+       
  labs(x= "Streamflow augmentation (ac-ft)", y= "Marginal cost ($/ac-ft)", title = paste0("Malott (a)"))+theme_bw()+ 
  theme(plot.title = element_text(size = 15),axis.title = element_blank(),
        strip.text = element_text(size = 20,face="bold"),
        axis.text.x = element_text(size = 22,color = "black",angle = 0, hjust=0.5),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.text = element_text(size=16, face="bold", color="black"), legend.title = element_blank(),legend.position = "right")
#Save supply curve
ggsave(P1, file="Figure/Malott_supply.png", width = 20, height = 15,dpi = 300, units = "cm")

##Panel b (Methow River at Pateros)
site_sp<- combined_sites_suuply %>% 
  filter(SITENAME=="Pateros") 
site_sp1<- site_sp %>% 
  filter(Year==2015) 

###Filter data frame for specific timeframe curve
#mayH2
mayH2<- filter(site_sp1,simulation=="Scenario02")
#julH1
julH1<- filter(site_sp1,simulation=="Scenario05")
#augH2
augH2<- filter(site_sp1,simulation=="Scenario08")


P1<- ggplot()+
  geom_line(mayH2 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="Scenario02"),lwd= 1.5)+
  geom_line(julH1 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="Scenario05"),lwd= 1.5)+
  geom_line(augH2 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="Scenario08"),lwd= 1.5)+
  theme_bw()+ scale_y_continuous("Marginal cost ($/ac-ft)",limits=c(0,1000), breaks=seq(0,1000,200))+
  scale_x_continuous("Streamflow augmentation (ac-ft)",limits=c(0,750), breaks=seq(0,750,150))+
  scale_color_manual(values = c(
    "Scenario02" = "#1F78B4",  # Blue
    "Scenario05" = "#FB9A99",  # Dark Blue
    "Scenario08" = "#FF7F00"   # Black
  ))+       
  labs(x= "Streamflow augmentation (ac-ft)", y= "Marginal cost ($/ac-ft)", title = paste0("Pateros (b)"))+theme_bw()+ 
  theme(plot.title = element_text(size = 15),axis.title = element_blank(),
        strip.text = element_text(size = 20,face="bold"),
        axis.text.x = element_text(size = 22,color = "black",angle = 0, hjust=0.5),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.text = element_text(size=16, face="bold", color="black"), legend.title = element_blank(),legend.position = "right")
#Save figure
ggsave(P1, file="Figure/Pateros_supply.png", width = 20, height = 15,dpi = 300, units = "cm")


##Panel c (Touchet River at Bolles)
site_sp<- combined_sites_suuply %>% 
  filter(SITENAME=="TR_Bolles") 
site_sp1<- site_sp %>% 
  filter(Year==2015) 

###Filter data frame for specific timeframe curve
#mayH2
mayH2<- filter(site_sp1,simulation=="Scenario02")
#julH1
julH1<- filter(site_sp1,simulation=="Scenario05")
#augH2
augH2<- filter(site_sp1,simulation=="Scenario08")


P1<- ggplot()+
  geom_line(mayH2 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="Scenario02"),lwd= 1.5)+
  geom_line(julH1 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="Scenario05"),lwd= 1.5)+
  geom_line(augH2 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="Scenario08"),lwd= 1.5)+
  theme_bw()+ scale_y_continuous("Marginal cost ($/ac-ft)",limits=c(0,1000), breaks=seq(0,1000,200))+
  scale_x_continuous("Streamflow augmentation (ac-ft)",limits=c(0,800), breaks=seq(0,800,200))+
  scale_color_manual(values = c(
    "Scenario02" = "#1F78B4",  # Blue
    "Scenario05" = "#FB9A99",  # Dark Blue
    "Scenario08" = "#FF7F00"   # Black
  ))+       
  labs(x= "Streamflow augmentation (ac-ft)", y= "Marginal cost ($/ac-ft)", title = paste0("TR_Bolles (c)"))+theme_bw()+ 
  theme(plot.title = element_text(size = 15),axis.title = element_blank(),
        strip.text = element_text(size = 20,face="bold"),
        axis.text.x = element_text(size = 22,color = "black",angle = 0, hjust=0.5),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.text = element_text(size=16, face="bold", color="black"), legend.title = element_blank(),legend.position = "right")
#Save figure
ggsave(P1, file="Figure/TR_Bolles_supply.png", width = 20, height = 15,dpi = 300, units = "cm")







##Stack bar chart associateed with crop information
######Creates acres data####

##NFT_Dayton
NFT_D_C<- NON_Interruptible %>% filter(site %in% TR_Dayton_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% mutate(SITENAME= "TR_Dayton")

##Detour Rd
Detour_Rd_C<- NON_Interruptible  %>% filter(site %in% WR_Detour_Rd_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% mutate(SITENAME= "WR_Detour_Rd")

##Touchet_R_bolls
Touchet_R_C<- NON_Interruptible %>% filter(site %in% TR_Bolles_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% mutate(SITENAME= "TR_Bolles")

##TOnasket
Okn_Tonasket_R_C<- NON_Interruptible  %>% filter(site %in% Tonasket_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% mutate(SITENAME= "Tonasket")

##Similkameen
Okn_Similkameen_R_C<- NON_Interruptible  %>% filter(site %in% Similkameen_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% mutate(SITENAME= "Similkameen")



##Malott
Okn_Malott_R_C<- NON_Interruptible %>% filter(site %in% Malott_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% mutate(SITENAME= "Malott")


##Pateros
Mtw_Pateros_R_C<- NON_Interruptible %>% filter(site %in% Pateros_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% mutate(SITENAME= "Pateros")


##Methow_River_at Twisp
Mtw_Twisp_R_C<- NON_Interruptible %>% filter(site %in% MR_Twisp_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% mutate(SITENAME= "MR_Twisp")

##Twisp_River_Near Twisp
Twisp_Twisp_R_C<- NON_Interruptible %>% filter(site %in% TRN_Twisp_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% mutate(SITENAME= "TRN_Twisp")


###List all sites acreas together
list_all_acres<- list(NFT_D_C,Detour_Rd_C,Touchet_R_C,Okn_Tonasket_R_C,Okn_Similkameen_R_C,Okn_Malott_R_C,Mtw_Pateros_R_C,Mtw_Twisp_R_C,Twisp_Twisp_R_C)
listed_crop<- c("Onion","Corn_grain")
combined_sites_acres<- do.call(rbind,list_all_acres) %>% 
  mutate(group= ifelse(crop %in% bmass, "Hay_crop",ifelse(crop %in% crp_y,"Annual_crop",0)),
         group1= ifelse(crop %in% bmass, "Hay crops", ifelse(!crop %in% listed_crop & crop %in% crp_y, "Grain crops", ifelse(crop %in% listed_crop, "Corn and onion",0))))

##Required modification in the dataset to create crop group
Acres_info<- combined_sites_acres %>% 
  group_by(SITENAME,group1) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% 
  group_by(SITENAME) %>% 
  mutate(Total_ac= sum(Acres, na.rm=T),
         Peercentage= round(Acres/Total_ac*100,digits = 1),
         group1= factor(group1,ordered = T, levels=c("Corn and onion","Grain crops","Hay crops")),
         Total_ac_v1= Acres/1000)
Acres_info_1<- Acres_info %>% dplyr::select(c("SITENAME","group1","Acres" )) %>% 
  pivot_wider(names_from = group1, values_from = Acres)


#Ensure stackbar folder exist
if (!dir.exists("Figure/stackbar")) {
  dir.create("Figure/stackbar", recursive = TRUE)
}

###loop to create figure for all stations
sites<- unique(Acres_info$SITENAME)
for ( i in 1: length(sites)){
  count_yr<- filter(Acres_info, SITENAME==sites[i] ) #%>% filter(group1 !="Annual crops" ) #sites[i]
  
  ##Create plot
  P1<- ggplot(count_yr, aes(x="", y=Peercentage, fill=group1)) +
    geom_bar(width = 0.5, stat = "identity") +  # Stacked bar chart
    scale_fill_manual(values = c("Hay crops" = "#C5C6C7",
                                 "Grain crops" = "#CBC1AE",
                                 "Corn and onion" = "#6e6f71")) +
    theme_bw() +
    labs(x=NULL, y=NULL, title=sites[i]) +  # Remove axis labels
    theme(plot.title = element_blank(),
          axis.text = element_blank(),  # Remove axis text
          axis.ticks = element_blank(), # Remove axis ticks
          panel.grid = element_blank(), # Remove grid lines
          panel.border = element_blank(), # Remove border
          legend.position = "top"
    ) +coord_flip()
  
  ##Save the plot
  ggsave(P1, file=paste0("Figure/stackbar/",sites[i],"Per_Ac_nl_v1.png"), width = 20, height = 5,dpi = 300, units = "cm")
  
  #Create plot writing percentage on it
  P1<- ggplot(count_yr, aes(x="", y=Peercentage, fill=group1)) +
    geom_bar(width = 0.5, stat = "identity") +  # Stacked bar chart
    scale_fill_manual(values = c("Hay crops" = "#C5C6C7",
                                 "Grain crops" = "#CBC1AE",
                                 "Corn and onion" = "#6e6f71")) +
    theme_bw() +
    labs(x=NULL, y=NULL, title=sites[i]) +  # Remove axis labels
    theme(plot.title = element_blank(),
          axis.text = element_blank(),  # Remove axis text
          axis.ticks = element_blank(), # Remove axis ticks
          panel.grid = element_blank(), # Remove grid lines
          panel.border = element_blank(), # Remove border
          legend.position = "top"
    ) +coord_flip()+geom_text(aes(label = Peercentage), position = position_stack(vjust = 0.5), size = 12, color="black")
  
  #Save the plot
  ggsave(P1, file=paste0("Figure/stackbar/",sites[i],"Per_Ac_V2.png"), width = 20, height = 5,dpi = 300, units = "cm")
  
}

