library(tidyverse)
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(Metrics)
library(dataRetrieval)
##Ensure you are still in same working directory
getwd()

###Load all the gauge stations list
gauges_and_VIC<- read.csv("data/study_area_gauges_VIC.csv") %>% 
  dplyr::select(c("SITENO","SITENAME", "LONGDD", "LATDD","SITEURL","join_lat","join_lon")) %>% 
  mutate(vic_lat= as.character(join_lat),
         vic_long= as.character(join_lon))
#R was converting into mathematical value, so rewriting again
gauges_and_VIC$SITENO[gauges_and_VIC$SITENO == "3.20E+51"] <- "32E050"

##Load all MIFR gauge_name
#Middle_okanogan-12445000 (Corrected), Similkameen-12439500
MIFR_gauges<- read.csv("data/MIFR_USGS_gauge.csv")
MIFR_gauges$SITENO[MIFR_gauges$SITENO == "3.20E+51"]<- "32E050" ##as it was converting into scientific



###Instreamflow rules###
##DOY and Month dataframe required to convert MIFR 15 days values to daily values
DOY_Month<- read.csv("data/Month_day_DOY.csv")

##Load all watersheds MIFR
#Methow Watershed
methow<- read.csv("data/Methow.csv")
methow<- melt(methow, id= c("Month","Start_date","End_date"))
#Okanogan Watershed
Okanogan<- read.csv("data/Okanogan.csv")
Okanogan<- melt(Okanogan, id= c("Month","Start_date","End_date"))
#WallaWalla Watershed
WallaWalla<- read.csv("data/WallaWalla.csv")
WallaWalla<- melt(WallaWalla, id= c("Month","Start_date","End_date"))
#Wenatchee Watershed
Wenatchee<- read.csv("data/Wenatchee.csv")
Wenatchee<- melt(Wenatchee, id= c("Month","Start_date","End_date"))


##List and join all instreamflow rules together
instream<- list(methow,Okanogan,WallaWalla,Wenatchee)
instreamflow_rules<- do.call(rbind,instream) %>% 
  rename("MIFR_SITE_NAME"="variable","MIFR_rules"="value") %>% 
  left_join(MIFR_gauges, by=c("MIFR_SITE_NAME"="MIFR_NAME")) %>% 
  mutate(Condition= ifelse(grepl("(Closure)",MIFR_rules),"Closure","Open"),
         MIFR_rules= str_replace(MIFR_rules," \\s*\\([^\\)]+\\)",""),
         MIFR_rules= gsub(',',"",MIFR_rules),
         MIFR_rules= as.numeric(MIFR_rules)) %>% 
  left_join(DOY_Month) %>% 
  filter(DOY %in% c(120:240))


###Streamflow discharge data can be download using the function in the "req_functions.R", 
#I have downloaded all saved into a file as reading it here from csv file
streamflow_data<- read.csv("data/combined34_USGS_DE1.csv")[2:8]

####calculate stream flow for targeted locations outlet for each stations
#Contributing grids informations are from 'req_functions.R" script
##NFT_Dayton
dayton_downstream_calc<- grid_non_interruptible %>% filter(site %in% TR_Dayton_grids$site) %>% 
  group_by(region,Year,simulation) %>% 
  summarise(Streamflow_aug_vol_acft= sum(Streamflow_aug_vol_acft, na.rm=T),
            streamflow_aug_cfs = sum(streamflow_aug_cfs, na.rm=T)) %>% mutate(SITENO= "32E050" ,SITENAME1="TR_Dayton")

##Detour Rd ##WallaWalla Outlet
Detour_downstream_calc<- grid_non_interruptible %>% filter(site %in% WR_Detour_Rd_grids$site) %>% 
  group_by(region,Year,simulation) %>% 
  summarise(Streamflow_aug_vol_acft= sum(Streamflow_aug_vol_acft, na.rm=T),
            streamflow_aug_cfs = sum(streamflow_aug_cfs, na.rm=T)) %>% mutate(SITENO= "32A100" ,SITENAME1="WR_Detour_Rd")

##Touchet_R_bolls
TR_bolles_downstream_calc<- grid_non_interruptible %>% filter(site %in% TR_Bolles_grids$site) %>% 
  group_by(region,Year,simulation) %>% 
  summarise(Streamflow_aug_vol_acft= sum(Streamflow_aug_vol_acft, na.rm=T),
            streamflow_aug_cfs = sum(streamflow_aug_cfs, na.rm=T)) %>% mutate(SITENO= "32B100" ,SITENAME1="TR_Bolles")

##TOnasket
Tonasket_downstream_calc<- grid_non_interruptible %>% filter(site %in% Tonasket_grids$site) %>% 
  group_by(region,Year,simulation) %>% 
  summarise(Streamflow_aug_vol_acft= sum(Streamflow_aug_vol_acft, na.rm=T),
            streamflow_aug_cfs = sum(streamflow_aug_cfs, na.rm=T)) %>% mutate(SITENO= "12445000" ,SITENAME1="Tonasket")

##Similkameen
Similkameen_downstream_calc<- grid_non_interruptible %>% filter(site %in% Similkameen_grids$site) %>% 
  group_by(region,Year,simulation) %>% 
  summarise(Streamflow_aug_vol_acft= sum(Streamflow_aug_vol_acft, na.rm=T),
            streamflow_aug_cfs = sum(streamflow_aug_cfs, na.rm=T)) %>% mutate(SITENO= "12442500" ,SITENAME1="Similkameen")


#Malott #okanogan Outlet
Malott_downstream_calc<- grid_non_interruptible %>% filter(site %in% Malott_grids$site, simulation !="optimal1") %>%
  group_by(region,Year,simulation) %>% 
  summarise(Streamflow_aug_vol_acft= sum(Streamflow_aug_vol_acft, na.rm=T),
            streamflow_aug_cfs = sum(streamflow_aug_cfs, na.rm=T)) %>% mutate(SITENO= "12447200",SITENAME1="Malott")

##Pateros ##Methow Outlet
Pateros_downstream_calc<- grid_non_interruptible %>% filter(site %in% Pateros_grids$site) %>% 
  group_by(region,Year,simulation) %>% 
  summarise(Streamflow_aug_vol_acft= sum(Streamflow_aug_vol_acft, na.rm=T),
            streamflow_aug_cfs = sum(streamflow_aug_cfs, na.rm=T)) %>% mutate(SITENO= "12449950",SITENAME1="Pateros")

##Methow_River_at Twisp
MR_Twisp_downstream_calc<- grid_non_interruptible %>% filter(site %in% MR_Twisp_grids$site) %>% 
  group_by(region,Year,simulation) %>% 
  summarise(Streamflow_aug_vol_acft= sum(Streamflow_aug_vol_acft, na.rm=T),
            streamflow_aug_cfs = sum(streamflow_aug_cfs, na.rm=T)) %>% mutate(SITENO= "12449500",SITENAME1="MR_Twisp")


##Twisp_River_Near Twisp
TRN_Twisp_downstream_calc<- grid_non_interruptible%>% filter(site %in% TRN_Twisp_grids$site) %>% 
  group_by(region,Year,simulation) %>% 
  summarise(Streamflow_aug_vol_acft= sum(Streamflow_aug_vol_acft, na.rm=T),
            streamflow_aug_cfs = sum(streamflow_aug_cfs, na.rm=T)) %>% mutate(SITENO= "12448998",SITENAME1="TRN_Twisp")



###Combine all downstream calculated flow together for further analysis
list_all_flow<- list(dayton_downstream_calc,Detour_downstream_calc,TR_bolles_downstream_calc,Tonasket_downstream_calc,
                     Similkameen_downstream_calc,Malott_downstream_calc,Pateros_downstream_calc,
                     MR_Twisp_downstream_calc,TRN_Twisp_downstream_calc)

#Combine all them together
combined_all_flow<- do.call(rbind,list_all_flow) %>% filter(simulation != "optimal")

####### FIFTEEN DAYS SHUTOFF SCENARIOS CALCULATIONS #######


FC_AD_15D_nonI<- inner_join(combined_all_flow,gauges_and_VIC, by=c("SITENO"))

##Creating groups columns based on simulations date for the streamflow data downloaded for 28 station for the time periods 2000-2022
Stream_group<- streamflow_data[streamflow_data$DOY %in% (121:240),] %>% 
  mutate(simulation= ifelse(DOY %in% Scenario01,"Scenario01",
                            ifelse(DOY %in% Scenario02,"Scenario02",
                                   ifelse(DOY %in% Scenario03,"Scenario03",
                                          ifelse(DOY %in% Scenario04,"Scenario04",
                                                 ifelse(DOY %in% Scenario05,"Scenario05",
                                                        ifelse(DOY %in% Scenario06,"Scenario06",
                                                               ifelse(DOY %in% Scenario07,"Scenario07",
                                                                      ifelse(DOY %in% Scenario08,"Scenario08",0)))))))))




###Join the streamflow gauges and vic grids with the Streamflow file
Stream_flow_gauges<- left_join(Stream_group,gauges_and_VIC, by=c("site_no"="SITENO"))
Stream_flow_gauges1<- Stream_flow_gauges %>% 
  mutate(site_no=as.character(site_no)) %>% 
  inner_join(instreamflow_rules, by=c("site_no"="SITENO","DOY")) 



###Join Calculated streamfow data with gage locations to create plots and further calculations
FC_AD_15D1_nonI<- inner_join(FC_AD_15D_nonI %>% dplyr::select(c("region","Year","simulation","SITENO","SITENAME","streamflow_aug_cfs","LONGDD","LATDD")),Stream_flow_gauges1, by=c("SITENAME","SITENO","Year","simulation")) %>%  #"flow_cfs_12",
  mutate(Contributing_flow= (streamflow_aug_cfs+Discharge_cfs)) %>% distinct()


##Required modification to the dataset to answer questions##
MIFR_required<- FC_AD_15D1_nonI %>% 
  mutate(Can_meet_MIFR=ifelse(Discharge_cfs < MIFR_rules & Contributing_flow > MIFR_rules, "YES", "NO")) %>% 
  dplyr::select(c("region","SITENAME","DOY","Year","simulation","streamflow_aug_cfs","Discharge_cfs","MIFR_rules", "Contributing_flow","Can_meet_MIFR")) %>% 
  mutate(Aug_Need= ifelse(Discharge_cfs < MIFR_rules, "Yes_Need","Not_Need"),
         MIFR_Dff= (MIFR_rules-Discharge_cfs), #Difference between MIFR and actual discharge
         MIFR_Dff1= ifelse(MIFR_Dff<=0,0,MIFR_Dff), #If dischareg is higher than MIFr rules setting it into zero
         MIFR_R_75= MIFR_rules*0.75, #75% of the MIFR
         MIFR_R_50= MIFR_rules*0.50, #50% of the MIFR
         Can_meet_MIFR_75= ifelse(Discharge_cfs < MIFR_rules & Discharge_cfs < MIFR_R_75 & Contributing_flow > MIFR_R_75, "YES", "NO"),
         Can_meet_MIFR_50 = ifelse(Discharge_cfs < MIFR_rules & Discharge_cfs < MIFR_R_75 & 
                                     Discharge_cfs < MIFR_R_50 & Contributing_flow > MIFR_R_50, "YES", "NO"),
         Aug_2_mt_MIFR= ifelse(MIFR_rules>Discharge_cfs,MIFR_rules-Discharge_cfs,0), #Amount required to meet MIFR
         Percentage_aug= ifelse(Contributing_flow>=MIFR_rules,100,(streamflow_aug_cfs/MIFR_Dff)*100), #Calculate augmentation percentage
         per_aug_not_meet_MIFR= ifelse(Can_meet_MIFR=="NO",Percentage_aug,0)) %>% 
  mutate(SITENAME= ifelse(SITENAME=="WENATCHEE RIVER AT MONITOR, WA", "Monitor",ifelse(SITENAME=="WENATCHEE RIVER AT PESHASTIN, WA", "Peshastin",ifelse(SITENAME=="METHOW RIVER AT TWISP, WA","MR_Twisp",ifelse(SITENAME=="METHOW RIVER NEAR PATEROS, WA","Pateros",ifelse(SITENAME=="TWISP RIVER NEAR TWISP, WA","TRN_Twisp",ifelse(SITENAME=="OKANOGAN RIVER AT MALOTT, WA","Malott",ifelse(SITENAME=="OKANOGAN RIVER AT OROVILLE, WA","Oroville",ifelse(SITENAME=="OKANOGAN RIVER NEAR TONASKET, WA","Tonasket",ifelse(SITENAME=="SIMILKAMEEN RIVER NEAR NIGHTHAWK, WA","Similkameen",ifelse(SITENAME=="N.F. Touchet R. abv Dayton","TR_Dayton",ifelse(SITENAME=="Touchet R. at Bolles","TR_Bolles",ifelse(SITENAME=="Walla Walla R.  E. Detour Rd.","WR_Detour_Rd",0)))))))))))),
         SITENAME= factor(SITENAME, ordered = TRUE, levels= c("Monitor","Peshastin","MR_Twisp","Pateros","TRN_Twisp","Malott" ,"Oroville","Tonasket","Similkameen","TR_Dayton","TR_Bolles","WR_Detour_Rd")))


###Filter out simulations where at least one day streamflow augmentation were required
MIFR_reach_all<- MIFR_required %>% 
  group_by(SITENAME) %>% 
  mutate(Analyzed_yrs= (max(Year)-min(Year)+1)) %>% 
  group_by(SITENAME,Year,simulation) %>% 
  filter(any(Aug_Need=="Yes_Need"))



####Filterout two consecutive days streamflow required 
###Filter out stations and simulation which augmented streamflow for atleast for two consecutive days
#Step 1: filter out all consecutive days
sites<- unique(MIFR_reach_all$SITENAME)

# Initialize an empty list to store results

result_list_v1 <- list()
for (i in 1: length(sites)){
  site1<- MIFR_reach_all %>% filter(SITENAME==sites[i])
  Yr<- unique(site1$Year)
  for (j in 1: length(Yr)){
    site11<- site1 %>% filter(Year==Yr[j])
    simulations<- unique(site11$simulation)
    for ( k in 1:length(simulations)){
      site111<- site11 %>% filter(simulation==simulations[k],Aug_Need=="Yes_Need") %>% arrange(DOY) #filtering out days augmentation required
      if (nrow(site111>1)){
        consecutive_days <- which(diff(site111$DOY) == 1)
        if (length(consecutive_days) > 0) {
          # Store the relevant rows
          for (l in 1:length(consecutive_days)) {
            result_list_v1[[length(result_list_v1) + 1]] <- site111[c(consecutive_days[l], consecutive_days[l] + 1), ]
          }
        }
      }
    }
  }
}



# Combine all consecutive days from the loop into a single dataframe
Consecutive_2_days_req <- do.call(rbind, result_list_v1)

# Remove duplicates, if any
Consecutive_2_days_req <- Consecutive_2_days_req[!duplicated(Consecutive_2_days_req), ] 

# Step 2: Calculate Req_aug with group-by and mutate
Req_aug <- Consecutive_2_days_req %>%
  group_by(SITENAME, Year, simulation) %>%
  arrange(DOY) %>%
  mutate(MovingSum_MIFRdef = (MIFR_Dff + lag(MIFR_Dff, 1)),
         MinMovingSum = min(MovingSum_MIFRdef, na.rm = TRUE),
         MaxOfTwo = if_else(MovingSum_MIFRdef == MinMovingSum, pmax(MIFR_Dff, lag(MIFR_Dff)), NA_real_)) %>%
  na.omit()

###Keep the days when it needs for consecutive ays
Req_aug1 <- Consecutive_2_days_req %>%
  group_by(SITENAME, Year, simulation) %>%
  arrange(DOY) %>%
  mutate(
    # Calculate the difference in DOY to identify consecutive days
    DOY_diff = DOY - lag(DOY, 1),
    
    # Use the difference to filter consecutive DOY pairs (DOY_diff == 1)
    MovingSum_MIFRdef = if_else(DOY_diff == 1, (MIFR_Dff + lag(MIFR_Dff, 1)), NA_real_),
    
    # Calculate the minimum moving sum while ignoring NA values
    MinMovingSum = min(MovingSum_MIFRdef, na.rm = TRUE),
    
    # For rows where the moving sum equals the minimum, take the max of the two values
    MaxOfTwo = if_else(MovingSum_MIFRdef == MinMovingSum, pmax(MIFR_Dff, lag(MIFR_Dff)), NA_real_)
  ) %>%
  na.omit()


# Step 3: Ensure Req_aug has only one row per group
Req_aug_distinct <- Req_aug1 %>%
  group_by(SITENAME, Year, simulation) %>%
  summarize(MaxOfTwo = max(MaxOfTwo, na.rm = TRUE)) %>%
  ungroup()



# Step 4: Perform a left_join, ensuring no many-to-many relationship
Consecutive_2_days_req_V2 <- Consecutive_2_days_req %>%
  left_join(Req_aug_distinct, by = c("SITENAME", "Year", "simulation")) %>% 
  mutate(Req_aug= MaxOfTwo,
         Req_aug_Acft= ((Req_aug*3600*24*15)/43560), #Req_aug column is in ft3/s
         Fiftyp_aug= Req_aug_Acft*0.5)




#### create dataset to calculate ratio of potential maximum augmentation and required augmentation and create figure 06
Calc_ratio<- Consecutive_2_days_req %>%
  group_by(SITENAME, Year, simulation) %>%
  arrange(DOY) %>%
  mutate(# Calculate the difference in DOY to identify consecutive days
    DOY_diff = DOY - lag(DOY, 1),
    
    # Use the difference to filter consecutive DOY pairs (DOY_diff == 1)
    MovingSum_MIFRdef = if_else(DOY_diff == 1, (MIFR_Dff + lag(MIFR_Dff, 1)), NA_real_),
    
    # Calculate the minimum moving sum while ignoring NA values
    MinMovingSum = min(MovingSum_MIFRdef, na.rm = TRUE),
    
    # For rows where the moving sum equals the minimum, take the max of the two values
    MaxOfTwo = if_else(MovingSum_MIFRdef == MinMovingSum, pmax(MIFR_Dff, lag(MIFR_Dff)), NA_real_)) %>% na.omit() %>% 
  filter(MaxOfTwo== max(MaxOfTwo)) %>% mutate(RelativeRatioOfAug= streamflow_aug_cfs/MIFR_Dff) %>% 
  mutate(RelativeRatioOfAug_adj = ifelse(RelativeRatioOfAug > 1, 1, RelativeRatioOfAug)) %>% 
  mutate(SITENAME= case_when(SITENAME== "MR_Twisp" ~ "METTW",
                             SITENAME== "Pateros" ~ "METPA",
                             SITENAME== "TRN_Twisp" ~ "TWITW",
                             SITENAME== "Similkameen" ~ "SIMNI",
                             SITENAME== "Malott" ~ "OKAMA",
                             SITENAME== "Tonasket" ~ "OKATO",
                             SITENAME== "TR_Dayton" ~ "NFTDA",
                             SITENAME== "TR_Bolles" ~ "TOUBO",
                             SITENAME== "WR_Detour_Rd" ~ "WALDE"),
         simulation_abb= case_when(simulation== "Scenario01" ~ "May H1",
                                   simulation== "Scenario02" ~ "May H2",
                                   simulation== "Scenario03" ~ "Jun H1",
                                   simulation== "Scenario04" ~ "Jun H2",
                                   simulation== "Scenario05" ~ "Jul H1",
                                   simulation== "Scenario06" ~ "Jul H2",
                                   simulation== "Scenario07" ~ "Aug H1",
                                   simulation== "Scenario08" ~ "Aug H2"),
         simulation_abb= factor(simulation_abb, ordered = TRUE, c(c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2"))),
         SITENAME= factor(SITENAME, ordered = TRUE, levels= c(c("Monitor","Peshastin", "Oroville",
                                                                "METPA","OKAMA","WALDE","METTW","OKATO","TOUBO","TWITW","SIMNI","NFTDA"))))



#### create plot to calculate ratio of potential maximum augmentation and and need
P1<- ggplot(Calc_ratio, aes(x=simulation_abb,y= RelativeRatioOfAug_adj,fill=simulation))+ #Cost_acre
  geom_hline(yintercept = c(0.25,0.5,0.75), linetype="dashed", color="red")+
  geom_boxplot(outlier.shape = NA)+ facet_wrap(~SITENAME)+
  stat_boxplot(geom='errorbar')+
  scale_fill_brewer(palette= "Paired")+
  scale_y_continuous(" ",limits=c(0,1), breaks=seq(0,1, by=0.25))+ 
  geom_hline(yintercept = c(0.5,0.75), linetype="dashed", color="red")+
  labs(x= "", y= " ", title = paste0(" "))+theme_bw()+ 
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 20),axis.title = element_blank(), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 18,angle = 90,hjust = 1, vjust=0.5,color="black"),
        axis.text.y = element_blank(), # Remove y-axis text from all facets
        axis.text.y.left = element_text(size = 20, color = "black"),
        legend.position = "none",axis.text.y.right = element_blank(),
        strip.text = element_text(size=20,color = "black"))
##Save figure
ggsave(P1, file="Figure/Ratio_v1.png", width = 30, height = 20,dpi = 300, units = "cm")




###Create Figure 08
##Filter out dataset when harvest date is only higher than targeted harvest date 
#if any targeted harvest date is higher than harvest date, it should filtered out during marginal cost calculation, but still if exist any
combined_sites_suuply_v1<- combined_sites_suuply %>% filter(harvest_date1>target_harvest_date)


###Calculate marginal cost and required cost for streamflow augmentation for years and locations using a loop
###loop is common for all policy scenarios
##datarframe to store
sites<- unique(combined_sites_suuply_v1$SITENAME)
Combined_sites_2Days<- rep()
for (h in 1:length(sites)){
  site1<- Consecutive_2_days_req_V2 %>% filter(SITENAME== sites[h])
  Yr<- unique(site1$Year)
  for ( i in 1: length(Yr)) {
    site11<- filter(site1, Year==Yr[i])
    mc_area1<- filter(combined_sites_suuply_v1,SITENAME== sites[h],Year==Yr[i]) ##This data is from "supply_curve.R" script
    scn<- unique(site11$simulation)
    for ( j in 1:length(scn)){
      un_data<- filter(site11,simulation==scn[j]) #Yr[i]
      mc_scn<- mc_area1[mc_area1$simulation==scn[j],] %>% rename("cost_acft"="cost_acrft") #rename("cost_acft"="cost_acrft")
      un_data1<- un_data %>% filter(Req_aug_Acft== max(Req_aug_Acft)) %>% slice(1)
      ##Pick the maximum augmentation value as cum_streamflow and marginal cost
      nearest_cos_vol<- mc_scn[min(which(mc_scn$cum_streamflow>= un_data1$Req_aug_Acft)),c("cost_acft","cum_streamflow")] %>% na.omit() %>%
        mutate(Meet_MIFR="YES")
      
      ##Pick the 50% of the augmentation and marginal cost
      nearest_cos_vol1<- mc_scn[min(which(mc_scn$cum_streamflow>= un_data1$Fiftyp_aug)),c("cost_acft","cum_streamflow")] %>% na.omit()
      
      #in any condition if required streamflow augmentation is not meeting then picking the second closest value
      if (nrow(nearest_cos_vol)==0) {
        #nearest_cos_vol<- mc_scn[max(which(mc_scn$cum_streamflow<= un_data1$Max_aug)),c("cost_acft","cum_streamflow")] %>% na.omit()
        if (nrow (un_data1>1)){
          un_data1<- un_data %>% filter(Req_aug_Acft== max(Req_aug_Acft)) %>% slice(1)
        }
        nearest_cos_vol<- mc_scn[max(which(mc_scn$cum_streamflow<= un_data1$Req_aug_Acft)),c("cost_acft")] %>% na.omit() %>% 
          mutate(cum_streamflow=un_data1$Req_aug_Acft,
                 Meet_MIFR="NO")
      }
      
      if (nrow(nearest_cos_vol1)==0) {
        nearest_cos_vol1<- mc_scn[min(which(mc_scn$cum_streamflow<= un_data1$Fiftyp_aug)),c("cost_acft","cum_streamflow")] %>% na.omit()
      }
      
      ##seperate marginal cost to calculate cost/acre
      mc_v1<- filter(mc_scn,cost_acft<= nearest_cos_vol$cost_acft, cum_streamflow<= nearest_cos_vol$cum_streamflow)
      
      un_data<- un_data %>% 
        mutate(MC_Cost= ifelse (nrow(nearest_cos_vol) > 0, nearest_cos_vol$cost_acft,0),
               nearest_cum_stream=ifelse (nrow(nearest_cos_vol) > 0, nearest_cos_vol$cum_streamflow,0),
               #Meet_MIFR_C= ifelse (nrow(nearest_cos_vol) > 0, nearest_cos_vol$Meet_MIFR,0),
               Total_Cost=MC_Cost*Req_aug_Acft,
               MC_Cost_50 = ifelse (nrow(nearest_cos_vol1) > 0, nearest_cos_vol1$cost_acft,0),
               nearest_cum_stream_50= ifelse (nrow(nearest_cos_vol1) > 0, nearest_cos_vol1$cum_streamflow,0),
               Contributing_acres= sum(mc_v1$Acres, na.rm=T),
               Cost_acre= Total_Cost/Contributing_acres,
               cost_AF= Total_Cost/Req_aug_Acft)
      Combined_sites_2Days<- rbind(Combined_sites_2Days,un_data)
    }
  }
}



####Filterout two consecutive days for years and locations which met required augmentation 
sites<- unique(Combined_sites_2Days$SITENAME)
# Initialize an empty list to store results
result_list <- list()
for (i in 1: length(sites)){
  site1<- Combined_sites_2Days %>% filter(SITENAME==sites[i])
  Yr<- unique(site1$Year)
  for (j in 1: length(Yr)){
    site11<- site1 %>% filter(Year==Yr[j])
    simulations<- unique(site11$simulation)
    for ( k in 1:length(simulations)){
      site111<- site11 %>% filter(simulation==simulations[k],Can_meet_MIFR=="YES") %>% arrange(DOY)
      if (nrow(site111>1)){
        consecutive_days <- which(diff(site111$DOY) == 1)
        if (length(consecutive_days) > 0) {
          # Store the relevant rows
          for (l in 1:length(consecutive_days)) {
            result_list[[length(result_list) + 1]] <- site111[c(consecutive_days[l], consecutive_days[l] + 1), ]
          }
        }
      }
    }
  }
}



# Combine all the results into a single dataframe
Combined_sites_2Days1 <- do.call(rbind, result_list)

# Remove duplicates, if any
Combined_sites_2Days1 <- Combined_sites_2Days1[!duplicated(Combined_sites_2Days1), ]


##Taking mean across the years and ading and modifying columns to create figure
Combined_sites_2Days22<- Combined_sites_2Days1 %>% 
  group_by(SITENAME,Year, simulation,Aug_Need,Can_meet_MIFR) %>% 
  summarise(Total_Cost= mean(Total_Cost,na.rm=T),
            Cost_acre= mean(Cost_acre, na.rm=T),
            cost_AF = mean(cost_AF)) %>% 
  group_by(SITENAME, simulation,Aug_Need,Can_meet_MIFR) %>% 
  summarise(Total_Cost= round(mean(Total_Cost,na.rm=T)),
            TC1 =round(Total_Cost/1000),
            Cost_acre= round(mean(Cost_acre, na.rm=T)),
            cost_AF = round(mean(cost_AF))) %>% 
  mutate(SITENAME= case_when(SITENAME== "MR_Twisp" ~ "METTW",
                             SITENAME== "Pateros" ~ "METPA",
                             SITENAME== "TRN_Twisp" ~ "TWITW",
                             SITENAME== "Similkameen" ~ "SIMNI",
                             SITENAME== "Malott" ~ "OKAMA",
                             SITENAME== "Tonasket" ~ "OKATO",
                             SITENAME== "TR_Dayton" ~ "NFTDA",
                             SITENAME== "TR_Bolles" ~ "TOUBO",
                             SITENAME== "WR_Detour_Rd" ~ "WALDE"),
         simulation_abb= case_when(simulation== "Scenario01" ~ "May H1",simulation== "Scenario02" ~ "May H2",
                                   simulation== "Scenario03" ~ "Jun H1",simulation== "Scenario04" ~ "Jun H2",
                                   simulation== "Scenario05" ~ "Jul H1",simulation== "Scenario06" ~ "Jul H2",
                                   simulation== "Scenario07" ~ "Aug H1",simulation== "Scenario08" ~ "Aug H2"))

##Add a an empty dataframe to keep May H2 in the X-axis and join it into the dataframe. 
add_dataframe<- data.frame(SITENAME= c("OKATO","TWITW"), simulation_abb="May H2")
Combined_sites_2Days221<- rbind(Combined_sites_2Days22,add_dataframe)%>% 
  mutate(simulation_abb= factor(simulation_abb, ordered = TRUE, c(c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2"))),
         SITENAME= factor(SITENAME, ordered = TRUE, levels= c(c("Monitor","Peshastin", "Oroville",
                                                                "WALDE","TOUBO","NFTDA","OKAMA","OKATO","SIMNI","METPA","METTW","TWITW"))))
#Panel a (cost/acre)
P1<- ggplot(Combined_sites_2Days221, mapping = aes(x=simulation_abb, y= SITENAME))+
  geom_tile(aes(fill= Cost_acre))+
  geom_text(aes(label = Cost_acre, color = ifelse(Cost_acre > 80, "black", "white")), size = 8) + 
  #geom_text(aes(label= Cost_acre), color="red", size= 8)+
  scale_fill_viridis(limits=c(0,100), breaks=seq(0,100, by=25), na.value = NA)+
  scale_color_identity() + 
  scale_x_discrete(labels = c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2"))+
  theme_bw()+labs(x= "", y= " ",title = paste0("Cost per acre (mean)"))+ 
  theme(
    axis.text = element_text(size = 20, color = "black"),
    plot.title = element_text(size = 20),
    axis.text.x = element_text(size = 20, color = "black", vjust = 0.5, hjust = 1, angle = 90),
    axis.title = element_text(size = 22),
    legend.text = element_text(size = 20, color = "black"), 
    legend.title = element_blank(),
    legend.position = c(0.1, 0.5),
    legend.key.height = unit(1.5, "cm"),
    legend.background = element_blank()
  )
#Save the figure
ggsave(P1, file="Figure/Cost_acre_v2.png", width = 25, height = 20,dpi = 300, units = "cm")

##Panel b (total cost)
P1<- ggplot(Combined_sites_2Days221, mapping = aes(x=simulation_abb, y= SITENAME)) +
  geom_tile(aes(fill = TC1)) +
  geom_text(aes(label = TC1, color = ifelse(TC1 > 800, "black", "white")), size = 8) +  # Dynamic text color
  scale_fill_viridis(limits = c(0, 1000), breaks = seq(0, 1000, by = 250), na.value = NA) +  # Keep Viridis color scale
  scale_color_identity() +  # Ensure the dynamic text color is applied
  scale_x_discrete(labels = c("May H1", "May H2", "Jun H1", "Jun H2", "Jul H1", "Jul H2", "Aug H1", "Aug H2")) +
  theme_bw() +
  labs(x = "", y = " ", title = paste0("Total cost (mean)")) + 
  theme(
    axis.text = element_text(size = 20, color = "black"),
    plot.title = element_text(size = 20),
    axis.text.x = element_text(size = 20, color = "black", vjust = 0.5, hjust = 1, angle = 90),
    axis.title = element_text(size = 22),
    legend.text = element_text(size = 20, color = "black"), 
    legend.title = element_blank(),
    legend.position = c(0.1, 0.5),
    legend.key.height = unit(1.5, "cm"),
    legend.background = element_blank()
  )
#Save figure
ggsave(P1, file="Figure/TC1.png", width = 25, height = 20,dpi = 300, units = "cm")



###Create dataset to create plots  for appendix (Augmentation available and Augmentation need)
selected_dataset<- Calc_ratio %>% 
  ungroup() %>% 
  dplyr::select("SITENAME","DOY","Year","simulation_abb","Discharge_cfs","MIFR_rules","MIFR_Dff1","streamflow_aug_cfs",
                "MaxOfTwo","RelativeRatioOfAug","RelativeRatioOfAug_adj") %>% 
  mutate(Req_aug_acft=((MaxOfTwo*3600*24*15)/43560),
         Aug_avail_acft= ((streamflow_aug_cfs*3600*24*15)/43560))

##create Appendix plot 3
#### create plot to calculate ratio of potential maximum augmentation and and need
P1<- ggplot(selected_dataset, aes(x=simulation_abb,y= streamflow_aug_cfs,fill=simulation_abb))+ 
  geom_boxplot(outlier.shape = NA)+ facet_wrap(~SITENAME, scales = "free_y")+
  stat_boxplot(geom='errorbar')+
  scale_fill_brewer(palette= "Paired")+
  labs(x= "", y= "Available augmentation (cfs)", title = paste0(" "))+theme_bw()+ 
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 20),axis.title = element_blank(), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 18,angle = 90,hjust = 1, vjust=0.5,color="black"),
        axis.text.y = element_blank(), # Remove y-axis text from all facets
        axis.text.y.left = element_text(size = 20, color = "black"),
        axis.title.y = element_text(size = 20, color = "black"),
        legend.position = "none",axis.text.y.right = element_blank(),
        strip.text = element_text(size=20,color = "black"))
##Save figure
ggsave(P1, file="Figure/Aug_avail.png", width = 30, height = 20,dpi = 300, units = "cm")

##create Appendix plot 4
#### create plot to calculate ratio of potential maximum augmentation and and need
P1<- ggplot(selected_dataset, aes(x=simulation_abb,y= MaxOfTwo,fill=simulation_abb))+ 
  geom_boxplot(outlier.shape = NA)+ facet_wrap(~SITENAME, scales = "free_y")+
  stat_boxplot(geom='errorbar')+
  scale_fill_brewer(palette= "Paired")+
  labs(x= "", y= "Augmentation need (cfs)", title = paste0(" "))+theme_bw()+ 
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 20),axis.title = element_blank(), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 18,angle = 90,hjust = 1, vjust=0.5,color="black"),
        axis.text.y = element_blank(), # Remove y-axis text from all facets
        axis.text.y.left = element_text(size = 20, color = "black"),
        axis.title.y = element_text(size = 20, color = "black"),
        legend.position = "none",axis.text.y.right = element_blank(),
        strip.text = element_text(size=20,color = "black"))
#Save figure
ggsave(P1, file="Figure/Aug_need.png", width = 30, height = 20,dpi = 300, units = "cm")





##Summarize it across days and required modification to create Figure 07
Percen_data1<- Calc_ratio %>%  
  group_by(SITENAME,Year, simulation) %>% 
  summarise(Percen_med= median(Percentage_aug, na.rm=T),
            Percen_mn= mean(Percentage_aug, na.rm=T),
            Percen_min= min(Percentage_aug, na.rm=T),
            Percen_max= max(Percentage_aug, na.rm=T)) %>% filter(!SITENAME %in% c("Monitor","Peshastin")) %>% 
  mutate(Percent_mn_1= ifelse(Percen_mn>0 & Percen_mn <= 25, "0 – 25", 
                              ifelse(Percen_mn> 25 & Percen_mn <= 50, "25 – 50", 
                                     ifelse(Percen_mn> 50 & Percen_mn <= 75, "50 – 75",
                                            ifelse(Percen_mn> 75 & Percen_mn < 100,"75 – 100",
                                                   ifelse(Percen_mn== 100,"100","0"))))),
         Percent_mn_1= as.character(Percent_mn_1),
         Percent_md_1= ifelse(Percen_med>0 & Percen_med <= 25, "0 – 25", 
                              ifelse(Percen_med> 25 & Percen_med <= 50, "25 – 50", 
                                     ifelse(Percen_med> 50 & Percen_med <= 75, "50 – 75",
                                            ifelse(Percen_med> 75 & Percen_med < 100,"75 – 100",
                                                   ifelse(Percen_med == 100,"100","0"))))),
         Percent_md_1= as.character(Percent_md_1),
         Percent_min_1= ifelse(Percen_min>0 & Percen_min <= 25, "0 – 25", 
                               ifelse(Percen_min> 25 & Percen_min <= 50, "25 – 50", 
                                      ifelse(Percen_min> 50 & Percen_min <= 75, "50 – 75",
                                             ifelse(Percen_min> 75 & Percen_min < 100,"75 – 100",
                                                    ifelse(Percen_min== 100 ,"100","0"))))),
         Percent_min_1= as.character(Percent_min_1),
         Percen_max = as.numeric(Percen_max),
         Percent_max_1= ifelse(Percen_max>= 0 & Percen_max <= 25, "[0, 25]", 
                               ifelse(Percen_max> 25 & Percen_max <= 50, "(25, 50]", 
                                      ifelse(Percen_max> 50 & Percen_max <= 75, "(50, 75]",
                                             ifelse(Percen_max> 75 & Percen_max < 100 ,"(75, 100)",
                                                    ifelse(Percen_max == 100 ,"100","0"))))),
         Percent_max_1= as.character(Percent_max_1),
         simulation_abb= case_when(simulation== "Scenario01" ~ "May H1",
                                   simulation== "Scenario02" ~ "May H2",
                                   simulation== "Scenario03" ~ "Jun H1",
                                   simulation== "Scenario04" ~ "Jun H2",
                                   simulation== "Scenario05" ~ "Jul H1",
                                   simulation== "Scenario06" ~ "Jul H2",
                                   simulation== "Scenario07" ~ "Aug H1",
                                   simulation== "Scenario08" ~ "Aug H2"),
         simulation_abb= factor(simulation_abb, ordered = TRUE, c(c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2"))),
         SITENAME= factor(SITENAME, ordered = TRUE, levels= c(c("Monitor","Peshastin", "Oroville",
                                                                "WALDE","TOUBO","NFTDA","OKAMA","OKATO","SIMNI","METPA","METTW","TWITW"))))


##create plate to print color value manually from RocolorBrewer color blindness friendly color
Percen_plate<- c("#FFFFCC","#FED976","#FD8D3C","#E31A1C","#800026")
names(Percen_plate)<- as.factor(c("[0, 25]","(25, 50]","(50, 75]","(75, 100)","100"))



P1<- ggplot(Percen_data1, mapping = aes(x=Year, y= SITENAME))+geom_tile(aes(fill= Percent_max_1))+
  scale_fill_manual(values= Percen_plate, breaks=c("[0, 25]","(25, 50]","(50, 75]","(75, 100)","100"),
                    guide="legend")+ 
  facet_wrap(~simulation_abb, ncol=4, nrow=2)+
  labs(x= "", y= " ",title = paste0("Percentage_streamflow_can be met by 15 days shutoff (Max)"))+ 
  theme(axis.text.x= element_text(size = 16,angle = 90, vjust=0.5,color="black"),
        axis.text.y= element_text(size = 16,color="black"),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size=15), strip.text = element_text(size = 15,face="bold"), 
        strip.background = element_rect(colour = "black", fill = "lightgray", size = 1),
        legend.title = element_text(size=17),
        legend.position = "right",
        legend.key.height = unit(1.5,"cm"))
#save plot
ggsave(P1, file="Figure/Max_v2.png", width = 30, height = 20,dpi = 300, units = "cm")


##Create table instead of this plot which has been provided in appendix
Count_years1<- Combined_sites_2Days1 %>% 
  group_by(SITENAME,simulation) %>% 
  summarise(met_years= n_distinct(Year))

###Seperate a dataframe to create percentatge augmentation plot
Percen_meet_15d<- MIFR_required %>% 
  group_by(SITENAME) %>% 
  mutate(Analyzed_yrs= (max(Year)-min(Year)+1)) %>% filter(Aug_Need=="Yes_Need") %>% 
  dplyr::select(c( "SITENAME","Year","DOY","simulation","streamflow_aug_cfs","Discharge_cfs","Contributing_flow","MIFR_rules","MIFR_R_75","MIFR_R_50","Aug_Need","Can_meet_MIFR","Can_meet_MIFR_75","Can_meet_MIFR_50","Analyzed_yrs"))


#Ensure table folder exists
if (!dir.exists("Figure/table")) {
  dir.create("Figure/table", recursive = TRUE)
}

##calculate when its need augmentation
Count_years<- Percen_meet_15d %>% 
  group_by(SITENAME,simulation,Analyzed_yrs) %>% 
  summarise(Req_years= n_distinct(Year)) %>% 
  #left_join(Count_years1) %>% 
  mutate(Percentage= round(Req_years/Analyzed_yrs*100,digits=1),
         SITENAME= case_when(SITENAME== "MR_Twisp" ~ "METTW",
                             SITENAME== "Pateros" ~ "METPA",
                             SITENAME== "TRN_Twisp" ~ "TWITW",
                             SITENAME== "Similkameen" ~ "SIMNI",
                             SITENAME== "Malott" ~ "OKAMA",
                             SITENAME== "Tonasket" ~ "OKATO",
                             SITENAME== "TR_Dayton" ~ "NFTDA",
                             SITENAME== "TR_Bolles" ~ "TOUBO",
                             SITENAME== "WR_Detour_Rd" ~ "WALDE"),
         simulation_abb= case_when(simulation== "Scenario01" ~ "May H1",
                                   simulation== "Scenario02" ~ "May H2",
                                   simulation== "Scenario03" ~ "Jun H1",
                                   simulation== "Scenario04" ~ "Jun H2",
                                   simulation== "Scenario05" ~ "Jul H1",
                                   simulation== "Scenario06" ~ "Jul H2",
                                   simulation== "Scenario07" ~ "Aug H1",
                                   simulation== "Scenario08" ~ "Aug H2")) %>%ungroup() %>%  
  dplyr::select(c(SITENAME,simulation_abb, Percentage)) %>% 
  pivot_wider(names_from = simulation_abb,
              values_from = Percentage)

##Write table to the folder
write.csv(Count_years,"Figure/table/count_years.csv")
