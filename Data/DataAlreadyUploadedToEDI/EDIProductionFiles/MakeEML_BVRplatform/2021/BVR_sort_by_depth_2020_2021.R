##sorting the BVR data by depth as opposed to position
#by A. Breef-Pilz 10 FEB 21
#Edited 10 FEB 22 by ABP

#since the water level varies in BVR this script will take the depth offset and apply for each depth
#then it will sort into depth about 1m bins. Ex. 3m is from 2.5m-3.49m
#All flags are removed

pacman::p_load(tidyverse,lubridate, plotly,plyr)

#download the data from the EDI folder on GitHub


download.file("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform/BVR_platform_2020_2021.csv", "BVR_platform_2020_2021.csv")
download.file("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform/BVR_Depth_offsets_2020_2021.csv", "BVR_Depth_offset_2020_2021.csv")

#Files can also be downloaded from EDI.

#read in data
bvr=read.csv("BVR_platform_2020_2021.csv")
depth=read.csv("BVR_Depth_offsets_2020_2021.csv")

bvr=bvrdata_clean


# take out EXO data so you can add it back in later
# the EXO has its own depth sensor which is calibrated to the atmosphere. 
# It is also on a buoy so should be about 1.5m 
EXO=bvr%>%
  select(Reservoir, Site, DateTime, starts_with("EXO"))


# select the sensors on the temp string because they are stationary.
# Then pivot the data frame longer to merge the offset file so you can add a depth to each sensor reading
bvr_new=bvr%>%
  select(Reservoir,Site,DateTime, starts_with("Ther"), starts_with("RDO"), starts_with("Lvl"),Depth_m_13)%>%
  pivot_longer(-c(Reservoir,Site,DateTime,Depth_m_13), names_to="Sensor", values_to="Reading", values_drop_na=FALSE)%>%
  separate(Sensor,c("Sensor","Units","Position"),"_")%>%
  mutate(Position=as.numeric(Position))%>%
  merge(.,depth)#add in the offset file



# The pressure sensor was moved to be in line with the bottom thermistor. The top two thermistors has slid closer to each other
# and were re-secured about a meter a part from each other. Because of this we need to filter before 2021-04-05 13:20:00 EST
# and after. The top two thermistors exact offset will have to be determined again when the water level is high enough again. 

bvr_pre_05APR21=bvr_new%>%
  filter(DateTime<="2021-04-05 13:20")%>%
  mutate(Sensor_depth=Depth_m_13-Offset_before_05APR21)%>% #this gives you the depth of the thermistors from the surface
  mutate(Rounded_depth_whole=round_any(Sensor_depth, 1))%>% #Round the depth to the nearest whole number
  mutate(Rounded_depth_hundreth=round_any(Sensor_depth, 0.01))#Round to the nearest hundredth 
  
bvr_post_05APR21=bvr_new%>%
  filter(DateTime>"2021-04-05 13:20")%>%
  mutate(Sensor_depth=Depth_m_13-Offset_after_05APR21)%>% #this gives you the depth of the thermistor from the surface
  mutate(Rounded_depth_whole=round_any(Sensor_depth, 1))%>% #Round the depth to the nearest whole number
  mutate(Rounded_depth_hundreth=round_any(Sensor_depth, 0.01)) #Round to the nearest hundredth 


# combine the pre April5th and the post April5th. Drop if the readings are NA. Drop if the sensor depth is NA because can't
# figure out the depth of the senesors. This will give you a depth for each sensor reading. 
bvr_by_depth=bvr_pre_05APR21%>%
  rbind(.,bvr_post_05APR21)%>%
  filter(!is.na(Reading))%>%
  filter(!is.na(Sensor_depth))%>%
  select(-Offset_before_05APR21, -Offset_after_05APR21, -Distance_above_sediments, -Depth_m_13)



#The rounding takes care of this but then have to eliminate the top thermistor. 
# add the depth range for the depth column. Ex. 3m is made up of sensor depths from 2.5m to 3.49m
# bvr_new=bvr_new%>%
#   select(Reservoir, Site, DateTime, Sensor, Reading, Units, Sensor_depth)%>%
#     mutate(Depth = 0)%>%
#     mutate(Depth = ifelse(is.na(Sensor_depth), NA, Depth))%>%
#     mutate(Depth = ifelse(Sensor_depth <= 0, NA, Depth))%>%
#     mutate(Depth = ifelse(Sensor_depth > 0 & Sensor_depth < 0.2, "0.1m", Depth))%>%
#     mutate(Depth = ifelse(Sensor_depth >= 0.2 & Sensor_depth < 0.5, "0.3m", Depth))%>%
#     mutate(Depth = ifelse(Sensor_depth >= 0.5 & Sensor_depth < 1.5, "1m", Depth))%>%
#     mutate(Depth = ifelse(Sensor_depth >= 1.5 & Sensor_depth < 2.5, "2m", Depth))%>%
#     mutate(Depth = ifelse(Sensor_depth >= 2.5 & Sensor_depth < 3.5, "3m", Depth))%>%
#     mutate(Depth = ifelse(Sensor_depth >= 3.5 & Sensor_depth < 4.5, "4m", Depth))%>%
#     mutate(Depth = ifelse(Sensor_depth >= 4.5 & Sensor_depth < 5.5, "5m", Depth))%>%
#     mutate(Depth = ifelse(Sensor_depth >= 5.5 & Sensor_depth < 6.5, "6m", Depth))%>%
#     mutate(Depth = ifelse(Sensor_depth >= 6.5 & Sensor_depth < 7.5, "7m", Depth))%>%
#     mutate(Depth = ifelse(Sensor_depth >= 7.5 & Sensor_depth < 8.5, "8m", Depth))%>%
#     mutate(Depth = ifelse(Sensor_depth >= 8.5 & Sensor_depth < 9.5, "9m", Depth))%>%
#     mutate(Depth = ifelse(Sensor_depth >= 9.5 & Sensor_depth < 10.5, "10m", Depth))%>%
#     mutate(Depth = ifelse(Sensor_depth >= 10.5 & Sensor_depth < 11.5, "11m", Depth))%>%
#     mutate(Depth = ifelse(Sensor_depth >= 11.5 & Sensor_depth < 12.5, "12m", Depth))

# change back to a wide data form. This puts the readings into depth columns but that means it does introduce
# NAs as the water depth changes. To do this thermistor in position 1 is dropped because thermistor 1 and thermistor 2
# are less than 1m apart and can't have two values in one time point for 1 depth. 
bvr_wide=bvr_by_depth%>%
  filter(Position>1)%>%
  select(c(-Sensor_depth, -Rounded_depth_hundreth, -Position))%>%
  mutate(Sensor=paste(Sensor,Units,Rounded_depth_whole,"m", sep = "_"))%>%
  pivot_wider(
    id_cols=c(Reservoir, Site, DateTime),
    names_from = Sensor,
    names_sep = "_",
    values_from = Reading)%>%
    mutate(ThermistorTemp_C_surface=Thermistor_C_0_m)%>%
  select(Reservoir, Site, DateTime, ThermistorTemp_C_surface, ThermistorTemp_C_1_m,ThermistorTemp_C_2_m,
         ThermistorTemp_C_3_m, ThermistorTemp_C_4_m, ThermistorTemp_C_5_m, ThermistorTemp_C_6_m, ThermistorTemp_C_7_m, ThermistorTemp_C_8_m,
         ThermistorTemp_C_9_m, ThermistorTemp_C_10_m, ThermistorTemp_C_11_m, RDO_mgL_2_m, RDO_mgL_3_m, RDO_mgL_4_m, RDO_mgL_5_m, 
         RDOsat_percent_2_m, RDOsat_percent_3_m, RDOsat_percent_4_m, RDOsat_percent_5_m, RDOTemp_C_2_m, RDOTemp_C_3_m, RDOTemp_C_4_m,RDOTemp_C_5_m, 
         RDO_mgL_10_m, RDO_mgL_11_m, RDO_mgL_12_m, RDOsat_percent_10_m, RDOsat_percent_11_m, RDOsat_percent_12_m, RDOTemp_C_11_m, RDOTemp_C_12_m, 
         Lvl_psi_10_m, Lvl_psi_11_m, Lvl_psi_12_m,LvlTemp_C_10_m,LvlTemp_C_11_m,LvlTemp_C_12_m)
  
#add the EXO back in 
  bvr_wide=bvr_wide%>%
    merge(.,EXO)
  
#write the csv  
setwd("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform")
  write.csv(bvr_wide, 'BVR_bydepth_2020_2021.csv', row.names = FALSE)