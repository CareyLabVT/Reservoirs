##sorting the BVR data by depth as opposed to position
#by A. Breef-Pilz 10 FEB21

#since the water level varies in BVR this script will take the depth offset and apply for each depth
#then it will sort into depth about 1m bins. Ex. 3m is from 2.5m-3.49m
#All flags are removed

pacman::p_load(tidyverse,lubridate, plotly)

#download the data from the EDI folder on GitHub

download.file("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform/BVR_EDI_2020.csv", "BVR_EDI_2020.csv")
download.file("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform/BVR_Depth_offsets_2020.csv", "BVR_Depth_offset_2020.csv")

#read in data
bvr=read.csv("BVR_EDI_2020.csv")
depth=read.csv("BVR_Depth_offset_2020.csv")

#take out EXO data so you can add it back in later
EXO=bvr%>%
  select(Reservoir, Site, DateTime, starts_with("EXO"))


#select the sensors on the temp string and then pivot the data frame longer to merge the offset 
#dataset. 
bvr_new=bvr%>%
  select(Reservoir,Site,DateTime, starts_with("Ther"), starts_with("RDO"), starts_with("Lvl"),Depth_m_13)%>%
  pivot_longer(-c(Reservoir,Site,DateTime,Depth_m_13), names_to="Sensor", values_to="Reading", values_drop_na=FALSE)%>%
  separate(Sensor,c("Sensor","Units","Position"),"_")%>%
  mutate(Position=as.numeric(Position))%>%
  merge(.,depth)%>%#add in the offset file
  mutate(Sensor_depth=Depth_m_13-Offset)#find the depth of the sensor

#add the depth range for the depth column. Ex. 3m is made up of sensor depths from 2.5m to 3.49m
bvr_new=bvr_new%>%
  select(Reservoir, Site, DateTime, Sensor, Reading, Units, Sensor_depth)%>%
    mutate(Depth = 0)%>%
    mutate(Depth = ifelse(is.na(Sensor_depth), NA, Depth))%>%
    mutate(Depth = ifelse(Sensor_depth <= 0, NA, Depth))%>%
    mutate(Depth = ifelse(Sensor_depth > 0 & Sensor_depth < 0.2, "0.1m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth >= 0.2 & Sensor_depth < 0.5, "0.3m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth >= 0.5 & Sensor_depth < 1.5, "1m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth >= 1.5 & Sensor_depth < 2.5, "2m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth >= 2.5 & Sensor_depth < 3.5, "3m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth >= 3.5 & Sensor_depth < 4.5, "4m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth >= 4.5 & Sensor_depth < 5.5, "5m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth >= 5.5 & Sensor_depth < 6.5, "6m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth >= 6.5 & Sensor_depth < 7.5, "7m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth >= 7.5 & Sensor_depth < 8.5, "8m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth >= 8.5 & Sensor_depth < 9.5, "9m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth >= 9.5 & Sensor_depth < 10.5, "10m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth >= 10.5 & Sensor_depth < 11.5, "11m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth >= 11.5 & Sensor_depth < 12.5, "12m", Depth))

#change back to a wide data form 
bvr_new=bvr_new%>%
  filter(!is.na(Depth))%>%
  mutate(Sensor=paste(Sensor,Units,Depth, sep = "_"))%>%
  pivot_wider(
    id_cols=c(Reservoir, Site, DateTime),
    names_from = Sensor,
    names_sep = "_",
    values_from = c(Reading))%>%
  select(Reservoir, Site, DateTime, ThermistorTemp_C_0.1m, ThermistorTemp_C_0.3m, ThermistorTemp_C_1m,ThermistorTemp_C_2m,
         ThermistorTemp_C_3m, ThermistorTemp_C_4m, ThermistorTemp_C_5m, ThermistorTemp_C_6m, ThermistorTemp_C_7m, ThermistorTemp_C_8m,
         ThermistorTemp_C_9m, ThermistorTemp_C_10m, ThermistorTemp_C_11m, ThermistorTemp_C_12m, RDO_mgL_3m, RDO_mgL_4m, RDO_mgL_5m, 
         RDOsat_percent_3m, RDOsat_percent_4m, RDOsat_percent_5m, RDOTemp_C_3m, RDOTemp_C_4m,RDOTemp_C_5m, RDO_mgL_11m, RDO_mgL_12m, 
         RDOsat_percent_11m, RDOsat_percent_12m, RDOTemp_C_11m, RDOTemp_C_12m, Lvl_psi_11m, Lvl_psi_12m,LvlTemp_C_11m,LvlTemp_C_12m)

#add the EXO back in 
  bvr_new=bvr_new%>%
    merge(.,EXO)
  
#write the csv  
setwd("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform")
  write.csv(bvr_new, 'BVR_EDI_bydepth_2020.csv', row.names = FALSE, quote=FALSE)