##sorting the CCR data by depth as opposed to position
#Modified from "ccr_sort_by_depth_2020.R"
#by A. Breef-Pilz 11 JAN 21

#since the water level varies in ccr this script will take the depth offset and apply for each depth
#then it will sort into depth about 1m bins. Ex. 3m is from 2.5m-3.49m
#All flags are removed

pacman::p_load(tidyverse,lubridate, plotly,plyr)

#download the data from the EDI folder on GitHub

download.file("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCRcatwalk/2021/CCR_Catwalk_EDI_2021.csv", "CCR_Catwalk_EDI_2021.csv")
download.file("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataAlreadyUploadedToEDI/EDIProductionFiles//MakeEML_CCRcatwalk/2021/CCR_Depth_offsets_2021.csv", "CCR_Depth_offset_2021.csv")

#read in data
ccr=read.csv("CCR_Catwalk_EDI_2021.csv")
depth=read.csv("CCR_Depth_offsets_2021.csv")

#take out EXO data so you can add it back in later
EXO=ccr%>%
  select(Reservoir, Site, DateTime, starts_with("EXO"))


#select the sensors on the temp string and then pivot the data frame longer to merge the offset 
#dataset. 
#
ccr_new=ccr%>%
  select(Reservoir,Site,DateTime, starts_with("Ther"), starts_with("Lvl"))%>%
  pivot_longer(-c(Reservoir,Site,DateTime,LvlDepth_m_13), names_to="Sensor", values_to="Reading", values_drop_na=FALSE)%>%
  separate(Sensor,c("Sensor","Units","Position"),"_")%>%
  mutate(Position=as.numeric(Position))%>%
  merge(.,depth)%>%#add in the offset file
  mutate(Sensor_depth=LvlDepth_m_13-Offset)%>%#find the depth of the sensor
  mutate(Rounded_depth=Sensor_depth)%>%
  mutate(Rounded_depth=round_any(Rounded_depth, 0.5))#rounded the depth to 0.5 if I did to 1 there would be duplicates
  #for some of the depth. To make the bin size larger or smaller change the closest multiplier which here is 0.5


#change back to a wide data form 
ccr_new2=ccr_new%>%
  mutate(Depth = Rounded_depth)%>%
  mutate(Depth = ifelse(is.na(Sensor_depth), NA, Depth))%>%
  mutate(Depth = ifelse(Sensor_depth <= 0, NA, Depth))%>%
  filter(!is.na(Depth))%>%
  mutate(Sensor=paste(Sensor,Units,Depth,"m", sep = "_"))%>%
  select(Reservoir,Site,DateTime,Sensor, Reading)%>%
  #This section makes the data into a wide format where each header is a sensor and a depth 
  #ex.ThermistorTemp_C_0_m is the surface readings and they can be from whichever thermistor is at 0-0.25m under the water
  pivot_wider(
    id_cols=c(Reservoir, Site, DateTime),
    names_from = Sensor,
    names_sep = "_",
    values_from = c(Reading))%>%
  #reorganize the headers
  select(Reservoir, Site, DateTime, ThermistorTemp_C_0_m,ThermistorTemp_C_0.5_m, ThermistorTemp_C_1_m,ThermistorTemp_C_1.5_m,
         ThermistorTemp_C_2_m,ThermistorTemp_C_2.5_m,ThermistorTemp_C_3_m,ThermistorTemp_C_3.5_m,ThermistorTemp_C_4_m,
         ThermistorTemp_C_4.5_m,ThermistorTemp_C_5_m,ThermistorTemp_C_5.5_m,ThermistorTemp_C_6_m,ThermistorTemp_C_6.5_m,
         ThermistorTemp_C_7_m,ThermistorTemp_C_7.5_m, ThermistorTemp_C_8_m,ThermistorTemp_C_8.5_m,ThermistorTemp_C_9_m,
         ThermistorTemp_C_9.5_m,ThermistorTemp_C_10_m,ThermistorTemp_C_10.5_m, ThermistorTemp_C_11_m, ThermistorTemp_C_13.5_m,
         ThermistorTemp_C_14_m,ThermistorTemp_C_14.5_m,ThermistorTemp_C_15_m,
         ThermistorTemp_C_17.5_m,ThermistorTemp_C_18_m,ThermistorTemp_C_18.5_m,ThermistorTemp_C_19_m,
         Lvl_psi_17.5_m,Lvl_psi_18_m,Lvl_psi_18.5_m,Lvl_psi_19_m,LvlTemp_C_17.5_m,LvlTemp_C_18_m,
         LvlTemp_C_18.5_m,LvlTemp_C_19_m)

#add the EXO back in 
  ccr_new2=ccr_new%>%
    merge(.,EXO)
  
#write the csv  
  write.csv(ccr_new2, 'CCR_EDI_bydepth_2021.csv', row.names = FALSE, quote=FALSE)