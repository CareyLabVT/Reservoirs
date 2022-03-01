##sorting the BVR data by depth as opposed to position
#by A. Breef-Pilz 10 FEB 21
#Edited 21 FEB 22 by ABP

# Since the water level varies in BVR this script will at first apply a depth to each reading.
# You can stop there or then put the be sorted into columns by depth. 
# For this data set all Flags are removed 

# Install packages
pacman::p_load(tidyverse,lubridate, plotly,plyr)

#folder the files could be found in
folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform/2021/"

#GET THE DATA FILES
#download the data from the EDI folder on GitHub or from EDI
#files on EDI
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.725.2

#Files on GitHub
#Files needed to add a depth column to your data:
#This the file from EDI. This is the same out put as if you ran the BVR QAQC scripts
download.file("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform/BVR_platform_2020_2021.csv", paste0(folder, "BVR_platform_2020_2021.csv"))

#This is the file of the offset depths of each sensor and needed to calculate the depth of each sensor at each reading.
download.file("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform/BVR_Depth_offsets_2020_2021.csv", paste0(folder, "BVR_Depth_offsets_2020_2021.csv"))

#Files can also be downloaded from EDI.

#read in data but make sure your file path is right
bvr=read.csv(paste0(folder, "/BVR_platform_2020_2021.csv"))
depth=read.csv(paste0(folder, "/BVR_Depth_offsets_2020_2021.csv"))


########This has to happen whether you want to end up with a data frame in the long format or the wide format########################


# take out EXO data so you can add it back in later
# the EXO has its own depth sensor which is calibrated to the atmosphere. 
# It is also on a buoy so should be about 1.5m 
EXO=bvr%>%
  select(Reservoir, Site, DateTime, starts_with("EXO"))


# Select the sensors on the temp string because they are stationary.
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


# combine the pre April 5th and the post April 5th. Drop if the readings are NA. Drop if the sensor depth is NA because can't
# figure out the depth of the senesors. This will give you a depth for each sensor reading. 
bvr_by_depth=bvr_pre_05APR21%>%
  rbind(.,bvr_post_05APR21)%>%
  filter(!is.na(Reading))%>%
  filter(!is.na(Sensor_depth))%>%
  select(-Offset_before_05APR21, -Offset_after_05APR21, -Distance_above_sediments, -Depth_m_13)

########### This keeps the data in the long format and adds the EXO ############################
#This data frame is ver very very long

#Puts the EXO data into a long format with the right columns

#First need to create a data frame to name the variables to align with BVR FLARE code 
Sensor <- c("EXOTemp", "EXOCond", "EXOSpCond", "EXOTDS", "EXODOsat",
            "EXODO", "EXOChla", "EXOChla", "EXOBGAPC", "EXOBGAPC",
            "EXOfDOM", "EXOfDOM","EXOTurbidity")
Variable <-c("temperature","cond","spcond","tds","oxygen",
             "oxygen","chla","chla","bgapc","bgapc",
             "fdom","fdom", "turb")
df<-data.frame(Sensor, Variable) 

# Now that's created with can put the EXO data into long format. Select the columns and then piviot longer. Add columns for
# method which is the name of the sensor and one for the deth. Then merge with df to get the variable names, take out 
# the NA readings and then select only the columns you need. 
EXO_long=EXO%>%
  select(Reservoir, Site, DateTime, EXOTemp_C_1_5, EXOCond_uScm_1_5, EXOSpCond_uScm_1_5, EXOTDS_mgL_1_5, EXODOsat_percent_1_5,
         EXODO_mgL_1_5, EXOChla_RFU_1_5, EXOChla_ugL_1_5, EXOBGAPC_RFU_1_5, EXOBGAPC_ugL_1_5,
         EXOfDOM_RFU_1_5, EXOfDOM_QSU_1_5,EXOTurbidity_FNU_1_5)%>%
  pivot_longer(-c(Reservoir,Site,DateTime), names_to="Sensor", values_to="Reading", values_drop_na=FALSE)%>%
  separate(Sensor,c("Sensor","Units","Meter","Half"),"_")%>%
  mutate(Method="exo_sensor")%>%
  mutate(Depth=1.5)%>%
  merge(.,df, by="Sensor")%>%
  filter(!is.na(Reading))%>%
  select(Reservoir,Site, DateTime, Method, Variable, Units, Reading, Depth)

# Get the bvr_by_depth (thermistor and RDO) data frame ready to merge with EXO_long
# Create a data frame to get variable names and the method for FLARE

Sensor<-c("ThermistorTemp", "RDO", "RDOsat", "RDOTemp", "Lvl", "LvlTemp")
Variable<-c("temperature", "oxygen", "oxygen", "temperature", "pressure", "temperature")
Method<-c("thermistor", "do_sensor", "do_sensor", "do_sensor", "pressure_sensor", "pressure_sensor")

df2<-data.frame(Sensor, Variable, Method)

temp_long=bvr_by_depth%>%
  select(Reservoir,Site,DateTime,Sensor, Units, Reading, Rounded_depth_hundreth)%>%
  rename("Depth"=Rounded_depth_hundreth)%>%
   merge(.,df2, by="Sensor")%>%
   select(Reservoir,Site, DateTime, Method, Variable, Units, Reading, Depth)
 
 HLW_FLARE_output=rbind(temp_long,EXO_long)


#write the csv  
setwd("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform/2021/")
write.csv(HLW_FLARE_output, 'BVR_longoutput_FLARE_2020_2021.csv', row.names = FALSE)

################################# END FOR HLW FOR FLARE ########################################

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

############### CONTINUE HERE IF YOU WANT TO PUT THE DATA BACK IN A WIDE FORMAT #######################

# change back to a wide data form. This puts the readings into depth columns but that means it does introduce
# NAs as the water depth changes. To do this thermistor in position 1 is dropped because thermistor 1 and thermistor 2
# are less than 1m apart and can't have two values in one time point for 1 depth. 
# bvr_wide=bvr_by_depth%>%
#   filter(Position>1)%>%
#   select(c(-Sensor_depth, -Rounded_depth_hundreth, -Position))%>%
#   mutate(Sensor=paste(Sensor,Units,Rounded_depth_whole,"m", sep = "_"))%>%
#   pivot_wider(
#     id_cols=c(Reservoir, Site, DateTime),
#     names_from = Sensor,
#     names_sep = "_",
#     values_from = Reading)%>%
#     mutate(ThermistorTemp_C_surface=ThermistorTemp_C_0_m)%>%
#   select(Reservoir, Site, DateTime, ThermistorTemp_C_surface, ThermistorTemp_C_1_m,ThermistorTemp_C_2_m,
#          ThermistorTemp_C_3_m, ThermistorTemp_C_4_m, ThermistorTemp_C_5_m, ThermistorTemp_C_6_m, ThermistorTemp_C_7_m, ThermistorTemp_C_8_m,
#          ThermistorTemp_C_9_m, ThermistorTemp_C_10_m, ThermistorTemp_C_11_m, RDO_mgL_2_m, RDO_mgL_3_m, RDO_mgL_4_m, RDO_mgL_5_m, 
#          RDOsat_percent_2_m, RDOsat_percent_3_m, RDOsat_percent_4_m, RDOsat_percent_5_m, RDOTemp_C_2_m, RDOTemp_C_3_m, RDOTemp_C_4_m,RDOTemp_C_5_m, 
#          RDO_mgL_10_m, RDO_mgL_11_m, RDO_mgL_12_m, RDOsat_percent_10_m, RDOsat_percent_11_m, RDOsat_percent_12_m, RDOTemp_C_11_m, RDOTemp_C_12_m, 
#          Lvl_psi_10_m, Lvl_psi_11_m, Lvl_psi_12_m,LvlTemp_C_10_m,LvlTemp_C_11_m,LvlTemp_C_12_m)
  
# add the EXO back in 
  # bvr_wide=bvr_wide%>%
  #   merge(.,EXO, by="DateTime")
  
# write the csv  
#setwd("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform/2021")
#  write.csv(bvr_wide, 'BVR_bydepth_2020_2021.csv', row.names = FALSE)