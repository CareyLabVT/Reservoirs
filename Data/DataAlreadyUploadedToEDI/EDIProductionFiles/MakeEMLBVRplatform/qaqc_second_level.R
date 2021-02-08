# Master QAQC script in prep for publishing catwalk sensor string data to EDI
# this script combines code from other QAQC scripts found in misc_QAQC_scipts folder
# as well as other data files from misc_data_files
# final EDI-ready file outputs directly to MakeEMLCatwalk/2020 folder
# Set up ----
pacman::p_load("RCurl","tidyverse","lubridate", "plotly", "magrittr")
folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles"
source(paste0(folder, "MakeEMLBVRplatform/qaqc_first_level.R"))


# run standard qaqc
#data_file <- paste0(folder, 'misc_data_files/Catwalk_2020.csv')
#maintenance_file <- paste0(folder, "misc_data_files/CAT_MaintenanceLog_2020.txt")
#output_file <- paste0(folder, "misc_data_files/Catwalk_first_QAQC_2020.csv")
#qaqc(data_file, maintenance_file, output_file)

# read in qaqc function output
bvrdata_clean <- read.csv("BVRplatform_clean.csv") 
bvrdata_clean$DateTime<-as.POSIXct(bvrdata_clean$DateTime,format = "%Y-%m-%d %H:%M:%S")

#setting the raw data for comparison
bvrdata_raw=bvrdata2


# Flag values
# 0: no flag
# 1: value removed due to maintenance and set to NA
# 2: negative or outlier value removed and set to NA, see Methods section for more detail on QAQC process
# 3: negative values set to 0
# 4: value removed due to fouling and set to NA
# 5: questionable value due to potential fouling
# 6: very questionable value due to potential fouling. Values adjusted using a linear or square root function to match high-resolution CTD profiles are given in RDO_mgL_5 and RDO_sat_percent_5
# 7: missing data
# 8: Value corrected using a constant offset due to two thermistor malfunctions in Fall 2020


###########################################################################################################################################################################
# temp qaqc ----

#Two of the thermistors started to read higher than the one above them in fall 2020. Fixed this using a constant offset. 
#methods described in metadat

#add a depth column which we set NAs for when the thermistor is out of the water. Flag 2
bvrdata_clean=bvrdata_clean%>%
  mutate(depth_1=Depth_m_13-11.81)%>%
  mutate(depth_2=Depth_m_13-11.478)


# check Temp_1 data
#Temp_1 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_1)) +
 # geom_point()
#ggplotly(Temp_1)

#change the temp to NA when the thermistor is clearly out of the water which we used to determine the depth of the temp string
bvrdata_clean=bvrdata_clean%>%
  mutate(Flag_Temp_1= ifelse(DateTime>="2020-10-26 12:10:00 tz=Etc/GMT+5 "&DateTime<="2020-10-30 09:40:00 tz=Etc/GMT+5",2,Flag_Temp_1))%>%
  mutate(ThermistorTemp_C_1= ifelse(DateTime>="2020-10-26 12:10:00 tz=Etc/GMT+5 "&DateTime<="2020-10-30 09:40:00 tz=Etc/GMT+5",NA,ThermistorTemp_C_1))%>%
  mutate(Flag_Temp_1= ifelse(!is.na(depth_1) & depth_1<0 ,2,Flag_Temp_1))%>%
  mutate(ThermistorTemp_C_1=ifelse(!is.na(depth_1) & depth_1<0,NA,ThermistorTemp_C_1))


#surf <- ggplot(data = bvrdata_clean_flag, aes(x = DateTime, y = ThermistorTemp_C_surface)) +
#  geom_point()
#ggplotly(surf)

# check thermistor 2 data
#Temp_2 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_2)) +
 # geom_point()
#ggplotly(Temp_2)

#change the temp to NA when the thermistor is clearly out of the water which we used to determine the depth of the temp string
bvrdata_clean=bvrdata_clean%>%
  mutate(Flag_Temp_2= ifelse(DateTime>="2020-10-26 12:10:00 tz=Etc/GMT+5 "&DateTime<="2020-10-30 09:40:00 tz=Etc/GMT+5",2,Flag_Temp_2))%>%#this is when the pressure sensor was unplugged
  mutate(ThermistorTemp_C_2= ifelse(DateTime>="2020-10-26 12:10:00 tz=Etc/GMT+5 "&DateTime<="2020-10-30 09:40:00 tz=Etc/GMT+5",NA,ThermistorTemp_C_2))%>%
  mutate(Flag_Temp_2= ifelse(!is.na(depth_2) & depth_2<0 ,2,Flag_Temp_2))%>%
  mutate(ThermistorTemp_C_2=ifelse(!is.na(depth_2) & depth_2<0,NA,ThermistorTemp_C_2))
  
#take this out after you set the values to NA
bvrdata_clean=bvrdata_clean%>%
  select(-depth_1,-depth_2)
  

# check Thermistor 3 temp data
#Temp_3 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_3)) +
 # geom_point()
#Temp_3
#ggplotly(Temp_3)



# check Thermistor 4 temp data
#Temp_4 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_4)) +
#  geom_point()
#Temp_4
#ggplotly(Temp_4)


# check Thermistor 5 temp data
#Temp_5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_5)) +
#  geom_point()
#Temp_5
#ggplotly(Temp_5)
# a few isolated points that we flagged but didn't change
bvrdata_clean <- bvrdata_clean %>%
  mutate(Flag_Temp_5 = ifelse(DateTime>="2020-07-11 9:00:00 tz=Etc/GMT+5" &DateTime<="2020-07-11 9:20:00 tz=Etc/GMT+5"
                              , 5, Flag_Temp_5)) %>%
  mutate(Flag_Temp_5 = ifelse(DateTime>="2020-07-13 02:20:00 tz=Etc/GMT+5" &DateTime<="2020-07-13 02:40:00 tz=Etc/GMT+5"
                               ,5, Flag_Temp_5))
  

# check Thermistor 6 temp data
#Temp_6 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_6)) +
#  geom_point()
#Temp_6
#ggplotly(Temp_6)

#check that the temp from the RDO and the thermistor line up
#plot(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_6, type='l')
#points(bvrdata_clean$DateTime, bvrdata_clean$RDOTemp_C_6, type='l', col="red")


# check Thermistor 7 temp data
#Temp_7 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_7)) +
#  geom_point()
#Temp_7
#ggplotly(Temp_7)


# check Thermistor 8 temp data
#Temp_8 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_8)) +
#  geom_point()
#Temp_8
#ggplotly(Temp_8)


# check Thermistor 9 temp data
#Temp_9 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_9)) +
#  geom_point()
#Temp_9
#ggplotly(Temp_9)


# check Thermistor 10 temp data
#Temp_10 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_10)) +
#  geom_point()
#Temp_10
#ggplotly(Temp_10)


#flag the NAs when the thermistors weren't programed. 
bvrdata_clean=bvrdata_clean%>%
  mutate(Flag_Temp_11=ifelse(DateTime<"2020-10-05 12:05:00 tz=Etc/GMT+5", 7, Flag_Temp_11))%>%
  mutate(Flag_Temp_12=ifelse(DateTime<"2020-10-05 12:05:00 tz=Etc/GMT+5", 7, Flag_Temp_12))%>%
  mutate(Flag_Temp_13=ifelse(DateTime<"2020-10-05 12:05:00 tz=Etc/GMT+5", 7, Flag_Temp_13))

# check Thermistor 11 temp data
#Temp_11 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_11)) +
#  geom_point()
#Temp_11
#ggplotly(Temp_11)

# check Thermistor 12 temp data
#Temp_12 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_12)) +
#  geom_point()
#Temp_12
#ggplotly(Temp_12)

# check Thermistor 13 temp data
#Temp_13 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_13)) +
#  geom_point()
#Temp_13
#ggplotly(Temp_13)



###Put all the points on the same plot using base R
#plot(bvrdata_clean$DateTime,bvrdata_clean$ThermistorTemp_C_1, main="Water Temp", xlab="Time", ylab="degrees C", type='l', col="black", lwd=1.5, ylim=c(0,40))
#points(bvrdata_clean$DateTime,bvrdata_clean$ThermistorTemp_C_1, col="firebrick4", type='l', lwd=1.5)
#points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_2, col="firebrick1", type='l', lwd=1.5)
#points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_3, col="DarkOrange1", type='l', lwd=1.5)
#points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_4, col="gold", type='l', lwd=1.5)
#points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_5, col="greenyellow", type='l', lwd=1.5)
#points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_6, col="medium sea green", type='l', lwd=1.5)
#points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_7, col="sea green", type='l', lwd=1.5)
#points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_8, col="DeepSkyBlue4", type='l', lwd=1.5)
#points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_9, col="blue2", type='l', lwd=1.5)
#points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_10, col="blue4", type='l', lwd=1.5)
#points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_11, col="darkslateblue", type='l', lwd=1.5)
#points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_12, col="magenta2", type='l', lwd=1.5)
#points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_13, col="darkmagenta", type='l', lwd=1.5)

###########################################################################################################################################################################
# DO qaqc ----

#Let's see what everything looks like after the first round of QAQC 
#Do at Thermistor 6
#DO_6 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDO_mgL_6)) +
#  geom_point()
#DO_6
#ggplotly(DO_6)
#DOsat_6 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDOsat_percent_6)) +
#  geom_point()
#DOsat_6
#ggplotly(DOsat_6)

#manually flag some high values
bvrdata_clean=bvrdata_clean%>%
  mutate(Flag_DO_6 = ifelse(DateTime>="2020-10-22 9:20:00 tz=Etc/GMT+5" &DateTime<="2020-10-22 10:20:00 tz=Etc/GMT+5"
         , 5, Flag_DO_6)) 

#Do at Thermistor 13
#DO_13 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDO_mgL_13)) +
#  geom_point()
#DO_13
#ggplotly(DO_13)
#DOsat_13 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDOsat_percent_13)) +
#  geom_point()
#DOsat_13
#ggplotly(DOsat_13)

#Do from the EXO
#EXODO_1.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXODO_mgL_1.5)) +
#  geom_point()
#EXODO_1.5
#ggplotly(EXODO_1.5)
#EXODOsat_1.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXODOsat_percent_1.5)) +
#  geom_point()
#EXODOsat_1.5
#ggplotly(EXODOsat_1.5)


##read in ctd values to see how they compare to sensor values
#ctd=read.csv("ctd_2018-2020_flagged.csv")
#ctd$Date<-as.POSIXct(ctd$Date,format = "%Y-%m-%d %H:%M:%S")

#read in YSI to check the DO 
#ysi=read.csv("YSI_PAR_profiles_2013-2020.csv")
#ysi$DateTime<-as.POSIXct(ysi$DateTime,format = "%Y-%m-%d %H:%M:%S")

#get the bottom DO values to compare with DO_13
#ctd_bottom=ctd%>%
#  filter(Reservoir=="BVR")%>%
#  filter(Date>"2020-01-01 00:00:00")%>%
#  select(Date, Depth_m, Temp_C,DO_mgL,DO_pSat)%>%
#  filter(Depth_m>10)%>%
#  group_by(Date)%>%
#  summarise(mean_DO=mean(DO_mgL))

#ysi_bottom=ysi%>%
#  filter(Reservoir=="BVR")%>%
#  filter(DateTime>"2020-01-01 00:00:00")%>%
#  select(DateTime, Depth_m, DO_mgL)%>%
#  filter(Depth_m>10)%>%
#  group_by(DateTime)%>%
# summarise(mean_DO=mean(DO_mgL))

#plot(bvrdata_clean$DateTime, bvrdata_clean$RDO_mgL_13, type='l')
#points(ctd_bottom$Date, ctd_bottom$mean_DO, type='p', col= "red", cex= 1.0)
#points(ysi_bottom$DateTime, ysi_bottom$mean_DO, type = "p", col = "purple")
  
#Check the EXO against the CTD and YSI
#ctd_1.5=ctd%>%
#  filter(Reservoir=="BVR")%>%
#  filter(Date>"2020-01-01 00:00:00")%>%
#  select(Date, Depth_m, Temp_C,DO_mgL,DO_pSat)%>%
#  filter(Depth_m>1.48 & Depth_m<1.52)%>%
#  group_by(Date)%>%
#  summarise(mean_DO=mean(DO_mgL))

#ysi_1.5=ysi%>%
#  filter(Reservoir=="BVR")%>%
#  filter(DateTime>"2020-06-22 00:00:00")%>%
#  select(DateTime, Depth_m, DO_mgL)%>%
#  filter(Depth_m>=1 & Depth_m<=2)%>%
#  mutate(day = floor_date(DateTime, "day")) %>%
#  group_by(day) %>%
#  summarize(avg = mean(DO_mgL))

#plot(bvrdata_clean$DateTime, bvrdata_clean$EXODO_mgL_1.5, type='l', ylim= c(3,13))
#points(ctd_1.5$Date, ctd_1.5$mean_DO, type='p', col= "red",pch=16, cex= 1.0)
#points(ysi_1.5$day, ysi_1.5$avg, type ='p', col="darkorange", pch=16,cex=1.0 )

#check DO_6 against ctd and ysi
#ctd_middle=ctd%>%
#  filter(Reservoir=="BVR")%>%
#  filter(Date>"2020-01-01 00:00:00")%>%
#  select(Date, Depth_m, Temp_C,DO_mgL,DO_pSat)%>%
#  filter(Depth_m>3.9 & Depth_m<5.1)%>%
#  mutate(day = floor_date(Date, "day")) %>%
#  group_by(day) %>%
#  summarize(avg = mean(DO_mgL))

#ysi_middle=ysi%>%
#  filter(Reservoir=="BVR")%>%
#  filter(DateTime>"2020-06-22 00:00:00")%>%
#  select(DateTime, Depth_m, DO_mgL)%>%
#  filter(Depth_m>=4 & Depth_m<=5)%>%
#  mutate(day = floor_date(DateTime, "day")) %>%
#  group_by(day) %>%
#  summarize(avg = mean(DO_mgL))



#plot(bvrdata_clean$DateTime, bvrdata_clean$RDO_mgL_6, type='l')
#points(ctd_middle$day, ctd_middle$avg, type='p', col= "red",pch=16, cex= 1.0)
#points(ysi_middle$day, ysi_middle$avg, type ='p', col="darkorange", pch=16,cex=1.0 )
  




###########################################################################################################################################################################
# chl and phyco qaqc ----


#chl_rfu <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOChla_RFU_1.5)) +
#  geom_point() 
#chl_rfu
#ggplotly(chl_rfu)

#plot(bvrdata_raw$TIMESTAMP, bvrdata_raw$Chla_RFU_1, type ="l")
#points(bvrdata_clean$DateTime, bvrdata_clean$EXOChla_RFU_1.5, type= "l", col="red")

#Take out and flagging some points
bvrdata_clean=bvrdata_clean%>%
  mutate(Flag_Chla= ifelse(DateTime=="2020-08-01 12:50:00 tz=Etc/GMT+5", 5, Flag_Chla))%>%
  mutate(Flag_Chla= ifelse(DateTime>="2020-10-29 10:10:00 tz=Etc/GMT+5" &DateTime<="2020-10-29 12:00:00 tz=Etc/GMT+5", 2, Flag_Chla))%>%
  mutate(Flag_Chla= ifelse(DateTime=="2020-12-28 03:20:00 tz=Etc/GMT+5", 2, Flag_Chla))
  mutate(EXOChla_RFU_1.5= ifelse(DateTime>="2020-10-29 10:10:00 tz=Etc/GMT+5" &DateTime<="2020-10-29 12:00:00 tz=Etc/GMT+5", NA, EXOChla_RFU_1.5))%>%
  mutate(EXOChla_mgL_1.5= ifelse(DateTime>="2020-10-29 10:10:00 tz=Etc/GMT+5" &DateTime<="2020-10-29 12:00:00 tz=Etc/GMT+5", NA, EXOChla_mgL_1.5))%>%
  mutate(EXOChla_RFU_1.5= ifelse(DateTime=="2020-12-28 03:20:00 tz=Etc/GMT+5",NA, EXOChla_RFU_1.5))%>%
  mutate(EXOChla_mgL_1.5= ifelse(DateTime=="2020-12-28 03:20:00 tz=Etc/GMT+5", NA, EXOChla_mgL_1.5))
  
#bvrdata_clean%>%
#    select(DateTime, Flag_Chla)%>%
#    filter(Flag_Chla>1)%>%
#    print()


#Checking out the phycos and flagging points, EXOBGAPC_RFU_1.5, EXOBGAPC_ugL_1.5  
  
#phyco_ugl <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOBGAPC_ugL_1.5)) +
#  geom_point() 
#phyco_ugl
#ggplotly(phyco_ugl)

#add flaggs and set to NA 

bvrdata_clean=bvrdata_clean%>%
  mutate(Flag_Phyco= ifelse(DateTime=="2020-08-01 12:50:00 tz=Etc/GMT+5", 5, Flag_Phyco))%>%
  mutate(Flag_Phyco= ifelse(DateTime>="2020-10-29 10:10:00 tz=Etc/GMT+5" &DateTime<="2020-10-29 12:00:00 tz=Etc/GMT+5", 2, Flag_Phyco))%>%
  mutate(Flag_Phyco= ifelse(DateTime=="2020-12-25 06:20:00 tz=Etc/GMT+5", 2, Flag_Phyco))%>%
  mutate(EXOBGAPC_RFU_1.5= ifelse(DateTime>="2020-10-29 10:10:00 tz=Etc/GMT+5" &DateTime<="2020-10-29 12:00:00 tz=Etc/GMT+5", NA, EXOBGAPC_RFU_1.5))%>%
  mutate(EXOBGAPC_ugL_1.5= ifelse(DateTime>="2020-10-29 10:10:00 tz=Etc/GMT+5" &DateTime<="2020-10-29 12:00:00 tz=Etc/GMT+5", NA, EXOBGAPC_ugL_1.5))%>%
  mutate(EXOBGAPC_RFU_1.5= ifelse(DateTime=="2020-12-28 03:20:00 tz=Etc/GMT+5",NA, EXOBGAPC_RFU_1.5))%>%
  mutate(EXOBGAPC_ugL_1.5= ifelse(DateTime=="2020-12-28 03:20:00 tz=Etc/GMT+5", NA, EXOBGAPC_ugL_1.5))

###########################################################################################################################################################################
# fdom qaqc----
# QAQC from DWH to remove major outliers from fDOM data that are 2 sd's greater than the previous and following datapoint
# QAQC done on 2018-2020 dataset

#plot(bvrdata_raw$TIMESTAMP, bvrdata_raw$fDOM_QSU_1, type ="l")
#points(bvrdata_clean$DateTime, bvrdata_clean$EXOfDOM_QSU_1.5, type= "l", col="red")

#fDOM_QSU <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOfDOM_QSU_1.5)) +
#    geom_point()
  #  ggtitle("fDOM (QSU) pre QAQC")
#fDOM_QSU
  #ggplotly(fDOM_pre_QAQC)

#plot(bvrdata_raw$TIMESTAMP, bvrdata_raw$fDOM_RFU_1, type ="l")
#points(bvrdata_clean$DateTime, bvrdata_clean$EXOfDOM_RFU_1.5, type= "l", col="red")

#fDOM_RFU <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOfDOM_RFU_1.5)) +
#  geom_point()
#  ggtitle("fDOM (QSU) pre QAQC")
#fDOM_RFU
#ggplotly(fDOM_pre_QAQC)

#bvrdata_clean%>%
#  select(DateTime, Flag_fDOM)%>%
#  filter(Flag_fDOM>1)%>%
#  print()
#conductivity check

#cond <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOCond_uScm_1.5)) +
#  geom_point()
#cond
#ggplotly(cond)

#flag some high vlaues
bvrdata_clean=bvrdata_clean%>%
  mutate(Flag_Cond= ifelse(DateTime=="2020-08-01 12:40:00", 5, Flag_Cond))%>%
  mutate(Flag_Cond= ifelse(DateTime>="2020-11-12 20:30:00" &DateTime<="2020-11-12 20:40:00", 5, Flag_Cond))

#cond_sp <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOSpCond_uScm_1.5)) +
#  geom_point()
#cond_sp
#ggplotly(cond_sp)


#tds <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOTDS_mgL_1.5)) +
#  geom_point()
#tds
#ggplotly(tds)

bvrdata_clean=bvrdata_clean%>%
  mutate(Flag_TDS= ifelse(DateTime=="2020-08-01 12:40:00", 5, Flag_TDS))%>%
  mutate(Flag_TDS= ifelse(DateTime>="2020-11-12 20:30:00" &DateTime<="2020-11-12 20:40:00", 5, Flag_TDS))
    
#Pressure graphs
#Exo_pressure <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXO_pressure_1.5)) +
#  geom_point()
#Exo_pressure
#ggplotly(Exo_pressure)

#Exo_depth <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXO_depth)) +
#  geom_point()
#Exo_depth
#ggplotly(Exo_depth)

#Check the depth and flag when the wire wasn't connected
#Lvl_pressure <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = Lvl_psi_13)) +
#  geom_point()
#Lvl_pressure
#ggplotly(Lvl_pressure)

#Add flags when the wire was not connected
bvrdata_clean=bvrdata_clean%>%
  mutate(Flag_Lvl=ifelse(is.na(Lvl_psi_13)& DateTime>="2020-10-26 12:00:00" & DateTime<="2020-10-30 09:40:00",
                         7, Flag_Lvl))


#temp_pressure <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = LvlTemp_C_13)) +
#  geom_point()
#temp_pressure
#ggplotly(temp_pressure)

#temp_do <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDOTemp_C_13)) +
#  geom_point()
#temp_do
#ggplotly(temp_do)

#plot(bvrdata_clean$DateTime, bvrdata_clean$LvlTemp_C_0.5, type="l")
#points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_0.5, type= "l", col="red")
#points(bvrdata_clean$DateTime, bvrdata_clean$RDOTemp_C_0.5, type= "l", col="darkorange")

#Exo_temp <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOTemp_C_1.5)) +
#  geom_point()
#Exo_temp
#ggplotly(Exo_temp)

#DO_temp_7.5<- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDOTemp_C_7.5)) +
#  geom_point()
#DO_temp_7.5
#ggplotly(DO_temp_7.5)

#plot(bvrdata_clean$DateTime, bvrdata_clean$CR6_Batt_V, type="l", ylim=c(10,16))
#points(bvrdata_clean$DateTime, bvrdata_clean$EXO_cablepower, type="l", col="darkorange")

#plot(bvrdata_clean$DateTime, bvrdata_clean$EXO_battery, type= "l")
#plot(bvrdata_clean$DateTime, bvrdata_clean$EXO_wiper, type="l", col="black")

#temp_panel<- ggplot(data = bvrdata_clean, aes(x = DateTime, y = CR6Panel_Temp_C)) +
#  geom_point()
#temp_panel
#ggplotly(temp_panel)

#record<- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RECORD)) +
# geom_point()
#record
#ggplotly(record)


####Records power and other variables to just check for anything funny

###########################################################################################################################################################################
# write final csv ----
# some final checking of flags
#for (i in 1:nrow(bvrdata_clean)) {
 # if(is.na(bvrdata_clean$Flag_fDOM[i])){
#    bvrdata_clean$Flag_fDOM[i] <- 0
 # }
#}
str(bvrdata_clean)

# rearrange the cols
bvrdata_clean <- bvrdata_clean %>% 
  select(Reservoir, Site, DateTime, ThermistorTemp_C_1:ThermistorTemp_C_13,
         RDO_mgL_6, RDOsat_percent_6,
         RDOTemp_C_6, RDO_mgL_13, RDOsat_percent_13, RDOTemp_C_13, Lvl_psi_13, Depth_m_13,LvlTemp_C_13,
         EXOTemp_C_1.5, EXOCond_uScm_1.5, EXOSpCond_uScm_1.5, EXOTDS_mgL_1.5, EXODOsat_percent_1.5, 
         EXODO_mgL_1.5, EXOChla_RFU_1.5, EXOChla_ugL_1.5, EXOBGAPC_RFU_1.5, EXOBGAPC_ugL_1.5, 
         EXOfDOM_RFU_1.5, EXOfDOM_QSU_1.5, EXO_pressure_1.5, EXO_depth, EXO_battery, EXO_cablepower, 
         EXO_wiper,  RECORD, CR6_Batt_V, CR6Panel_Temp_C, 
         Flag_All,Flag_Temp_1:Flag_Temp_13,Flag_DO_1.5, Flag_DO_6, Flag_DO_13,Flag_Lvl, 
         Flag_Chla,Flag_Phyco,Flag_TDS,Flag_fDOM,Flag_Cond )


write.csv(bvrdata_clean, 'BVR_EDI_2020.csv', row.names = FALSE)
