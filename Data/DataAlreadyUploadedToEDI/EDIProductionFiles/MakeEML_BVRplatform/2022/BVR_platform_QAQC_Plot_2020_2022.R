# Master QAQC script in prep for publishing BVR platform sensor string data to EDI
# this script combines code from the first level qaqc script which takes out maintenance times
# negative and infainite values and automated qaqc as described in the metadata
# final EDI-ready file outputs directly to MakeEMLBVR
# Set up ----
pacman::p_load(RCurl,tidyverse,lubridate, plotly, magrittr)
folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform/2022/"
source(paste0(folder, "BVR_platform_QAQC_function_2020_2022.R"))

# download most up to date bvr data streaming from the sensors
#manually downloaded from the datalogger that fills in missing data from the streamed data 
#maintenance log so we can flag when the sensors were being worked on or other problems
download.file('https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data/bvre-waterquality.csv',paste0(folder, "/bvre-waterquality.csv")) 
download.file('https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/BVRPlatform/BVR_manual_2022.csv',paste0(folder, "/BVRmanualplatform.csv"))
download.file("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data/BVR_maintenance_log.txt",paste0(folder, "/BVR_maintenance_log_2020_2021.txt"))

# run standard qaqc these are where the data entered in the function are defined
data_file <- paste0(folder, '/bvre-waterquality.csv')#this is from github and pushed every 4 hours
data2_file <- paste0(folder, '/BVRmanualplatform.csv')# this is data downloaded directly from the data logger and gets up dated periodiclly to account for missing data gaps
maintenance_file <- paste0(folder, "/BVR_maintenance_log_2020_2021.txt") #this is the maintenance log for QAQC purposes
output_file <- paste0(folder, "/BVRplatform_clean.csv")
qaqc(data_file, data2_file, maintenance_file, output_file)


# read in qaqc function output
bvrdata_clean <- read.csv(output_file) 

# subset file to only publish for the current year
bvrdata_clean = bvrdata_clean%>%
  filter(DateTime<"2022-01-01 00:00")

#check the flag column to make sure there are no NAs in the Flag column
Flags=bvrdata_clean%>%
  select(starts_with("Flag"))

for(b in 1:nrow(Flags)){
  print(colnames(Flags[b]))
  print(table(Flags[,b], useNA = "always"))
}

#current time of QAQC for graphing
start_time="2021-01-01 00:00"
end_time="2021-12-31 23:59"

#change the datetime into a useable and easy to graph format
bvrdata_clean$DateTime<-as.POSIXct(bvrdata_clean$DateTime, "%Y-%m-%d %H:%M", tz = "EST")


# Flag values
# 0: no flag
# 1: value removed due to maintenance and set to NA
# 2: negative or outlier value removed and set to NA, see Methods section for more detail on QAQC process
# 3: negative values set to 0
# 4: value removed due to fouling and set to NA
# 5: questionable value but left in the dataset
# 6: very questionable value due to potential fouling. Values adjusted using a linear or square root function to match high-resolution CTD profiles are given in RDO_mgL_5 and RDO_sat_percent_5
# 7: missing data
# 8: Value corrected using a constant offset due to two thermistor malfunctions in Fall 2020


###########################################################################################################################################################################
# Thermistor Position QAQC

#Figuring out exact position of thermistors based on when they are out of the water. This all determined
# and can be found in "BVR_Depth_offsets_2020_2021.csv

#Below is how we do it and code for the plots to do it. 

# In 2020 Thermistors 1 and 2 were in the water part of the time and then were out of the water. We can use
# plots to figure out when this happened base on temperature. When the temperature pattern is more like the air
# than the water and figure out when that change happened by changing the date range of the plot and using ggplotly
# which gives you the date and temperature of each point. 
# Thermistors that don't come out of the water are assumed to be equidistant from each other ~1m. See metadata for more details. 
# In 2021 the only thermistor that was in the water and then out of the water was Thermistor 3. 

#Figure out when Thermistor 3 is out of the water by graphing

#  Temp_3 <- bvrdata_clean%>%
# filter(DateTime>"2021-09-10 00:00" & DateTime<"2021-09-11 00:00")%>%
#   #select(DateTime, ThermistorTemp_C_1, ThermistorTemp_C_2, ThermistorTemp_C_3, Lvl_psi_13,Depth_m_13, Flag_ThermistorTemp_C_1,Flag_ThermistorTemp_C_2,Flag_LvlPres )
#   ggplot(., aes(x = DateTime)) +
#   geom_point(aes(y=ThermistorTemp_C_2, color="blue"))+
#   geom_point(aes(y=ThermistorTemp_C_3, color="red"))+
#   #geom_point(aes(y=ThermistorTemp_C_4, color='green')) +
#   geom_point(aes(y=Lvl_psi_13, color='black'))
# # Temp_3
#  ggplotly(Temp_3)

# Once that depth  is determined we can use that to determine the depth from the surface the thermistor.
# when readings are out of the water and need to be changed to NA and the depth of each reading which can be done
# using the R script BVR_sort_by_depth_2020_2021.R


#########################################################################################################################
# Visually inspect the plots to see if any maintenance points were missing or other points that need to be QAQCd

# check the graph of Thermistor at postion 1 
Temp_1 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_1)) +
  geom_point()
Temp_1
#ggplotly(Temp_1)

m_1_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_1)) +
  geom_point()
m_1_21

# check the graph of Thermistor at postion 2
Temp_2 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_2)) +
  geom_point()
Temp_2
#ggplotly(Temp_2)

m_2_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_2)) +
  geom_point()
m_2_21

# check the graph of Thermistor at postion 3
# check Thermistor 3 temp data
Temp_3 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_3)) +
  geom_point()
Temp_3
#ggplotly(Temp_3)

m_3_21=bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_3)) +
  geom_point()
m_3_21

#check the EXO temp which is deployed at 1_5m
Exo_temp <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOTemp_C_1_5)) +
 geom_point()
Exo_temp
#ggplotly(Exo_temp)

Exo_temp_21=bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = EXOTemp_C_1_5)) +
  geom_point()
Exo_temp_21


# check the graph of Thermistor at postion 4
Temp_4 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_4)) +
 geom_point()
Temp_4
#ggplotly(Temp_4)

m_4_21=bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_4)) +
  geom_point()
m_4_21
#ggplotly(m_4_21)


# check the graph of Thermistor at postion 5
Temp_5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_5)) +
 geom_point()
Temp_5
ggplotly(Temp_5)

m_5_21=bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_5)) +
  geom_point()
m_5_21
ggplotly(m_5_21)

# check the graph of Thermistor at postion 6
Temp_6 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_6)) +
 geom_point()
Temp_6
#ggplotly(Temp_6)

m_6_21=bvrdata_clean%>%
  #filter(DateTime>start_time & DateTime<end_time)%>%
  filter(DateTime>"2021-03-29 00:00" & DateTime<"2021-06-01 00:00")%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_6)) +
  geom_point()
m_6_21
ggplotly(m_6_21)

#Check the temperature from the RDO at position 6
DO_temp_6<- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDOTemp_C_6)) +
 geom_point()
DO_temp_6
#ggplotly(DO_temp_6)

DO_temp_6_21=bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = RDOTemp_C_6)) +
  geom_point()
DO_temp_6_21

#check that the temp from the RDO and the thermistor line up
#plot(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_6, type='l')
#points(bvrdata_clean$DateTime, bvrdata_clean$RDOTemp_C_6, type='l', col="red")


# check the graph of Thermistor at postion 7
Temp_7 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_7)) +
 geom_point()
Temp_7
#ggplotly(Temp_7)

m_7_21=bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_7)) +
  geom_point()
m_7_21
ggplotly(m_7_21)


# check the graph of Thermistor at postion 8
Temp_8 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_8)) +
 geom_point()
Temp_8
#ggplotly(Temp_8)

m_8_21=bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_8)) +
  geom_point()
m_8_21
ggplotly(m_8_21)

# check the graph of Thermistor at postion 9
Temp_9 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_9)) +
 geom_point()
Temp_9
#ggplotly(Temp_9)

m_9_21=bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_9)) +
  geom_point()
m_9_21
ggplotly(m_9_21)


# check the graph of Thermistor at postion 10
Temp_10 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_10)) +
 geom_point()
Temp_10
#ggplotly(Temp_10)

m_10_21=bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_10)) +
  geom_point()
m_10_21
ggplotly(m_10_21)

# check the graph of Thermistor at postion 11
Temp_11 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_11)) +
 geom_point()
Temp_11
ggplotly(Temp_11)

m_11_21=bvrdata_clean%>%
  #filter(DateTime>start_time & DateTime<end_time)%>%
  filter(DateTime>"2021-03-30 00:00" & DateTime<"2021-04-03 00:00")%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_11)) +
  geom_point()
m_11_21
ggplotly(m_11_21)

# check the graph of Thermistor at postion 12
Temp_12 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_12)) +
 geom_point()
Temp_12
#ggplotly(Temp_12)

m_12_21=bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_12)) +
  geom_point()
m_12_21
ggplotly(m_12_21)

# check the graph of Thermistor at postion 13
Temp_13 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_13)) +
  geom_point()
Temp_13
#ggplotly(Temp_13)

m_13_21=bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_13)) +
  geom_point()
m_13_21
ggplotly(m_13_21)

#Check the other temp sensors at position 13 for the RDO and the pressure transducer
temp_pressure <- ggplot(data = bvrdata_clean, aes(x = DateTime, y=LvlTemp_C_13))+
 geom_point()  
temp_pressure
ggplotly(temp_pressure)

temp_do <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDOTemp_C_13)) +
 geom_point()
temp_do
ggplotly(temp_do)

#plot all on the same graph to make sure they line up 
plot(bvrdata_clean$DateTime, bvrdata_clean$LvlTemp_C_13, type="l")
points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_13, type= "l", col="red")
points(bvrdata_clean$DateTime, bvrdata_clean$RDOTemp_C_13, type= "l", col="darkorange")


clean_current=bvrdata_clean%>%
  filter(DateTime>=start_time)

###Put all the points on the same plot using base R
plot(clean_current$DateTime,clean_current$ThermistorTemp_C_1, main="Water Temp", xlab="Time", ylab="degrees C", type='l', col="black", lwd=1.5, ylim=c(0,40))
points(clean_current$DateTime,clean_current$ThermistorTemp_C_1, col="firebrick4", type='l', lwd=1.5)
points(clean_current$DateTime, clean_current$ThermistorTemp_C_2, col="firebrick1", type='l', lwd=1.5)
points(clean_current$DateTime, clean_current$ThermistorTemp_C_3, col="DarkOrange1", type='l', lwd=1.5)
points(clean_current$DateTime, clean_current$ThermistorTemp_C_4, col="gold", type='l', lwd=1.5)
points(clean_current$DateTime, clean_current$ThermistorTemp_C_5, col="greenyellow", type='l', lwd=1.5)
points(clean_current$DateTime, clean_current$ThermistorTemp_C_6, col="medium sea green", type='l', lwd=1.5)
points(clean_current$DateTime, clean_current$ThermistorTemp_C_7, col="sea green", type='l', lwd=1.5)
points(clean_current$DateTime, clean_current$ThermistorTemp_C_8, col="DeepSkyBlue4", type='l', lwd=1.5)
points(clean_current$DateTime, clean_current$ThermistorTemp_C_9, col="blue2", type='l', lwd=1.5)
points(clean_current$DateTime, clean_current$ThermistorTemp_C_10, col="blue4", type='l', lwd=1.5)
points(clean_current$DateTime, clean_current$ThermistorTemp_C_11, col="darkslateblue", type='l', lwd=1.5)
points(clean_current$DateTime, clean_current$ThermistorTemp_C_12, col="magenta2", type='l', lwd=1.5)
points(clean_current$DateTime, clean_current$ThermistorTemp_C_13, col="darkmagenta", type='l', lwd=1.5)

###########################################################################################################################################################################
# DO qaqc ----

#Let's see what everything looks like after the first round of QAQC 
#Do at Thermistor 6
DO_6 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDO_mgL_6)) +
 geom_point()
DO_6
#ggplotly(DO_6)

DO_6_21 <- bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(., aes(x = DateTime, y = RDO_mgL_6)) +
  geom_point()
DO_6_21
ggplotly(DO_6_21)


DOsat_6 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDOsat_percent_6)) +
 geom_point()
DOsat_6
#ggplotly(DOsat_6)

DOsat_6_21 <- bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(., aes(x = DateTime, y = RDO_mgL_6)) +
  geom_point()
DOsat_6_21
#ggplotly(DOsat_6_21)

#Do at Thermistor 13
DO_13 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDO_mgL_13)) +
 geom_point()
DO_13
ggplotly(DO_13)

do_check=bvrdata_clean%>%
  select(DateTime, starts_with("EXO"))
  filter(DateTime>"2021-10-12 00:00" & DateTime<"2021-10-13 12:00")
  

DO_13_21 <- bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(., aes(x = DateTime, y = RDO_mgL_13)) +
  geom_point()
DO_13_21
ggplotly(DO_13_21)

DOsat_13 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDOsat_percent_13)) +
 geom_point()
DOsat_13
#ggplotly(DOsat_13)

DOsat_13_21 <- bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(., aes(x = DateTime, y = RDOsat_percent_13)) +
  geom_point()
DOsat_13_21

#Do from the EXO
EXODO_1_5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXODO_mgL_1_5)) +
 geom_point()
EXODO_1_5
ggplotly(EXODO_1_5)

EXODO_1_5_21 <- bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(., aes(x = DateTime, y = EXODO_mgL_1_5)) +
  geom_point()
EXODO_1_5_21

EXODOsat_1_5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXODOsat_percent_1_5)) +
 geom_point()
EXODOsat_1_5
#ggplotly(EXODOsat_1_5)

EXODOsat_1_5_21 <- bvrdata_clean%>%
  #filter(DateTime>start_time & DateTime<end_time)%>%
  filter(DateTime>"2021-07-01 00:00" & DateTime<"2021-07-20 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXODOsat_percent_1_5)) +
  geom_point()
EXODOsat_1_5_21
ggplotly(EXODOsat_1_5_21)


##read in ctd values to see how they compare to sensor values
#download.file("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLCatwalk/2020/misc_data_files/ctd_2018-2020_flagged.csv", "ctd.csv")
#ctd=read.csv("ctd.csv")
#ctd$Date<-as.POSIXct(ctd$Date,format = "%Y-%m-%d %H:%M:%S")

#read in YSI to check the DO 
# download.file("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021/YSI_PAR_profiles_2013-2021.csv", "ysi.csv")
# ysi=read.csv("ysi.csv")
# ysi$DateTime<-as.POSIXct(ysi$DateTime,format = "%Y-%m-%d %H:%M:%S", tz="EST")

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


# ysi_exo=ysi%>%
#  filter(Reservoir=="BVR")%>%
#  filter(DateTime>"2020-06-01 00:00:00")%>%
#  select(DateTime, Depth_m, DO_mgL, DOSat)%>%
#  filter(Depth_m<3)
#  group_by(DateTime)%>%
# summarise(mean_DO=mean(DO_mgL))

#plot(bvrdata_clean$DateTime, bvrdata_clean$EXODOsat_percent_1_5, type='l')
#points(bvrdata_clean$DateTime, bvrdata_clean$RDOsat_percent_6, type='l', col="blue")
#points(ctd_bottom$Date, ctd_bottom$mean_DO, type='p', col= "red", cex= 1.0)
#points(ysi_bottom$DateTime, ysi_bottom$mean_DO, type = "p", col = "purple")
#points(ysi_exo$DateTime, ysi_exo$DOSat, type = "p", col = "red")
  
#Check the EXO against the CTD and YSI
#ctd_1_5=ctd%>%
#  filter(Reservoir=="BVR")%>%
#  filter(Date>"2020-01-01 00:00:00")%>%
#  select(Date, Depth_m, Temp_C,DO_mgL,DO_pSat)%>%
#  filter(Depth_m>1.48 & Depth_m<1_52)%>%
#  group_by(Date)%>%
#  summarise(mean_DO=mean(DO_mgL))

#ysi_1_5=ysi%>%
#  filter(Reservoir=="BVR")%>%
#  filter(DateTime>"2020-06-22 00:00:00")%>%
#  select(DateTime, Depth_m, DO_mgL)%>%
#  filter(Depth_m>=1 & Depth_m<=2)%>%
#  mutate(day = floor_date(DateTime, "day")) %>%
#  group_by(day) %>%
#  summarize(avg = mean(DO_mgL))

#plot(bvrdata_clean$DateTime, bvrdata_clean$EXODO_mgL_1_5, type='l', ylim= c(3,13))
#points(ctd_1_5$Date, ctd_1_5$mean_DO, type='p', col= "red",pch=16, cex= 1.0)
#points(ysi_1_5$day, ysi_1_5$avg, type ='p', col="darkorange", pch=16,cex=1.0 )

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

#plot the chal from the EXO
chl_rfu <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOChla_RFU_1_5)) +
 geom_point()
chl_rfu
#ggplotly(chl_rfu)

chl_rfu_21 <- bvrdata_clean%>%
  #filter(DateTime>start_time & DateTime<end_time)%>%
  filter(DateTime>"2021-04-01 00:00" & DateTime<"2021-04-20 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXOChla_RFU_1_5)) +
  geom_point()
chl_rfu_21
#ggplotly(chl_rfu_21)


#plot the chal from the EXO
chl_ugL <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOChla_ugL_1_5)) +
  geom_point()
chl_ugL
#ggplotly(chl_ugL)

chl_ugL_21 <- bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(., aes(x = DateTime, y = EXOChla_ugL_1_5)) +
  geom_point()
chl_ugL_21
#ggplotly(chl_ugL_21)

chl_mean <- bvrdata_clean %>%
  select(DateTime, EXOChla_ugL_1_5) %>%
  mutate(day = date(DateTime)) %>%
  group_by(day) %>%
  mutate(daily_mean = mean(EXOChla_ugL_1_5, na.rm = TRUE)) %>%
  distinct(day, .keep_all = TRUE)
# 
# # plot the daily mean
chl_mean_plot <- ggplot(data = chl_mean, aes(x = day, y = daily_mean)) +
  geom_point()
chl_mean_plot
# # Plotly so can pick out questionable values
# ggplotly(chl_mean)
# 
# # Plot the chla and the daily mean on the same graph
plot(bvrdata_clean$DateTime, bvrdata_clean$EXOChla_ugL_1_5)
points(chl_mean$DateTime, chl_mean$daily_mean, type="l", col="green")


#Checking out the phycos and flagging points, EXOBGAPC_RFU_1_5, EXOBGAPC_ugL_1_5  
  
#plot the phyco
phyco_ugl <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOBGAPC_ugL_1_5)) +
 geom_point()
phyco_ugl
#ggplotly(phyco_ugl)

phyco_ugl_21 <- bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(., aes(x = DateTime, y = EXOBGAPC_ugL_1_5)) +
  geom_point() 
phyco_ugl_21
#ggplotly(phyco_ugl_21)

phyco_RFU <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOBGAPC_RFU_1_5)) +
 geom_point()
phyco_RFU
#ggplotly(phyco_RFU)

phyco_RFU_21 <- bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(., aes(x = DateTime, y = EXOBGAPC_RFU_1_5)) +
  geom_point() 
phyco_RFU_21
#ggplotly(phyco_RFU_21)





###########################################################################################################################################################################
# fdom qaqc----
# QAQC from DWH to remove major outliers from fDOM data that are 2 sd's greater than the previous and following datapoint
# QAQC done on 2018-2020 dataset

#plot the raw fdom values with the cleaned fdom values
#plot(bvrdata_raw$TIMESTAMP, bvrdata_raw$fDOM_QSU_1, type ="l")
#points(bvrdata_clean$DateTime, bvrdata_clean$EXOfDOM_QSU_1_5, type= "l", col="red")

#plot the clean fdom values and look for outliers
#fDOM_QSU <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOfDOM_QSU_1_5)) +
#    geom_point()
  #  ggtitle("fDOM (QSU) pre QAQC")
#fDOM_QSU
  #ggplotly(fDOM_pre_QAQC)

fDOM_RFU <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOfDOM_RFU_1_5)) +
   geom_point()
fDOM_RFU
#ggplotly(fDOM_RFU)

fDOM_RFU_21 <- bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(., aes(x = DateTime, y = EXOfDOM_RFU_1_5)) +
  geom_point()
fDOM_RFU_21
#ggplotly(fDOM_RFU_21)


###check the conductivity values and the specific conductivity

#plot the conductivity
cond <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOCond_uScm_1_5)) +
 geom_point()
cond
#ggplotly(cond)

cond_21 <- bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(. , aes(x = DateTime, y = EXOCond_uScm_1_5)) +
  geom_point()
cond_21
ggplotly(cond_21)


#plot the specific conductivity 
cond_sp <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOSpCond_uScm_1_5)) +
 geom_point()
cond_sp
#ggplotly(cond_sp)

cond_sp_21 <- bvrdata_clean%>%
  #filter(DateTime>start_time & DateTime<end_time)%>%
  filter(DateTime>"2021-03-06 00:00" & DateTime<"2021-04-20 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXOSpCond_uScm_1_5)) +
  geom_point()
cond_sp_21
ggplotly(cond_sp_21)

#plot the total dissolved solids
tds <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOTDS_mgL_1_5)) +
 geom_point()
tds
#ggplotly(tds)


#turbidity
turb <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOTurbidity_FNU_1_5)) +
  geom_point()
turb
#ggplotly(turb)

turb_21 <- bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(., aes(x = DateTime, y = EXOTurbidity_FNU_1_5)) +
  geom_point()
turb_21
ggplotly(turb_21)
    
#Pressure graphs
#graph the EXO pressure
Exo_pressure <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXO_pressure_psi)) +
 geom_point()
Exo_pressure
#ggplotly(Exo_pressure)

#graph the EXO depth which is based on pressure so should be the same as above
Exo_depth <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXO_depth_m)) +
 geom_point()
Exo_depth
ggplotly(Exo_depth)

Exo_depth_21 <- bvrdata_clean%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(., aes(x = DateTime, y = EXO_depth_m)) +
  geom_point()
Exo_depth_21
ggplotly(Exo_depth_21)

#Check the pressure of the pressure gauge at position 13 and flag when the wire wasn't connected
#plot the pressure at position 13
Lvl_pressure <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = Lvl_psi_13)) +
 geom_point()
Lvl_pressure
#ggplotly(Lvl_pressure)

Lvl_pressure_21 <- bvrdata_clean%>%
  #filter(DateTime>start_time & DateTime<end_time)%>%
  filter(DateTime>"2021-03-30 00:00" & DateTime<"2021-04-03 00:00")%>%
  ggplot(., aes(x = DateTime, y = Lvl_psi_13)) +
  geom_point()
Lvl_pressure_21
ggplotly(Lvl_pressure_21)



####Records power and other variables to just check for anything funny

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



###########################################################################################################################################################################
# write final csv ----
# some final checking of flags
#for (i in 1:nrow(bvrdata_clean)) {
 # if(is.na(bvrdata_clean$Flag_fDOM[i])){
#    bvrdata_clean$Flag_fDOM[i] <- 0
 # }
#}
str(bvrdata_clean)

#take out TSS since it isn't calibrated

# rearrange the cols
bvrdata_clean <- bvrdata_clean %>% 
  select(Reservoir, Site, DateTime, ThermistorTemp_C_1:ThermistorTemp_C_13,
         RDO_mgL_6, RDOsat_percent_6,
         RDOTemp_C_6, RDO_mgL_13, RDOsat_percent_13, RDOTemp_C_13,
         EXOTemp_C_1_5, EXOCond_uScm_1_5, EXOSpCond_uScm_1_5, EXOTDS_mgL_1_5, EXODOsat_percent_1_5,
         EXODO_mgL_1_5, EXOChla_RFU_1_5, EXOChla_ugL_1_5, EXOBGAPC_RFU_1_5, EXOBGAPC_ugL_1_5,
         EXOfDOM_RFU_1_5, EXOfDOM_QSU_1_5,EXOTurbidity_FNU_1_5, EXO_pressure_psi, EXO_depth_m, EXO_battery_V, EXO_cablepower_V,
         EXO_wiper_V, Lvl_psi_13,Depth_m_13, LvlTemp_C_13, RECORD, CR6_Batt_V, CR6Panel_Temp_C,
         Flag_ThermistorTemp_C_1:Flag_ThermistorTemp_C_13,Flag_RDO_mgL_6, Flag_RDOsat_percent_6, Flag_RDOTemp_C_6,
         Flag_RDO_mgL_13, Flag_RDOsat_percent_13, Flag_RDOTemp_C_13,Flag_EXOTemp_C_1_5, Flag_EXOCond_uScm_1_5, Flag_EXOSpCond_uScm_1_5,Flag_EXOTDS_mgL_1_5,
         Flag_EXODOsat_percent_1_5, Flag_EXODO_mgL_1_5, Flag_EXOChla_RFU_1_5,Flag_EXOChla_ugL_1_5, Flag_EXOBGAPC_RFU_1_5,Flag_EXOBGAPC_ugL_1_5,
         Flag_EXOfDOM_RFU_1_5,Flag_EXOfDOM_QSU_1_5, Flag_EXOTurbidity_FNU_1_5,
         Flag_EXO_pressure_psi, Flag_EXO_depth_m, Flag_EXO_battery_V, Flag_EXO_cablepower_V,Flag_EXO_wiper_V,Flag_Lvl_psi_13)

#order from oldest to newest
bvrdata_clean=bvrdata_clean[order(bvrdata_clean$DateTime),]

setwd("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform/2022/")

write.csv(bvrdata_clean, 'BVR_platform_data_2020_2022.csv', row.names = FALSE, quote=FALSE)
