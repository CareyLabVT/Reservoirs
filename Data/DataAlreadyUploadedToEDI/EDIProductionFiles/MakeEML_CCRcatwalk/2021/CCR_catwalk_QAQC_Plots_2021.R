# Master QAQC script in prep for publishing catwalk sensor string data to EDI
# this script combines code from other QAQC scripts found in misc_QAQC_scipts folder
# as well as other data files from misc_data_files
# final EDI-ready file outputs directly to MakeEML_CCRcatwalk/2020 folder
# Set up ----
pacman::p_load("RCurl","tidyverse","lubridate", "plotly", "magrittr")
folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCRcatwalk/2021/"
source(paste0(folder, "CCR_catwalk_QAQC_function_2021.R"))

# download most up to date catwalk data and maintenance log for input into the function
download.file("https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/ccre-dam-data/CCRW_maintenance_log.txt",paste0(folder, "CCRW_maintenance_log_2021.txt"))
download.file("https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/ccre-dam-data/ccre-waterquality.csv",paste0(folder, "ccre-waterquality.csv"))
#download.file('https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/CR6_Files/FCRcatwalk_manual_2021.csv', paste0(folder, "CAT_2.csv"))

# run standard qaqc function from FCR_catwalk_QAQC_function_2021.R
data_file <- paste0(folder, "ccre-waterquality.csv")#current file from the data logger
#data2_file <- paste0(folder, 'CAT_2.csv')#manual downloads to add missing data 
maintenance_file <- paste0(folder, "CCRW_maintenance_log_2021.txt")#maintenance file
output_file <- paste0(folder, "CCRwaterquality_first_QAQC_2021.csv")#name of the output file
qaqc(data_file, maintenance_file, output_file)#function to do the main qaqc


# read in qaqc function output

ccrwater <- read.csv(output_file)

#current time of QAQC for graphing
current_time="2021-12-31 23:59"


# subset file to only unpublished data
ccrwater <- ccrwater[ccrwater$DateTime<"2021-12-31 23:59",]


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



#graph to figure out off set
depth=ccrwater%>%
  select(DateTime,LvlDepth_m_13, Lvl_psi_13, ThermistorTemp_C_1,ThermistorTemp_C_2,ThermistorTemp_C_3)%>%
  drop_na(ThermistorTemp_C_1)

depth$DateTime<-as.POSIXct(strptime(depth$DateTime, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5")

#
#Setting the temperature to NA when the thermistors are out of the water
#add a depth column which we set NAs for when the thermistor is out of the water. Flag 2
ccrwater=ccrwater%>%
  mutate(depth_1=LvlDepth_m_13-11.82)%>%
  mutate(depth_2=LvlDepth_m_13-11.478)%>%
  mutate(Flag_Temp_1= ifelse(!is.na(depth_1) & depth_1<0 ,2,Flag_Temp_1))%>%
  mutate(ThermistorTemp_C_1=ifelse(!is.na(depth_1) & depth_1<0,NA,ThermistorTemp_C_1))%>%
  mutate(Flag_Temp_2= ifelse(!is.na(depth_2) & depth_2<0 ,2,Flag_Temp_2))%>%
  mutate(ThermistorTemp_C_2=ifelse(!is.na(depth_2) & depth_2<0,NA,ThermistorTemp_C_2))%>%
  select(-depth_1,-depth_2)


#change the temp to NA when the thermistor is clearly out of the water which we used to determine the depth of the temp string
#negative depths are changed to NA
#the date ifelse statement is when the pressure transducer was unplugged

#for thermistor at position 1 when it was out of the water 
bvrdata_clean=bvrdata_clean%>%
  mutate(Flag_Temp_1= ifelse(DateTime>="2020-10-26 12:10:00 tz=Etc/GMT+5 "&DateTime<="2020-10-30 09:40:00 tz=Etc/GMT+5",2,Flag_Temp_1))%>%
  mutate(ThermistorTemp_C_1= ifelse(DateTime>="2020-10-26 12:10:00 tz=Etc/GMT+5 "&DateTime<="2020-10-30 09:40:00 tz=Etc/GMT+5",NA,ThermistorTemp_C_1))%>%
  



################################################################################################################
# #graphing temperature
# 
# 
# #Surface Temp
# #From 2018-current
# surf <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = ThermistorTemp_C_surface)) +
#   geom_point()
# surf
# #Plotly so can pick out questionable values
# ggplotly(surf)
# 
# #Just the current year
# surf21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_surface)) +
#   geom_point()
# surf21
# #Plotly so can pick out questionable values
# ggplotly(surf21)
# 
# # check 1m temp data
# #From 2018-current
# m_1 <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = ThermistorTemp_C_1)) +
#  geom_point()
# m_1
# #Plotly so can pick out questionable values
# ggplotly(m_1)
# 
# #Just the current year
# m_1_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_1)) +
#   geom_point()
# m_1_21
# #Plotly so can pick out questionable values
# ggplotly(m_1_21)
# 
# # check 2m temp data
# #Plot 2018-current
# m_2 <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = ThermistorTemp_C_2)) +
#  geom_point()
# m_2
# #Plotly so can pick out questionable values
# ggplotly(m_2)
# 
# #Just the current year
# m_2_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_2)) +
#   geom_point()
# m_2_21
# #Plotly so can pick out questionable values
# ggplotly(m_2_21)
# 
# # check 3m temp data
# #Plot From 2018-current
# m_3 <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = ThermistorTemp_C_3)) +
#  geom_point()
# m_3
# #Plotly so can pick out questionable values
# ggplotly(m_3)
# 
# #Just the current year
# m_3_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_3)) +
#   geom_point()
# m_3_21
# #Plotly so can pick out questionable values
# ggplotly(m_3_21)
# 
# # check 4m temp data
# #Plot from 2018-current
# m_4 <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = ThermistorTemp_C_4)) +
#  geom_point()
# m_4
# #Plotly so can pick out questionable values
# ggplotly(m_4)
# 
# # Just from current year
# m_4_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_4)) +
#   geom_point()
# m_4_21
# # Plotly so can pick out questionable values
# ggplotly(m_4_21)
# 
# # check 5m temp data
# # Plot from 2018-current
# m_5 <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = ThermistorTemp_C_5)) +
#   geom_point()
# m_5
# # Plotly so can pick out questionable values
# ggplotly(m_5)
# 
# # Just current year
# m_5_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_5)) +
#   geom_point()
# m_5_21
# # Plotly so can pick out questionable values
# ggplotly(m_5_21)
# 
# # check 6m temp data
# # Plot from 2018-current
# m_6 <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = ThermistorTemp_C_6)) +
#  geom_point()
# m_6
# # Plotly so can pick out questionable values
# ggplotly(m_6)
# 
# # Just the current year
# m_6_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_6)) +
#   geom_point()
# m_6_21
# # Plotly so can pick out questionable values
# ggplotly(m_6_21)
# 
# # check 7m temp data
# # all the temp 2018-current
# m_7 <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = ThermistorTemp_C_7)) +
#  geom_point()
# m_7
# # Plotly so can pick out questionable values
# ggplotly(m_7)
# 
# #filter for the current year
# m_7_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_7)) +
#   geom_point()
# m_7_21
# # plotly so you can pick out the questionable values
# ggplotly(m_7_21)
# 
# # check 8m temp data
# # Plot 2018-current
# m_8 <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = ThermistorTemp_C_8)) +
#  geom_point()
# m_8
# # Plotly so can pick out questionable values
# ggplotly(m_8)
# 
# # Plot just the current year
# m_8_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_8)) +
#   geom_point()
# m_8_21
# # Plotly so can pick out questionable values
# ggplotly(m_8_21)
# 
# 
# # check 9m temp data
# # Plot 2018-current
# m_9 <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = ThermistorTemp_C_9)) +
#  geom_point()
# m_9
# # Plotly so can pick out questionable values
# ggplotly(m_9)
# 
# # Just the current year
# m_9_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_9)) +
#   geom_point()
# m_9_21
# # Plotly so can pick out questionable values
# ggplotly(m_9_21)
# 
# 
# 
# #graph all the temps in 2021on the same graph so use base R
# #create a new data frame for 2021
# t2021=ccrwater_flag%>%
#   filter(DateTime>current_time)
# 
# #this part taken from the daily email script
# par(mfrow=c(1,1))
# par(oma=c(1,1,1,4))
# plot(t2021$DateTime,t2021$ThermistorTemp_C_surface, main="Water Temp", xlab="Time", ylab="degrees C", type='l', col="firebrick4", lwd=1.5, ylim=c(0,35))
# points(t2021$DateTime, t2021$ThermistorTemp_C_1, col="firebrick1", type='l', lwd=1.5)
# points(t2021$DateTime, t2021$ThermistorTemp_C_2, col="DarkOrange1", type='l', lwd=1.5)
# points(t2021$DateTime, t2021$ThermistorTemp_C_3, col="gold", type='l', lwd=1.5)
# points(t2021$DateTime, t2021$ThermistorTemp_C_4, col="greenyellow", type='l', lwd=1.5)
# points(t2021$DateTime, t2021$ThermistorTemp_C_5, col="medium sea green", type='l', lwd=1.5)
# points(t2021$DateTime, t2021$ThermistorTemp_C_6, col="sea green", type='l', lwd=1.5)
# points(t2021$DateTime, t2021$ThermistorTemp_C_7, col="DeepSkyBlue4", type='l', lwd=1.5)
# points(t2021$DateTime, t2021$ThermistorTemp_C_8, col="blue2", type='l', lwd=1.5)
# points(t2021$DateTime, t2021$ThermistorTemp_C_9, col="blue4", type='l', lwd=1.5)
# par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right",c("0.1m","1m", "2m", "3m", "4m", "5m", "6m", "7m","8m", "9m"),
#        text.col=c("firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
#                   "DeepSkyBlue4", "blue2", "blue4"), cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
# 
# #check the DO temp compared to temp string in 2021
# 
# plot(t2021$DateTime,t2021$ThermistorTemp_C_1, main="EXO vs. Temp String", xlab="Time", ylab="degrees C", type='l', col="firebrick4", lwd=1.5, ylim=c(0,35))
# points(t2021$DateTime, t2021$ThermistorTemp_C_2, col="firebrick1", type='l', lwd=1.5)
# points(t2021$DateTime, t2021$EXOTemp_C_1, col="black", type='l', lwd=1.5)
# 
# plot(t2021$DateTime,t2021$RDOTemp_C_5, main="RDO 5m vs. Temp String", xlab="Time", ylab="degrees C", type='l', col="black", lwd=1.5, ylim=c(0,15))
# points(t2021$DateTime, t2021$ThermistorTemp_C_5, col="medium sea green", type='l', lwd=1.5)
# 
# plot(t2021$DateTime,t2021$RDOTemp_C_9, main="RDO 9m vs. Temp String", xlab="Time", ylab="degrees C", type='l', col="black", lwd=1.5, ylim=c(0,10))
# points(t2021$DateTime, t2021$ThermistorTemp_C_9, col="blue4", type='l', lwd=1.5)
# 
# 
# 



###########################################################################################################################################################################


#Creating a new flag "6" started in 2019: "very questionable value due to potential fouling. Values adjusted using a linear or square root function to match high-resolution CTD profiles are given in RDO_mgL_5 and RDO_sat_percent_5"
#Creating a new flag "5" which means that the values are very questionable due to fouling but not adjusted (starting in 2019)
################
#Correcting some DO values 

ccrwater_flag <- ccrwater_flag%>%
  mutate(
    DateTime=as.character(DateTime),#change DateTime to as.character so they line up when making changes
    #For 5m READ NOTE: These are the sections I noticed apparent following in TS and have tried to correct with linear adjustments
    Flag_DO_5_obs = ifelse(DateTime<"2019-08-12 12:35:00" & DateTime>"2019-08-11 00:00:00", 6, Flag_DO_5_obs),
    Flag_DO_5_sat = ifelse(DateTime<"2019-08-12 12:35:00" & DateTime> "2019-08-11 00:00:00", 6, Flag_DO_5_sat),
    RDO_mgL_5_adjusted = ifelse(DateTime<"2019-08-12 12:35:00" & DateTime> "2019-08-11 00:00:00", 
                                RDO_mgL_5 + sqrt(as.numeric(difftime(DateTime,"2019-08-11 00:00:00", units = "mins")))/30,
                                RDO_mgL_5),
    RDOsat_percent_5_adjusted = ifelse(DateTime<"2019-08-12 12:35:00" & DateTime> "2019-08-11 00:00:00", 
                                       RDOsat_percent_5 + sqrt(as.numeric(difftime(DateTime,"2019-08-11 00:00:00", units = "mins")))/30/11.3*100,
                                       RDOsat_percent_5),
    Flag_DO_5_obs = ifelse(DateTime<"2019-07-17 11:40:00" & DateTime>"2019-07-13 00:00:00", 5, Flag_DO_5_obs),
    Flag_DO_5_sat = ifelse(DateTime<"2019-07-17 11:40:00" & DateTime>"2019-07-13 00:00:00", 5, Flag_DO_5_sat),
    #9 meters #### READ NOTE: These are the sections I noticed apparent following in TS and have tried to correct with linear adjustments
    #first section fixed 11aug to 17aug
    Flag_DO_9_obs = ifelse(DateTime<"2020-08-17 12:50:00" & DateTime> "2020-08-11 7:00:00",6, Flag_DO_9_obs),
    Flag_DO_9_sat = ifelse(DateTime<"2020-08-17 12:50:00" & DateTime> "2020-08-11 15:00:00",6, Flag_DO_9_sat),
    RDO_mgL_9_adjusted = ifelse(DateTime<"2020-08-17 12:50:00" & DateTime> "2020-08-11 7:00:00", 
                                RDO_mgL_9 + as.numeric(difftime(DateTime,"2020-08-11 7:00:00", units = "mins"))/6500, #A linear adjustment here
                                RDO_mgL_9),
    RDOsat_percent_9_adjusted = ifelse(DateTime<"2020-08-17 12:50:00" & DateTime> "2020-08-11 15:00:00", 
                                       RDOsat_percent_9 + (as.numeric(difftime("2020-08-17 12:50:00","2020-08-12 15:00:00", units = "mins")))/6500/11.3*100,
                                       RDOsat_percent_9),
    
    # 9 meters 19aug to 24aug
    Flag_DO_9_obs = ifelse(DateTime<"2020-08-24 10:40:00" & DateTime> "2020-08-19 20:00:00",6, Flag_DO_9_obs),
    Flag_DO_9_sat = ifelse(DateTime<"2020-08-24 10:40:00" & DateTime> "2020-08-19 20:00:00",6, Flag_DO_9_sat),
    RDO_mgL_9_adjusted = ifelse(DateTime<"2020-08-24 10:40:00" & DateTime> "2020-08-19 20:00:00", 
                                RDO_mgL_9 + (as.numeric(difftime(DateTime,"2020-08-19 20:00:00", units = "mins")))/6500, #A linear adjustment here
                                RDO_mgL_9_adjusted),
    RDOsat_percent_9_adjusted = ifelse(DateTime<"2020-08-24 10:40:00" & DateTime> "2020-08-19 20:00:00", 
                                       RDOsat_percent_9 + as.numeric(difftime(DateTime,"2020-08-19 20:00:00", units = "mins"))/6500/11.3*100,
                                       RDOsat_percent_9_adjusted),
    
    # 9 meters 26aug to 02sep
    Flag_DO_9_obs = ifelse(DateTime<"2020-09-02 10:50:00" & DateTime> "2020-08-26 12:00:00" ,6, Flag_DO_9_obs),
    Flag_DO_9_sat = ifelse(DateTime<"2020-09-02 10:50:00" & DateTime> "2020-08-26 12:00:00" ,6, Flag_DO_9_sat),
    RDO_mgL_9_adjusted = ifelse(DateTime< "2020-09-02 10:50:00" & DateTime> "2020-08-26 12:00:00", 
                                RDO_mgL_9 + (as.numeric(difftime(DateTime, "2020-08-26 12:00:00", units = "mins")))/10000, #A linear adjustment here
                                RDO_mgL_9_adjusted),
    RDOsat_percent_9_adjusted = ifelse(DateTime< "2020-09-02 10:50:00" & DateTime> "2020-08-26 12:00:00", 
                                       RDOsat_percent_9 + as.numeric(difftime(DateTime, "2020-08-26 12:00:00", units = "mins"))/10000/11.3*100,
                                       RDOsat_percent_9_adjusted),
    
    # 9 meters 02sep to 11sep
    Flag_DO_9_obs = ifelse(DateTime<"2020-09-09 17:50:00" & DateTime> "2020-09-05 06:00:00" ,6, Flag_DO_9_obs),
    Flag_DO_9_sat = ifelse(DateTime<"2020-09-09 17:50:00" & DateTime> "2020-09-05 06:00:00" ,6, Flag_DO_9_sat),
    RDO_mgL_9_adjusted = ifelse(DateTime< "2020-09-09 17:50:00" & DateTime> "2020-09-05 06:00:00", 
                                RDO_mgL_9 + (as.numeric(difftime(DateTime, "2020-09-05 06:00:00", units = "mins")))/3000, #A linear adjustment here
                                RDO_mgL_9_adjusted),
    RDOsat_percent_9_adjusted = ifelse(DateTime< "2020-09-09 17:00:00" & DateTime> "2020-09-05 06:00:00", 
                                       RDOsat_percent_9 + as.numeric(difftime(DateTime, "2020-09-05 06:00:00", units = "mins"))/3000/11.3*100,
                                       RDOsat_percent_9_adjusted))

#take out questionable values for the 5m DO from 2018-2021
ccrwater_flag <- ccrwater_flag %>%
  mutate(
    Flag_DO_5_obs = ifelse(DateTime >= "2018-09-25 10:40" & DateTime<"2018-09-25 10:55",2, Flag_DO_5_obs),
    Flag_DO_5_sat = ifelse(DateTime >= "2018-09-25 10:40" & DateTime<"2018-09-25 10:55",2, Flag_DO_5_sat),
    RDOsat_percent_5 = ifelse(DateTime >= "2018-09-25 10:40"& DateTime<"2018-09-25 10:55",NA, RDOsat_percent_5), 
    RDO_mgL_5 = ifelse(DateTime >= "2018-09-25 10:40"& DateTime<"2018-09-25 10:55",NA, RDO_mgL_5),
    Flag_DO_5_obs = ifelse(DateTime >= "2019-06-17 13:50" & DateTime < "2019-06-17 15:40" & 
                         (! is.na(RDO_mgL_5)) ,2, Flag_DO_5_obs),
    Flag_DO_5_sat = ifelse(DateTime >= "2019-06-17 13:50" & DateTime < "2019-06-17 15:40" & 
                             (! is.na(RDO_mgL_5)) ,2, Flag_DO_5_sat),
    RDOsat_percent_5 = ifelse(DateTime >= "2019-06-17 13:50" & DateTime < "2019-06-17 15:40" & 
                                (! is.na(RDOsat_percent_5)) ,NA, RDOsat_percent_5),
    RDO_mgL_5 = ifelse(DateTime >= "2019-06-17 13:50" & DateTime < "2019-06-17 15:40" &
                         (! is.na(RDO_mgL_5)),NA, RDO_mgL_5),
    Flag_DO_5_sat = ifelse(DateTime >= "2020-07-06 14:10" & DateTime < "2020-07-06 14:40" & 
                         (! is.na(RDO_mgL_5)) ,2, Flag_DO_5_sat),
    Flag_DO_5_obs = ifelse(DateTime >= "2020-07-06 14:10" & DateTime < "2020-07-06 14:40" & 
                             (! is.na(RDO_mgL_5)) ,2, Flag_DO_5_obs),
    RDOsat_percent_5 = ifelse(DateTime >= "2020-07-06 14:10" & DateTime < "2020-07-06 14:40" & 
                                (! is.na(RDOsat_percent_5)) ,NA, RDOsat_percent_5), 
    RDO_mgL_5 = ifelse(DateTime >= "2020-07-06 14:10" & DateTime < "2020-07-06 14:40" &
                         (! is.na(RDO_mgL_5)),NA, RDO_mgL_5),
    Flag_DO_5_obs = ifelse(DateTime >= "2020-07-13 13:00" & DateTime < "2020-07-13 14:50" & 
                         (! is.na(RDO_mgL_5)) ,2, Flag_DO_5_obs),
    Flag_DO_5_sat = ifelse(DateTime >= "2020-07-13 13:00" & DateTime < "2020-07-13 14:50" & 
                             (! is.na(RDO_mgL_5)) ,2, Flag_DO_5_sat),
    RDOsat_percent_5 = ifelse(DateTime >= "2020-07-13 13:00" & DateTime < "2020-07-13 14:50" & 
                                (! is.na(RDOsat_percent_5)) ,NA, RDOsat_percent_5), 
    RDO_mgL_5 = ifelse(DateTime >= "2020-07-13 13:00" & DateTime < "2020-07-13 14:50" &
                         (! is.na(RDO_mgL_5)),NA, RDO_mgL_5))

#take out questionable values for the 9m DO from 2018-2021
ccrwater_flag <- ccrwater_flag %>%
  mutate(
    Flag_DO_9_obs = ifelse(DateTime >= "2018-11-19 15:00" & DateTime < "2018-11-20 12:20" & 
                         (! is.na(RDO_mgL_9)) ,2, Flag_DO_9_obs),
    Flag_DO_9_sat = ifelse(DateTime >= "2018-11-19 15:00" & DateTime < "2018-11-20 12:20" & 
                             (! is.na(RDO_mgL_9)) ,2, Flag_DO_9_sat),
    RDOsat_percent_9 = ifelse(DateTime >= "2018-11-19 15:00" & DateTime < "2018-11-20 12:20" & 
                                (! is.na(RDOsat_percent_9)),NA, RDOsat_percent_9), 
    RDO_mgL_9 = ifelse(DateTime >= "2018-11-19 15:00" & DateTime < "2018-11-20 12:20" &
                         (! is.na(RDO_mgL_9)),NA, RDO_mgL_9),
    Flag_DO_9_obs = ifelse(DateTime >= "2020-12-02 15:00" & DateTime < "2020-12-02 18:00" & 
                         (! is.na(RDO_mgL_9)) ,2, Flag_DO_9_obs),
    Flag_DO_9_sat = ifelse(DateTime >= "2020-12-02 15:00" & DateTime < "2020-12-02 18:00" & 
                             (! is.na(RDO_mgL_9)) ,2, Flag_DO_9_sat),
    RDOsat_percent_9 = ifelse(DateTime >= "2020-12-02 15:00" & DateTime < "2020-12-02 18:00" & 
                                (! is.na(RDOsat_percent_9)) ,NA, RDOsat_percent_9),
    RDO_mgL_9 = ifelse(DateTime >= "2020-12-02 15:00" & DateTime < "2020-12-02 18:00" &
                         (! is.na(RDO_mgL_9)),NA, RDO_mgL_9),
    Flag_DO_9_obs = ifelse(DateTime >= "2021-02-08 17:40" & DateTime < "2021-02-08 18:10" & 
                         (! is.na(RDO_mgL_9)) ,2, Flag_DO_9_obs),
    Flag_DO_9_sat = ifelse(DateTime >= "2021-02-08 17:40" & DateTime < "2021-02-08 18:10" & 
                             (! is.na(RDO_mgL_9)) ,2, Flag_DO_9_sat),
    RDOsat_percent_9 = ifelse(DateTime >= "2021-02-08 17:40" & DateTime < "2021-02-08 18:10" & 
                                (! is.na(RDOsat_percent_9)) ,NA, RDOsat_percent_9),
    RDO_mgL_9 = ifelse(DateTime >= "2021-02-08 17:40" & DateTime < "2021-02-08 18:10" &
                         (! is.na(RDO_mgL_9)),NA, RDO_mgL_9))

#check the DO with graphs
  
#convert DateTime to make the graphs nice
  #Convert the time and put it in current time zone so the times line up when changing NAs.
  #+5 is during EDT and +4 is during EST(make sure to check this in December)
  #Have to do strpttime or you get some NAs in the DateTime column
  #Do this so the graphing is nice
#  ccrwater_flag$DateTime<-as.POSIXct(strptime(ccrwater_flag$DateTime, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+4")

#Check 1.6m EXO DO data
#Plot 2018-current
# EXODO <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = EXODO_mgL_1)) +
#   geom_point()
# EXODO
# 
# # Plotly so can pick out questionable values
# ggplotly(EXODO)
# 
# #Plot Just the current year
# EXODO_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = EXODO_mgL_1)) +
#   geom_point()
# EXODO_21
# # Plotly so can pick out questionable values
# ggplotly(EXODO_21)
# 
# # Check the 5m RDO
# # plot of 5m DO from 2018 to present
# RDO5 <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = RDO_mgL_5)) +
#   geom_point()
# RDO5
# # Plotly so can pick out questionable values
# ggplotly(RDO5)
# 
# # Plot the current year
# RDO5_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = RDO_mgL_5)) +
#   geom_point()
# RDO5_21
# # Plotly so can pick out questionable values
# ggplotly(RDO5_21)
# 
# 
# # Plot the 9m Do
# # From 2018-current 
# RDO9=ggplot(ccrwater_flag, aes(x = DateTime, y = RDO_mgL_9)) +
#   geom_point()
# RDO9
# # Plotly so can pick out questionable values
# ggplotly(RDO9)
# 
# #Just the current year
# RDO9_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = RDO_mgL_9)) +
#   geom_point()
# RDO9_21
# # Plotly so can pick out questionable values
# ggplotly(RDO9_21)
# 


###########################################################################################################################################################################
# chl and phyco visual qaqc-plot to see if everything looks right

# Chla
# Plot for 2018-current
# chl_ugl <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = EXOChla_ugL_1)) +
#   geom_point() 
# chl_ugl
# # Plotly so can pick out questionable values
# ggplotly(chl_ugl)
# 
# # Plot just the current year
# chl_ugl_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = EXOChla_ugL_1)) +
#   geom_point()
# chl_ugl_21
# 
# 
# # plot the daily mean
# # calculate the daily mean
# chl_mean <- ccrwater_flag %>%
#  select(DateTime, EXOChla_ugL_1) %>%
#  mutate(day = date(DateTime)) %>%
#  group_by(day) %>%
#  mutate(daily_mean = mean(EXOChla_ugL_1, na.rm = TRUE)) %>%
#  distinct(day, .keep_all = TRUE)
# 
# # plot the daily mean
# chl_mean_plot <- ggplot(data = chl_mean, aes(x = day, y = daily_mean)) +
#  geom_point()
# chl_mean_plot
# # Plotly so can pick out questionable values
# ggplotly(chl_mean)
# 
# # Plot the chla and the daily mean on the same graph
# plot(ccrwater_flag$DateTime, ccrwater_flag$EXOChla_ugL_1)
# points(chl_mean$DateTime, chl_mean$daily_mean, type="l", col="green")
# 
# 
# # Chla-RFU
# # Plot 2018-current
# chl_rfu <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = EXOChla_RFU_1)) +
#   geom_point()
# chl_rfu
# # Plotly so can pick out questionable values
# ggplotly(chl_rfu)
# 
# 
# # Just the current year
# chl_rfu_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = EXOChla_RFU_1)) +
#   geom_point()
# chl_rfu_21
# 
# # Phyco-RFU
# # Plot 2018-current
# phyco_rfu <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = EXOBGAPC_RFU_1)) +
#   geom_point() 
# phyco_rfu
# # Plotly so can pick out questionable values
# ggplotly(phyco_rfu)
# 
# # Just the current year
# phyco_rfu_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = EXOBGAPC_RFU_1)) +
#   geom_point()
# # Plotly so can pick out questionable values
# phyco_rfu_21
# 
# #################################################################################################
# 
# # fDOM-RFU
# # Plot 2018-current
# fDOM_rfu <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = EXOfDOM_RFU_1)) +
#   geom_point() 
# fDOM_rfu
# # Plotly so can pick out questionable values
# ggplotly(fDOM_rfu)
# 
# # Just the current year
# fDOM_rfu_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = EXOfDOM_RFU_1)) +
#   geom_point()
# # Plotly so can pick out questionable values
# fDOM_rfu_21
# 
# # fDOM-QSU
# # Plot 2018-current
# fDOM_qsu <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = EXOfDOM_QSU_1)) +
#   geom_point() 
# fDOM_qsu
# # Plotly so can pick out questionable values
# ggplotly(fDOM_qsu)
# 
# # Just the current year
# fDOM_qsu_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = EXOfDOM_QSU_1)) +
#   geom_point()
# # Plotly so can pick out questionable values
# fDOM_qsu_21
# 


###########################################################################################################################################################################
# conductivity visual QAQC

#Flag high conductivity values in 2020

ccrwater_flag <- ccrwater_flag %>%
  mutate(
    DateTime=as.character(DateTime),#change DateTime to as.character so they line up when making changes
    Flag_Cond = ifelse(DateTime >"2020-05-01 00:00" & DateTime < "2020-08-31 23:59" &
                             EXOCond_uScm_1>50 ,5, Flag_Chla),
    Flag_SpCond = ifelse(DateTime >"2020-05-01 00:00" & DateTime < "2020-08-31 23:59" &
                           EXOSpCond_uScm_1>50 ,5, Flag_SpCond),
    Flag_TDS = ifelse(DateTime >"2020-05-01 00:00" & DateTime < "2020-08-31 23:59" &
                        EXOTDS_mgL_1>30 ,5, Flag_TDS),
    ) 

#convert DateTime to make the graphs nice
#Convert the time and put it in current time zone so the times line up when changing NAs.
#+5 is during EDT and +4 is during EST(make sure to check this in December)
#Have to do strpttime or you get some NAs in the DateTime column
#Do this so the graphing is nice
# ccrwater_flag$DateTime<-as.POSIXct(strptime(ccrwater_flag$DateTime, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+4")
# 
# #Plot from 2018-current
# Cond <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = EXOCond_uScm_1)) +
#   geom_point()
# Cond
# # Plotly so can pick out questionable values
# ggplotly(Cond)
# 
# 
# Con=ccrwater_flag%>%
#   select(c(DateTime,EXOTemp_C_1,EXOCond_uScm_1,EXOSpCond_uScm_1,EXO_wiper,EXO_pressure))
# #Just the current year
# 
# Cond_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = EXOCond_uScm_1)) +
#   geom_point()
# Cond_21
# # Plotly so can pick out questionable values
# ggplotly(Cond_21)
# 
# #Specific Conductivity
# 
# #Plot from 2018-current
# SpCond <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = EXOSpCond_uScm_1)) +
#   geom_point()
# SpCond
# # Plotly so can pick out questionable values
# ggplotly(SpCond)
# 
# #Just the current year
# SpCond_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = EXOSpCond_uScm_1)) +
#   geom_point()
# SpCond_21
# # Plotly so can pick out questionable values
# ggplotly(SpCond_21)
# 
# #Total Dissolved Solids
# TDS <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = EXOTDS_mgL_1)) +
#   geom_point()
# TDS
# # Plotly so can pick out questionable values
# ggplotly(TDS)
# 
# #Just the current year
# TDS_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = EXOTDS_mgL_1)) +
#   geom_point()
# TDS_21
# # Plotly so can pick out questionable values
# ggplotly(TDS_21)
# 
# #Depth
# Depth <- ggplot(data = ccrwater_flag, aes(x = DateTime, y = EXO_depth)) +
#   geom_point()
# Depth
# # Plotly so can pick out questionable values
# ggplotly(Depth)
# 
# #Just the current year
# Depth_21=ccrwater_flag%>%
#   filter(DateTime>current_time)%>%
#   ggplot(.,aes(x = DateTime, y = EXO_depth)) +
#   geom_point()
# Depth_21
# # Plotly so can pick out questionable values
# ggplotly(Depth_21)

###########################################################################################################################################################################
# write final csv ----
# some final checking of flags
# for (i in 1:nrow(ccrwater_all)) {
#   if(is.na(ccrwater_all$Flag_fDOM[i])){
#     ccrwater_all$Flag_fDOM[i] <- 0
#   }
# }

#Order by date and time

ccrwater_flag <- ccrwater_flag[order(ccrwater_flag$DateTime),]


# for(b in 1:nrow())
# str(ccrwater_flag)

#rearrange the cols
ccrwater_flag <- ccrwater_flag %>%
  select(Reservoir, Site, DateTime, ThermistorTemp_C_surface:ThermistorTemp_C_9,
         RDO_mgL_5, RDOsat_percent_5, RDO_mgL_5_adjusted, RDOsat_percent_5_adjusted,
         RDOTemp_C_5, RDO_mgL_9, RDOsat_percent_9, RDO_mgL_9_adjusted, RDOsat_percent_9_adjusted, RDOTemp_C_9,
         EXOTemp_C_1, EXOCond_uScm_1, EXOSpCond_uScm_1, EXOTDS_mgL_1, EXODOsat_percent_1,
         EXODO_mgL_1, EXOChla_RFU_1, EXOChla_ugL_1, EXOBGAPC_RFU_1, EXOBGAPC_ugL_1,
         EXOfDOM_RFU_1, EXOfDOM_QSU_1, EXO_pressure, EXO_depth, EXO_battery, EXO_cablepower,
         EXO_wiper, Lvl_psi_9, LvlTemp_C_9, RECORD, CR6_Batt_V, CR6Panel_Temp_C,
         Flag_All, Flag_Temp_Surf:Flag_Temp_9,Flag_DO_5_obs, Flag_DO_5_sat, Flag_DO_5_temp,
         Flag_DO_9_obs, Flag_DO_9_sat, Flag_DO_9_temp,Flag_EXOTemp, Flag_Cond, Flag_SpCond,Flag_TDS,
         Flag_DO_1_sat, Flag_DO_1_obs, Flag_Chla, Flag_Phyco,Flag_fDOM, Flag_Pres )

# convert datetimes to characters so that they are properly formatted in the output file
ccrwater_flag$DateTime <- as.character(ccrwater_flag$DateTime)
  
  
write.csv(ccrwater_flag, paste0(folder, '/Catwalk_EDI_2018_2021.csv'), row.names = FALSE)


