# QAQC in prep for publishing catwalk sensor string data to EDI
# Set up ----
pacman::p_load("RCurl","tidyverse","lubridate", "plotly", "magrittr")
folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLCatwalk"
source(paste0(folder, "/temp_oxy_chla_qaqc.R"))

# download most up to date catwalk data and maintenance log
#download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/CAT_MaintenanceLog.txt",paste0(folder, "/CAT_MaintenanceLog_2020.txt"))
#download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/Catwalk.csv","Catwalk_2020.csv")

# run standard qaqc
data_file <- paste0(folder, '/Catwalk_2020.csv')
maintenance_file <- paste0(folder, "/CAT_MaintenanceLog_2020.txt")
output_file <- paste0(folder, "/Catwalk_first_QAQC_2020.csv")
temp_oxy_chla_qaqc(data_file, maintenance_file, output_file)

# read in qaqc function output
catdata <- read.csv(output_file) 
catdata$DateTime<-as.POSIXct(catdata$DateTime,format = "%Y-%m-%d %H:%M:%S")

# subset file to only unpublished data
catdata_flag <- catdata[catdata$DateTime>"2019-12-31",]
catdata_flag <- catdata_flag[!is.na(catdata_flag$Reservoir),]


# assign new flags for temp string, set to 0
catdata_flag$Flag_Temp_Surf <- 0
catdata_flag$Flag_Temp_1 <- 0
catdata_flag$Flag_Temp_2 <- 0
catdata_flag$Flag_Temp_3 <- 0
catdata_flag$Flag_Temp_4 <- 0
catdata_flag$Flag_Temp_5 <- 0
catdata_flag$Flag_Temp_6 <- 0
catdata_flag$Flag_Temp_7 <- 0
catdata_flag$Flag_Temp_8 <- 0
catdata_flag$Flag_Temp_9 <- 0



# Flag values
# 0: no flag
# 1: value removed due to maintenance and set to NA
# 2: value set to NA, major outlier which is more than 2 standard deviations different from previous or following datapoint
# 3: negative values set to 0
# 4: value removed due to fouling and set to NA
# 5: questionable value due to potential fouling
# 6: very questionable value due to potential fouling. Values adjusted using a linear or square root function to match high-resolution CTD profiles are given in RDO_mgL_5 and RDO_sat_percent_5
# 7: missing data
# 8: Value corrected using a constant offset due to two thermistor malfunctions in Fall 2020
# ADD IN ABP FLAGS HERE

###########################################################################################################################################################################
# temp qaqc ----

#Two of the thermistors started to read higher than the one above them in fall 2020. Fixed this using a constant offset. 
#methods described in metadat

#start time for 1m is 30 Oct 2020 13:00EST
#start time for 4m is 31 Oct 2020 5:00EST
catdata_flag <- catdata_flag %>%
  mutate(Flag_Temp_1 = ifelse(DateTime >= "2020-10-30 13:00" & DateTime < "2020-12-31 23:50" & (! is.na(ThermistorTemp_C_1)) ,8, Flag_Temp_1))%>%
  mutate(Flag_Temp_4 = ifelse(DateTime >= "2020-10-31 5:00" & DateTime < "2020-12-31 23:50" &
                                (! is.na(ThermistorTemp_C_4)),8, Flag_Temp_1))%>%
  mutate(ThermistorTemp_C_1 = ifelse(DateTime >= "2020-10-30 13:00" & DateTime < "2020-12-31 23:50" &
                                        (! is.na(ThermistorTemp_C_1)), (ThermistorTemp_C_1-0.22617), ThermistorTemp_C_1 )) %>%
  mutate(ThermistorTemp_C_4 = ifelse(DateTime >= "2020-10-30 13:00" & DateTime < "2020-12-31 23:50" &
                                        (! is.na(ThermistorTemp_C_4)), (ThermistorTemp_C_4-0.18122), ThermistorTemp_C_4 )) 

# check surface temp data
#surf <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_surface)) +
#  geom_point()
#ggplotly(surf)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
catdata_flag$ThermistorTemp_C_surface[catdata_flag$DateTime=='2020-09-15 12:50:00'] <- NA
catdata_flag$Flag_Temp_Surf[catdata_flag$DateTime=='2020-09-15 12:50:00'] <- 1
catdata_flag$ThermistorTemp_C_surface[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA
catdata_flag$Flag_Temp_Surf[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_surface[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA
catdata_flag$Flag_Temp_Surf[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- 1
#surf <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_surface)) +
#  geom_point()
#ggplotly(surf)

# check 1m temp data
#m_1 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_1)) +
#  geom_point()
#ggplotly(m_1)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
catdata_flag$ThermistorTemp_C_1[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA
catdata_flag$Flag_Temp_1[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_1[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA
catdata_flag$Flag_Temp_1[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_1[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- NA
catdata_flag$Flag_Temp_1[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_1[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- NA
catdata_flag$Flag_Temp_1[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- 1
#m_1 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_1)) +
#  geom_point()
#ggplotly(m_1)

# check 2m temp data
#m_2 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_2)) +
#  geom_point()
#ggplotly(m_2)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
catdata_flag$ThermistorTemp_C_2[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA
catdata_flag$Flag_Temp_2[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_2[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA
catdata_flag$Flag_Temp_2[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_2[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- NA
catdata_flag$Flag_Temp_2[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_2[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- NA
catdata_flag$Flag_Temp_2[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- 1
catdata_flag$ThermistorTemp_C_2[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- NA
catdata_flag$Flag_Temp_2[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- 1
#m_2 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_2)) +
#  geom_point()
#ggplotly(m_2)

# check 3m temp data
#m_3 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_3)) +
#  geom_point()
#ggplotly(m_3)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
catdata_flag$ThermistorTemp_C_3[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA
catdata_flag$Flag_Temp_3[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_3[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA
catdata_flag$Flag_Temp_3[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_3[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- NA
catdata_flag$Flag_Temp_3[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_3[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- NA
catdata_flag$Flag_Temp_3[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- 1
# no maintenance recorded on this day but this was a field day, so forcing this datapoint to NA
catdata_flag$ThermistorTemp_C_3[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_flag$Flag_Temp_3[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- 1
#m_3 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_3)) +
#  geom_point()
#ggplotly(m_3)

# check 4m temp data
#m_4 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_4)) +
#  geom_point()
#ggplotly(m_4)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- NA
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- NA
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- 1
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- 1
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-09-15 12:50:00'] <- NA 
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-09-15 12:50:00'] <- 1
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-09-15 12:40:00'] <- NA 
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-09-15 12:40:00'] <- 1
#m_4 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_4)) +
#  geom_point()
#ggplotly(m_4)

# check 5m temp data
#m_5 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_5)) +
#  geom_point()
#ggplotly(m_5)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
catdata_flag$ThermistorTemp_C_5[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA
catdata_flag$Flag_Temp_5[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_5[catdata_flag$DateTime=='2020-11-09 11:40:00'] <- NA
catdata_flag$Flag_Temp_5[catdata_flag$DateTime=='2020-11-09 11:40:00'] <- 1
catdata_flag$ThermistorTemp_C_5[catdata_flag$DateTime=='2020-11-09 11:50:00'] <- NA
catdata_flag$Flag_Temp_5[catdata_flag$DateTime=='2020-11-09 11:50:00'] <- 1
catdata_flag$ThermistorTemp_C_5[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA
catdata_flag$Flag_Temp_5[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- 1
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
catdata_flag$ThermistorTemp_C_5[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_flag$Flag_Temp_5[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- 1
catdata_flag$ThermistorTemp_C_5[catdata_flag$DateTime=='2020-09-15 12:50:00'] <- NA 
catdata_flag$Flag_Temp_5[catdata_flag$DateTime=='2020-09-15 12:50:00'] <- 1
catdata_flag$ThermistorTemp_C_5[catdata_flag$DateTime=='2020-09-15 12:40:00'] <- NA 
catdata_flag$Flag_Temp_5[catdata_flag$DateTime=='2020-09-15 12:40:00'] <- 1
#m_5 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_5)) +
#  geom_point()
#ggplotly(m_5)

# check 6m temp data
#m_6 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_6)) +
#  geom_point()
#ggplotly(m_6)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
catdata_flag$ThermistorTemp_C_6[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA
catdata_flag$Flag_Temp_6[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_6[catdata_flag$DateTime=='2020-11-09 11:40:00'] <- NA
catdata_flag$Flag_Temp_6[catdata_flag$DateTime=='2020-11-09 11:40:00'] <- 1
catdata_flag$ThermistorTemp_C_6[catdata_flag$DateTime=='2020-11-09 11:50:00'] <- NA
catdata_flag$Flag_Temp_6[catdata_flag$DateTime=='2020-11-09 11:50:00'] <- 1
catdata_flag$ThermistorTemp_C_6[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA
catdata_flag$Flag_Temp_6[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_6[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- NA
catdata_flag$Flag_Temp_6[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_6[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- NA
catdata_flag$Flag_Temp_6[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- 1
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
catdata_flag$ThermistorTemp_C_6[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_flag$Flag_Temp_6[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- 1
catdata_flag$ThermistorTemp_C_6[catdata_flag$DateTime=='2020-08-10 12:30:00'] <- NA 
catdata_flag$Flag_Temp_6[catdata_flag$DateTime=='2020-08-10 12:30:00'] <- 1


days <- c('2020-07-13 10:50:00', '2020-07-13 11:00:00', '2020-07-13 11:10:00', '2020-07-20 11:00:00', '2020-07-20 11:10:00', '2020-07-27 11:50:00', 
          '2020-07-27 12:00:00', '2020-08-03 12:20:00', '2020-08-03 12:30:00', '2020-08-07 12:30:00', '2020-08-07 12:40:00', '2020-08-07 12:50:00', 
          '2020-08-10 12:20:00', '2020-08-10 12:30:00', '2020-08-10 12:40:00', '2020-08-17 15:20:00', '2020-08-17 15:30:00', '2020-08-17 15:40:00', 
          '2020-08-24 11:10:00', '2020-08-24 11:20:00', '2020-09-02 11:40:00', '2020-09-02 11:50:00', '2020-09-02 12:00:00', '2020-09-11 10:00:00', 
          '2020-09-15 10:10:00', '2020-09-15 13:00:00', '2020-09-15 13:10:00', '2020-09-15 13:20:00', '2020-09-15 12:40:00', '2020-09-15 12:50:00', 
          '2020-09-19 13:10:00', '2020-11-02 11:30:00', '2020-11-09 11:30:00', '2020-11-09 11:40:00', '2020-11-09 11:50:00', '2020-11-24 11:30:00', 
          '2020-11-24 11:40:00')
days <- as.POSIXct(days,format = "%Y-%m-%d %H:%M:%S")
catdata_flag$Flag_Temp_6[catdata_flag$DateTime %in% days] <- 1
catdata_flag$ThermistorTemp_C_6[catdata_flag$DateTime %in% days] <- NA
#m_6 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_6)) +
#  geom_point()
#ggplotly(m_6)

# check 7m temp data
#m_7 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_7)) +
#  geom_point()
#ggplotly(m_7)

catdata_flag$Flag_Temp_7[catdata_flag$DateTime %in% days] <- 1
catdata_flag$ThermistorTemp_C_7[catdata_flag$DateTime %in% days] <- NA
# spot checks:
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
catdata_flag$ThermistorTemp_C_7[catdata_flag$DateTime=='2020-06-12 10:30:00'] <- NA 
catdata_flag$Flag_Temp_7[catdata_flag$DateTime=='2020-06-12 10:30:00'] <- 1
#m_7 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_7)) +
#  geom_point()
#ggplotly(m_7)

# check 8m temp data
#m_8 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_8)) +
#  geom_point()
#ggplotly(m_8)

catdata_flag$Flag_Temp_8[catdata_flag$DateTime %in% days] <- 1
catdata_flag$ThermistorTemp_C_8[catdata_flag$DateTime %in% days] <- NA
# spot checks:
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
catdata_flag$ThermistorTemp_C_8[catdata_flag$DateTime=='2020-06-12 10:30:00'] <- NA 
catdata_flag$Flag_Temp_8[catdata_flag$DateTime=='2020-06-12 10:30:00'] <- 1
catdata_flag$ThermistorTemp_C_8[catdata_flag$DateTime=='2020-10-14 12:10:00'] <- NA 
catdata_flag$Flag_Temp_8[catdata_flag$DateTime=='2020-10-14 12:10:00'] <- 1
catdata_flag$ThermistorTemp_C_8[catdata_flag$DateTime=='2020-10-14 12:20:00'] <- NA 
catdata_flag$Flag_Temp_8[catdata_flag$DateTime=='2020-10-14 12:20:00'] <- 1

# check 9m temp data
#m_9 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_9)) +
#  geom_point()
#ggplotly(m_9)

catdata_flag$Flag_Temp_9[catdata_flag$DateTime %in% days] <- 1
catdata_flag$ThermistorTemp_C_9[catdata_flag$DateTime %in% days] <- NA
# spot checks:
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
catdata_flag$ThermistorTemp_C_9[catdata_flag$DateTime=='2020-06-12 10:30:00'] <- NA 
catdata_flag$Flag_Temp_9[catdata_flag$DateTime=='2020-06-12 10:30:00'] <- 1
catdata_flag$ThermistorTemp_C_9[catdata_flag$DateTime=='2020-06-12 10:40:00'] <- NA 
catdata_flag$Flag_Temp_9[catdata_flag$DateTime=='2020-06-12 10:40:00'] <- 1
catdata_flag$ThermistorTemp_C_9[catdata_flag$DateTime=='2020-09-30 10:30:00'] <- NA 
catdata_flag$Flag_Temp_9[catdata_flag$DateTime=='2020-09-30 10:30:00'] <- 1

###########################################################################################################################################################################
# DO qaqc ----

# now fix the negative DO values
catdata_flag <- catdata_flag %>%  #RDO at 5m
  mutate(Flag_DO_5 = ifelse(RDO_mgL_5 < 0 | RDOsat_percent_5 < 0, 3, Flag_DO_5), #Add a flag for DO<0
         RDO_mgL_5 = ifelse(RDO_mgL_5 < 0, 0, RDO_mgL_5), #Change negative to 0
         RDOsat_percent_5 = ifelse(RDOsat_percent_5 < 0, 0, RDOsat_percent_5), #Change negative %sat to 0
         Flag_DO_5 = ifelse(is.na(RDO_mgL_5),7,Flag_DO_5), #Flag NA values
         
         Flag_DO_9 = ifelse(RDO_mgL_9 < 0 | RDOsat_percent_9 < 0, 3, Flag_DO_9), #repeat for 9m
         RDO_mgL_9 = ifelse(RDO_mgL_9 < 0, 0, RDO_mgL_9),
         RDOsat_percent_9 = ifelse(RDOsat_percent_9 < 0, 0, RDOsat_percent_9),
         Flag_DO_9 = ifelse(is.na(RDO_mgL_9),7,Flag_DO_9),
         
         Flag_DO_1 = ifelse(EXODO_mgL_1 < 0 | EXODOsat_percent_1 <0, 3, Flag_DO_1), #and for 1m
         EXODO_mgL_1 = ifelse(EXODO_mgL_1 < 0, 0, EXODO_mgL_1),
         EXODOsat_percent_1 = ifelse(EXODOsat_percent_1 <0, 0, EXODOsat_percent_1),
         Flag_DO_1 = ifelse(is.na(EXODO_mgL_1),7,Flag_DO_1))

#Deal with when the sensors were up
maint = read.csv(paste0(folder, "/CAT_MaintenanceLog_2020.txt"))
maint = maint[!grepl("EXO",maint$parameter),] #creating file "maint" with all sensor string maintenance
maint = maint%>%
  filter(!colnumber %in% c(" c(24:26)"," 40"," 41"))
clean_start<-as.POSIXct(maint$TIMESTAMP_start, tz = "EST")
clean_end <- as.POSIXct(maint$TIMESTAMP_end, tz = "EST")

ADJ_PERIOD = 2*60*60 #amount of time to stabilization after cleaning in seconds

for (i in 1:length(clean_start)){ #Set all data during cleaning and for ADJ_PERIOD after to NA
  catdata_flag$RDO_mgL_5[catdata_flag$DateTime>clean_start[i]&catdata_flag$DateTime<(clean_end[i]+ADJ_PERIOD)] <- NA
  catdata_flag$RDO_mgL_9[catdata_flag$DateTime>clean_start[i]&catdata_flag$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
  catdata_flag$RDOsat_percent_5[catdata_flag$DateTime>clean_start[i]&catdata_flag$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
  catdata_flag$RDOsat_percent_9[catdata_flag$DateTime>clean_start[i]&catdata_flag$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
  catdata_flag$Flag_DO_5[catdata_flag$DateTime>clean_start[i]&catdata_flag$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
  catdata_flag$Flag_DO_9[catdata_flag$DateTime>clean_start[i]&catdata_flag$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
}

################
#Creating a new flag "6" started in 2019: "very questionable value due to potential fouling. Values adjusted using a linear or square root function to match high-resolution CTD profiles are given in RDO_mgL_5 and RDO_sat_percent_5"
#Creating a new flag "5" which means that the values are very questionable due to fouling but not adjusted (starting in 2019)
################



catdata_flag <- catdata_flag%>%
  mutate(
    #9 meters #### READ NOTE: These are the sections I noticed apparent following in TS and have tried to correct with linear adjustments
    #first section fixed 11aug to 17aug
    Flag_DO_9 = ifelse(DateTime<"2020-08-17 12:50:00" & DateTime> "2020-08-11 7:00:00",6, Flag_DO_9),
    RDO_mgL_9_adjusted = ifelse(DateTime<"2020-08-17 12:50:00" & DateTime> "2020-08-11 7:00:00", 
                                RDO_mgL_9 + as.numeric(difftime(DateTime,"2020-08-11 7:00:00", units = "mins"))/6500, #A linear adjustment here
                                RDO_mgL_9),
    RDOsat_percent_9_adjusted = ifelse(DateTime<"2020-08-17 12:50:00" & DateTime> "2020-08-11 15:00:00", 
                                       RDOsat_percent_9 + (as.numeric(difftime("2020-08-17 12:50:00","2020-08-12 15:00:00", units = "mins")))/6500/11.3*100,
                                       RDOsat_percent_9),
    
    # 9 meters 19aug to 24aug
    Flag_DO_9 = ifelse(DateTime<"2020-08-24 10:40:00" & DateTime> "2020-08-19 20:00:00",6, Flag_DO_9),
    RDO_mgL_9_adjusted = ifelse(DateTime<"2020-08-24 10:40:00" & DateTime> "2020-08-19 20:00:00", 
                                RDO_mgL_9 + (as.numeric(difftime(DateTime,"2020-08-19 20:00:00", units = "mins")))/6500, #A linear adjustment here
                                RDO_mgL_9_adjusted),
    RDOsat_percent_9_adjusted = ifelse(DateTime<"2020-08-24 10:40:00" & DateTime> "2020-08-19 20:00:00", 
                                       RDOsat_percent_9 + as.numeric(difftime(DateTime,"2020-08-19 20:00:00", units = "mins"))/6500/11.3*100,
                                       RDOsat_percent_9_adjusted),
    
    # 9 meters 26aug to 02sep
    Flag_DO_9 = ifelse(DateTime<"2020-09-02 10:50:00" & DateTime> "2020-08-26 12:00:00" ,6, Flag_DO_9),
    RDO_mgL_9_adjusted = ifelse(DateTime< "2020-09-02 10:50:00" & DateTime> "2020-08-26 12:00:00", 
                                RDO_mgL_9 + (as.numeric(difftime(DateTime, "2020-08-26 12:00:00", units = "mins")))/10000, #A linear adjustment here
                                RDO_mgL_9_adjusted),
    RDOsat_percent_9_adjusted = ifelse(DateTime< "2020-09-02 10:50:00" & DateTime> "2020-08-26 12:00:00", 
                                       RDOsat_percent_9 + as.numeric(difftime(DateTime, "2020-08-26 12:00:00", units = "mins"))/10000/11.3*100,
                                       RDOsat_percent_9_adjusted),
    
    # 9 meters 02sep to 11sep
    Flag_DO_9 = ifelse(DateTime<"2020-09-09 17:50:00" & DateTime> "2020-09-05 06:00:00" ,6, Flag_DO_9),
    RDO_mgL_9_adjusted = ifelse(DateTime< "2020-09-09 17:50:00" & DateTime> "2020-09-05 06:00:00", 
                                RDO_mgL_9 + (as.numeric(difftime(DateTime, "2020-09-05 06:00:00", units = "mins")))/3000, #A linear adjustment here
                                RDO_mgL_9_adjusted),
    RDOsat_percent_9_adjusted = ifelse(DateTime< "2020-09-09 17:00:00" & DateTime> "2020-09-05 06:00:00", 
                                       RDOsat_percent_9 + as.numeric(difftime(DateTime, "2020-09-05 06:00:00", units = "mins"))/3000/11.3*100,
                                       RDOsat_percent_9_adjusted))

###########################################################################################################################################################
# merge 2018-2019 EDI data with 2020 data ----
catdata_flag <- catdata_flag[catdata_flag$DateTime<'2021-01-01',]
catdata_flag$RDO_mgL_5_adjusted <- catdata_flag$RDO_mgL_5
catdata_flag$RDOsat_percent_5_adjusted <- catdata_flag$RDOsat_percent_5
catdata_flag <- catdata_flag %>% select(Reservoir, Site, DateTime, ThermistorTemp_C_surface:RDOTemp_C_9, 
                                        EXOTemp_C_1, EXOCond_uScm_1, EXOSpCond_uScm_1, EXOTDS_mgL_1, EXODOsat_percent_1, 
                                        EXODO_mgL_1, EXOChla_RFU_1, EXOChla_ugL_1, EXOBGAPC_RFU_1, EXOBGAPC_ugL_1, 
                                        EXOfDOM_RFU_1, EXOfDOM_QSU_1, EXO_pressure, EXO_depth, EXO_battery, EXO_cablepower, 
                                        EXO_wiper, RECORD, CR6_Batt_V, CR6Panel_Temp_C, 
                                        RDO_mgL_5_adjusted, RDOsat_percent_5_adjusted,RDO_mgL_9_adjusted, RDOsat_percent_9_adjusted, 
                                        Flag_All:Flag_Temp_9)

# read in catwalk data already published on EDI
catdata_pub <- read_csv(paste0(folder, '/catdata_EDI_2019_downloaded12Jan21.csv'), col_types = cols(.default = "d", 
                                                                                                    Reservoir = "c",
                                                                                                    DateTime = "T"))
catdata_pub$DateTime<-as.POSIXct(catdata_pub$DateTime,format = "%Y-%m-%d %H:%M:%S")
catdata_pub <- catdata_pub[catdata_pub$DateTime<'2020-01-01',]
# check for duplicated hour???

# assign new flags for temp string, set to 0
catdata_pub$Flag_Temp_Surf <- 0
catdata_pub$Flag_Temp_1 <- 0
catdata_pub$Flag_Temp_2 <- 0
catdata_pub$Flag_Temp_3 <- 0
catdata_pub$Flag_Temp_4 <- 0
catdata_pub$Flag_Temp_5 <- 0
catdata_pub$Flag_Temp_6 <- 0
catdata_pub$Flag_Temp_7 <- 0
catdata_pub$Flag_Temp_8 <- 0
catdata_pub$Flag_Temp_9 <- 0

catdata_all <- rbind(catdata_pub, catdata_flag)

###########################################################################################################################################################################
# chl and phyco qaqc ----
# perform qaqc on the entire dataset for chl and phyco

# assign standard deviation thresholds
sd_4 <- 4*sd(catdata_all$EXOChla_ugL_1, na.rm = TRUE)
threshold <- sd_4
sd_4_phyco <- 4*sd(catdata_all$EXOBGAPC_ugL_1, na.rm = TRUE)
threshold_phyco <- sd_4_phyco

chl_ugl <- ggplot(data = catdata_all, aes(x = DateTime, y = EXOChla_ugL_1)) +
  geom_point() +
  geom_hline(yintercept = sd_4)
ggplotly(chl_ugl)


# QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
catdata_all <- catdata_all %>% 
  mutate(Chla = lag(EXOChla_ugL_1, 0),
         Chla_lag1 = lag(EXOChla_ugL_1, 1),
         Chla_lead1 = lead(EXOChla_ugL_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOChla_ugL_1 = ifelse((abs(Chla_lag1 - Chla) > (threshold))  & (abs(Chla_lead1 - Chla) > (threshold)), 
                                NA, EXOChla_ugL_1)) %>%   
  mutate(EXOChla_RFU_1 = ifelse((abs(Chla_lag1 - Chla) > (threshold))  & (abs(Chla_lead1 - Chla) > (threshold)), 
                                   NA, EXOChla_RFU_1)) %>% 
  mutate(Flag_Chla = ifelse(((abs(Chla_lag1 - Chla) > (threshold))  & (abs(Chla_lead1 - Chla) > (threshold))), 
                            2, Flag_Chla)) %>% 
  select(-Chla, -Chla_lag1, -Chla_lead1)

  

# some spot checking of days with isolated weird data which occured either on maintenance days or field days
catdata_all$EXOChla_ugL_1[catdata_all$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_all$Flag_Chla[catdata_all$DateTime=='2020-08-10 12:20:00'] <- 1
catdata_all$EXOChla_ugL_1[catdata_all$DateTime=='2020-09-15 12:40:00'] <- NA 
catdata_all$Flag_Chla[catdata_all$DateTime=='2020-09-15 12:40:00'] <- 1
catdata_all$EXOChla_ugL_1[catdata_all$DateTime=='2020-09-15 12:50:00'] <- NA 
catdata_all$Flag_Chla[catdata_all$DateTime=='2020-09-15 12:50:00'] <- 1
catdata_all$EXOChla_ugL_1[catdata_all$DateTime=='2020-09-15 13:00:00'] <- NA 
catdata_all$Flag_Chla[catdata_all$DateTime=='2020-09-15 13:00:00'] <- 1
catdata_all$EXOChla_ugL_1[catdata_all$DateTime=='2020-11-02 11:30:00'] <- NA 
catdata_all$Flag_Chla[catdata_all$DateTime=='2020-11-02 11:30:00'] <- 1
catdata_all$EXOChla_ugL_1[catdata_all$DateTime=='2020-11-09 11:30:00'] <- NA 
catdata_all$Flag_Chla[catdata_all$DateTime=='2020-11-09 11:30:00'] <- 1
catdata_all$EXOChla_ugL_1[catdata_all$DateTime=='2020-11-24 11:40:00'] <- NA 
catdata_all$Flag_Chla[catdata_all$DateTime=='2020-11-24 11:40:00'] <- 1
catdata_all$EXOChla_ugL_1[catdata_all$DateTime=='2020-10-26 09:10:00'] <- NA 
catdata_all$Flag_Chla[catdata_all$DateTime=='2020-10-26 09:10:00'] <- 1

chl_mean <- catdata_flag %>% 
  select(DateTime, EXOChla_ugL_1) %>% 
  mutate(day = date(DateTime)) %>% 
  group_by(day) %>% 
  mutate(daily_mean = mean(EXOChla_ugL_1, na.rm = TRUE)) %>% 
  distinct(day, .keep_all = TRUE)
#chl_mean_plot <- ggplot(data = chl_mean, aes(x = day, y = daily_mean)) +
#  geom_point() 
#ggplotly(chl_mean)

#chl_rfu <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOChla_RFU_1)) +
#  geom_point() +
#  geom_hline(yintercept = sd_4)
#ggplotly(chl_rfu)
catdata_all$EXOChla_RFU_1[catdata_all$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_all$EXOChla_RFU_1[catdata_all$DateTime=='2020-09-15 12:40:00'] <- NA 
catdata_all$EXOChla_RFU_1[catdata_all$DateTime=='2020-09-15 12:50:00'] <- NA 
catdata_all$EXOChla_RFU_1[catdata_all$DateTime=='2020-09-15 13:00:00'] <- NA 
catdata_all$EXOChla_RFU_1[catdata_all$DateTime=='2020-11-02 11:30:00'] <- NA 
catdata_all$EXOChla_RFU_1[catdata_all$DateTime=='2020-11-09 11:30:00'] <- NA 
catdata_all$EXOChla_RFU_1[catdata_all$DateTime=='2020-11-24 11:40:00'] <- NA 
catdata_all$EXOChla_RFU_1[catdata_all$DateTime=='2020-10-26 09:10:00'] <- NA 


#phyco_ugl <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOBGAPC_ugL_1)) +
#  geom_point() 
#ggplotly(phyco_ugl)

# QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
catdata_all <- catdata_all %>% 
  mutate(phyco = lag(EXOBGAPC_ugL_1, 0),
         phyco_lag1 = lag(EXOBGAPC_ugL_1, 1),
         phyco_lead1 = lead(EXOBGAPC_ugL_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOBGAPC_ugL_1 = ifelse((abs(phyco_lag1 - phyco) > (threshold_phyco))  & (abs(phyco_lead1 - phyco) > (threshold_phyco)), 
                                 NA, EXOBGAPC_ugL_1)) %>%   
  mutate(EXOBGAPC_RFU_1 = ifelse((abs(phyco_lag1 - phyco) > (threshold_phyco))  & (abs(phyco_lead1 - phyco) > (threshold_phyco)), 
                                 NA, EXOBGAPC_RFU_1)) %>% 
  mutate(Flag_Phyco = ifelse((abs(phyco_lag1 - phyco) > (threshold_phyco))  & (abs(phyco_lead1 - phyco) > (threshold_phyco)), 
                             2, Flag_Phyco)) %>%
  select(-phyco, -phyco_lag1, -phyco_lead1)

# some spot checking of days with isolated weird data which occured either on maintenance days or field days
catdata_all$EXOBGAPC_ugL_1[catdata_all$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_all$Flag_Phyco[catdata_all$DateTime=='2020-08-10 12:20:00'] <- 1 
catdata_all$EXOBGAPC_ugL_1[catdata_all$DateTime=='2020-10-26 09:10:00'] <- NA 
catdata_all$Flag_Phyco[catdata_all$DateTime=='2020-10-26 09:10:00'] <- 1 
catdata_all$EXOBGAPC_ugL_1[catdata_all$DateTime=='2020-11-02 11:30:00'] <- NA 
catdata_all$Flag_Phyco[catdata_all$DateTime=='2020-11-02 11:30:00'] <- 1 
catdata_all$EXOBGAPC_ugL_1[catdata_all$DateTime=='2020-11-09 11:30:00'] <- NA 
catdata_all$Flag_Phyco[catdata_all$DateTime=='2020-11-09 11:30:00'] <- 1 

#phyco_rfu <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOBGAPC_RFU_1)) +
#  geom_point() 
#ggplotly(phyco_rfu)
catdata_all$EXOBGAPC_RFU_1[catdata_all$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_all$EXOBGAPC_RFU_1[catdata_all$DateTime=='2020-10-26 09:10:00'] <- NA 
catdata_all$EXOBGAPC_RFU_1[catdata_all$DateTime=='2020-11-02 11:30:00'] <- NA 
catdata_all$EXOBGAPC_RFU_1[catdata_all$DateTime=='2020-11-09 11:30:00'] <- NA 


###########################################################################################################################################################################
# fdom qaqc----
# QAQC from DWH to remove major outliers from fDOM data that are 2 sd's greater than the previous and following datapoint
# QAQC done on 2018-2020 dataset
sd_fDOM <- sd(catdata_all$EXOfDOM_QSU_1, na.rm = TRUE) #deteriming the standard deviation of fDOM data 

fDOM_pre_QAQC <- ggplot(data = catdata_all, aes(x = DateTime, y = EXOfDOM_QSU_1)) +
  geom_point()+
  ggtitle("fDOM (QSU) pre QAQC")
ggplotly(fDOM_pre_QAQC)

catdata_all <- catdata_all %>% 
  mutate(Flag_fDOM = ifelse(is.na(EXOfDOM_QSU_1), 1, 0)) %>%  #This creates Flag column for fDOM data, setting all NA's going into QAQC as 1 for missing data, and to 0 for the rest
  mutate(fDOM = lag(EXOfDOM_QSU_1, 0),
         fDOM_lag1 = lag(EXOfDOM_QSU_1, 1),
         fDOM_lead1 = lead(EXOfDOM_QSU_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOfDOM_QSU_1 = ifelse(fDOM < 0, NA, EXOfDOM_QSU_1),
         EXOfDOM_RFU_1 = ifelse(fDOM < 0, NA, EXOfDOM_RFU_1),
         Flag_fDOM = ifelse(fDOM < 0, 2, Flag_fDOM)   ) %>% #These mutates are QAQCing for negative fDOM QSU values and setting these to NA and making a flag for these. This was done outside of the 2 sd deviation rule because there were two negative points in a row and one was not removed with the follwoing if else statements. 
  mutate(EXOfDOM_QSU_1 = ifelse(
    ( abs(fDOM_lag1 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1 - fDOM) > (2*sd_fDOM)   ), NA, EXOfDOM_QSU_1
  )) %>%  #QAQC to remove outliers for QSU fDOM data 
  mutate(EXOfDOM_RFU_1 = ifelse(
    ( abs(fDOM_lag1 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1 - fDOM) > (2*sd_fDOM)   ), NA, EXOfDOM_RFU_1
  )) %>% #QAQC to remove outliers for RFU fDOM data
  mutate(Flag_fDOM = ifelse(
    ( abs(fDOM_lag1 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1 - fDOM) > (2*sd_fDOM)   ), 2, Flag_fDOM
  ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
  select(-fDOM, -fDOM_lag1, -fDOM_lead1)  #This removes the columns used to run ifelse statements since they are no longer needed. 



###########################################################################################################################################################################
# write final csv ----
# some final checking of flags
for (i in 1:nrow(catdata_flag)) {
  if(is.na(catdata_flag$Flag_Chla[i])){
    catdata_flag$Flag_Chla[i] <- 0
  }
  if(is.na(catdata_flag$Flag_Phyco[i])){
    catdata_flag$Flag_Phyco[i] <- 0
  }
}


catdata_all <- rbind()


str(catdata_all)
write.csv(catdata_all, paste0(folder, '/Catwalk_EDI_2020.csv'), row.names = FALSE)
