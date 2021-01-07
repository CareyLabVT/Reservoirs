pacman::p_load("RCurl","tidyverse","lubridate", "plotly")
source("C:Users/wwoel/Desktop/Reservoirs_2/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLCatwalk/temp_oxy_chla_qaqc.R")
folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLCatwalk"

# download most up to date catwalk data and maintenance log
download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/CAT_MaintenanceLog.txt",paste0(folder, "/CAT_MaintenanceLog_2020_test.txt"))
download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/Catwalk.csv","Catwalk_2020.csv")

# run standard qaqc
data_file <- paste0(folder, '/Catwalk_2020.csv')
maintenance_file <- paste0(folder, "/CAT_MaintenanceLog_2020.txt")
output_file <- paste0(folder, "/Catwalk_final_2020.csv")
temp_oxy_chla_qaqc(data_file, maintenance_file, output_file)

# read in qaqc function output
catdata <- read.csv(output_file) #Has not been updated
catdata$DateTime<-as.POSIXct(catdata$DateTime,format = "%Y-%m-%d %H:%M:%S")

catdata_published <- catdata[catdata$DateTime<="2019-12-31",] # coverage of the last published dataset
catdata_published <- catdata_published[!is.na(catdata_published$Flag_All),]
catdata_flag <- catdata[catdata$DateTime>"2019-12-31",]
catdata_flag <- catdata_flag[!is.na(catdata_flag$Reservoir),]

# assign all flags 0
catdata_flag$Flag_All <- 0
catdata_flag$Flag_DO_1 <- 0
catdata_flag$Flag_DO_5 <- 0
catdata_flag$Flag_DO_9 <- 0
catdata_flag$Flag_Chla <- 0
catdata_flag$Flag_Phyco <- 0
catdata_flag$Flag_TDS <- 0
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
# 3: negative values set to 0
# 5: questionable value due to potential fouling
# 6: very questionable value due to potential fouling. Values adjusted using a linear or square root function to match high-resolution CTD profiles are given in RDO_mgL_5 and RDO_sat_percent_5
# 7: missing data

###########################################################################################################################################################################
# temp qaqc
vars <- c( "ThermistorTemp_C_surface", "ThermistorTemp_C_1",       "ThermistorTemp_C_2",     "ThermistorTemp_C_3"  ,    
"ThermistorTemp_C_4" ,      "ThermistorTemp_C_5" ,      "ThermistorTemp_C_6"  ,     "ThermistorTemp_C_7"  ,     "ThermistorTemp_C_8"  ,    
 "ThermistorTemp_C_9" )

# check surface temp data
surf <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_surface)) +
  geom_point()
ggplotly(surf)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
catdata_flag$ThermistorTemp_C_surface[catdata_flag$DateTime=='2020-09-15 12:50:00'] <- NA
catdata_flag$Flag_Temp_Surf[catdata_flag$DateTime=='2020-09-15 12:50:00'] <- 1
catdata_flag$ThermistorTemp_C_surface[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA
catdata_flag$Flag_Temp_Surf[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_surface[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA
catdata_flag$Flag_Temp_Surf[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- 1

# check 1m temp data
m_1 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_1)) +
  geom_point()
ggplotly(m_1)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
catdata_flag$ThermistorTemp_C_1[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA
catdata_flag$Flag_Temp_1[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_1[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA
catdata_flag$Flag_Temp_1[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_1[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- NA
catdata_flag$Flag_Temp_1[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- 1

# check 2m temp data
m_2 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_2)) +
  geom_point()
ggplotly(m_2)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
catdata_flag$ThermistorTemp_C_2[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA
catdata_flag$Flag_Temp_2[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_2[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA
catdata_flag$Flag_Temp_2[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_2[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- NA
catdata_flag$Flag_Temp_2[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- 1

# check 3m temp data
m_3 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_3)) +
  geom_point()
ggplotly(m_3)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
catdata_flag$ThermistorTemp_C_3[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA
catdata_flag$Flag_Temp_3[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_3[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA
catdata_flag$Flag_Temp_3[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_3[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- NA
catdata_flag$Flag_Temp_3[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- 1
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
catdata_flag$ThermistorTemp_C_3[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_flag$Flag_Temp_3[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- 1

# check 4m temp data
m_4 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_4)) +
  geom_point()
ggplotly(m_4)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- 1
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- NA
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-11-24 11:30:00'] <- 1
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- 1
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-09-15 12:50:00'] <- NA 
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-09-15 12:50:00'] <- 1
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-09-15 12:40:00'] <- NA 
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-09-15 12:40:00'] <- 1

# check 5m temp data
m_5 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_5)) +
  geom_point()
ggplotly(m_5)
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

# check 6m temp data
m_6 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_6)) +
  geom_point()
ggplotly(m_6)
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

# check 7m temp data
m_7 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_7)) +
  geom_point()
ggplotly(m_7)

catdata_flag$Flag_Temp_7[catdata_flag$DateTime %in% days] <- 1
catdata_flag$ThermistorTemp_C_7[catdata_flag$DateTime %in% days] <- NA
# spot checks:
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
catdata_flag$ThermistorTemp_C_7[catdata_flag$DateTime=='2020-06-12 10:30:00'] <- NA 
catdata_flag$Flag_Temp_7[catdata_flag$DateTime=='2020-06-12 10:30:00'] <- 1

# check 8m temp data
m_8 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_8)) +
  geom_point()
ggplotly(m_8)

catdata_flag$Flag_Temp_8[catdata_flag$DateTime %in% days] <- 1
catdata_flag$ThermistorTemp_C_8[catdata_flag$DateTime %in% days] <- NA
# spot checks:
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
catdata_flag$ThermistorTemp_C_8[catdata_flag$DateTime=='2020-06-12 10:30:00'] <- NA 
catdata_flag$Flag_Temp_8[catdata_flag$DateTime=='2020-06-12 10:30:00'] <- 1

# check 9m temp data
m_9 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_9)) +
  geom_point()
ggplotly(m_9)

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
# chl and phyco qaqc
# check Chla ugL data
sd_4 <- 4*sd(catdata_flag$EXOChla_ugL_1, na.rm = TRUE)

chl_ugl <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOChla_ugL_1)) +
  geom_point() +
  geom_hline(yintercept = sd_4)
ggplotly(chl_ugl)
chl_mean <- catdata_flag %>% 
  select(DateTime, EXOChla_ugL_1) %>% 
  mutate(day = date(DateTime)) %>% 
  group_by(day) %>% 
  mutate(daily_mean = mean(EXOChla_ugL_1, na.rm = TRUE)) %>% 
  distinct(day, .keep_all = TRUE)
chl_mean <- ggplot(data = chl_mean, aes(x = day, y = daily_mean)) +
  geom_point() +
  ggtitle('figure 2')
ggplotly(chl_mean)



###########################################################################################################################################################################
# DO qaqc


###########################################################################################################################################################################
# fdom qaqc