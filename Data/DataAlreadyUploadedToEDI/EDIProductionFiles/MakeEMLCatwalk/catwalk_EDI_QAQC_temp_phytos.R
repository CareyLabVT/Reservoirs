# QAQC in prep for publishing catwalk sensor string data to EDI

pacman::p_load("RCurl","tidyverse","lubridate", "plotly", "magrittr")
folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLCatwalk"
source(paste0(folder, "/temp_oxy_chla_qaqc.R"))

# download most up to date catwalk data and maintenance log
#download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/CAT_MaintenanceLog.txt",paste0(folder, "/CAT_MaintenanceLog_2020.txt"))
#download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/Catwalk.csv","Catwalk_2020.csv")

# run standard qaqc
data_file <- paste0(folder, '/Catwalk_2020.csv')
maintenance_file <- paste0(folder, "/CAT_MaintenanceLog_2020.txt")
output_file <- paste0(folder, "/Catwalk_final_2020.csv")
temp_oxy_chla_qaqc(data_file, maintenance_file, output_file)

# read in qaqc function output
catdata_flag <- read.csv(output_file) #Has not been updated
catdata_flag$DateTime<-as.POSIXct(catdata_flag$DateTime,format = "%Y-%m-%d %H:%M:%S")

#catdata_published <- catdata[catdata$DateTime<="2019-12-31",] # coverage of the last published dataset
#catdata_published <- catdata_published[!is.na(catdata_published$Flag_All),]
#catdata_flag <- catdata[catdata$DateTime>"2019-12-31",]
#catdata_flag <- catdata_flag[!is.na(catdata_flag$Reservoir),]

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

for (i in 1:nrow(catdata_flag)) {
  if(is.na(catdata_flag$Flag_Chla[i])){
    catdata_flag$Flag_Chla[i] <- 0
  }
  if(is.na(catdata_flag$Flag_Phyco[i])){
    catdata_flag$Flag_Phyco[i] <- 0
  }
}

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
# 22: value set to NA, major outlier which is more than 4 standard deviations different from previous or following datapoint
# ADD IN ABP FLAGS HERE

###########################################################################################################################################################################
# temp qaqc

#Two of the thermistors started to read higher than the one above them in fall 2020. Fixed this using a constant offset. 
#methods described in metadat

#start time for 1m is 30 Oct 2020 13:00EST
#start time for 4m is 31 Oct 2020 5:00EST
catdata_flag <- catdata_flag %>%
  mutate(Flag_Temp_1 = ifelse(DateTime >= "2020-10-30 13:00" & DateTime < "2020-12-31 23:50" & (! is.na(ThermistorTemp_C_1)) ,8, Flag_Temp_1))%>%
  mutate(Flag_Temp_4 = ifelse(DateTime >= "2020-10-31 5:00" & DateTime < "2020-12-31 23:50" &
                                (! is.na(ThermistorTemp_C_4)),8, Flag_Temp_1))%>%
  mutate(ThermistorTemp_C_11 = ifelse(DateTime >= "2020-10-30 13:00" & DateTime < "2020-12-31 23:50" &
                                        (! is.na(ThermistorTemp_C_1)), (ThermistorTemp_C_1-0.22617), ThermistorTemp_C_1 )) %>%
  mutate(ThermistorTemp_C_41 = ifelse(DateTime >= "2020-10-30 13:00" & DateTime < "2020-12-31 23:50" &
                                        (! is.na(ThermistorTemp_C_4)), (ThermistorTemp_C_4-0.18122), ThermistorTemp_C_4 )) 

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
surf <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_surface)) +
  geom_point()
ggplotly(surf)

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
catdata_flag$ThermistorTemp_C_1[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- NA
catdata_flag$Flag_Temp_1[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- 1
m_1 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_1)) +
  geom_point()
ggplotly(m_1)

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
catdata_flag$ThermistorTemp_C_2[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- NA
catdata_flag$Flag_Temp_2[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- 1
catdata_flag$ThermistorTemp_C_2[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- NA
catdata_flag$Flag_Temp_2[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- 1
m_2 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_2)) +
  geom_point()
ggplotly(m_2)

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
catdata_flag$ThermistorTemp_C_3[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- NA
catdata_flag$Flag_Temp_3[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- 1
# no maintenance recorded on this day but this was a field day, so forcing this datapoint to NA
catdata_flag$ThermistorTemp_C_3[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_flag$Flag_Temp_3[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- 1
m_3 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_3)) +
  geom_point()
ggplotly(m_3)

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
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- NA
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- 1
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- 1
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-09-15 12:50:00'] <- NA 
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-09-15 12:50:00'] <- 1
catdata_flag$ThermistorTemp_C_4[catdata_flag$DateTime=='2020-09-15 12:40:00'] <- NA 
catdata_flag$Flag_Temp_4[catdata_flag$DateTime=='2020-09-15 12:40:00'] <- 1
m_4 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_4)) +
  geom_point()
ggplotly(m_4)

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
m_5 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_5)) +
  geom_point()
ggplotly(m_5)

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
m_6 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_6)) +
  geom_point()
ggplotly(m_6)

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
m_7 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_7)) +
  geom_point()
ggplotly(m_7)

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
catdata_flag$ThermistorTemp_C_8[catdata_flag$DateTime=='2020-10-14 12:10:00'] <- NA 
catdata_flag$Flag_Temp_8[catdata_flag$DateTime=='2020-10-14 12:10:00'] <- 1
catdata_flag$ThermistorTemp_C_8[catdata_flag$DateTime=='2020-10-14 12:20:00'] <- NA 
catdata_flag$Flag_Temp_8[catdata_flag$DateTime=='2020-10-14 12:20:00'] <- 1

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
threshold <- sd_4
sd_4_phyco <- 4*sd(catdata_flag$EXOBGAPC_ugL_1, na.rm = TRUE)
threshold_phyco <- sd_4_phyco

chl_ugl <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOChla_ugL_1)) +
  geom_point() +
  geom_hline(yintercept = sd_4)
ggplotly(chl_ugl)


# QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
catdata_flag <- catdata_flag %>% 
  mutate(Chla = lag(EXOChla_ugL_1, 0),
         Chla_lag1 = lag(EXOChla_ugL_1, 1),
         Chla_lead1 = lead(EXOChla_ugL_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOChla_ugL_1 = ifelse((abs(Chla_lag1 - Chla) > (threshold))  & (abs(Chla_lead1 - Chla) > (threshold)), 
                                NA, EXOChla_ugL_1)) %>%   
  mutate(EXOChla_RFU_1 = ifelse((abs(Chla_lag1 - Chla) > (threshold))  & (abs(Chla_lead1 - Chla) > (threshold)), 
                                   NA, EXOChla_RFU_1)) %>% 
  mutate(Flag_Chla = ifelse((abs(Chla_lag1 - Chla) > (threshold))  & (abs(Chla_lead1 - Chla) > (threshold)), 
                            2, Flag_Chla)) %>% 
  select(-Chla, -Chla_lag1, -Chla_lead1)

  

# some spot checking of days with isolated weird data which occured either on maintenance days or field days
catdata_flag$EXOChla_ugL_1[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_flag$Flag_Chla[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- 1
catdata_flag$EXOChla_ugL_1[catdata_flag$DateTime=='2020-09-15 12:40:00'] <- NA 
catdata_flag$Flag_Chla[catdata_flag$DateTime=='2020-09-15 12:40:00'] <- 1
catdata_flag$EXOChla_ugL_1[catdata_flag$DateTime=='2020-09-15 12:50:00'] <- NA 
catdata_flag$Flag_Chla[catdata_flag$DateTime=='2020-09-15 12:50:00'] <- 1
catdata_flag$EXOChla_ugL_1[catdata_flag$DateTime=='2020-09-15 13:00:00'] <- NA 
catdata_flag$Flag_Chla[catdata_flag$DateTime=='2020-09-15 13:00:00'] <- 1
catdata_flag$EXOChla_ugL_1[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA 
catdata_flag$Flag_Chla[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- 1
catdata_flag$EXOChla_ugL_1[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA 
catdata_flag$Flag_Chla[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- 1
catdata_flag$EXOChla_ugL_1[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- NA 
catdata_flag$Flag_Chla[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- 1
catdata_flag$EXOChla_ugL_1[catdata_flag$DateTime=='2020-10-26 09:10:00'] <- NA 
catdata_flag$Flag_Chla[catdata_flag$DateTime=='2020-10-26 09:10:00'] <- 1

chl_mean <- catdata_flag %>% 
  select(DateTime, EXOChla_ugL_1) %>% 
  mutate(day = date(DateTime)) %>% 
  group_by(day) %>% 
  mutate(daily_mean = mean(EXOChla_ugL_1, na.rm = TRUE)) %>% 
  distinct(day, .keep_all = TRUE)
chl_mean_plot <- ggplot(data = chl_mean, aes(x = day, y = daily_mean)) +
  geom_point() 
ggplotly(chl_mean)

chl_rfu <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOChla_RFU_1)) +
  geom_point() +
  geom_hline(yintercept = sd_4)
ggplotly(chl_rfu)
catdata_flag$EXOChla_RFU_1[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_flag$EXOChla_RFU_1[catdata_flag$DateTime=='2020-09-15 12:40:00'] <- NA 
catdata_flag$EXOChla_RFU_1[catdata_flag$DateTime=='2020-09-15 12:50:00'] <- NA 
catdata_flag$EXOChla_RFU_1[catdata_flag$DateTime=='2020-09-15 13:00:00'] <- NA 
catdata_flag$EXOChla_RFU_1[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA 
catdata_flag$EXOChla_RFU_1[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA 
catdata_flag$EXOChla_RFU_1[catdata_flag$DateTime=='2020-11-24 11:40:00'] <- NA 
catdata_flag$EXOChla_RFU_1[catdata_flag$DateTime=='2020-10-26 09:10:00'] <- NA 


phyco_ugl <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOBGAPC_ugL_1)) +
  geom_point() 
ggplotly(phyco_ugl)

# QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
catdata_flag <- catdata_flag %>% 
  mutate(phyco = lag(EXOBGAPC_ugL_1, 0),
         phyco_lag1 = lag(EXOBGAPC_ugL_1, 1),
         phyco_lead1 = lead(EXOBGAPC_ugL_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOBGAPC_ugL_1 = ifelse((abs(phyco_lag1 - phyco) > (threshold_phyco))  & (abs(phyco_lead1 - phyco) > (threshold_phyco)), 
                                 NA, EXOBGAPC_ugL_1)) %>%   
  mutate(EXOBGAPC_RFU_1 = ifelse((abs(phyco_lag1 - phyco) > (threshold_phyco))  & (abs(phyco_lead1 - phyco) > (threshold_phyco)), 
                                 NA, EXOBGAPC_RFU_1)) %>% 
  mutate(Flag_Phyco = ifelse((abs(phyco_lag1 - phyco) > (threshold_phyco))  & (abs(phyco_lead1 - phyco) > (threshold_phyco)), 
                             22, Flag_Phyco)) %>%
  select(-phyco, -phyco_lag1, -phyco_lead1)

# some spot checking of days with isolated weird data which occured either on maintenance days or field days
catdata_flag$EXOBGAPC_ugL_1[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_flag$Flag_Phyco[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- 1 
catdata_flag$EXOBGAPC_ugL_1[catdata_flag$DateTime=='2020-10-26 09:10:00'] <- NA 
catdata_flag$Flag_Phyco[catdata_flag$DateTime=='2020-10-26 09:10:00'] <- 1 
catdata_flag$EXOBGAPC_ugL_1[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA 
catdata_flag$Flag_Phyco[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- 1 
catdata_flag$EXOBGAPC_ugL_1[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA 
catdata_flag$Flag_Phyco[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- 1 

phyco_rfu <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOBGAPC_RFU_1)) +
  geom_point() 
ggplotly(phyco_rfu)
catdata_flag$EXOBGAPC_RFU_1[catdata_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
catdata_flag$EXOBGAPC_RFU_1[catdata_flag$DateTime=='2020-10-26 09:10:00'] <- NA 
catdata_flag$EXOBGAPC_RFU_1[catdata_flag$DateTime=='2020-11-02 11:30:00'] <- NA 
catdata_flag$EXOBGAPC_RFU_1[catdata_flag$DateTime=='2020-11-09 11:30:00'] <- NA 




###########################################################################################################################################################################
# DO qaqc


###########################################################################################################################################################################
# fdom qaqc


###########################################################################################################################################################################
# write final csv
catdata_flag <- catdata_flag[catdata_flag$DateTime<'2021-01-01',]
catdata_flag <- catdata_flag %>% select(Reservoir, Site, DateTime, ThermistorTemp_C_surface:RDOTemp_C_9, 
                                        EXOTemp_C_1, EXOCond_uScm_1, EXOSpCond_uScm_1, EXOTDS_mgL_1, EXODOsat_percent_1, 
                                        EXODO_mgL_1, EXOChla_RFU_1, EXOChla_ugL_1, EXOBGAPC_RFU_1, EXOBGAPC_ugL_1, 
                                        EXOfDOM_RFU_1, EXOfDOM_QSU_1, EXO_pressure, EXO_depth, EXO_battery, EXO_cablepower, 
                                        EXO_wiper, RECORD, CR6_Batt_V, CR6Panel_Temp_C, 
                                        #RDO_mgL_5_adjusted, RDOsat_percent_5_adjusted,RDO_mgL_9_adjusted, RDOsat_percent_9_adjusted, 
                                        Flag_All:Flag_Temp_9)
cols <- c("Flag_All", "Flag_DO_1",  "Flag_DO_5",  "Flag_DO_9", "Flag_Chla", "Flag_Phyco", "Flag_TDS", "Flag_Temp_Surf", "Flag_Temp_1",
          "Flag_Temp_2", "Flag_Temp_3", "Flag_Temp_4", "Flag_Temp_5", "Flag_Temp_6", "Flag_Temp_7", "Flag_Temp_8","Flag_Temp_9")
catdata_flag <- catdata_flag %<>% mutate_at(cols, funs(factor(.)))
str(catdata_flag)
write.csv(catdata_flag, paste0(folder, '/Catwalk_EDI_2020.csv'), row.names = FALSE)
