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



# assign new flags for temp string, set to 0
bvrdata_clean_flag$Flag_Temp_Surf <- 0
bvrdata_clean_flag$Flag_Temp_1 <- 0
bvrdata_clean_flag$Flag_Temp_2 <- 0
bvrdata_clean_flag$Flag_Temp_3 <- 0
bvrdata_clean_flag$Flag_Temp_4 <- 0
bvrdata_clean_flag$Flag_Temp_5 <- 0
bvrdata_clean_flag$Flag_Temp_6 <- 0
bvrdata_clean_flag$Flag_Temp_7 <- 0
bvrdata_clean_flag$Flag_Temp_8 <- 0
bvrdata_clean_flag$Flag_Temp_9 <- 0



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



# check surface temp data
Temp_12.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_12.5)) +
  geom_point()
ggplotly(Temp_12.5)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
bvrdata_clean_flag$ThermistorTemp_C_surface[bvrdata_clean_flag$DateTime=='2020-09-15 12:50:00'] <- NA
bvrdata_clean_flag$Flag_Temp_Surf[bvrdata_clean_flag$DateTime=='2020-09-15 12:50:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_surface[bvrdata_clean_flag$DateTime=='2020-11-09 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_Surf[bvrdata_clean_flag$DateTime=='2020-11-09 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_surface[bvrdata_clean_flag$DateTime=='2020-11-02 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_Surf[bvrdata_clean_flag$DateTime=='2020-11-02 11:30:00'] <- 1
#surf <- ggplot(data = bvrdata_clean_flag, aes(x = DateTime, y = ThermistorTemp_C_surface)) +
#  geom_point()
#ggplotly(surf)

# check 1m temp data
Temp_11.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_11.5)) +
  geom_point()
ggplotly(Temp_11.5)

# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
bvrdata_clean_flag$ThermistorTemp_C_1[bvrdata_clean_flag$DateTime=='2020-11-09 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_1[bvrdata_clean_flag$DateTime=='2020-11-09 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_1[bvrdata_clean_flag$DateTime=='2020-11-02 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_1[bvrdata_clean_flag$DateTime=='2020-11-02 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_1[bvrdata_clean_flag$DateTime=='2020-11-24 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_1[bvrdata_clean_flag$DateTime=='2020-11-24 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_1[bvrdata_clean_flag$DateTime=='2020-11-24 11:40:00'] <- NA
bvrdata_clean_flag$Flag_Temp_1[bvrdata_clean_flag$DateTime=='2020-11-24 11:40:00'] <- 1
#m_1 <- ggplot(data = bvrdata_clean_flag, aes(x = DateTime, y = ThermistorTemp_C_1)) +
#  geom_point()
#ggplotly(m_1)

# check 2m temp data
Temp_10.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_10.5)) +
  geom_point()
ggplotly(Temp_10.5)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
bvrdata_clean_flag$ThermistorTemp_C_2[bvrdata_clean_flag$DateTime=='2020-11-09 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_2[bvrdata_clean_flag$DateTime=='2020-11-09 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_2[bvrdata_clean_flag$DateTime=='2020-11-02 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_2[bvrdata_clean_flag$DateTime=='2020-11-02 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_2[bvrdata_clean_flag$DateTime=='2020-11-24 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_2[bvrdata_clean_flag$DateTime=='2020-11-24 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_2[bvrdata_clean_flag$DateTime=='2020-11-24 11:40:00'] <- NA
bvrdata_clean_flag$Flag_Temp_2[bvrdata_clean_flag$DateTime=='2020-11-24 11:40:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_2[bvrdata_clean_flag$DateTime=='2020-08-10 12:20:00'] <- NA
bvrdata_clean_flag$Flag_Temp_2[bvrdata_clean_flag$DateTime=='2020-08-10 12:20:00'] <- 1
#m_2 <- ggplot(data = bvrdata_clean_flag, aes(x = DateTime, y = ThermistorTemp_C_2)) +
#  geom_point()
#ggplotly(m_2)

# check 3m temp data
Temp_9.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_9.5)) +
  geom_point()
Temp_9.5
ggplotly(Temp_9.5)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
bvrdata_clean_flag$ThermistorTemp_C_3[bvrdata_clean_flag$DateTime=='2020-11-09 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_3[bvrdata_clean_flag$DateTime=='2020-11-09 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_3[bvrdata_clean_flag$DateTime=='2020-11-02 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_3[bvrdata_clean_flag$DateTime=='2020-11-02 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_3[bvrdata_clean_flag$DateTime=='2020-11-24 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_3[bvrdata_clean_flag$DateTime=='2020-11-24 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_3[bvrdata_clean_flag$DateTime=='2020-11-24 11:40:00'] <- NA
bvrdata_clean_flag$Flag_Temp_3[bvrdata_clean_flag$DateTime=='2020-11-24 11:40:00'] <- 1
# no maintenance recorded on this day but this was a field day, so forcing this datapoint to NA
bvrdata_clean_flag$ThermistorTemp_C_3[bvrdata_clean_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_3[bvrdata_clean_flag$DateTime=='2020-08-10 12:20:00'] <- 1
#m_3 <- ggplot(data = bvrdata_clean_flag, aes(x = DateTime, y = ThermistorTemp_C_3)) +
#  geom_point()
#ggplotly(m_3)

# check 4m temp data
Temp_8.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_8.5)) +
  geom_point()
Temp_8.5
ggplotly(Temp_8.5)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
bvrdata_clean_flag$ThermistorTemp_C_4[bvrdata_clean_flag$DateTime=='2020-11-09 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_4[bvrdata_clean_flag$DateTime=='2020-11-09 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_4[bvrdata_clean_flag$DateTime=='2020-11-02 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_4[bvrdata_clean_flag$DateTime=='2020-11-02 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_4[bvrdata_clean_flag$DateTime=='2020-11-24 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_4[bvrdata_clean_flag$DateTime=='2020-11-24 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_4[bvrdata_clean_flag$DateTime=='2020-11-24 11:40:00'] <- NA
bvrdata_clean_flag$Flag_Temp_4[bvrdata_clean_flag$DateTime=='2020-11-24 11:40:00'] <- 1
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
bvrdata_clean_flag$ThermistorTemp_C_4[bvrdata_clean_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_4[bvrdata_clean_flag$DateTime=='2020-08-10 12:20:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_4[bvrdata_clean_flag$DateTime=='2020-09-15 12:50:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_4[bvrdata_clean_flag$DateTime=='2020-09-15 12:50:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_4[bvrdata_clean_flag$DateTime=='2020-09-15 12:40:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_4[bvrdata_clean_flag$DateTime=='2020-09-15 12:40:00'] <- 1
#m_4 <- ggplot(data = bvrdata_clean_flag, aes(x = DateTime, y = ThermistorTemp_C_4)) +
#  geom_point()
#ggplotly(m_4)

# check 5m temp data
Temp_7.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_7.5)) +
  geom_point()
Temp_7.5
ggplotly(Temp_7.5)

plot(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_7.5, type='l')
points(bvrdata_clean$DateTime, bvrdata_clean$RDOTemp_C_7.5, type='l', col="red")
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
bvrdata_clean_flag$ThermistorTemp_C_5[bvrdata_clean_flag$DateTime=='2020-11-09 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_5[bvrdata_clean_flag$DateTime=='2020-11-09 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_5[bvrdata_clean_flag$DateTime=='2020-11-09 11:40:00'] <- NA
bvrdata_clean_flag$Flag_Temp_5[bvrdata_clean_flag$DateTime=='2020-11-09 11:40:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_5[bvrdata_clean_flag$DateTime=='2020-11-09 11:50:00'] <- NA
bvrdata_clean_flag$Flag_Temp_5[bvrdata_clean_flag$DateTime=='2020-11-09 11:50:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_5[bvrdata_clean_flag$DateTime=='2020-11-02 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_5[bvrdata_clean_flag$DateTime=='2020-11-02 11:30:00'] <- 1
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
bvrdata_clean_flag$ThermistorTemp_C_5[bvrdata_clean_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_5[bvrdata_clean_flag$DateTime=='2020-08-10 12:20:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_5[bvrdata_clean_flag$DateTime=='2020-09-15 12:50:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_5[bvrdata_clean_flag$DateTime=='2020-09-15 12:50:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_5[bvrdata_clean_flag$DateTime=='2020-09-15 12:40:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_5[bvrdata_clean_flag$DateTime=='2020-09-15 12:40:00'] <- 1
#m_5 <- ggplot(data = bvrdata_clean_flag, aes(x = DateTime, y = ThermistorTemp_C_5)) +
#  geom_point()
#ggplotly(m_5)

# check 6m temp data
Temp_6.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_6.5)) +
  geom_point()
Temp_6.5
ggplotly(Temp_6.5)
# a few isolated points with weird data, all on days when the sensors string was pulled up earlier so turning them to NA and setting flag to 1
bvrdata_clean_flag$ThermistorTemp_C_6[bvrdata_clean_flag$DateTime=='2020-11-09 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_6[bvrdata_clean_flag$DateTime=='2020-11-09 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_6[bvrdata_clean_flag$DateTime=='2020-11-09 11:40:00'] <- NA
bvrdata_clean_flag$Flag_Temp_6[bvrdata_clean_flag$DateTime=='2020-11-09 11:40:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_6[bvrdata_clean_flag$DateTime=='2020-11-09 11:50:00'] <- NA
bvrdata_clean_flag$Flag_Temp_6[bvrdata_clean_flag$DateTime=='2020-11-09 11:50:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_6[bvrdata_clean_flag$DateTime=='2020-11-02 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_6[bvrdata_clean_flag$DateTime=='2020-11-02 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_6[bvrdata_clean_flag$DateTime=='2020-11-24 11:30:00'] <- NA
bvrdata_clean_flag$Flag_Temp_6[bvrdata_clean_flag$DateTime=='2020-11-24 11:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_6[bvrdata_clean_flag$DateTime=='2020-11-24 11:40:00'] <- NA
bvrdata_clean_flag$Flag_Temp_6[bvrdata_clean_flag$DateTime=='2020-11-24 11:40:00'] <- 1
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
bvrdata_clean_flag$ThermistorTemp_C_6[bvrdata_clean_flag$DateTime=='2020-08-10 12:20:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_6[bvrdata_clean_flag$DateTime=='2020-08-10 12:20:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_6[bvrdata_clean_flag$DateTime=='2020-08-10 12:30:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_6[bvrdata_clean_flag$DateTime=='2020-08-10 12:30:00'] <- 1


days <- c('2020-07-13 10:50:00', '2020-07-13 11:00:00', '2020-07-13 11:10:00', '2020-07-20 11:00:00', '2020-07-20 11:10:00', '2020-07-27 11:50:00', 
          '2020-07-27 12:00:00', '2020-08-03 12:20:00', '2020-08-03 12:30:00', '2020-08-07 12:30:00', '2020-08-07 12:40:00', '2020-08-07 12:50:00', 
          '2020-08-10 12:20:00', '2020-08-10 12:30:00', '2020-08-10 12:40:00', '2020-08-17 15:20:00', '2020-08-17 15:30:00', '2020-08-17 15:40:00', 
          '2020-08-24 11:10:00', '2020-08-24 11:20:00', '2020-09-02 11:40:00', '2020-09-02 11:50:00', '2020-09-02 12:00:00', '2020-09-11 10:00:00', 
          '2020-09-15 10:10:00', '2020-09-15 13:00:00', '2020-09-15 13:10:00', '2020-09-15 13:20:00', '2020-09-15 12:40:00', '2020-09-15 12:50:00', 
          '2020-09-19 13:10:00', '2020-11-02 11:30:00', '2020-11-09 11:30:00', '2020-11-09 11:40:00', '2020-11-09 11:50:00', '2020-11-24 11:30:00', 
          '2020-11-24 11:40:00')
days <- as.POSIXct(days,format = "%Y-%m-%d %H:%M:%S")
bvrdata_clean_flag$Flag_Temp_6[bvrdata_clean_flag$DateTime %in% days] <- 1
bvrdata_clean_flag$ThermistorTemp_C_6[bvrdata_clean_flag$DateTime %in% days] <- NA
#m_6 <- ggplot(data = bvrdata_clean_flag, aes(x = DateTime, y = ThermistorTemp_C_6)) +
#  geom_point()
#ggplotly(m_6)

# check 7m temp data
Temp_5.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_5.5)) +
  geom_point()
Temp_5.5
ggplotly(Temp_5.5)

bvrdata_clean_flag$Flag_Temp_7[bvrdata_clean_flag$DateTime %in% days] <- 1
bvrdata_clean_flag$ThermistorTemp_C_7[bvrdata_clean_flag$DateTime %in% days] <- NA
# spot checks:
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
bvrdata_clean_flag$ThermistorTemp_C_7[bvrdata_clean_flag$DateTime=='2020-06-12 10:30:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_7[bvrdata_clean_flag$DateTime=='2020-06-12 10:30:00'] <- 1
#m_7 <- ggplot(data = bvrdata_clean_flag, aes(x = DateTime, y = ThermistorTemp_C_7)) +
#  geom_point()
#ggplotly(m_7)

# check 8m temp data
Temp_4.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_4.5)) +
  geom_point()
Temp_4.5
ggplotly(Temp_4.5)

bvrdata_clean_flag$Flag_Temp_8[bvrdata_clean_flag$DateTime %in% days] <- 1
bvrdata_clean_flag$ThermistorTemp_C_8[bvrdata_clean_flag$DateTime %in% days] <- NA
# spot checks:
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
bvrdata_clean_flag$ThermistorTemp_C_8[bvrdata_clean_flag$DateTime=='2020-06-12 10:30:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_8[bvrdata_clean_flag$DateTime=='2020-06-12 10:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_8[bvrdata_clean_flag$DateTime=='2020-10-14 12:10:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_8[bvrdata_clean_flag$DateTime=='2020-10-14 12:10:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_8[bvrdata_clean_flag$DateTime=='2020-10-14 12:20:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_8[bvrdata_clean_flag$DateTime=='2020-10-14 12:20:00'] <- 1

# check 9m temp data
Temp_3.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_3.5)) +
  geom_point()
Temp_3.5
ggplotly(Temp_3.5)

# check 9m temp data
Temp_2.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_2.5)) +
  geom_point()
Temp_2.5
ggplotly(Temp_2.5)

# check 9m temp data
Temp_1.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_1.5)) +
  geom_point()
Temp_1.5
ggplotly(Temp_1.5)

# check 9m temp data
Temp_0.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = ThermistorTemp_C_0.5)) +
  geom_point()
Temp_0.5
ggplotly(Temp_0.5)

bvrdata_clean_flag$Flag_Temp_9[bvrdata_clean_flag$DateTime %in% days] <- 1
bvrdata_clean_flag$ThermistorTemp_C_9[bvrdata_clean_flag$DateTime %in% days] <- NA
# spot checks:
# no maintenance on this day but this datapoint is way above any others and this was a field day, so forcing this datapoint to NA
bvrdata_clean_flag$ThermistorTemp_C_9[bvrdata_clean_flag$DateTime=='2020-06-12 10:30:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_9[bvrdata_clean_flag$DateTime=='2020-06-12 10:30:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_9[bvrdata_clean_flag$DateTime=='2020-06-12 10:40:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_9[bvrdata_clean_flag$DateTime=='2020-06-12 10:40:00'] <- 1
bvrdata_clean_flag$ThermistorTemp_C_9[bvrdata_clean_flag$DateTime=='2020-09-30 10:30:00'] <- NA 
bvrdata_clean_flag$Flag_Temp_9[bvrdata_clean_flag$DateTime=='2020-09-30 10:30:00'] <- 1


###Put all the points on the same plot using base R
plot(bvrdata_clean$DateTime,bvrdata_clean$ThermistorTemp_C_10.5, main="Water Temp", xlab="Time", ylab="degrees C", type='l', col="black", lwd=1.5, ylim=c(0,40))
#points(bvrdata_clean$DateTime,bvrdata_clean$wtr_1, col="firebrick4", type='l', lwd=1.5)
#points(bvrdata_clean$DateTime, bvrdata_clean$wtr_2, col="firebrick1", type='l', lwd=1.5)
#points(bvrdata_clean$DateTime, bvrdata_clean$wtr_3, col="DarkOrange1", type='l', lwd=1.5)
points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_9.5, col="gold", type='l', lwd=1.5)
points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_8.5, col="greenyellow", type='l', lwd=1.5)
points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_7.5, col="medium sea green", type='l', lwd=1.5)
points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_6.5, col="sea green", type='l', lwd=1.5)
points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_5.5, col="DeepSkyBlue4", type='l', lwd=1.5)
points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_4.5, col="blue2", type='l', lwd=1.5)
points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_3.5, col="blue4", type='l', lwd=1.5)
points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_2.5, col="darkslateblue", type='l', lwd=1.5)
points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_1.5, col="magenta2", type='l', lwd=1.5)
points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_0.5, col="darkmagenta", type='l', lwd=1.5)

###########################################################################################################################################################################
# DO qaqc ----

#Let's see what everything looks like after the first round of QAQC 
DO_7.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDO_mgL_7.5)) +
  geom_point()
DO_7.5
ggplotly(DO_7.5)
DOsat_7.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDOsat_percent_7.5)) +
  geom_point()
DOsat_7.5
ggplotly(DOsat_7.5)

DO_0.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDO_mgL_0.5)) +
  geom_point()
DO_0.5
ggplotly(DO_0.5)
DOsat_0.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDOsat_percent_7.5)) +
  geom_point()
DOsat_0.5
ggplotly(DOsat_0.5)

EXODO_1.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXODO_mgL_1.5)) +
  geom_point()
EXODO_1.5
ggplotly(EXODO_1.5)
EXODOsat_1.5 <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXODOsat_percent_1.5)) +
  geom_point()
EXODOsat_1.5
ggplotly(EXODOsat_1.5)


##read in ctd values to see how they compare to sensor values
ctd=read.csv("ctd_2018-2020_flagged.csv")
ctd$Date<-as.POSIXct(ctd$Date,format = "%Y-%m-%d %H:%M:%S")

#read in YSI to check the DO 
ysi=read.csv("YSI_PAR_profiles_2013-2020.csv")
ysi$DateTime<-as.POSIXct(ysi$DateTime,format = "%Y-%m-%d %H:%M:%S")

ctd_0.5=ctd%>%
  filter(Reservoir=="BVR")%>%
  filter(Date>"2020-01-01 00:00:00")%>%
  select(Date, Depth_m, Temp_C,DO_mgL,DO_pSat)%>%
  filter(Depth_m>10)%>%
  group_by(Date)%>%
  summarise(mean_DO=mean(DO_mgL))

ysi_0.5=ysi%>%
  filter(Reservoir=="BVR")%>%
  filter(DateTime>"2020-01-01 00:00:00")%>%
  select(DateTime, Depth_m, DO_mgL)%>%
  filter(Depth_m>10)%>%
  group_by(DateTime)%>%
  summarise(mean_DO=mean(DO_mgL))

plot(bvrdata_clean$DateTime, bvrdata_clean$RDO_mgL_0.5, type='l')
points(ctd_0.5$Date, ctd_0.5$mean_DO, type='p', col= "red", cex= 1.0)
points(ysi_0.5$DateTime, ysi_0.5$mean_DO, type = "p", col = "purple")
  

ctd_1.5=ctd%>%
  filter(Reservoir=="BVR")%>%
  filter(Date>"2020-01-01 00:00:00")%>%
  select(Date, Depth_m, Temp_C,DO_mgL,DO_pSat)%>%
  filter(Depth_m>1.48 & Depth_m<1.52)%>%
  group_by(Date)%>%
  summarise(mean_DO=mean(DO_mgL))

ysi_1.5=ysi%>%
  filter(Reservoir=="BVR")%>%
  filter(DateTime>"2020-06-22 00:00:00")%>%
  select(DateTime, Depth_m, DO_mgL)%>%
  filter(Depth_m>=1 & Depth_m<=2)%>%
  mutate(day = floor_date(DateTime, "day")) %>%
  group_by(day) %>%
  summarize(avg = mean(DO_mgL))

plot(bvrdata_clean$DateTime, bvrdata_clean$EXODO_mgL_1.5, type='l', ylim= c(3,13))
points(ctd_1.5$Date, ctd_1.5$mean_DO, type='p', col= "red",pch=16, cex= 1.0)
points(ysi_1.5$day, ysi_1.5$avg, type ='p', col="darkorange", pch=16,cex=1.0 )

ctd_7.5=ctd%>%
  filter(Reservoir=="BVR")%>%
  filter(Date>"2020-01-01 00:00:00")%>%
  select(Date, Depth_m, Temp_C,DO_mgL,DO_pSat)%>%
  filter(Depth_m>3.9 & Depth_m<5.1)%>%
  mutate(day = floor_date(Date, "day")) %>%
  group_by(day) %>%
  summarize(avg = mean(DO_mgL))

ysi_7.5=ysi%>%
  filter(Reservoir=="BVR")%>%
  filter(DateTime>"2020-06-22 00:00:00")%>%
  select(DateTime, Depth_m, DO_mgL)%>%
  filter(Depth_m>=4 & Depth_m<=5)%>%
  mutate(day = floor_date(DateTime, "day")) %>%
  group_by(day) %>%
  summarize(avg = mean(DO_mgL))



plot(bvrdata_clean$DateTime, bvrdata_clean$RDO_mgL_7.5, type='l')
points(ctd_7.5$day, ctd_7.5$avg, type='p', col= "red",pch=16, cex= 1.0)
points(ysi_7.5$day, ysi_7.5$avg, type ='p', col="darkorange", pch=16,cex=1.0 )
  

#fixed in first qaqc script
# now fix the negative DO values


#Deal with when the sensors were up
maint = read.csv(paste0(folder, "/CAT_MaintenanceLog_2020.txt"))
maint = maint[!grepl("EXO",maint$parameter),] #creating file "maint" with all sensor string maintenance
maint = maint%>%
  filter(!colnumber %in% c(" c(24:26)"," 40"," 41"))
clean_start<-as.POSIXct(maint$TIMESTAMP_start, tz = "EST")
clean_end <- as.POSIXct(maint$TIMESTAMP_end, tz = "EST")

ADJ_PERIOD = 2*60*60 #amount of time to stabilization after cleaning in seconds

for (i in 1:length(clean_start)){ #Set all data during cleaning and for ADJ_PERIOD after to NA
  bvrdata_clean_flag$RDO_mgL_5[bvrdata_clean_flag$DateTime>clean_start[i]&bvrdata_clean_flag$DateTime<(clean_end[i]+ADJ_PERIOD)] <- NA
  bvrdata_clean_flag$RDO_mgL_9[bvrdata_clean_flag$DateTime>clean_start[i]&bvrdata_clean_flag$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
  bvrdata_clean_flag$RDOsat_percent_5[bvrdata_clean_flag$DateTime>clean_start[i]&bvrdata_clean_flag$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
  bvrdata_clean_flag$RDOsat_percent_9[bvrdata_clean_flag$DateTime>clean_start[i]&bvrdata_clean_flag$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
  bvrdata_clean_flag$Flag_DO_5[bvrdata_clean_flag$DateTime>clean_start[i]&bvrdata_clean_flag$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
  bvrdata_clean_flag$Flag_DO_9[bvrdata_clean_flag$DateTime>clean_start[i]&bvrdata_clean_flag$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
}

################
#Creating a new flag "6" started in 2019: "very questionable value due to potential fouling. Values adjusted using a linear or square root function to match high-resolution CTD profiles are given in RDO_mgL_5 and RDO_sat_percent_5"
#Creating a new flag "5" which means that the values are very questionable due to fouling but not adjusted (starting in 2019)
################



bvrdata_clean_flag <- bvrdata_clean_flag%>%
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
bvrdata_clean_flag <- bvrdata_clean_flag[bvrdata_clean_flag$DateTime<'2021-01-01',]
bvrdata_clean_flag$RDO_mgL_5_adjusted <- bvrdata_clean_flag$RDO_mgL_5
bvrdata_clean_flag$RDOsat_percent_5_adjusted <- bvrdata_clean_flag$RDOsat_percent_5
bvrdata_clean_flag <- bvrdata_clean_flag %>% select(Reservoir, Site, DateTime, ThermistorTemp_C_surface:ThermistorTemp_C_9,
                                        RDO_mgL_5, RDOsat_percent_5, RDO_mgL_5_adjusted, RDOsat_percent_5_adjusted,
                                        RDOTemp_C_5, RDO_mgL_9, RDOsat_percent_9, RDO_mgL_9_adjusted, RDOsat_percent_9_adjusted, RDOTemp_C_9, 
                                        EXOTemp_C_1, EXOCond_uScm_1, EXOSpCond_uScm_1, EXOTDS_mgL_1, EXODOsat_percent_1, 
                                        EXODO_mgL_1, EXOChla_RFU_1, EXOChla_ugL_1, EXOBGAPC_RFU_1, EXOBGAPC_ugL_1, 
                                        EXOfDOM_RFU_1, EXOfDOM_QSU_1, EXO_pressure, EXO_depth, EXO_battery, EXO_cablepower, 
                                        EXO_wiper, Lvl_psi_9, LvlTemp_C_9, RECORD, CR6_Batt_V, CR6Panel_Temp_C, 
                                        RDO_mgL_5_adjusted, RDOsat_percent_5_adjusted,RDO_mgL_9_adjusted, RDOsat_percent_9_adjusted, 
                                        Flag_All:Flag_Temp_9)

# read in catwalk data already published on EDI
bvrdata_clean_pub <- read_csv(paste0(folder, '/bvrdata_clean_EDI_2019_downloaded12Jan21.csv'), col_types = cols(.default = "d", 
                                                                                                    Reservoir = "c",
                                                                                                    DateTime = "T"))
bvrdata_clean_pub$DateTime<-as.POSIXct(bvrdata_clean_pub$DateTime,format = "%Y-%m-%d %H:%M:%S")
bvrdata_clean_pub <- bvrdata_clean_pub[bvrdata_clean_pub$DateTime<'2020-01-01',]
# add the two pressure columns
bvrdata_clean_pub$Lvl_psi_9 <- NA
bvrdata_clean_pub$LvlTemp_C_9 <- NA

# assign new flags for temp string, set to 0
bvrdata_clean_pub$Flag_Temp_Surf <- 0
bvrdata_clean_pub$Flag_Temp_1 <- 0
bvrdata_clean_pub$Flag_Temp_2 <- 0
bvrdata_clean_pub$Flag_Temp_3 <- 0
bvrdata_clean_pub$Flag_Temp_4 <- 0
bvrdata_clean_pub$Flag_Temp_5 <- 0
bvrdata_clean_pub$Flag_Temp_6 <- 0
bvrdata_clean_pub$Flag_Temp_7 <- 0
bvrdata_clean_pub$Flag_Temp_8 <- 0
bvrdata_clean_pub$Flag_Temp_9 <- 0

bvrdata_clean_all <- rbind(bvrdata_clean_pub, bvrdata_clean_flag)

###########################################################################################################################################################################
# chl and phyco qaqc ----
# perform qaqc on the entire dataset for chl and phyco


# assign standard deviation thresholds
sd_4 <- 4*sd(bvrdata_clean$EXOChla_ugL_1.5, na.rm = TRUE)
threshold <- sd_4
sd_4_phyco <- 4*sd(bvrdata_clean$EXOBGAPC_ugL_1.5, na.rm = TRUE)
threshold_phyco <- sd_4_phyco

chl_ugl <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOChla_ugL_1.5)) +
  geom_point() 
  geom_hline(yintercept = sd_4)
chl_ugl
ggplotly(chl_ugl)

# QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
bvrdata_clean <- bvrdata_clean %>% 
  mutate(Chla = lag(EXOChla_ugL_1.5, 0),
         Chla_lag1.5 = lag(EXOChla_ugL_1.5, 1),
         Chla_lead1.5 = lead(EXOChla_ugL_1.5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(Flag_Chla = ifelse(Chla < 0 & !is.na(Chla), 3, Flag_Chla)) %>% 
  mutate(Flag_Chla = ifelse(Chla < 0 & !is.na(Chla), 3, Flag_Chla)) %>% 
  mutate(EXOChla_ugL_1.5 = ifelse(Chla < 0 & !is.na(Chla), 0, EXOChla_ugL_1.5)) %>% 
  mutate(EXOChla_RFU_1.5 = ifelse(Chla < 0 & !is.na(Chla), 0, EXOChla_RFU_1.5)) %>% 
  mutate(EXOChla_ugL_1.5 = ifelse((abs(Chla_lag1.5 - Chla) > (threshold))  & (abs(Chla_lead1.5 - Chla) > (threshold) & !is.na(Chla)), 
                                NA, EXOChla_ugL_1.5)) %>%   
  mutate(EXOChla_RFU_1.5 = ifelse((abs(Chla_lag1.5 - Chla) > (threshold))  & (abs(Chla_lead1.5 - Chla) > (threshold) & !is.na(Chla)), 
                                NA, EXOChla_RFU_1.5)) %>% 
  mutate(Flag_Chla = ifelse((abs(Chla_lag1.5 - Chla) > (threshold))  & (abs(Chla_lead1.5 - Chla) > (threshold)) & !is.na(Chla), 
                            2, Flag_Chla)) %>% 
  select(-Chla, -Chla_lag1.5, -Chla_lead1.5)

#check the raw chal to the cleaned up one

plot(bvrdata_raw$TIMESTAMP, bvrdata_raw$Chla_1, type ="l")
points(bvrdata_clean$DateTime, bvrdata_clean$EXOChla_ugL_1.5, type= "l", col="red")

# some spot checking of days with isolated weird data which occured either on maintenance days or field days

#chl_mean <- bvrdata_clean_flag %>% 
#  select(DateTime, EXOChla_ugL_1) %>% 
#  mutate(day = date(DateTime)) %>% 
#  group_by(day) %>% 
#  mutate(daily_mean = mean(EXOChla_ugL_1, na.rm = TRUE)) %>% 
#  distinct(day, .keep_all = TRUE)
#chl_mean_plot <- ggplot(data = chl_mean, aes(x = day, y = daily_mean)) +
#  geom_point() 
#ggplotly(chl_mean)

chl_rfu <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOChla_RFU_1.5)) +
  geom_point() 
#  geom_hline(yintercept = sd_4)
chl_rfu
#ggplotly(chl_rfu)

plot(bvrdata_raw$TIMESTAMP, bvrdata_raw$Chla_RFU_1, type ="l")
points(bvrdata_clean$DateTime, bvrdata_clean$EXOChla_RFU_1.5, type= "l", col="red")


phyco_ugl <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOBGAPC_ugL_1.5)) +
  geom_point() 
phyco_ugl
#ggplotly(phyco_ugl)

# QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
bvrdata_clean <- bvrdata_clean %>% 
  mutate(phyco = lag(EXOBGAPC_ugL_1.5, 0),
         phyco_lag1.5 = lag(EXOBGAPC_ugL_1.5, 1),
         phyco_lead1.5 = lead(EXOBGAPC_ugL_1.5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(Flag_Phyco = ifelse(phyco < 0 & !is.na(phyco), 3, Flag_Phyco)) %>% 
  mutate(Flag_Phyco = ifelse(phyco < 0 & !is.na(phyco), 3, Flag_Phyco)) %>% 
  mutate(EXOBGAPC_RFU_1.5 = ifelse(phyco < 0 & !is.na(phyco), 0, EXOBGAPC_RFU_1.5)) %>% 
  mutate(EXOBGAPC_ugL_1.5 = ifelse(phyco < 0 & !is.na(phyco), 0, EXOBGAPC_ugL_1.5)) %>% 
  mutate(EXOBGAPC_ugL_1.5 = ifelse((abs(phyco_lag1.5 - phyco) > (threshold_phyco))  & (abs(phyco_lead1.5 - phyco) > (threshold_phyco) & !is.na(phyco)), 
                                 NA, EXOBGAPC_ugL_1.5)) %>%   
  mutate(EXOBGAPC_RFU_1.5 = ifelse((abs(phyco_lag1.5 - phyco) > (threshold_phyco))  & (abs(phyco_lead1.5 - phyco) > (threshold_phyco) & !is.na(phyco)), 
                                 NA, EXOBGAPC_RFU_1.5)) %>% 
  mutate(Flag_Phyco = ifelse((abs(phyco_lag1.5 - phyco) > (threshold_phyco))  & (abs(phyco_lead1.5 - phyco) > (threshold_phyco) & !is.na(phyco)), 
                             2, Flag_Phyco)) %>%
  select(-phyco, -phyco_lag1.5, -phyco_lead1.5)

plot(bvrdata_raw$TIMESTAMP, bvrdata_raw$BGAPC_1, type ="l")
points(bvrdata_clean$DateTime, bvrdata_clean$EXOBGAPC_ugL_1.5, type= "l", col="red")

# some spot checking of days with isolated weird data which occured either on maintenance days or field days
bvrdata_clean_all$EXOBGAPC_ugL_1[bvrdata_clean_all$DateTime=='2020-08-10 12:20:00'] <- NA 
bvrdata_clean_all$Flag_Phyco[bvrdata_clean_all$DateTime=='2020-08-10 12:20:00'] <- 1 
bvrdata_clean_all$EXOBGAPC_ugL_1[bvrdata_clean_all$DateTime=='2020-10-26 09:10:00'] <- NA 
bvrdata_clean_all$Flag_Phyco[bvrdata_clean_all$DateTime=='2020-10-26 09:10:00'] <- 1 
bvrdata_clean_all$EXOBGAPC_ugL_1[bvrdata_clean_all$DateTime=='2020-11-02 11:30:00'] <- NA 
bvrdata_clean_all$Flag_Phyco[bvrdata_clean_all$DateTime=='2020-11-02 11:30:00'] <- 1 
bvrdata_clean_all$EXOBGAPC_ugL_1[bvrdata_clean_all$DateTime=='2020-11-09 11:30:00'] <- NA 
bvrdata_clean_all$Flag_Phyco[bvrdata_clean_all$DateTime=='2020-11-09 11:30:00'] <- 1 

#phyco_rfu <- ggplot(data = bvrdata_clean_flag, aes(x = DateTime, y = EXOBGAPC_RFU_1)) +
#  geom_point() 
#ggplotly(phyco_rfu)
bvrdata_clean_all$EXOBGAPC_RFU_1[bvrdata_clean_all$DateTime=='2020-08-10 12:20:00'] <- NA 
bvrdata_clean_all$EXOBGAPC_RFU_1[bvrdata_clean_all$DateTime=='2020-10-26 09:10:00'] <- NA 
bvrdata_clean_all$EXOBGAPC_RFU_1[bvrdata_clean_all$DateTime=='2020-11-02 11:30:00'] <- NA 
bvrdata_clean_all$EXOBGAPC_RFU_1[bvrdata_clean_all$DateTime=='2020-11-09 11:30:00'] <- NA 


###########################################################################################################################################################################
# fdom qaqc----
# QAQC from DWH to remove major outliers from fDOM data that are 2 sd's greater than the previous and following datapoint
# QAQC done on 2018-2020 dataset
sd_fDOM <- sd(bvrdata_clean$EXOfDOM_QSU_1.5, na.rm = TRUE) #deteriming the standard deviation of fDOM data 

#fDOM_pre_QAQC <- ggplot(data = bvrdata_clean_all, aes(x = DateTime, y = EXOfDOM_QSU_1)) +
#  geom_point()+
#  ggtitle("fDOM (QSU) pre QAQC")
#ggplotly(fDOM_pre_QAQC)

bvrdata_clean <- bvrdata_clean %>% 
  mutate(Flag_fDOM = ifelse(is.na(EXOfDOM_QSU_1.5), 1, 0)) %>%  #This creates Flag column for fDOM data, setting all NA's going into QAQC as 1 for missing data, and to 0 for the rest
  mutate(fDOM = lag(EXOfDOM_QSU_1.5, 0),
         fDOM_lag1.5 = lag(EXOfDOM_QSU_1.5, 1),
         fDOM_lead1.5 = lead(EXOfDOM_QSU_1.5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(Flag_fDOM = ifelse(fDOM < 0 & !is.na(fDOM), 3, Flag_fDOM),
         EXOfDOM_QSU_1.5 = ifelse(fDOM < 0 & !is.na(fDOM), NA, EXOfDOM_QSU_1.5),
         EXOfDOM_RFU_1.5 = ifelse(fDOM < 0 & !is.na(fDOM), NA, EXOfDOM_RFU_1.5),
         Flag_fDOM = ifelse(fDOM < 0, 2, Flag_fDOM)   ) %>% #These mutates are QAQCing for negative fDOM QSU values and setting these to NA and making a flag for these. This was done outside of the 2 sd deviation rule because there were two negative points in a row and one was not removed with the follwoing if else statements. 
  mutate(EXOfDOM_QSU_1.5 = ifelse(
    ( abs(fDOM_lag1.5 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1.5 - fDOM) > (2*sd_fDOM)  & !is.na(fDOM) ), NA, EXOfDOM_QSU_1.5
  )) %>%  #QAQC to remove outliers for QSU fDOM data 
  mutate(EXOfDOM_RFU_1.5 = ifelse(
    ( abs(fDOM_lag1.5 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1.5 - fDOM) > (2*sd_fDOM)  & !is.na(fDOM)  ), NA, EXOfDOM_RFU_1.5
  )) %>% #QAQC to remove outliers for RFU fDOM data
  mutate(Flag_fDOM = ifelse(
    ( abs(fDOM_lag1.5 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1.5 - fDOM) > (2*sd_fDOM)  & !is.na(fDOM)  ), 2, Flag_fDOM
  ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
  select(-fDOM, -fDOM_lag1.5, -fDOM_lead1.5)  #This removes the columns used to run ifelse statements since they are no longer needed. 

plot(bvrdata_raw$TIMESTAMP, bvrdata_raw$fDOM_QSU_1, type ="l")
points(bvrdata_clean$DateTime, bvrdata_clean$EXOfDOM_QSU_1.5, type= "l", col="red")

fDOM_QSU <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOfDOM_QSU_1.5)) +
    geom_point()
  #  ggtitle("fDOM (QSU) pre QAQC")
fDOM_QSU
  #ggplotly(fDOM_pre_QAQC)

plot(bvrdata_raw$TIMESTAMP, bvrdata_raw$fDOM_RFU_1, type ="l")
points(bvrdata_clean$DateTime, bvrdata_clean$EXOfDOM_RFU_1.5, type= "l", col="red")

fDOM_RFU <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOfDOM_RFU_1.5)) +
  geom_point()
#  ggtitle("fDOM (QSU) pre QAQC")
fDOM_RFU
#ggplotly(fDOM_pre_QAQC)

#conductivity check

cond <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOCond_uScm_1.5)) +
  geom_point()
cond
ggplotly(cond)

cond_sp <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOSpCond_uScm_1.5)) +
  geom_point()
cond_sp
ggplotly(cond_sp)


tds <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOTDS_mgL_1.5)) +
  geom_point()
tds
ggplotly(tds)
    
#Pressure graphs
Exo_pressure <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXO_pressure_1.5)) +
  geom_point()
Exo_pressure
ggplotly(Exo_pressure)

Exo_depth <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXO_depth)) +
  geom_point()
Exo_depth
ggplotly(Exo_depth)

Lvl_pressure <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = Lvl_psi_0.5)) +
  geom_point()
Lvl_pressure
ggplotly(Lvl_pressure)

temp_pressure <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = LvlTemp_C_0.5)) +
  geom_point()
temp_pressure
ggplotly(temp_pressure)

temp_do <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDOTemp_C_0.5)) +
  geom_point()
temp_do
ggplotly(temp_do)

plot(bvrdata_clean$DateTime, bvrdata_clean$LvlTemp_C_0.5, type="l")
points(bvrdata_clean$DateTime, bvrdata_clean$ThermistorTemp_C_0.5, type= "l", col="red")
points(bvrdata_clean$DateTime, bvrdata_clean$RDOTemp_C_0.5, type= "l", col="darkorange")

Exo_temp <- ggplot(data = bvrdata_clean, aes(x = DateTime, y = EXOTemp_C_1.5)) +
  geom_point()
Exo_temp
ggplotly(Exo_temp)

DO_temp_7.5<- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RDOTemp_C_7.5)) +
  geom_point()
DO_temp_7.5
ggplotly(DO_temp_7.5)

plot(bvrdata_clean$DateTime, bvrdata_clean$CR6_Batt_V, type="l", ylim=c(10,16))
points(bvrdata_clean$DateTime, bvrdata_clean$EXO_cablepower, type="l", col="darkorange")

plot(bvrdata_clean$DateTime, bvrdata_clean$EXO_battery, type= "l")
plot(bvrdata_clean$DateTime, bvrdata_clean$EXO_wiper, type="l", col="black")

temp_panel<- ggplot(data = bvrdata_clean, aes(x = DateTime, y = CR6Panel_Temp_C)) +
  geom_point()
temp_panel
ggplotly(temp_panel)

record<- ggplot(data = bvrdata_clean, aes(x = DateTime, y = RECORD)) +
  geom_point()
record
ggplotly(record)


####Records power and other variables to just check for anything funny

###########################################################################################################################################################################
# write final csv ----
# some final checking of flags
for (i in 1:nrow(bvrdata_clean_all)) {
  if(is.na(bvrdata_clean_all$Flag_fDOM[i])){
    bvrdata_clean_all$Flag_fDOM[i] <- 0
  }
}
str(bvrdata_clean_all)

# rearrange the cols
bvrdata_clean_all <- bvrdata_clean_all %>% 
  select(Reservoir, Site, DateTime, ThermistorTemp_C_surface:ThermistorTemp_C_9,
         RDO_mgL_5, RDOsat_percent_5, RDO_mgL_5_adjusted, RDOsat_percent_5_adjusted,
         RDOTemp_C_5, RDO_mgL_9, RDOsat_percent_9, RDO_mgL_9_adjusted, RDOsat_percent_9_adjusted, RDOTemp_C_9, 
         EXOTemp_C_1, EXOCond_uScm_1, EXOSpCond_uScm_1, EXOTDS_mgL_1, EXODOsat_percent_1, 
         EXODO_mgL_1, EXOChla_RFU_1, EXOChla_ugL_1, EXOBGAPC_RFU_1, EXOBGAPC_ugL_1, 
         EXOfDOM_RFU_1, EXOfDOM_QSU_1, EXO_pressure, EXO_depth, EXO_battery, EXO_cablepower, 
         EXO_wiper, Lvl_psi_9, LvlTemp_C_9, RECORD, CR6_Batt_V, CR6Panel_Temp_C, 
         Flag_All:Flag_TDS, Flag_fDOM, Flag_Temp_Surf:Flag_Temp_9)


write.csv(bvrdata_clean_all, paste0(folder, '/Catwalk_EDI_2020.csv'), row.names = FALSE)
