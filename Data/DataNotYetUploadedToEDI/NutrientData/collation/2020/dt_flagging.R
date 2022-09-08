library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(flipTime)


setwd("~/Dropbox/chem_times")

chem <- read.csv("chemistry.csv")


# Flag all rows with the inexact time flag
chem$Flag_DateTime <- 1

chem <- select(chem, Reservoir, Site, DateTime, Depth_m, TN_ugL, TP_ugL, NH4_ugL, NO3NO2_ugL, SRP_ugL, DOC_mgL, DIC_mgL, DC_mgL, DN_mgL, Flag_DateTime, Flag_TN, 
               Flag_TP, Flag_NH4, Flag_NO3NO2, Flag_SRP, Flag_DOC, Flag_DIC, Flag_DC, Flag_DN)

# Split dataframe into two: chem_at is when exact times are beginning to be recorded; chem_bt is before exact times were recorded. 

chem_at <- filter(chem, DateTime >= "2018-04-01")
chem_bt <- filter(chem, DateTime < "2018-04-01")


?strptime

chem_at$DateTime <- as.POSIXct(chem_at$DateTime, tz = 'UTC', format = "%Y-%m-%d %H:%M:%S")
chem_bt$DateTime <- as.POSIXct(chem_bt$DateTime, tz = 'UTC', format = "%Y-%m-%d %H:%M:%S")


# Create hour and minute columns for easier flagging later on. 
chem_at$Hour <- hour(chem_at$DateTime)
chem_at$Minute <- minute(chem_at$DateTime)


?as.POSIXct

chem_at <- select(chem_at, Reservoir, Site, DateTime, Hour, Minute, Depth_m, TN_ugL, TP_ugL, NH4_ugL, NO3NO2_ugL, SRP_ugL, DOC_mgL, DIC_mgL, DC_mgL, DN_mgL, Flag_DateTime, Flag_TN, 
                  Flag_TP, Flag_NH4, Flag_NO3NO2, Flag_SRP, Flag_DOC, Flag_DIC, Flag_DC, Flag_DN)

filter(chem_at, Hour == "12:00:00")

# Ensure that flags at noon are 1 to begin with. 
chem_at$Flag_DateTime[chem_at$Hour == 12 & chem_at$Minute == 0] = 1

# Remove flags from rows in chem_at that are not noon, eg exact times. 
chem_at$Flag_DateTime[chem_at$Hour != 12 | chem_at$Hour == 12 & chem_at$Minute != 0] = 0

# Changing flag to 0 for points that have the time of 12:00:00 but are exactly recorded. 
chem_at$Flag_DateTime[chem_at$Reservoir == 'FCR' & chem_at$Site == 50 & chem_at$Depth_m == 1.6 & chem_at$TN_ugL == 236.1 & chem_at$TP_ugL == 21.9] <-  0
chem_at$Flag_DateTime[chem_at$Reservoir == 'BVR' & chem_at$Site == 50 & chem_at$Depth_m == 9.0 & chem_at$TN_ugL == 1990.5 & chem_at$TP_ugL == 44.0] <- 0
chem_at$Flag_DateTime[chem_at$Reservoir == 'FCR' & chem_at$Site == 50 & chem_at$Depth_m == 1.6 & chem_at$TN_ugL == 895.5 & chem_at$TP_ugL == 26.8] <- 0
chem_at$Flag_DateTime[chem_at$Reservoir == 'FCR' & chem_at$Site == 50 & chem_at$Depth_m == 9.0 & chem_at$TN_ugL == 1217.0 & chem_at$TP_ugL == 24.3] <- 0
chem_at$Flag_DateTime[chem_at$Reservoir == 'FCR' & chem_at$Site == 50 & chem_at$Depth_m == 9.0 & chem_at$TN_ugL == 837.0 & chem_at$TP_ugL == 22.7] <- 0

# Correcting datetime formatting at a specific point 
chem_at$DateTime[chem_at$Reservoir == 'BVR' & chem_at$Site == 50 & chem_at$Depth_m == 9.0 & chem_at$TN_ugL == 789.0 & chem_at$TP_ugL == 18.4] <- as.POSIXct("2019-08-01 11:35:00", tz = 'UTC', format = "%Y-%m-%d %H:%M:%S")

# Removing hour and minute columns 
chem_at <- select(chem_at, -Hour, -Minute)


chemistry <- rbind(chem_bt, chem_at)

?write_csv
?write_csv2
?write.csv

str(chemistry)



write.csv(chemistry, file = "chemistry.csv")


ct <- read.csv("Data/chemistry-8.csv")
str(ct)

