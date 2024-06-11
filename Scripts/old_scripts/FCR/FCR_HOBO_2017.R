### HOBO DATA from FCR catwalk
### Author, RYAN MCCLURE
### DATE: 070617
setwd("~/Dropbox/2017/CTD_2017")

library(dplyr)
library(tidyr)
library(lubridate)
library(rLakeAnalyzer)

hobo0 <- read.csv("FCR_HOBO_0.1_071317.csv", header = F)
hobo1 <- read.csv("FCR_HOBO_1_071317.csv", header = F)
hobo2 <- read.csv("FCR_HOBO_2_071317.csv", header = F)
hobo3 <- read.csv("FCR_HOBO_3_071317.csv", header = F)
hobo4 <- read.csv("FCR_HOBO_4_071317.csv", header = F)
hobo5 <- read.csv("FCR_HOBO_5_071317.csv", header = F)
hobo6 <- read.csv("FCR_HOBO_6_071317.csv", header = F)
hobo7 <- read.csv("FCR_HOBO_7_071317.csv", header = F)
hobo8 <- read.csv("FCR_HOBO_8_071317.csv", header = F)
hobo9.3 <- read.csv("FCR_HOBO_9_071317.csv", header = F)

hobo0 <- select(hobo0, V2, V3)
hobo1 <- select(hobo1, V2, V3)
hobo2 <- select(hobo2, V2, V3)
hobo3 <- select(hobo3, V2, V3)
hobo4 <- select(hobo4, V2, V3)
hobo5 <- select(hobo5, V2, V3)
hobo6 <- select(hobo6, V2, V3)
hobo7 <- select(hobo7, V2, V3)
hobo8 <- select(hobo8, V2, V3)
hobo9.3 <- select(hobo9.3, V2, V3)

hobo0 <- slice(hobo0, 3:13071)
hobo1 <- slice(hobo1, 3:13071)
hobo2 <- slice(hobo2, 3:13071)
hobo3 <- slice(hobo3, 3:13071)
hobo4 <- slice(hobo4, 3:13071)
hobo5 <- slice(hobo5, 3:13071)
hobo6 <- slice(hobo6, 3:13071)
hobo7 <- slice(hobo7, 3:13071)
hobo8 <- slice(hobo8, 3:13071)
hobo9.3 <- slice(hobo9.3, 3:13071)

hobo_2017 <- cbind(hobo0, 
                   hobo1$V3, 
                   hobo2$V3, 
                   hobo3$V3, 
                   hobo4$V3, 
                   hobo5$V3, 
                   hobo6$V3, 
                   hobo7$V3, 
                   hobo8$V3, 
                   hobo9.3$V3, 
                   deparse.level = 1)

names(hobo_2017) <- c("datetime","0", "1", "2", "3", "4", "5", "6", "7", "8", "9.3")

colnames(hobo_2017)[-1] = paste0('wtr_',colnames(hobo_2017)[-1])


write.csv(hobo_2017,"fcr_temp_wide_2017.csv", row.names = F)
hobo_2017 <- read.csv("fcr_temp_wide_2017.csv", header = T)
premix_hobo_2017 <- read.csv("fcr_temp_wide_premix1_2017.csv", header = T)
postmix_hobo_2017 <- read.csv("fcr_temp_wide_postmix1_2017.csv", header = T)

hobo_2017$datetime <-as.POSIXct(strptime(hobo_2017$datetime, "%Y-%m-%d %H:%M:%S", tz="EST"))
premix_hobo_2017$datetime <- as.POSIXct(strptime(premix_hobo_2017$datetime, "%Y-%m-%d %H:%M:%S", tz="EST"))
postmix_hobo_2017$datetime <- as.POSIXct(strptime(postmix_hobo_2017$datetime, "%Y-%m-%d %H:%M:%S", tz="EST"))





FCR_thermo_2017 <- ts.thermo.depth(hobo_2017)
FCR_meta_2017 <- ts.meta.depths(hobo_2017)



wtr.heatmap.layers(hobo_2017, main = "FCR Temperature Heat Map")
wtr.heat.map(hobo_2017, main = "FCR Temperature Heat Map")

  
  
