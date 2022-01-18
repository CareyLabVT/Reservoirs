library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)


###########################################################################

setwd("~/Dropbox/chlorophyll_processing/data")

chla_12Aug21 <- read.csv("12Aug21_chla_data.csv")

chla_12Aug21$Flag_Chla <- 0
chla_12Aug21$Flag_Pheo <- 0

flag_pheo_12Aug21 <- c("F_15Sep20_1.6", "F_3Aug20_1.6", "B_15Sep20_1.6_dup",
                       "F_03Aug20_1.6_dup", "F_30Sep20_1.6_dup", "F_10Aug20_1.6_dup", 
                       "F_30Sep20_1.6"
)
for(value in flag_pheo_12Aug21){
  chla_12Aug21$Flag_Pheo[chla_12Aug21$sample_ID == value] <- 4
}


chla_12Aug21 <- select(chla_12Aug21, -sample_ID)



###########################################################################


chla_13Aug21 <- read.csv("13Aug21_chla_data.csv")

chla_13Aug21$Flag_Chla <- 0
chla_13Aug21$Flag_Pheo <- 0

flag_pheo_13Aug21 <- c("F_27Jul20_1.6", "F_27Jul20_1.6_dup"
)
for(value in flag_pheo_13Aug21){
  chla_13Aug21$Flag_Pheo[chla_13Aug21$sample_id == value] <- 4
}

chla_13Aug21 <- select(chla_13Aug21, -sample_id)


###########################################################################


chla_19Aug21 <- read.csv("19Aug21_chla_data.csv")

chla_19Aug21$Flag_Chla <- 0
chla_19Aug21$Flag_Pheo <- 0

flag_pheo_19Aug21 <- c("S_20Jul21_1.0", "CIM_01Jun21_site2", 
                       "CIM_06Jun21_site5", "CIM_06Jun21_site1"
)
for(value in flag_pheo_19Aug21){
  chla_19Aug21$Flag_Pheo[chla_19Aug21$sample_ID == value] <- 4
}

chla_19Aug21 <- select(chla_19Aug21, -sample_ID)


###########################################################################


chla_20Aug21 <- read.csv("20Aug21_chla_data.csv")

chla_20Aug21$Flag_Chla <- 0
chla_20Aug21$Flag_Pheo <- 0

flag_pheo_20Aug21 <- c("B_31May21_1.6", "B_28Jun21_1.6_dup", "B_08Mar21_1.6_dup", 
                       "B_15Jun21_1.6", "F_14May21_1.6", "F_15Jun21_1.6", 
                       "B_28Jun21_1.6", "B_08Mar21_1.6", "F_24May21_1.6_dup", 
                       "F_22Mar21_1.6_dup", "F_28Jun21_1.6", "F_07Jun21_1.6", 
                       "F_21Jun21_1.6", "B_05Apr21_1.6", "F_16Apr21_1.6", 
                       "F_06May21_1.6", "F_16Apr21_1.6", "B_14May21_1.6", 
                       "F_31May21_1.6", "B_05Apr21_1.6"
)
for(value in flag_pheo_20Aug21){
  chla_20Aug21$Flag_Pheo[chla_20Aug21$sample_id == value] <- 4
}

chla_20Aug21 <- select(chla_20Aug21, -sample_id)


###########################################################################

chla_16Dec21 <- read.csv("16Dec21_chla_data.csv")

chla_16Dec21$Flag_Chla <- 0
chla_16Dec21$Flag_Pheo <- 0

flag_pheo_16Dec21 <- c("B_06Dec21_1.6_dup", "F_14Sep21_1.6"
)
for(value in flag_pheo_16Dec21){
  chla_16Dec21$Flag_Pheo[chla_16Dec21$sample_ID == value] <- 4
}

chla_16Dec21 <- select(chla_16Dec21, -sample_ID)


###########################################################################


chla_17Dec21 <- read.csv("17Dec21_chla_data.csv")

chla_17Dec21$Flag_Chla <- 0
chla_17Dec21$Flag_Pheo <- 0

flag_pheo_17Dec21 <- c("F_09Aug21_1.6_dup", "F_21Jul21_1.6", "B_12Jul21_1.6", 
                       "F02Aug21_1.6"
)
for(value in flag_pheo_17Dec21){
  chla_17Dec21$Flag_Pheo[chla_17Dec21$sample_id == value] <- 4
}

chla_17Dec21$Reservoir <- gsub("SUNP", "SNP", chla_17Dec21$Reservoir)


chla_17Dec21 <- select(chla_17Dec21, -sample_id)




chla_new <- rbind(chla_12Aug21, chla_13Aug21, chla_19Aug21, chla_20Aug21, chla_16Dec21, chla_17Dec21)

# Pulled into excel to reformat dates 


# Read back into R

chla_new$Date <- as.character(chla_new$DateTime)

chla_new$Time <- as.character("12:00:00")

chla_new$DateTime <- ymd_hms(paste(chla_new$Date, chla_new$Time))

colnames(chla_new)

chla_new <- select(chla_new, -Date, -Time)

chla_old <- read.csv("chla_master_df_dt.csv")


str(chla_new)
str(chla_old)

chla_old$DateTime <- ymd_hms(chla_old$DateTime)

chla_new$Data <- "new"
chla_old$Data <- "old"



chla <- rbind(chla_old, chla_new)

chla$Reservoir <- gsub("CIM", "CCR", chla$Reservoir)

chla_withoutccr <- filter(chla, Reservoir != "CCR")
chla_withccr <- filter(chla, Reservoir == "CCR")
chla_withccr$Site <- gsub(6, 50, chla_withccr$Site)


chla <- rbind(chla_withoutccr, chla_withccr)

chla <- select(chla, -Data)

write.csv(chla, "manual_chlorophyll_2014_2021.csv", row.names = FALSE)




