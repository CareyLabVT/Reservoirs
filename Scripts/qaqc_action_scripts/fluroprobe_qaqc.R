#Title: Script for aggregating and QAQCing FP txt files for publication to EDI
#Author: Mary Lofton
#Updated version: Austin Delany
#Date created: 16DEC19
#Last updated: 2023-09-07


library(tidyverse)
library(lubridate)
library(httr)

rm(list=ls())

# load packages
#install.packages('pacman')
#pacman::p_load(tidyverse, lubridate, googlesheets4)

fluoroprobe_qaqc <- function(){

# Load in column names for .txt files to get template
col_names <- names(read_tsv("./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/20230111_BVR_50.txt", n_max = 0))

# Load in all txt files
fp_casts <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe", pattern = paste0("*.txt")) %>%
  map_df(~ data_frame(x = .x), .id = "cast")

raw_fp <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe", pattern = paste0("*.txt")) %>% 
  map_df(~ read_tsv(file.path(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe", .), 
                    col_types = cols(.default = "c"), col_names = col_names, skip = 2), .id = "cast")

#split out column containing filename to get Reservoir and Site data
fp2 <- left_join(raw_fp, fp_casts, by = c("cast")) %>%
  rowwise() %>% 
  mutate(Reservoir = unlist(strsplit(x, split='_', fixed=TRUE))[2],
         Site = unlist(strsplit(x, split='_', fixed=TRUE))[3],
         Site = unlist(strsplit(Site, split='.', fixed=TRUE))[1]) %>%
  ungroup()
fp2$Site <- as.numeric(fp2$Site)

#check to make sure strsplit function worked for all casts
check <- subset(fp2, is.na(fp2$Site))
unique(fp2$Reservoir)
unique(fp2$Site)

# Rename and select useful columns; drop metrics we don't use or publish such as cell count;
# eliminate shallow depths because of quenching
fp3 <- fp2 %>%
  mutate(DateTime = `Date/Time`, GreenAlgae_ugL = as.numeric(`Green Algae...2`), Bluegreens_ugL = as.numeric(`Bluegreen...3`),
         BrownAlgae_ugL = as.numeric(`Diatoms...4`), MixedAlgae_ugL = as.numeric(`Cryptophyta...5`), YellowSubstances_ugL = as.numeric(`Yellow substances...9`),
         TotalConc_ugL = as.numeric(`Total conc.`), Transmission = as.numeric(`Transmission`), Depth_m = as.numeric(`Depth`), Temp_degC = as.numeric(`Temp. Sample`),
         RFU_525nm = as.numeric(`LED 3 [525 nm]`), RFU_570nm = as.numeric(`LED 4 [570 nm]`), RFU_610nm = as.numeric(`LED 5 [610 nm]`),
         RFU_370nm = as.numeric(`LED 6 [370 nm]`), RFU_590nm = as.numeric(`LED 7 [590 nm]`), RFU_470nm = as.numeric(`LED 8 [470 nm]`),
         Pressure_unit = as.numeric(`Pressure`)) %>%
  select(x,cast, Reservoir, Site, DateTime, GreenAlgae_ugL, Bluegreens_ugL, BrownAlgae_ugL, MixedAlgae_ugL, YellowSubstances_ugL,
         TotalConc_ugL, Transmission, Depth_m, Temp_degC, RFU_525nm, RFU_570nm, RFU_610nm,
         RFU_370nm, RFU_590nm, RFU_470nm) %>%
  mutate(DateTime = as.POSIXct(as_datetime(DateTime, tz = "", format = "%m/%d/%Y %I:%M:%S %p"))) %>%
  filter(Depth_m >= 0.2) |> 
  dplyr::mutate(DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                DateTime = lubridate::with_tz(DateTime, tzone = "UTC"))

#trim casts to eliminate poor readings due to sediment interference at bottom of reservoir
#for our purposes, we consider the "max depth" of each reservoir to be:
#FCR = 9.5 m; BVR = 10.0 m; CCR = 21 m 

fp4 = fp3[FALSE,]

for (i in 1:length(unique(fp3$cast))){
  profile = subset(fp3, cast == unique(fp3$cast)[i])
  if(profile$Reservoir[1] == "FCR"){
    profile_trim <- profile %>% filter(Depth_m <= 9.5)
  } else if (profile$Reservoir[1] == "CCR"){
    profile_trim <- profile %>% filter(Depth_m <= 21)
  } else if (profile$Reservoir[1] == "BVR"){
    profile_trim <- profile %>% filter(Depth_m <= 10)
  }
  fp4 <- bind_rows(fp4, profile_trim)
} 

fp6 <- fp4

#get rid of columns we don't need for final publication
fp7 <- fp6 %>%
  select(-x, -cast) %>%
  mutate(Site = as.numeric(Site))

#reorder columns to match current EDI data package
fp8 <- fp7[,c(1,2,3,11,4,5,6,7,8,9,10,12,13,14,15,16,17,18)]


#ADD FLAGS

fp_final <- fp8 %>%
  mutate(Flag_GreenAlgae_ugL = 0,
         Flag_BluegreenAlgae_ugL = 0,
         Flag_BrownAlgae_ugL = 0,
         Flag_MixedAlgae_ugL = 0,
         Flag_TotalConc_ugL = 0,
         Flag_Temp_C = 0, # example: ifelse(date(DateTime) %in% bad_temp_days,2,0),
         Flag_Transmission_perc = 0,
         Flag_RFU_525nm = 0,
         Flag_RFU_570nm = 0,
         Flag_RFU_610nm = 0,
         Flag_RFU_370nm = 0,
         Flag_RFU_590nm = 0,
         Flag_RFU_470nm = 0) %>%
  rename(Temp_C = Temp_degC,
         Transmission_perc = Transmission) 

write.csv(fp_final, "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/fluoroprobe_L1.csv", row.names = FALSE)

}

# run the function
fluoroprobe_qaqc()

