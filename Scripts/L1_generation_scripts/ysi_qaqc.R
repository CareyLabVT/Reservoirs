# YSI and PAR QAQC/collation
# last edited: Austin Delany
# 2023-11-02

#install.packages('pacman') ## Run this line if you don't have "pacman" package installed
pacman::p_load(tidyverse, lubridate,dplyr) ## Use pacman package to install/load other packages

#### YSI Profiles ####

#Note: I re-processed all data from 2019-2021 because the flags were messed up...(3 was used instead of 4 for negative values)

#----------------------------------- HmmTHINGS TO GO IN THE MAINTENANCE LOG (THESE RECEIVE NEW FLAG [6] -- HUMAN ERROR) -------------------------------------------#
# read in file from last year (only need to do this to add sp cond and then rewrite to folder)
# ysi_old <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021/YSI_PAR_profiles_2013-2021.csv")
# 
# #change DO %sat for fcr 100 15aug 2016 to NA because same as DO mg/L (and is wrong...) 
# ysi_old$DOSat[ysi_old$Reservoir=="FCR" & ysi_old$Site==100 & as.Date(ysi_old$DateTime)=="2016-08-15"] <- NA
# 
# 
# #manually change a few digitizing errors in 2020
# ysi$DO_mgL[as.Date(ysi$DateTime)=="2020-08-17" & ysi$Reservoir=="FCR" & ysi$Depth_m==2.4] <- 5.82
# 
# ysi$DO_mgL[as.Date(ysi$DateTime)=="2020-05-25" & ysi$Reservoir=="FCR" & ysi$Depth_m==4.0] <- 5.05
# ysi$DOsat_percent[as.Date(ysi$DateTime)=="2020-05-25" & ysi$Reservoir=="FCR" & ysi$Depth_m==4.0] <- 46.6

#------------------------------------------------------------------------------#
#read in new data
#raw_profiles <- read_csv(file.path("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2022/Data/2022_YSI_PAR_profiles.csv"))
raw_profiles <- read_csv(file.path("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2022/Data/2022_YSI_PAR_profiles.csv")) ##open file directly from Google Drive -- Ask Adrienne

#rename sp cond column because for some reason it changed...
names(raw_profiles)[names(raw_profiles) == 'SpCond_uScm'] <- 'Sp_cond_uScm'

#date format
raw_profiles$DateTime <- as.POSIXct(strptime(raw_profiles$DateTime, "%Y-%m-%d %H:%M:%S" ))#"%m/%d/%y %H:%M"

#make depth numeric
raw_profiles$Depth_m <- as.numeric(raw_profiles$Depth_m)

#rename notes col
names(raw_profiles)[20] <- "Notes"

#add flag datetime col
raw_profiles$Flag_DateTime <- NA

#QAQC data + add flags
profiles <- raw_profiles %>%
  mutate(Flag_DateTime = ifelse(Notes=="There is no time recorded",1,0)) %>%
  select(!Notes) %>%
  group_by(Reservoir, DateTime) %>% # columns not to parse to numeric
  mutate_if(is.character,funs(round(as.double(.), 2))) %>%  # parse all other columns to numeric
  
  # Add 'flag' columns for each variable; 1 = flag for NA value
  mutate(Flag_pH = ifelse(is.na(pH), 1,
                          ifelse((pH > 14 | pH < 4), 2, # Flag 2 = inst. malfunction
                                 ifelse(pH < 0, 4, #Flag 4 = negative set to 0
                                        ifelse(pH < 5, paste0(Flag_pH,5), Flag_pH)))), 
         Flag_ORP = ifelse(is.na(ORP_mV), 1, 
                           ifelse(ORP_mV > 750, 2, 0)),  # Flag 2 = inst. malfunction
         Flag_PAR = ifelse(is.na(PAR_umolm2s), 1,
                           ifelse(PAR_umolm2s < 0, 4, 0)), #Flag 4 = negative set to 0
         Flag_Temp = ifelse(is.na(Temp_C), 1, 
                            ifelse(Temp_C > 35, 2, 0)), # Flag 2 = inst. malfunction
         Flag_DO = ifelse(is.na(DO_mgL), 1,
                          ifelse(DO_mgL > 70, 2, # Flag 2 = inst. malfunction
                                 ifelse(DO_mgL < 0, 4, 0))),  #Flag 4 = negative set to 0
         Flag_DOSat = ifelse(is.na(DOSat), 1,
                             ifelse(DOSat > 200, 2, # Flag 2 = inst. malfunction
                                    ifelse(DO_mgL < 0, 4, 0))),  #Flag 4 = negative set to 0
         Flag_Cond = ifelse(is.na(Cond_uScm), 1,
                            ifelse((Cond_uScm < 10 | Cond_uScm > 250), 2, # Flag 2 = inst. malfunction
                                   ifelse(DO_mgL < 0, 4, 0))),  #Flag 4 = negative set to 0
         Flag_Sp_Cond = ifelse(is.na(Sp_cond_uScm), 1,
                               ifelse(DO_mgL > 250, 2,  # Flag 2 = inst. malfunction
                                      ifelse(DO_mgL < 0, 4, 0)))) %>%  #Flag 4 = negative set to 0
  
  
  #set data for any 2 flags to NA and any 4 flags to 0
  mutate(pH = ifelse(Flag_pH == 2, NA, 
                     ifelse(Flag_pH == 4, 0, paste0(pH))),
         ORP_mV = ifelse(Flag_ORP == 2, NA, ORP_mV),
         Temp_C = ifelse(Flag_Temp == 2, NA, paste0(Temp_C)),
         PAR_umolm2s = ifelse(Flag_PAR == 4, 0, paste0(PAR_umolm2s)),
         DO_mgL = ifelse(Flag_DO == 2, NA, 
                         ifelse(Flag_DO == 4, 0, DO_mgL)),
         DOSat = ifelse(Flag_DOSat == 2, NA,
                        ifelse(Flag_DOSat == 4, 0, DOSat)),
         Cond_uScm = ifelse(Flag_Cond == 2, NA, 
                            ifelse(Flag_Cond == 4, 0, paste0(Cond_uScm))),
         Sp_cond_uScm = ifelse(Flag_Sp_Cond == 2, NA, 
                               ifelse(Flag_Sp_Cond == 4, 0, paste0(Sp_cond_uScm)))) %>%
  
  # Arrange order of columns for final data table
  select(Reservoir, Site, DateTime, Depth_m, Temp_C, DO_mgL, DOSat, 
         Cond_uScm, Sp_cond_uScm, PAR_umolm2s, ORP_mV, pH, Flag_DateTime, Flag_Temp, Flag_DO, Flag_DOSat,
         Flag_Cond, Flag_Sp_Cond, Flag_PAR, Flag_ORP, Flag_pH) %>%
  arrange(Reservoir, DateTime, Depth_m) 

#manually replace NA flags with 0
profiles$Flag_Sp_Cond[is.na(profiles$Flag_Sp_Cond)] <- 0
profiles$Flag_DOSat[is.na(profiles$Flag_DOSat)] <- 0
profiles$Flag_DateTime[is.na(profiles$Flag_DateTime)] <- 0

#change saome variable names and flags to include units in the final df
names(profiles) <- c(names(profiles)[1:6],"DOsat_percent","Cond_uScm","SpCond_uScm",names(profiles)[10:13],"Flag_Temp_C","Flag_DO_mgL",
                "Flag_DOsat_percent","Flag_Cond_uScm","Flag_SpCond_uScm",
                "Flag_PAR_umolm2s","Flag_ORP_mV","Flag_pH")

# Write to CSV -- save as L1 file
write.csv(profiles, file.path('./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2022/Data/YSI_PAR_profiles_2022_final.csv'), row.names=F)
