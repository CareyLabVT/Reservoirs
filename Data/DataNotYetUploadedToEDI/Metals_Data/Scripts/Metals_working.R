rm(list=ls(all=TRUE))
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(stringr)

#read in most recent ICPMS sheet
ICP<-read.csv("~/Documents/GitHub/Reservoirs/Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/2023/ICPMS_230203_230501.csv",
                     skip = 5) %>% 
  select(X, X.14, X.15) %>%
  rename(Jeff_ID = X, Fe_mgL = X.14, Mn_mgL = X.15) %>% #note: Fe and Mn are still ppb or ug/L
  separate(Jeff_ID,c("DateTime","Sample")," - ")
 ICP$Sample <- as.numeric(ICP$Sample) #must be numeric for join to work
 ICP$Fe_mgL <- ICP$Fe_mgL/1000 #converting to ppm
 ICP$Mn_mgL <- ICP$Mn_mgL/1000 #converting to ppm
 ICP$DateTime <-as.Date(ICP$DateTime, format = "%m/%d/%Y")
   
#read in metals ID, reservoir, site, depth, and total/soluble key
 metals_key <- read.csv('~/Documents/GitHub/Reservoirs/Data/DataNotYetUploadedToEDI/Metals_Data/Scripts/Metals_Sample_Depth.csv') %>%
   rename(Depth_m = Sample.Depth..m.)
 
#set up final data frame with correct formatting!
 frame1 <- ICP %>%
   group_by(DateTime) %>% 
   expand(Sample = 1:30) %>% #note - 1:30 includes ALL of the FCR and BVR sites that are sampled
   left_join(ICP, by = c('Sample', 'DateTime')) %>% 
   full_join(metals_key, by = 'Sample') %>% 
   pivot_wider(names_from = 'Filter', values_from = c('Fe_mgL', 'Mn_mgL')) %>% 
   rename(TFe_mgL = Fe_mgL_T, SFe_mgL = Fe_mgL_S, TMn_mgL = Mn_mgL_T, SMn_mgL = Mn_mgL_S) %>% 
   group_by(DateTime, Reservoir, Depth_m, Site) %>% 
   summarise(TFe_mgL = mean(TFe_mgL, na.rm = TRUE),
             TMn_mgL = mean(TMn_mgL, na.rm = TRUE), 
             SFe_mgL = mean(SFe_mgL, na.rm = TRUE),
             SMn_mgL = mean(SMn_mgL, na.rm = TRUE),
             n_TFe = sum(!is.na(TFe_mgL)),
             n_TMn = sum(!is.na(TMn_mgL)),
             n_SFe = sum(!is.na(SFe_mgL)),
             n_SMn = sum(!is.na(SMn_mgL))) %>%
   ungroup () 
 
  #need to get rid of NaNs created by taking mean during summarise step
  frame1$TFe_mgL[is.nan(frame1$TFe_mgL)] <- NA
  frame1$TMn_mgL[is.nan(frame1$TMn_mgL)] <- NA
  frame1$SFe_mgL[is.nan(frame1$SFe_mgL)] <- NA
  frame1$SMn_mgL[is.nan(frame1$SMn_mgL)] <- NA


frame1 <- frame1 %>% 
   arrange(DateTime, Reservoir, Site, Depth_m) %>% 
   rowwise %>% 
   mutate(Sum = sum(TFe_mgL, SFe_mgL, TMn_mgL, SMn_mgL, na.rm = TRUE)) %>% 
   subset(Sum != 0) %>% 
   ungroup()

 
 #let's set up flags! Some will be manually entered, but we can at least make the columns
 frame1 <- frame1 %>% 
  mutate(Flag_DateTime = 0, #needs to be manually entered
         Flag_TFe_mgL = 0,
         Flag_TFe_mgL = ifelse(is.na(TFe_mgL), 1, Flag_TFe_mgL), #missing value
         Flag_TFe_mgL = ifelse(TFe_mgL < 0.01 & !is.na(TFe_mgL), 3, Flag_TFe_mgL), #below reporting level
         Flag_TFe_mgL = ifelse(TFe_mgL < 0 & !is.na(TFe_mgL), 4, Flag_TFe_mgL), #negative value, set to zero later
         Flag_TFe_mgL = ifelse(n_TFe > 1, 7, Flag_TFe_mgL), #flag for sample run multiple times, mean
         Flag_TFe_mgL = ifelse(TFe_mgL > 18.47622 & !is.na(TFe_mgL), 8, Flag_TFe_mgL), # 18.47622 is 3 sd above mean (2014-2022), flag for abnormally high value
         Flag_TMn_mgL = 0,
         Flag_TMn_mgL = ifelse(is.na(TMn_mgL), 1, Flag_TMn_mgL), #missing value
         Flag_TMn_mgL = ifelse(TMn_mgL < 0.0001 & !is.na(TMn_mgL), 3, Flag_TMn_mgL), #below reporting level
         Flag_TMn_mgL = ifelse(TMn_mgL < 0 & !is.na(TMn_mgL), 4, Flag_TMn_mgL), #negative value, set to zero later
         Flag_TMn_mgL = ifelse(n_TMn > 1, 7, Flag_TMn_mgL), #flag for sample run multiple times, mean
         Flag_TMn_mgL = ifelse(TMn_mgL > 2.70027 & !is.na(TMn_mgL), 8, Flag_TMn_mgL), # 2.70027 is 3 sd above mean (2014-2022), flag for abnormally high value
         Flag_SFe_mgL = 0,
         Flag_SFe_mgL = ifelse(is.na(SFe_mgL), 1, Flag_SFe_mgL), #missing value
         Flag_SFe_mgL = ifelse(SFe_mgL < 0.01 & !is.na(SFe_mgL), 3, Flag_SFe_mgL), #below reporting level
         Flag_SFe_mgL = ifelse(SFe_mgL < 0 & !is.na(SFe_mgL), 4, Flag_SFe_mgL), #negative value, set to zero later
         Flag_SFe_mgL = ifelse(n_SFe > 1, 7, Flag_SFe_mgL), #flag for sample run multiple times, mean
         Flag_SFe_mgL = ifelse(SFe_mgL > 17.22513 & !is.na(SFe_mgL), 8, Flag_SFe_mgL), # 17.22513 is 3 sd above mean (2014-2022), flag for abnormally high value
         Flag_SMn_mgL = 0,
         Flag_SMn_mgL = ifelse(is.na(SMn_mgL), 1, Flag_SMn_mgL), #missing value
         Flag_SMn_mgL = ifelse(SMn_mgL < 0.0001 & !is.na(SMn_mgL), 3, Flag_SMn_mgL), #below reporting level
         Flag_SMn_mgL = ifelse(SMn_mgL < 0 & !is.na(SMn_mgL), 4, Flag_SMn_mgL), #negative value, set to zero later
         Flag_SMn_mgL = ifelse(n_SMn > 1, 7, Flag_SMn_mgL), #flag for sample run multiple times, mean
         Flag_SMn_mgL = ifelse(SMn_mgL > 2.628231 & !is.na(SMn_mgL), 8, Flag_SMn_mgL)) # 2.628231 is 3 sd above mean (2014-2022), flag for abnormally high value)
 
    #metals flags 2 (instrument malfunction) and 6 (non-standard method) will have to be entered manually
  
 
 #let's write the final csv
 frame1 <- frame1 %>% 
   select(DateTime, Reservoir, Depth_m, Site, TFe_mgL, TMn_mgL, SFe_mgL, SMn_mgL, Flag_DateTime,
          Flag_TFe_mgL, Flag_TMn_mgL, Flag_SFe_mgL, Flag_SMn_mgL) %>% 
   arrange(DateTime, Reservoir, Site, Depth_m)
 
write.csv(frame1, file = '~/Documents/GitHub/Reservoirs/Data/DataNotYetUploadedToEDI/Metals_Data/EDI_Working/2023/Metals_230203_230501.csv')
 