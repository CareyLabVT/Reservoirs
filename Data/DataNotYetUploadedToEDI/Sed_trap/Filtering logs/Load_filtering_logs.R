##Load Sed Trap Filtering Logs
##Author: Cece Wood
##Date edited: 25 June 2024
##Edited by: Carly Bauer

#load all the libraries first
rm(list=ls(all=TRUE))
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(stringr)
library(readr)

###
# 2023 data
###


#read Excel sheets in; need to read in filtering logs for first data frame
  #for now, need to set working directories to read sheets in
setwd("~/Data/DataNotYetUploadedToEDI/Sed_trap/Filtering logs")
filteringlog <- read_csv("2023_FilteringLog_EDI.csv") %>% 
  mutate(DateFiltered = as.POSIXct(DateFiltered, format = '%d%b%y'))
log_2018 <- read_excel("2018_FilteringLog_EDI.xlsx")
log_2019 <- read_excel("2019_FilteringLog_EDI.xlsx")
log_2020 <- read_excel("2020_FilteringLog_EDI.xlsx")
log_2021 <- read_excel("2021_FilteringLog_EDI.xlsx")
log_2022 <- read_excel("2022_FilteringLog_EDI.xlsx")

filteringlog = filteringlog%>%
  full_join(log_2018)%>%
  full_join(log_2019)%>%
  full_join(log_2020)%>%
  full_join(log_2021) %>% 
  full_join(log_2022)

#create first data frame
frame1 <- select(filteringlog, FilterID, FilterVol_L, VolDiscarded_L, 
                 FilterMassPre_g, FilterMassPost_g, SedMass_g, Duration_days)

#let's make the right columns
#Reservoir
frame1 <- frame1 %>% 
  mutate(Reservoir = ifelse(substring(frame1$FilterID, 1,1) == 'F','FCR',NA),
         Reservoir = ifelse(substring(frame1$FilterID, 1,1) == 'B','BVR', Reservoir))

#Date
frame1 <- frame1 %>% 
  mutate(Date = as.Date(as.character(str_extract(frame1$FilterID, '(?<=_)[:digit:]*[:alpha:]*[:digit:]*$')), format = '%d%b%y'))

#Site
frame1$Site <- 50

#Depth_m
frame1 <- frame1 %>% 
  mutate(Depth_m = as.numeric(str_extract(frame1$FilterID, '(?<=_)[:digit:]+(?=m)')))

#TrapRep
frame1 <- frame1 %>% 
  mutate(TrapRep = as.numeric(str_extract(frame1$FilterID, '(?<=_R)[:digit:]+')))

#TrapXSA_m2
frame1$TrapXSA_m2 <- 0.0040715

    #TrapVol_L
frame1$TrapVol_L <- 1.8

    #CollectionVol_L
frame1 <- frame1 %>% 
  mutate(VolDiscarded_L = ifelse(year(Date)==2019&Date<"2019-09-15",NA,VolDiscarded_L))%>% #Mistakes in filtering log before this point
  group_by(Reservoir, Depth_m, TrapRep, Date) %>% 
  mutate(CollectionVol_L = (sum(FilterVol_L) + unique(VolDiscarded_L))) %>% 
  ungroup()

    #FilterRep
frame1 <- frame1 %>% 
  mutate(FilterRep = as.numeric(str_extract(frame1$FilterID, '(?<=_F)[:digit:]+')))


#reorder columns in the first data frame and rename. Prep for EDI publishing
frame1 <- frame1 %>% 
  select(Reservoir, Date, Site, Duration_days, Depth_m, TrapRep, TrapXSA_m2, TrapVol_L, 
         CollectionVol_L, FilterRep, FilterVol_L, FilterMassPre_g,
         FilterMassPost_g, SedMass_g, FilterID)


# if collection volume is NA, flag it 2 because it wasn't calculated 
frame1 = frame1%>%
  mutate(Flag_CollectionVol_L = ifelse(is.na(CollectionVol_L),2,1),
         CollectionVol_L = ifelse(is.na(CollectionVol_L),1.7,CollectionVol_L), # if NA, then changed to preset that abby and cece calculated, might need to change this
                                                                               # might want to code something like in metals, ex: 3 std away flag it 
                                                                               # 1.7 = average collection volume 
         
         Flag_SedMass_g = ifelse(is.na(SedMass_g),2,1), # unsure exactly why would be NA, but flagging that it is NA
         Flag_SedMass_g = ifelse(!is.na(SedMass_g)&SedMass_g<0,3,Flag_SedMass_g), # if neg, then flag as 3
         SedMass_g = ifelse(SedMass_g<0,NA,SedMass_g)) # if neg, set to NA now that it's flagged


#Some durations are unknown at the beginning of each year
frame1 = frame1%>%
  mutate(Flag_Duration_days = 1,
         Flag_Duration_days = ifelse(Date == "2018-05-21" & Reservoir == "FCR", 
                                     2, 
                                     Flag_Duration_days),
         Flag_Duration_days = ifelse(Date == "2020-07-06" & Reservoir == "FCR", 
                                     2, 
                                     Flag_Duration_days),
         Flag_Duration_days = ifelse(Date == "2020-07-02" & Reservoir == "BVR", 
                                     2, 
                                     Flag_Duration_days),
         Flag_Duration_days = ifelse(Date == "2021-06-28" & Reservoir == "BVR", 
                                     2, 
                                     Flag_Duration_days))
  
#SedFlux_gm2d, put at the end because we need Duration and Collection Vol to be fixed before we can calculate this
frame1 <- frame1 %>% 
  mutate(SedFlux_gm2d = (((SedMass_g/FilterVol_L) * CollectionVol_L) / (TrapXSA_m2 * Duration_days)))


#order columns correctly
frame1 <- frame1 %>% 
  select(Reservoir, Date, Site, Duration_days, Depth_m, TrapRep, TrapXSA_m2, TrapVol_L,
         CollectionVol_L, FilterRep, FilterVol_L, FilterMassPre_g, FilterMassPost_g, 
         SedMass_g, SedFlux_gm2d, FilterID, Flag_CollectionVol_L, Flag_SedMass_g, 
         Flag_Duration_days)

#ready to write csv!
frame1 <- arrange(frame1, frame1$Date)
write.csv(frame1,"FilteringLog_EDI.csv",row.names = F)
