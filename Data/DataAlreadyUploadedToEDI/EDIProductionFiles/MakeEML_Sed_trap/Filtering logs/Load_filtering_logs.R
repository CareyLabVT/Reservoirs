#load all the libraries first
rm(list=ls(all=TRUE))
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(stringr)

###
# 2022 data
###


#read Excel sheets in; need to read in filtering logs for first data frame
  #for now, need to set working directories to read sheets in
setwd("~/Documents/GitHub/Reservoirs/Data/DataNotYetUploadedToEDI/Sed_trap/Filtering logs")
filteringlog <- read_excel('2022_SedTraps_FilteringLog.xlsx')
log_2018 <- read_excel("2018_FilteringLog_EDI.xlsx")
log_2019 <- read_excel("2019_FilteringLog_EDI.xlsx")
log_2020 <- read_excel("2020_FilteringLog_EDI.xlsx")
log_2021 <- read_excel("2021_FilteringLog_EDI.xlsx")

filteringlog = filteringlog%>%
  rbind(log_2018)%>%
  rbind(log_2019)%>%
  rbind(log_2020)%>%
  rbind(log_2021)

#create first data frame
frame1 <- select(filteringlog, Sample_ID, Volume_filtered_mL, Volume_discarded_mL, 
                 Filter_mass_pre_filtering_g, Filter_mass_post_filtering_g, Mass_of_sediment_g, Duration_days)

  #let's make the right columns

    #Reservoir
frame1 <- frame1 %>% 
  mutate(Reservoir = ifelse(substring(frame1$Sample_ID, 1,1) == 'F','FCR',NA),
        Reservoir = ifelse(substring(frame1$Sample_ID, 1,1) == 'B','BVR', Reservoir))

    #Date
#frame1$Date <- as.Date(substring(frame1$Sample_ID, 16,22), format = '%d%b%y')
frame1 <- frame1 %>% 
  mutate(Date = as.Date(as.character(str_extract(frame1$Sample_ID, '(?<=_)[:digit:]*[:alpha:]*[:digit:]*$')), format = '%d%b%y'))
         
    #Site
frame1$Site <- 50

    #Depth_m
frame1 <- frame1 %>% 
  mutate(Depth_m = as.numeric(str_extract(frame1$Sample_ID, '(?<=_)[:digit:]+(?=m)')))

    #TrapRep
frame1 <- frame1 %>% 
  mutate(TrapRep = as.numeric(str_extract(frame1$Sample_ID, '(?<=_R)[:digit:]+')))

    #TrapXSA_m2
frame1$TrapXSA_m2 <- 0.0040715

    #TrapVol_L
frame1$TrapVol_L <- 2

    #CollectionVol_L
frame1 <- frame1 %>% 
  mutate(Volume_discarded_mL = ifelse(year(Date)==2019&Date<"2019-09-15",NA,Volume_discarded_mL))%>% #Mistakes in filtering log before this point
  group_by(Reservoir, Depth_m, TrapRep, Date) %>% 
  mutate(CollectionVol_L = (sum(Volume_filtered_mL) + unique(Volume_discarded_mL))/1000) %>% 
  ungroup()

    #FilterRep
frame1 <- frame1 %>% 
  mutate(FilterRep = as.numeric(str_extract(frame1$Sample_ID, '(?<=_F)[:digit:]+')))

    #make Volume_filtered column in correct units
frame1 <- frame1 %>% 
  rename("Volume_filtered_L" = "Volume_filtered_mL")
frame1$Volume_filtered_L <- frame1$Volume_filtered_L/1000


#reorder columns in the first data frame and rename. Prep for EDI publishing
frame1 <- frame1 %>% 
  select(Reservoir, Date, Site, Duration_days, Depth_m, TrapRep, TrapXSA_m2, TrapVol_L, 
  CollectionVol_L, FilterRep, Volume_filtered_L, Filter_mass_pre_filtering_g,
  Filter_mass_post_filtering_g, Mass_of_sediment_g, Sample_ID)

frame1 <- frame1 %>% rename("FilterVol_L" = "Volume_filtered_L", 
                            "FilterMassPre_g" = "Filter_mass_pre_filtering_g",
                            "FilterMassPost_g" = "Filter_mass_post_filtering_g", 
                            "SedMass_g" = "Mass_of_sediment_g", "FilterID" = "Sample_ID")

frame1 = frame1%>%
  mutate(Flag_CollectionVol_L = ifelse(is.na(CollectionVol_L),2,1),
         CollectionVol_L = ifelse(is.na(CollectionVol_L),1.7,CollectionVol_L),
         
         Flag_SedMass_g = ifelse(is.na(SedMass_g),2,1),
         Flag_SedMass_g = ifelse(!is.na(SedMass_g)&SedMass_g<0,3,Flag_SedMass_g),
         SedMass_g = ifelse(SedMass_g<0,NA,SedMass_g))

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
