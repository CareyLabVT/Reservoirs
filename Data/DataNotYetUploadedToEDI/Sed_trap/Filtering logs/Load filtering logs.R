#load all the libraries first
rm(list=ls(all=TRUE))
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(stringr)

#read Excel sheets in; need to read in filtering logs for first data frame
  #for now, need to set working directories to read sheets in
setwd("./Data/DataNotYetUploadedToEDI/Sed_trap/Filtering logs")
filteringlog <- read_excel('2022_SedTraps_FilteringLog.xlsx')

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
  group_by(Reservoir, Depth_m, TrapRep, Date) %>% 
  mutate(CollectionVol_L = sum(Volume_filtered_mL) + unique(Volume_discarded_mL)) %>% 
  ungroup()

    #FilterRep
frame1 <- frame1 %>% 
  mutate(FilterRep = as.numeric(str_extract(frame1$Sample_ID, '(?<=_F)[:digit:]+')))

    #SedFlux_gm2d
frame1 <- frame1 %>% 
    mutate(SedFlux_gm2d = (((Mass_of_sediment_g/Volume_filtered_mL) * CollectionVol_L) / (TrapXSA_m2 * Duration_days)))

#reorder columns in the first data frame and rename. Prep for EDI publishing
frame1 <- frame1 %>% 
  select(Reservoir, Date, Site, Duration_days, Depth_m, TrapRep, TrapXSA_m2, TrapVol_L, 
  CollectionVol_L, FilterRep, Volume_filtered_mL, Filter_mass_pre_filtering_g,
  Filter_mass_post_filtering_g, Mass_of_sediment_g, SedFlux_gm2d, Sample_ID)

frame1 <- frame1 %>% rename("FilterVol_L" = "Volume_filtered_mL", 
                            "FilterMassPre_g" = "Filter_mass_pre_filtering_g",
                            "FilterMassPost_g" = "Filter_mass_post_filtering_g", 
                            "SedMass_g" = "Mass_of_sediment_g", "FilterID" = "Sample_ID")

frame1 = frame1%>%
  mutate(Flag_CollectionVol_L = ifelse(is.na(CollectionVol_L),2,1),
         CollectionVol_L = ifelse(is.na(CollectionVol_L),1700,CollectionVol_L),
         Flag_SedMass_g = ifelse(SedMass_g<0,2,1),
         SedMass_g = ifelse(SedMass_g<0,NA,SedMass_g))
  

write.csv(frame1,"FilteringLog_EDI.csv",row.names = F)