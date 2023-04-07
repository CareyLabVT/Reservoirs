#load all the libraries first
rm(list=ls(all=TRUE))
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(stringr)

#read Excel sheets in; need to read in filtering logs for first data frame
  #for now, need to set working directories to read sheets in
setwd("./Data/DataNotYetUploadedToEDI/Sed_trap/Metals")
filteringlog <- read_excel('2022_SedTraps_FilteringLog.xlsx')

#create first data frame
frame1 <- select(filteringlog, Sample_ID, Volume_filtered_mL, Volume_discarded_mL, 
                 Filter_mass_pre_filtering_g, Filter_mass_post_filtering_g, Mass_of_sediment_g, Duration_days)

  #let's make the right columns

    #Reservoir
frame1 <- frame1 %>% 
  mutate(Reservoir = ifelse(substring(frame1$Sample_ID, 1,1) == 'F','FCR',NA),
        Reservoir = ifelse(substring(frame1$Sample_ID, 1,1) == 'B','BVR', Reservoir))

    #DateTime
#frame1$DateTime <- as.Date(substring(frame1$Sample_ID, 16,22), format = '%d%b%y')
frame1 <- frame1 %>% 
  mutate(DateTime = as.Date(as.character(str_extract(frame1$Sample_ID, '(?<=_)[:digit:]*[:alpha:]*[:digit:]*$')), format = '%d%b%y'))
         
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
  group_by(Reservoir, Depth_m, TrapRep, DateTime) %>% 
  mutate(CollectionVol_L = sum(Volume_filtered_mL) + unique(Volume_discarded_mL)) %>% 
  ungroup()

    #FilterRep
frame1 <- frame1 %>% 
  mutate(FilterRep = as.numeric(str_extract(frame1$Sample_ID, '(?<=_F)[:digit:]+')))

    #SedFlux_gm2d
frame1 <- frame1 %>% 
    mutate(SedFlux_gm2d = (((Mass_of_sediment_g/Volume_filtered_mL) * CollectionVol_L) / (TrapXSA_m2 * Duration_days)))

#reorder columns in the first data frame and rename
frame1 <- frame1 %>% 
  select(Reservoir, DateTime, Site, Duration_days, Depth_m, TrapRep, TrapXSA_m2, TrapVol_L, 
  CollectionVol_L, FilterRep, Volume_filtered_mL, Filter_mass_pre_filtering_g,
  Filter_mass_post_filtering_g, Mass_of_sediment_g, SedFlux_gm2d, Sample_ID)

frame1 <- frame1 %>% rename("FilterVol_L" = "Volume_filtered_mL", 
                            "FilterMassPre_g" = "Filter_mass_pre_filtering_g",
                            "FilterMassPost_g" = "Filter_mass_post_filtering_g", 
                            "SedMass_g" = "Mass_of_sediment_g", "FilterID" = "Sample_ID")

write.csv(frame1,"FilteringLog_EDI.csv",row.names = F)

#
#
#
#
#frame 2 let's gooo
  #need to read in the Jeff sheets, will do a little QA/QC to account for Jeff's formatting
ICPData <- read_excel('2022_ICPData.xlsx', skip = 3)
glimpse(ICPData)
ICPData <- ICPData %>% select(...1, `54Fe (STDR)`, `55Mn (STDR)`)
ICPData <- ICPData %>% rename('Fe_ppb' = `54Fe (STDR)`, 'Mn_ppb' = `55Mn (STDR)`, 'JeffID' = '...1')

  #need to join digestion spreadsheet while sample names are still in code
Digestion <-  read_excel('2022_SedTraps_FilteringLog.xlsx', sheet = 'Sheet2')
frame2 <- full_join(ICPData, Digestion, by = join_by(JeffID == Sample), multiple = "all")

  #separating to get sample date
frame2 <- frame2 %>% 
    separate(JeffID,c("Sample","DateTime"),"_")
frame2$DateTime <- as.Date(frame2$DateTime, format = '%d%b%y') #NIST Standard and Acid Blank will go to NA
frame2 <- frame2[!(is.na(frame2$DateTime)), ] #removes NIST Standard and Acid Blank


  #let's use the first filter ID to extract reservoir and depth info (should be the same year to year)
  #also add in info that never changes
frame2 <- frame2 %>% 
  mutate(Reservoir = ifelse(substring(frame2$Filter1ID, 1,1) == 'F','FCR',NA),
         Reservoir = ifelse(substring(frame2$Filter1ID, 1,1) == 'B','BVR', Reservoir),
         Depth_m = as.numeric(str_extract(frame2$Filter1ID, '(?<=_)[:digit:]+(?=m)')),
         Site = 50, TrapXSA_m2 = 0.0040715)

#

#Set up empty columns
frame2_complete = frame2%>%
  mutate(CombinedCollectionVol_L = NA,
         CombinedFilterVol_L = NA,
         CombinedSedMass_g = NA,
         CombinedXSA_m2 = NA)
#Loop through all rows and sum data
for(i in 1:nrow(frame2_complete)){
  filter1 = frame1%>%filter(FilterID==frame2_complete$Filter1ID[i]) #filter to the first filter
  filter2 = frame1%>%filter(FilterID==frame2_complete$Filter2ID[i]) #filter to the second filter
  if(nrow(filter1)==1){
    frame2_complete$CombinedCollectionVol_L[i]=filter1$CollectionVol_L+filter2$CollectionVol_L #sum collection volumes
    frame2_complete$CombinedFilterVol_L[i]=filter1$FilterVol_L+filter2$FilterVol_L #sum filter volumes
    frame2_complete$CombinedSedMass_g[i]=filter1$SedMass_g+filter2$SedMass_g #sum sed mass
    frame2_complete$CombinedXSA_m2[i]=filter1$TrapXSA_m2+filter2$TrapXSA_m2 #sum surface area
  } else { warning(
    paste0("Filter ",
           frame2_complete$Filter1ID[i], 
           " is in the filtering log ",  nrow(filter1)," times\n"))}
}
  

frame2 <- frame2_complete %>% mutate(ICPTFe_mgL = Fe_ppb/1000, ICPTMn_mgL = Mn_ppb/1000, DilutionFactor = 20,
                            TFe_g = (ICPTFe_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L), 
                            TMn_g = (ICPTMn_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L))

#sort out the names and column order
frame2 <- frame2 %>% rename("AcidVol_L" = "Vol_acid_L")

#whoops still need to add Duration column         

write.csv(frame2,"2022_FeMnCN.csv",row.names = F)
