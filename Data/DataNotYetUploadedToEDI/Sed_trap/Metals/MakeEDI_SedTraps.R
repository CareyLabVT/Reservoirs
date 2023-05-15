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

frame1 = read.csv("../Filtering logs/FilteringLog_EDI.csv")

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
    separate(JeffID,c("Sample","Date"),"_")
frame2$Date <- as.Date(frame2$Date, format = '%d%b%y') #NIST Standard and Acid Blank will go to NA
frame2 <- frame2[!(is.na(frame2$Date)), ] #removes NIST Standard and Acid Blank


  #let's use the first filter ID to extract reservoir and depth info (should be the same year to year)
  #also add in info that never changes
frame2 <- frame2 %>% 
  mutate(Reservoir = ifelse(substring(frame2$Filter1ID, 1,1) == 'F','FCR',NA),
         Reservoir = ifelse(substring(frame2$Filter1ID, 1,1) == 'B','BVR', Reservoir),
         Depth_m = as.numeric(str_extract(frame2$Filter1ID, '(?<=_)[:digit:]+(?=m)')),
         Site = 50, TrapXSA_m2 = 0.0040715)

#

#Need to get data from each filter separately. Doing that in a for loop
#Set up empty columns
frame2_complete = frame2%>%
  mutate(CombinedCollectionVol_L = NA,
         CombinedFilterVol_L = NA,
         CombinedSedMass_g = NA,
         CombinedXSA_m2 = NA,
         Duration_days = NA)
#Loop through all rows and sum data
for(i in 1:nrow(frame2_complete)){
  filter1 = frame1%>%filter(FilterID==frame2_complete$Filter1ID[i]) #filter to the first filter
  filter2 = frame1%>%filter(FilterID==frame2_complete$Filter2ID[i]) #filter to the second filter
  if(nrow(filter1)==1){
    frame2_complete$CombinedCollectionVol_L[i]=filter1$CollectionVol_L+filter2$CollectionVol_L #sum collection volumes
    frame2_complete$CombinedFilterVol_L[i]=filter1$FilterVol_L+filter2$FilterVol_L #sum filter volumes
    frame2_complete$CombinedSedMass_g[i]=filter1$SedMass_g+filter2$SedMass_g #sum sed mass
    frame2_complete$CombinedXSA_m2[i]=filter1$TrapXSA_m2+filter2$TrapXSA_m2 #sum surface area
    frame2_complete$Duration_days[i]=filter1$Duration_days #save duration
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
