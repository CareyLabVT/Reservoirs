# Title: Sed traps processing script for all metals
# Author: Cece Wood and Carly Bauer
# Date: 22SEP23


#load all the libraries first
rm(list=ls(all=TRUE))
library(plyr)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(stringr)
library(readr)


#read Excel sheets in; need to read in filtering logs for first data frame
#for now, need to set working directories to read sheets in
setwd("./Data/DataNotYetUploadedToEDI/Sed_trap")

frame1 = read_csv("Filtering logs/FilteringLog_EDI.csv")


#
#
#
#frame 2 let's gooo
#need to read in the Jeff sheets, will do a little QA/QC to account for Jeff's formatting
ICP2023 <- read_excel('Metals/Raw_Data/2023_ICPData.xlsx', skip = 3) %>% select(...1, `7Li (STDR)`, `23Na (STDR)`, `24Mg (STDR)`, `27Al (STDR)`, `39K (STDR)`, `44Ca (STDR)`, `54Fe (STDR)`, `55Mn (STDR)`, `65Cu (STDR)`, `88Sr (STDR)`, `138Ba (STDR)`) # need to change this set up so it reads in all of the 2023 ICP data sheets
ICP2022 <- read_excel('Metals/Raw_Data/2022_ICPData.xlsx', skip = 3) %>% select(...1, `7Li (STDR)`, `23Na (STDR)`, `24Mg (STDR)`, `27Al (STDR)`, `39K (STDR)`, `44Ca (STDR)`, `54Fe (STDR)`, `55Mn (STDR)`, `65Cu (STDR)`, `88Sr (STDR)`, `138Ba (STDR)`)
ICP2021 <- read_excel('Metals/Raw_Data/2021_ICPData.xlsx', skip = 3) %>% select(...1, `7Li (STDR)`, `23Na (STDR)`, `24Mg (STDR)`, `27Al (STDR)`, `39K (STDR)`, `44Ca (STDR)`, `54Fe (STDR)`, `55Mn (STDR)`, `65Cu (STDR)`, `88Sr (STDR)`, `138Ba (STDR)`)
ICP2020 <- read_excel('Metals/Raw_Data/2020_ICPData.xlsx', skip = 3) %>% select(...1, `7Li (STDR)`, `23Na (STDR)`, `24Mg (STDR)`, `27Al (STDR)`, `39K (STDR)`, `44Ca (STDR)`, `54Fe (STDR)`, `55Mn (STDR)`, `65Cu (STDR)`, `88Sr (STDR)`, `138Ba (STDR)`)
ICP2019 <- read_excel('Metals/Raw_Data/2019_ICPData.xlsx', skip = 3) %>% select(...1, `7Li (STDR)`, `23Na (STDR)`, `24Mg (STDR)`, `27Al (STDR)`, `39K (STDR)`, `44Ca (STDR)`, `54Fe (STDR)`, `55Mn (STDR)`, `65Cu (STDR)`, `88Sr (STDR)`, `138Ba (STDR)`)
ICP2018 <- read_excel('Metals/Raw_Data/2018_ICPData.xlsx', skip = 3) %>% select(...1,  `23Na (STDR)`, `24Mg (STDR)`, `27Al (STDR)`, `39K (STDR)`, `44Ca (STDR)`, `54Fe (STDR)`, `55Mn (STDR)`, `65Cu (STDR)`, `88Sr (STDR)`, `138Ba (STDR)`)

# add NAs for Li because 2018 doesn't have Li data
if (!"7Li (STDR)" %in% names(ICP2018)) {
  ICP2018 <- ICP2018 %>% mutate(`7Li (STDR)` = NA)
}


ICPData = ICP2023 %>% 
  rbind(ICP2022)%>%
  rbind(ICP2021)%>%
  rbind(ICP2020)%>%
  rbind(ICP2019)%>%
  rbind(ICP2018)

#rename headers to element in ppb
glimpse(ICPData)
ICPData <- ICPData %>% dplyr::rename('Li_ppb' = `7Li (STDR)`,
                              'Na_ppb' = `23Na (STDR)`, 
                              'Mg_ppb' = `24Mg (STDR)`,
                              'Al_ppb' = `27Al (STDR)`,
                              'K_ppb' = `39K (STDR)`,
                              'Ca_ppb' = `44Ca (STDR)`,
                              'Fe_ppb' = `54Fe (STDR)`,
                              'Mn_ppb' = `55Mn (STDR)`,
                              'Cu_ppb' = `65Cu (STDR)`,
                              'Sr_ppb' = `88Sr (STDR)`,
                              'Ba_ppb' = `138Ba (STDR)`,'JeffID' = '...1')

#need to join digestion spreadsheet while sample names are still in code
Digestion2023 <- read_excel('Metals/Digestions/2023_AcidDigestion_EDI.xlsx')
Digestion2022 <-  read_excel('Metals/Digestions/2022_AcidDigestion_EDI.xlsx')
Digestion2021 <-  read_excel('Metals/Digestions/2021_AcidDigestion_EDI.xlsx')
Digestion2020 <-  read_excel('Metals/Digestions/2020_AcidDigestion_EDI.xlsx')
Digestion2019 <-  read_excel('Metals/Digestions/2019_AcidDigestions_EDI.xlsx')
Digestion2018 <-  read_excel('Metals/Digestions/2018_AcidDigestions_EDI.xlsx')

Digestion <- Digestion2023 %>% 
  rbind(Digestion2022)%>%
  rbind(Digestion2021)%>%
  rbind(Digestion2020)%>%
  rbind(Digestion2019)%>%
  rbind(Digestion2018)

#combine all ICPdata with digestion information
frame2 <- full_join(ICPData, Digestion, by = join_by(JeffID == Sample), multiple = "all", relationship = "many-to-many")

#separating to get sample date
frame2 <- frame2 %>% 
  separate(JeffID,c("Sample","Date"),"_")
frame2$Date <- as.Date(frame2$Date, format = '%d%b%y') #NIST Standard and Acid Blank will go to NA
frame2 <- frame2[!(is.na(frame2$Date)), ] #removes NIST Standard and Acid Blank


#let's use the first filter ID to extract reservoir and depth info (should be the same year to year)
#also add in info that never changes
# frame2 <- frame2 %>% 
#   mutate(Reservoir = ifelse(substring(frame2$Filter1ID, 1,1) == 'F','FCR',NA),
#          Reservoir = ifelse(substring(frame2$Filter1ID, 1,1) == 'B','BVR', Reservoir),
#          Depth_m = as.numeric(str_extract(frame2$Filter1ID, '(?<=_)[:digit:]+(?=m)')),
#          Site = 50, TrapXSA_m2 = 0.0040715)

frame2 <- frame2 %>% 
  mutate(Reservoir = case_when(
    substring(Filter1ID, 1, 1) == 'F' ~ 'FCR',
    substring(Filter1ID, 1, 1) == 'B' ~ 'BVR',
    TRUE ~ NA_character_),
    Depth_m = as.numeric(str_extract(Filter1ID, '(?<=_)[:digit:]+(?=m)')),
    Site = 50, TrapXSA_m2 = 0.0040715)

#

#Need to get data from each filter separately. Doing that in a for loop
#Set up empty columns
frame2_complete = frame2%>%
  mutate(CombinedCollectionVol_L = NA,
         CombinedFilterVol_L = NA,
         CombinedSedMass_g = NA,
         CombinedXSA_m2 = NA,
         Duration_days = NA,
         Flag_CombinedCollectionVol_L = 1,
         Flag_Filter2ID = 1, 
         Flag_CombinedSedMass_g = 1,
         Flag_ICPTLi_mgL = 1,
         Flag_ICPTNa_mgL = 1,
         Flag_ICPTMg_mgL = 1,
         Flag_ICPTAl_mgL = 1,
         Flag_ICPTK_mgL = 1,
         Flag_ICPTCa_mgL = 1,
         Flag_ICPTFe_mgL = 1, #Not currently changing these below
         Flag_ICPTMn_mgL= 1,
         Flag_ICPTCu_mgL = 1,
         Flag_ICPTSr_mgL = 1,
         Flag_ICPTBa_mgL = 1)
#Loop through all rows and sum data
for(i in 1:nrow(frame2_complete)){
  filter1 = frame1%>%filter(tolower(FilterID)==tolower(frame2_complete$Filter1ID[i])) #filter to the first filter
  filter2 = frame1%>%filter(tolower(FilterID)==tolower(frame2_complete$Filter2ID[i])) #filter to the second filter
  if(nrow(filter1)==1&nrow(filter2)==1){
    frame2_complete$nTraps[i]=if_else(filter1$TrapRep==filter2$TrapRep, 1, 2) #number of trap reps/tubes used for each digestion
    frame2_complete$CombinedCollectionVol_L[i]=if_else(filter1$TrapRep==filter2$TrapRep, filter1$CollectionVol_L, filter1$CollectionVol_L + filter2$CollectionVol_L) #sum collection volumes only if the trap reps are not the same
    frame2_complete$CombinedFilterVol_L[i]=filter1$FilterVol_L+filter2$FilterVol_L #sum filter volumes
    frame2_complete$CombinedSedMass_g[i]=filter1$SedMass_g+filter2$SedMass_g #sum sed mass
    frame2_complete$CombinedXSA_m2[i]=if_else(filter1$TrapRep==filter2$TrapRep, filter1$TrapXSA_m2, filter1$TrapXSA_m2 + filter2$TrapXSA_m2) #sum surface area only if the trap reps are not the same
    frame2_complete$Duration_days[i]=filter1$Duration_days #save duration
    frame2_complete$Flag_CombinedCollectionVol_L[i]=ifelse(filter1$Flag_CollectionVol_L==2|filter2$Flag_CollectionVol_L==2,
                                                           3,
                                                           frame2_complete$Flag_CombinedCollectionVol_L[i]) #Save flag for volume
  } else { 
    if(!nrow(filter1)==1){warning(
      paste0("Filter ",
             frame2_complete$Filter1ID[i], 
             " is in the filtering log ",  nrow(filter1)," times\n"))}
    if(!nrow(filter2)==1&!is.na(frame2_complete$Filter2ID[i])){warning(
      paste0("Filter ",
             frame2_complete$Filter2ID[i], 
             " is in the filtering log ",  nrow(filter2)," times\n"))}
    if(is.na(frame2_complete$Filter2ID[i])&nrow(filter1)==1){ #If only one filter was used
      frame2_complete$CombinedCollectionVol_L[i]=filter1$CollectionVol_L 
      frame2_complete$CombinedFilterVol_L[i]=filter1$FilterVol_L
      frame2_complete$CombinedSedMass_g[i]=filter1$SedMass_g
      frame2_complete$CombinedXSA_m2[i]=filter1$TrapXSA_m2 
      frame2_complete$Duration_days[i]=filter1$Duration_days #save duration
      frame2_complete$Flag_CombinedCollectionVol_L[i]=2
      frame2_complete$Flag_Filter2ID[i]=2
      frame2_complete$Flag_CombinedCollectionVol_L[i]=2
      frame2_complete$Flag_CombinedSedMass_g[i]=2
    }
    
  }
}
#make ppb values numeric 
frame2_complete=frame2_complete%>%
  mutate(Li_ppb=as.numeric(Li_ppb),
         Na_ppb=as.numeric(Na_ppb),
         Mg_ppb=as.numeric(Mg_ppb),
         Al_ppb=as.numeric(Al_ppb),
         K_ppb=as.numeric(K_ppb),
         Ca_ppb=as.numeric(Ca_ppb),
         Fe_ppb=as.numeric(Fe_ppb),
         Mn_ppb=as.numeric(Mn_ppb),
         Cu_ppb=as.numeric(Cu_ppb),
         Sr_ppb=as.numeric(Sr_ppb),
         Ba_ppb=as.numeric(Ba_ppb))

#change ppb to mg/L by dividing by 1000 #changing ppb to concentration
frame2 <- frame2_complete %>% 
  mutate(ICPTLi_mgL = Li_ppb/1000,
         ICPTNa_mgL = Na_ppb/1000,
         ICPTMg_mgL = Mg_ppb/1000,
         ICPTAl_mgL = Al_ppb/1000,
         ICPTK_mgL = K_ppb/1000,
         ICPTCa_mgL = Ca_ppb/1000,
         ICPTFe_mgL = Fe_ppb/1000, 
         ICPTMn_mgL = Mn_ppb/1000, 
         ICPTCu_mgL = Cu_ppb/1000,
         ICPTSr_mgL = Sr_ppb/1000,
         ICPTBa_mgL = Ba_ppb/1000,
         DilutionFactor = 20,
         TLi_g = case_when(
           nTraps == 2 ~ (ICPTLi_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L/2)/CombinedFilterVol_L),
           .default = (ICPTLi_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L)/CombinedFilterVol_L)
           ),
         TNa_g = case_when(
           nTraps == 2 ~ (ICPTNa_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L/2)/CombinedFilterVol_L),
           .default = (ICPTNa_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L)/CombinedFilterVol_L)
           ),
         TMg_g = case_when(
           nTraps == 2 ~ (ICPTMg_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L/2)/CombinedFilterVol_L),
           .default = (ICPTMg_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L)/CombinedFilterVol_L)
            ),
         TAl_g = case_when(
           nTraps == 2 ~ (ICPTAl_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L/2)/CombinedFilterVol_L),
           .default = (ICPTAl_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L)/CombinedFilterVol_L)
            ),
         TK_g = case_when(
           nTraps == 2 ~ (ICPTK_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L/2)/CombinedFilterVol_L),
           .default = (ICPTK_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L)/CombinedFilterVol_L)
            ),
         TCa_g = case_when(
           nTraps == 2 ~ (ICPTCa_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L/2)/CombinedFilterVol_L),
           .default = (ICPTCa_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L)
            ), 
         TFe_g = case_when(
           nTraps == 2 ~ (ICPTFe_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L/2)/CombinedFilterVol_L),
           .default = (ICPTFe_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L)
            ),
         TMn_g = case_when(
           nTraps == 2 ~ (ICPTMn_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L/2)/CombinedFilterVol_L),
           .default = (ICPTMn_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L)/CombinedFilterVol_L)
            ),
         TCu_g = case_when(
           nTraps == 2 ~ (ICPTCu_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L/2)/CombinedFilterVol_L),
           .default = (ICPTCu_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L)/CombinedFilterVol_L)
           ),
         TSr_g = case_when(
           nTraps == 2 ~ (ICPTSr_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L/2)/CombinedFilterVol_L),
           .default = (ICPTSr_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L)/CombinedFilterVol_L)
           ),
         TBa_g = case_when(
           nTraps == 2 ~ (ICPTBa_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L/2)/CombinedFilterVol_L),
           .default = (ICPTBa_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*((CombinedCollectionVol_L)/CombinedFilterVol_L)
           ),
         TLiFlux_gm2d = case_when(
           nTraps == 2 ~ TLi_g/(CombinedXSA_m2/2)/Duration_days,
           .default = TLi_g/CombinedXSA_m2/Duration_days
           ),
         TNaFlux_gm2d = case_when(
           nTraps == 2 ~ TNa_g/(CombinedXSA_m2/2)/Duration_days,
           .default = TNa_g/CombinedXSA_m2/Duration_days
           ),
         TMgFlux_gm2d = case_when(
           nTraps == 2 ~ TMg_g/(CombinedXSA_m2/2)/Duration_days,
           .default = TMg_g/CombinedXSA_m2/Duration_days
           ),
         TAlFlux_gm2d = case_when( 
           nTraps == 2 ~ TAl_g/(CombinedXSA_m2/2)/Duration_days,
           .default = TAl_g/CombinedXSA_m2/Duration_days
           ),
         TKFlux_gm2d = case_when(
           nTraps == 2 ~ TK_g/(CombinedXSA_m2/2)/Duration_days,
           .default = TK_g/CombinedXSA_m2/Duration_days
           ),
         TCaFlux_gm2d = case_when(
           nTraps == 2 ~ TCa_g/(CombinedXSA_m2/2)/Duration_days,
           .default = TCa_g/CombinedXSA_m2/Duration_days
           ),
         TFeFlux_gm2d = case_when(
           nTraps == 2 ~ TFe_g/(CombinedXSA_m2/2)/Duration_days,
           .default = TFe_g/CombinedXSA_m2/Duration_days
           ),
         TMnFlux_gm2d = case_when(
           nTraps == 2 ~ TMn_g/(CombinedXSA_m2/2)/Duration_days,
           .default = TMn_g/CombinedXSA_m2/Duration_days
           ),
         TCuFlux_gm2d = case_when(
           nTraps == 2 ~ (TCu_g/CombinedXSA_m2/2)/Duration_days,
           .default = TCu_g/CombinedXSA_m2/Duration_days
           ),
         TSrFlux_gm2d = case_when(
           nTraps == 2 ~ TSr_g/CombinedXSA_m2/Duration_days,
           .default = TSr_g/CombinedXSA_m2/Duration_days
           ),
         TBaFlux_gm2d = case_when(
           nTraps == 2 ~ TBa_g/(CombinedXSA_m2/2)/Duration_days,
           .default = TBa_g/CombinedXSA_m2/Duration_days
           ))

#sort out the names and column order
frame2 <- frame2 %>% 
  dplyr::rename("AcidVol_L" = "Vol_acid_L") %>% 
  select(-(ends_with("ppb")))

#save frame2 as a .csv
write.csv(frame2, file="./Metals/2023_allMetals.csv",row.names = F)
