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


#read Excel sheets in; need to read in filtering logs for first data frame
#for now, need to set working directories to read sheets in
setwd('~/Documents/R/GitHub/Metals')

frame1 = read.csv("../Filtering logs/FilteringLog_EDI.csv")


#
#
#
#frame 2 let's gooo
#need to read in the Jeff sheets, will do a little QA/QC to account for Jeff's formatting
ICP2022 <- read_excel('2022_ICPData.xlsx', skip = 3) 
ICP2021 <- read_excel('2021_ICPData.xlsx', skip = 3) 
ICP2020 <- read_excel('2020_ICPData.xlsx', skip = 3) 
ICP2019 <- read_excel('2019_ICPData.xlsx', skip = 3) 
ICP2018 <- read_excel('2018_ICPData.xlsx', skip = 3)

#combine all ICP data into one frame - ICPData
ICPData = ICP2022%>%
  rbind.fill(ICP2021)%>%
  rbind.fill(ICP2020)%>%
  rbind.fill(ICP2019)%>%
  rbind.fill(ICP2018)
#rename headers to element in ppb
glimpse(ICPData)
ICPData <- ICPData %>% dplyr::rename('Li_ppb'=`7Li (STDR)`,
                               'Na_ppb' =`23Na (STDR)`,
                               'Mg_ppb' =`24Mg (STDR)`,
                               'Al_ppb' = `27Al (STDR)`,
                               'Si_ppb' = `29Si (STDR)`,
                               'P_ppb' = `31P (STDR)`,
                               'S_ppb' = `34S (STDR)`,
                               'Cl_ppb' = `35Cl (STDR)`,
                               'K_ppb' = `39K (STDR)`,
                               'Ca_ppb' = `44Ca (STDR)`,
                               'Ti_ppb' = `47Ti (STDR)`,
                               'V_ppb' = `51V (STDR)`,
                               'Cr_ppb' = `52Cr (STDR)`,
                               'Fe_ppb' = `54Fe (STDR)`,
                               'Mn_ppb' = `55Mn (STDR)`,
                               'Co_ppb' = `59Co (STDR)`,
                               'Ni_ppb' = `60Ni (STDR)`,
                               'Cu_ppb' = `65Cu (STDR)`,
                               'Zn_ppb' = `66Zn (STDR)`,
                               'As_ppb' = `75As (STDR)`,
                               'Se_ppb' = `78Se (STDR)`,
                               'Sr_ppb' = `88Sr (STDR)`,
                               'Mo_ppb' = `95Mo (STDR)`,
                               'Ag_ppb' = `107Ag (STDR)`,
                               'Cd_ppb' = `111Cd (STDR)`,
                               'Sn_ppb' = `112Sn (STDR)`,
                               'Ba_ppb' = `138Ba (STDR)`,
                               'Pb_ppb' = `208Pb (STDR)`,
                               'U_ppb' = `238U (STDR)`,
                               'Sn120_ppb' = `120Sn (STDR)`,
                               'Tb_ppb' = `159Tb (STDR)`,
                               'Ho_ppb' = `165Ho (STDR)`,
                               'Ti48_ppb' = `48Ti (STDR)`,
                               'JeffID' = '...1')

#need to join digestion spreadsheet while sample names are still in code
Digestion2022 <-  read_excel('2022_AcidDigestion_EDI.xlsx')
Digestion2021 <-  read_excel('2021_AcidDigestion_EDI.xlsx')
Digestion2020 <-  read_excel('2020_AcidDigestion_EDI.xlsx')
Digestion2019 <-  read_excel('2019_AcidDigestions_EDI.xlsx')
Digestion2018 <-  read_excel('2018_AcidDigestions_EDI.xlsx')

#combine all digestion years into one frame
Digestion <- Digestion2022%>%
  rbind(Digestion2021)%>%
  rbind(Digestion2020)%>%
  rbind(Digestion2019)%>%
  rbind(Digestion2018)

#combine all ICPdata with digestion information
frame2 <- full_join(ICPData, Digestion, by = join_by(JeffID == Sample), multiple = "all",relationship = "many-to-many")

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
         Duration_days = NA,
         Flag_CombinedCollectionVol_L = 1,
         Flag_Filter2ID = 1, 
         Flag_CombinedSedMass_g = 1, 
         Flag_ICPTFe_mgL = 1, #Not currently changing these below
         Flag_ICPTMn_mgL= 1)
#Loop through all rows and sum data
for(i in 1:nrow(frame2_complete)){
  filter1 = frame1%>%filter(tolower(FilterID)==tolower(frame2_complete$Filter1ID[i])) #filter to the first filter
  filter2 = frame1%>%filter(tolower(FilterID)==tolower(frame2_complete$Filter2ID[i])) #filter to the second filter
  if(nrow(filter1)==1&nrow(filter2)==1){
    frame2_complete$CombinedCollectionVol_L[i]=filter1$CollectionVol_L+filter2$CollectionVol_L #sum collection volumes
    frame2_complete$CombinedFilterVol_L[i]=filter1$FilterVol_L+filter2$FilterVol_L #sum filter volumes
    frame2_complete$CombinedSedMass_g[i]=filter1$SedMass_g+filter2$SedMass_g #sum sed mass
    frame2_complete$CombinedXSA_m2[i]=filter1$TrapXSA_m2+filter2$TrapXSA_m2 #sum surface area
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
         Si_ppb=as.numeric(Si_ppb),
         P_ppb=as.numeric(P_ppb),
         S_ppb=as.numeric(S_ppb),
         Cl_ppb=as.numeric(Cl_ppb),
         K_ppb=as.numeric(K_ppb),
         Ca_ppb=as.numeric(Ca_ppb),
         Ti_ppb=as.numeric(Ti_ppb),
         V_ppb=as.numeric(V_ppb),
         Cr_ppb=as.numeric(Cr_ppb),
         Fe_ppb=as.numeric(Fe_ppb),
         Mn_ppb=as.numeric(Mn_ppb),
         Co_ppb=as.numeric(Co_ppb),
         Ni_ppb=as.numeric(Ni_ppb),
         Cu_ppb=as.numeric(Cu_ppb),
         Zn_ppb=as.numeric(Zn_ppb),
         As_ppb=as.numeric(As_ppb),
         Se_ppb=as.numeric(Se_ppb),
         Sr_ppb=as.numeric(Sr_ppb),
         Mo_ppb=as.numeric(Mo_ppb),
         Mg_ppb=as.numeric(Mg_ppb),
         Ag_ppb=as.numeric(Ag_ppb),
         Cd_ppb=as.numeric(Cd_ppb),
         Sn_ppb=as.numeric(Sn_ppb),
         Ba_ppb=as.numeric(Ba_ppb),
         Pb_ppb=as.numeric(Pb_ppb),
         U_ppb=as.numeric(U_ppb),
         Sn120_ppb=as.numeric(Sn120_ppb),
         Tb_ppb=as.numeric(Tb_ppb),
         Ho_ppb=as.numeric(Ho_ppb),
         Ti48_ppb=as.numeric(Ti48_ppb))
#change ppb to mg/L by dividing by 1000 #changing ppb to concentration
frame2 <- frame2_complete %>% mutate(ICPTLi_mgL = Li_ppb/1000,
                                     ICPTNa_mgL = Na_ppb/1000,
                                     ICPTMg_mgL = Mg_ppb/1000,
                                     ICPTAl_mgL = Al_ppb/1000,
                                     ICPTSi_mgL = Si_ppb/1000,
                                     ICPTP_mgL = P_ppb/1000,
                                     ICPTS_mgL = S_ppb/1000,
                                     ICPTCl_mgL = Cl_ppb/1000,
                                     ICPTK_mgL = K_ppb/1000,
                                     ICPTCa_mgL = Ca_ppb/1000,
                                     ICPTTi_mgL = Ti_ppb/1000,
                                     ICPTV_mgL = V_ppb/1000,
                                     ICPTCr_mgL = Cr_ppb/1000,
                                     ICPTFe_mgL = Fe_ppb/1000,
                                     ICPTMn_mgL = Mn_ppb/1000, 
                                     ICPTCo_mgL = Co_ppb/1000,
                                     ICPTNi_mgL = Ni_ppb/1000,
                                     ICPTCu_mgL = Cu_ppb/1000,
                                     ICPTZn_mgL = Zn_ppb/1000,
                                     ICPTAs_mgL = As_ppb/1000,
                                     ICPTSe_mgL = Se_ppb/1000,
                                     ICPTSr_mgL = Sr_ppb/1000,
                                     ICPTMo_mgL = Mo_ppb/1000,
                                     ICPTAg_mgL = Ag_ppb/1000,
                                     ICPTCd_mgL = Cd_ppb/1000,
                                     ICPTSn_mgL = Sn_ppb/1000,
                                     ICPTBa_mgL = Ba_ppb/1000,
                                     ICPTPb_mgL = Pb_ppb/1000,
                                     ICPTU_mgL = U_ppb/1000,
                                     ICPTSn120_mgL = Sn120_ppb/1000,
                                     ICPTTb_mgL = Tb_ppb/1000,
                                     ICPTHo_mgL = Ho_ppb/1000,
                                     ICPTTi48_mgL = Ti48_ppb/1000,
                                     DilutionFactor = 20, #insert dilution factor which is constant
                                     TLi_g = (ICPTLi_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L), #total mass of metals in sed traps
                                     TNa_g = (ICPTNa_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TMg_g = (ICPTMg_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TAl_g = (ICPTAl_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TSi_g = (ICPTSi_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TP_g = (ICPTP_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TS_g = (ICPTS_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TCl_g = (ICPTCl_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TK_g = (ICPTK_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TCa_g = (ICPTCa_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TTi_g = (ICPTTi_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TV_g = (ICPTV_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TCr_g = (ICPTCr_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TFe_g = (ICPTFe_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L), 
                                     TMn_g = (ICPTMn_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TCo_g = (ICPTCo_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TNi_g = (ICPTNi_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TCu_g = (ICPTCu_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TZn_g = (ICPTZn_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TAs_g = (ICPTAs_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TSe_g = (ICPTSe_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TSr_g = (ICPTSr_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TMo_g = (ICPTMo_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TAg_g = (ICPTAg_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TCd_g = (ICPTCd_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TSn_g = (ICPTSn_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TBa_g = (ICPTBa_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TPb_g = (ICPTPb_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TU_g = (ICPTU_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TSn120_g = (ICPTSn120_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TTb_g = (ICPTTb_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     THo_g = (ICPTHo_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TTi48_g = (ICPTTi48_mgL/1000)*(Vol_acid_L)*(DilutionFactor)*(CombinedCollectionVol_L/CombinedFilterVol_L),
                                     TLiFlux_gm2d = TLi_g/CombinedXSA_m2/Duration_days, #total mass per trap changed to flux
                                     TNaFlux_gm2d = TNa_g/CombinedXSA_m2/Duration_days,
                                     TMgFlux_gm2d = TMg_g/CombinedXSA_m2/Duration_days,
                                     TAlFlux_gm2d = TAl_g/CombinedXSA_m2/Duration_days,
                                     TSiFlux_gm2d = TSi_g/CombinedXSA_m2/Duration_days,
                                     TPFlux_gm2d = TP_g/CombinedXSA_m2/Duration_days,
                                     TSFlux_gm2d = TS_g/CombinedXSA_m2/Duration_days,
                                     TClFlux_gm2d = TCl_g/CombinedXSA_m2/Duration_days,
                                     TKFlux_gm2d = TK_g/CombinedXSA_m2/Duration_days,
                                     TCaFlux_gm2d = TCa_g/CombinedXSA_m2/Duration_days,
                                     TTiFlux_gm2d = TTi_g/CombinedXSA_m2/Duration_days,
                                     TVFlux_gm2d = TV_g/CombinedXSA_m2/Duration_days,
                                     TCrFlux_gm2d = TCr_g/CombinedXSA_m2/Duration_days,
                                     TFeFlux_gm2d = TFe_g/CombinedXSA_m2/Duration_days,
                                     TMnFlux_gm2d = TMn_g/CombinedXSA_m2/Duration_days,
                                     TCoFlux_gm2d = TCo_g/CombinedXSA_m2/Duration_days,
                                     TNiFlux_gm2d = TNi_g/CombinedXSA_m2/Duration_days,
                                     TCuFlux_gm2d = TCu_g/CombinedXSA_m2/Duration_days,
                                     TZnFlux_gm2d = TZn_g/CombinedXSA_m2/Duration_days,
                                     TAsFlux_gm2d = TAs_g/CombinedXSA_m2/Duration_days,
                                     TSeFlux_gm2d = TSe_g/CombinedXSA_m2/Duration_days,
                                     TSrFlux_gm2d = TSr_g/CombinedXSA_m2/Duration_days,
                                     TMoFlux_gm2d = TMo_g/CombinedXSA_m2/Duration_days,
                                     TAgFlux_gm2d = TAg_g/CombinedXSA_m2/Duration_days,
                                     TCdFlux_gm2d = TCd_g/CombinedXSA_m2/Duration_days,
                                     TSnFlux_gm2d = TSn_g/CombinedXSA_m2/Duration_days,
                                     TBaFlux_gm2d = TBa_g/CombinedXSA_m2/Duration_days,
                                     TPbFlux_gm2d = TPb_g/CombinedXSA_m2/Duration_days,
                                     TUFlux_gm2d = TU_g/CombinedXSA_m2/Duration_days,
                                     TSn120Flux_gm2d = TSn120_g/CombinedXSA_m2/Duration_days,
                                     TTbFlux_gm2d = TTb_g/CombinedXSA_m2/Duration_days,
                                     THoFlux_gm2d = THo_g/CombinedXSA_m2/Duration_days,
                                     TTi48Flux_gm2d = TTi48_g/CombinedXSA_m2/Duration_days)

#sort out the names and column order
frame2 <- frame2 %>% dplyr::rename("AcidVol_L" = "Vol_acid_L")

#save frame2 as a .csv
write.csv(frame2,file="2022_allMetals.csv",row.names = F)

#plot 8 scatter plots in one window
par(mfrow=c(2,4))
plot(frame2$Fe_ppb,frame2$Al_ppb,cex=0.5,col=as.factor(frame2$Reservoir))
plot(frame2$Fe_ppb,frame2$Ca_ppb,cex=0.5,col=as.factor(frame2$Reservoir))
plot(frame2$Fe_ppb,frame2$Ni_ppb,cex=0.5,col=as.factor(frame2$Reservoir))
plot(frame2$Fe_ppb,frame2$Sn_ppb,cex=0.5,col=as.factor(frame2$Reservoir))

plot(frame2$TFe_g,frame2$TAl_g,cex=0.5,col=as.factor(frame2$Reservoir))
plot(frame2$TFe_g,frame2$TCa_g,cex=0.5,col=as.factor(frame2$Reservoir))
plot(frame2$TFe_g,frame2$TNi_g,cex=0.5,col=as.factor(frame2$Reservoir))
plot(frame2$TFe_g,frame2$TSn_g,cex=0.5,col=as.factor(frame2$Reservoir))

