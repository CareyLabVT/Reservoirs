# 2020 nutrient chemistry collation
#includes TNTP, DOC, and np 2020
#Note: flags are aggregated without commas for final EDI upload
#figure out what do do when dups are super different

library(tidyverse)
library(dplyr)

TNTP <- read.csv("./collation/2020/2020_TNTP_collation.csv")

#drop samplID col 
TNTP <- TNTP [,!(names(TNTP) %in% c("SampleID_lachat"))]

#drop rows with NA values
TNTP <- TNTP[!is.na(TNTP$TP_ugL) | !is.na(TNTP$TN_ugL) ,]

#remove a couple of samples with neg. TN values (these were rerun so just going to take TN/TP from second run instead of averaging)
TNTP <- TNTP[TNTP$TN_ugL > 0 ,]


# set flags for TN & TP
###################################################
# add 7 for rows that will be averaged
###################################################
# create flag columns
# no flag value = 0
TNTP$Flag_TP <- 0
TNTP$Flag_TN <- 0

#order TNTP df
TNTP <- TNTP %>% arrange(Reservoir, DateTime, Site, Depth_m)

# look for duplicate values and average them, while adding flag = 7
TNTP_dups <- duplicated(TNTP[,1:4]) & TNTP$Rep==""
table(TNTP_dups)['TRUE']

# create col to put average of reps into
TNTP$TP_ugL_AVG <- 'NA'
TNTP$TN_ugL_AVG <- 'NA'

########## AVERAGING DUPS ####
#average all samples run twice data
for (i in 1:length(TNTP_dups)) {
  if(TNTP_dups[i]=='TRUE'){
    ifelse(TNTP$Rep[i]=="",TNTP$TP_ugL_AVG[i]<- mean(c(TNTP$TP_ugL[i], TNTP$TP_ugL[i-1])), TNTP$TP_ugL_AVG[i])
    ifelse(TNTP$Rep[i]=="",TNTP$TN_ugL_AVG[i]<- mean(c(TNTP$TN_ugL[i], TNTP$TN_ugL[i-1])), TNTP$TN_ugL_AVG[i])
    # assign this to the other duplicate as well
    ifelse(TNTP$Rep[i]=="",TNTP$TP_ugL_AVG[i-1]<- mean(c(TNTP$TP_ugL[i], TNTP$TP_ugL[i-1])), TNTP$TP_ugL_AVG[i-1])
    ifelse(TNTP$Rep[i]=="",TNTP$TN_ugL_AVG[i-1]<- mean(c(TNTP$TN_ugL[i], TNTP$TN_ugL[i-1])), TNTP$TN_ugL_AVG[i-1])
  
    # flag as 7, average of two reps
    ifelse(TNTP$Rep[i]=="", TNTP$Flag_TP[i] <- 7, TNTP$Flag_TP[i])
    ifelse(TNTP$Rep[i]=="",TNTP$Flag_TN[i] <- 7, TNTP$Flag_TN[i])
    ifelse(TNTP$Rep[i]=="",TNTP$Flag_TP[i-1] <- 7, TNTP$Flag_TP[i-1])
    ifelse(TNTP$Rep[i]=="",TNTP$Flag_TN[i-1] <- 7, TNTP$Flag_TN[i-1])
  }  
}

# remove dups (no dups here, just field dups so keeping)
TNTP <- TNTP[!TNTP_dups,]

# move the averaged data over to the original columns
for (i in 1:nrow(TNTP)) {
  if(!TNTP$TP_ugL_AVG[i]=='NA'){
    TNTP$TP_ugL[i] <- TNTP$TP_ugL_AVG[i]
    TNTP$TN_ugL[i] <- TNTP$TN_ugL_AVG[i]
  }
}

#remove avg columns at end and rep col
TNTP <- TNTP[,-c(12,13)]

#keep/format rep col
TNTP$Rep <- ifelse(TNTP$Rep=="R2",2,1)

############################################################
#        2020 average "rolling spiked blank 250"           #
#            if below detection, flag as 3                 #
#                using the following MDL's:                #
#                      TP      TN                          #
#                     6.8     72.2                         #
############################################################ 
#note - reran in 2022 w/ updated MDL's! (TN was originally 42...)

for (i in 1:nrow(TNTP)) {
  ifelse(TNTP$TP_ugL[i] < 6.8 & TNTP$Flag_TP[i]==7,
    TNTP$Flag_TP[i] <- "73",
  ifelse(TNTP$TP_ugL[i] < 6.8,
    TNTP$Flag_TP[i] <- 3, TNTP$Flag_TP[i]))
  }


for (i in 1:nrow(TNTP)) {
  ifelse(TNTP$TN_ugL[i] < 72.2 & TNTP$Flag_TN[i]==7,
    TNTP$Flag_TN[i] <- "73",
  ifelse(TNTP$TN_ugL[i] < 72.2,
    TNTP$Flag_TN[i] <- 3, TNTP$Flag_TN[i]))
}

###################################################
# if negative, set to zero and set flag to 4
###################################################

for (i in 1:nrow(TNTP)) {
  if(TNTP$TP_ugL[i] < 0) {TNTP$TP_ugL[i]== 0}
  
  ifelse(TNTP$TP_ugL[i] == 0.000000 & TNTP$Flag_TP[i]=="7",
      TNTP$Flag_TP[i] <- "74",
  ifelse(TNTP$TP_ugL[i] == 0.000000 & TNTP$Flag_TP[i]=="3",
      TNTP$Flag_TP[i] <- "43", 
  ifelse(TNTP$TP_ugL[i] ==	0.000000 & TNTP$Flag_TP[i]=="73",
      TNTP$Flag_TP[i] <- "743",
  ifelse(TNTP$TP_ugL[i] == 0.000000, TNTP$Flag_TP[i] <- "4",
      TNTP$Flag_TP[i]))))
}


for (i in 1:nrow(TNTP)) {
  if(TNTP$TN_ugL[i] < 0) {TNTP$TN_ugL[i] <- 0}
  
  ifelse(TNTP$TN_ugL[i] == 0.000000 & TNTP$Flag_TN[i]=="7",
      TNTP$Flag_TN[i] <- "74",
  ifelse(TNTP$TN_ugL[i] == 0.000000 & TNTP$Flag_TN[i]=="3",
      TNTP$Flag_TN[i] <- "43", 
  ifelse(TNTP$TN_ugL[i] ==	0.000000 & TNTP$Flag_TN[i]=="73",
      TNTP$Flag_TN[i] <- "743",
  ifelse(TNTP$TN_ugL[i] == 0.000000, TNTP$Flag_TN[i] <- "4",
      TNTP$Flag_TN[i]))))
}


############################################
#### WMW code for soluble/DOC collation ####
############################################

#read in 2020_chemistry_collation for DOC 
doc <- read.csv("./collation/2020/2020_TOC_collation.csv")

# create flag columns
# no flag value = 0
doc$Flag_DC <- 0
doc$Flag_DIC <- 0
doc$Flag_DOC <- 0
doc$Flag_DN <- 0

#order TNTP df
doc <- doc %>% arrange(Reservoir, DateTime, Site, Depth_m)

#function to select rows based on characters from the end
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#make sure all analytes are numeric
doc$DOC_mgL <- as.numeric(doc$DOC_mgL)
doc$DIC_mgL <- as.numeric(doc$DIC_mgL)
doc$DC_mgL <- as.numeric(doc$DC_mgL)
doc$DN_mgL <- as.numeric(doc$DN_mgL)

#delete samples that have negative values AND that were rerun - assuming that we didn't trust the first run
doc <- doc[!(doc$DOC_mgL < 0 & doc$Notes_DOC=="rerun at later date"),]

##################################
### if negative, correct to zero #
###         flag = 4             #
##################################

for (i in 1:nrow(doc)) {
  if(doc$DOC_mgL[i] <0){
    doc$DOC_mgL[i] <- 0
    if(doc$Flag_DOC[i]>0){
      doc$Flag_DOC[i] <- paste0(doc$Flag_DOC[i], 4)
      
    }else{doc$Flag_DOC[i] <- 4}
  }
}

for (i in 1:nrow(doc)) {
  if(doc$DIC_mgL[i] <0){
    doc$DIC_mgL[i] <- 0
    if(doc$Flag_DIC[i]>0){
      doc$Flag_DIC[i] <- paste0(doc$Flag_DIC[i], 4)
      
    }else{doc$Flag_DIC[i] <- 4}
    
  }
}


for (i in 1:nrow(doc)) {
  if(doc$DC_mgL[i] <0){
    doc$DC_mgL[i] <- 0
    if(doc$Flag_DC[i]>0){
      doc$Flag_DC[i] <- paste0(doc$Flag_DC[i], 4)
      
    }else{doc$Flag_DC[i] <- 4}
    
  }
}

for (i in 1:nrow(doc)) {
  if(doc$DN_mgL[i] <0){
    doc$DN_mgL[i] <- 0
    if(doc$Flag_DN[i]>0){
      doc$Flag_DN[i] <- paste0(doc$Flag_DN[i], 4)
      
    }else{doc$Flag_DN[i] <- 4}
    
  }
}

##################################
###       average reps           #
###         flag = 7             #
##################################

# clean up DOC data
# look for duplicate values and average them, while adding flag = 7
doc_dups <- duplicated(doc[,1:4],fromLast = TRUE) & doc$Rep==""
table(doc_dups)['TRUE']

# create col to put average of reps into
doc$DOC_mgLAVG <- 'NA'
doc$DIC_mgLAVG <- 'NA'
doc$DC_mgLAVG <- 'NA'
doc$DN_mgLAVG <- 'NA'

# calculate the average of the two dups
for (i in 1:length(doc_dups)){
  if(doc_dups[i]=='TRUE'){
    doc$DOC_mgLAVG[i]= mean(c(doc$DOC_mgL[i], doc$DOC_mgL[i+1]))
    doc$DIC_mgLAVG[i]= mean(c(doc$DIC_mgL[i], doc$DIC_mgL[i+1]))
    doc$DC_mgLAVG[i]= mean(c(doc$DC_mgL[i], doc$DC_mgL[i+1]))
    doc$DN_mgLAVG[i]= mean(c(doc$DN_mgL[i], doc$DN_mgL[i+1]))
    # assign this to the other duplicate as well
    doc$DOC_mgLAVG[i+1]= mean(c(doc$DOC_mgL[i], doc$DOC_mgL[i+1]))
    doc$DIC_mgLAVG[i+1]= mean(c(doc$DIC_mgL[i], doc$DIC_mgL[i+1]))
    doc$DC_mgLAVG[i+1]= mean(c(doc$DC_mgL[i], doc$DC_mgL[i+1]))
    doc$DN_mgLAVG[i+1]= mean(c(doc$DN_mgL[i], doc$DN_mgL[i+1]))

    ifelse(doc$Flag_DOC[i]==0,  doc$Flag_DOC[i] <- 7, doc$Flag_DOC[i] <- 74)
    ifelse(doc$Flag_DIC[i]==0, doc$Flag_DIC[i] <- 7,doc$Flag_DIC[i] <- 74)
    ifelse(doc$Flag_DC[i]==0, doc$Flag_DC[i] <- 7, doc$Flag_DC[i] <- 74)
    ifelse(doc$Flag_DN[i]==0, doc$Flag_DN[i] <- 7, doc$Flag_DN[i] <- 74)
    ifelse(doc$Flag_DOC[i]==0, doc$Flag_DOC[i+1] <- 7, doc$Flag_DOC[i+1] <- doc$Flag_DOC[i])
    ifelse(doc$Flag_DIC[i]==0, doc$Flag_DIC[i+1] <- 7, doc$Flag_DIC[i+1] <- doc$Flag_DIC[i])
    ifelse(doc$Flag_DC[i]==0, doc$Flag_DC[i+1] <- 7, doc$Flag_DC[i+1] <- doc$Flag_DC[i])
    ifelse(doc$Flag_DN[i]==0, doc$Flag_DN[i+1] <- 7, doc$Flag_DN[i+1] <- doc$Flag_DN[i])
    
  }
}

# get rid of dups
doc <- doc[!(doc_dups=="TRUE"),]

# move the averaged data over to the original columns
for (i in 1:nrow(doc)) {
  if(!doc$DOC_mgLAVG[i]=='NA'){
    doc$DOC_mgL[i] <- doc$DOC_mgLAVG[i]
    doc$DIC_mgL[i] <- doc$DIC_mgLAVG[i]
    doc$DC_mgL[i] <-  doc$DC_mgLAVG[i]
    doc$DN_mgL[i] <-  doc$DN_mgLAVG[i]
  }
}

# and get rid of the average column since that is now in the normal data column
doc <- doc %>% select(-c(DOC_mgLAVG, DIC_mgLAVG, DC_mgLAVG, DN_mgLAVG))

#keep/format rep col
doc$Rep <- ifelse(doc$Rep=="R2",2,1)

#drop 20Jul20 5m because there are two samples with this label when one should be 8m
doc <- doc[!(doc$DateTime=="7/20/20 0:00" & doc$Depth_m==5),] 

###############################################################################
### if below detection, flag = 3; updated MDLs in 2021 - rolling spiked blank #
#                               MDLS (in mg/L)                                #  
#                          DIC     DOC     DC    DN                           #
#                          0.97   0.76   0.63   0.05                          #
###############################################################################
#note - reran in 2022 w/ updated MDL's! (really didn't change much)

# DIC
for (i in 1:nrow(doc)) {
  if(doc$DIC_mgL[i] <0.97){
    if(doc$Flag_DIC[i]>0){
      doc$Flag_DIC[i] <- paste0(doc$Flag_DIC[i], 3)
      
    }else{doc$Flag_DIC[i] <- 3}
  }
}

# DOC
for (i in 1:nrow(doc)) {
  if(doc$DOC_mgL[i] <0.76){
    if(doc$Flag_DOC[i]>0){
      doc$Flag_DOC[i] <- paste0(doc$Flag_DOC[i], 3)
      
    }
    else{doc$Flag_DOC[i] <- 3}
  }
}

# DC
for (i in 1:nrow(doc)) {
  if(doc$DC_mgL[i] < 0.63){
    if(doc$Flag_DC[i]>0){
      doc$Flag_DC[i] <- paste0(doc$Flag_DOC[i], 3)
      
    }else{doc$Flag_DC[i] <- 3}
  }
}

# DN
for (i in 1:nrow(doc)) {
  if(doc$DN_mgL[i] <0.05){
    if(doc$Flag_DN[i]>0){
      doc$Flag_DN[i] <- paste0(doc$Flag_DN[i], 3)
      
    }else{doc$Flag_DN[i] <- 3}
  }
}

#get rid of notes col
doc<- doc %>% select(-Notes_DOC)

############################################################
############################################################
#read in soluble data
np <- read.csv("./collation/2020/2020_soluble_NP_collation.csv")

#convert to character string for subsetting below
np$SampleID_lachat <- as.character(np$SampleID_lachat)
doc$SampleID_DOC <- as.character(doc$SampleID_DOC)

#order np df
np <- np %>% arrange(Reservoir, DateTime, Site, Depth_m)

#drop rows with NA values
np <- np[!is.na(np$NH4_ugL) | !is.na(np$PO4_ugL) | !is.na(np$NO3NO2_ugL),]

#drop samples that were rerun with super different values in first run (assuming we don't trust these as much as run 2)
np <- np[!(np$DateTime=="10/1/20 0:00" & np$Depth_m=="10" & np$Notes_lachat=="rerun at later date"),]

##############################################
#           set flags for N & P              #
# if negative, set to zero and set flag to 4 #
##############################################

# initialize flag columns
# no flag value = 0
np$Flag_NH4 <- 0
np$Flag_PO4 <- 0
np$Flag_NO3NO2 <- 0

for (i in 1:nrow(np)) {
  if(!is.na(np$NH4_ugL[i]) & np$NH4_ugL[i] <0){
    np$NH4_ugL[i] <- 0
    if(np$Flag_NH4[i]>0){
      np$Flag_NH4[i] <- paste0(np$Flag_NH4[i], 4)
      }else{np$Flag_NH4[i] <- 4}
  }
}

for (i in 1:nrow(np)) {
  if(!is.na(np$PO4_ugL[i]) & np$PO4_ugL[i] <0){
    np$PO4_ugL[i] <- 0
    if(np$Flag_PO4[i]>0){
      np$Flag_PO4[i] <- paste0(np$Flag_PO4[i], 4)
      
    }else{np$Flag_PO4[i] <- 4}
    
    
  }
}

for (i in 1:nrow(np)) {
  if(!is.na(np$NO3NO2_ugL[i]) & np$NO3NO2_ugL[i] <0){
    np$NO3NO2_ugL[i] <- 0
    if(np$Flag_NO3NO2[i]>0){
      np$Flag_NO3NO2[i] <- paste0(np$Flag_NO3NO2[i], 4)
      
    }else{np$Flag_NO3NO2[i] <- 4}
  }
}

####################
#  averaging dups  #
#    flag = 7      #
####################

# average dups and those with two reps
np_dups <- duplicated(np[,1:4]) & np$Rep==""
table(np_dups)['TRUE']

# create col to put average of reps into
np$NH4_ugLAVG <- 'NA'
np$PO4_ugLAVG <- 'NA'
np$NO3NO2_ugLAVG <- 'NA'

# calculate the average of the two dups
for (i in 1:length(np_dups)) {
  if(np_dups[i]=='TRUE'){
    np$NH4_ugLAVG[i]= mean(c(np$NH4_ugL[i], np$NH4_ugL[i-1]),na.rm=T)
    np$PO4_ugLAVG[i]= mean(c(np$PO4_ugL[i], np$PO4_ugL[i-1]),na.rm=T)
    np$NO3NO2_ugLAVG[i]= mean(c(np$NO3NO2_ugL[i], np$NO3NO2_ugL[i-1]),na.rm=T)
    # assign this to the other duplicate as well
    np$NH4_ugLAVG[i-1]= mean(c(np$NH4_ugL[i], np$NH4_ugL[i-1]),na.rm=T)
    np$PO4_ugLAVG[i-1]= mean(c(np$PO4_ugL[i], np$PO4_ugL[i-1]),na.rm=T)
    np$NO3NO2_ugLAVG[i-1]= mean(c(np$NO3NO2_ugL[i], np$NO3NO2_ugL[i-1]),na.rm=T)
    
    # flag as 7, average of two reps (conditional to prevent 7 flag if one set of dups include NA)
    ifelse(is.na(np$NH4_ugL[i]) | is.na(np$PO4_ugL[i]) | is.na(np$NO3NO2_ugL[i]), np$Flag_NH4[i] <- np$Flag_NH4[i], np$Flag_NH4[i] <- paste0(7,np$Flag_NH4[i]))
    ifelse(is.na(np$NH4_ugL[i]) | is.na(np$PO4_ugL[i]) | is.na(np$NO3NO2_ugL[i]), np$Flag_PO4[i] <-np$Flag_PO4[i], np$Flag_PO4[i] <- paste0(7,np$Flag_PO4[i]))
    ifelse(is.na(np$NH4_ugL[i]) | is.na(np$PO4_ugL[i]) | is.na(np$NO3NO2_ugL[i]), np$Flag_NO3NO2[i] <- np$Flag_NO3NO2[i], np$Flag_NO3NO2[i] <- paste0(7,np$Flag_NO3NO2[i]))
    ifelse(is.na(np$NH4_ugL[i-1]) | is.na(np$PO4_ugL[i-1]) | is.na(np$NO3NO2_ugL[i-1]) , np$Flag_NH4[i-1] <- np$Flag_NH4[i-1], np$Flag_NH4[i-1] <- paste0(7,np$Flag_NH4[i-1]))
    ifelse(is.na(np$NH4_ugL[i-1]) | is.na(np$PO4_ugL[i-1]) | is.na(np$NO3NO2_ugL[i-1]), np$Flag_PO4[i-1] <- np$Flag_PO4[i-1], np$Flag_PO4[i-1] <- paste0(7,np$Flag_PO4[i-1]))
    ifelse(is.na(np$NH4_ugL[i-1]) | is.na(np$PO4_ugL[i-1]) | is.na(np$NO3NO2_ugL[i-1]), np$Flag_NO3NO2[i-1] <- np$Flag_NO3NO2[i-1], np$Flag_NO3NO2[i-1] <- paste0(7,np$Flag_NO3NO2[i-1]))
  }  
}


# get rid of dups
np_nodups <- np[!np_dups,]

# move the averaged data over to the original columns
for (i in 1:nrow(np_nodups)) {
  if(!np_nodups$NH4_ugLAVG[i]=='NA'){
    np_nodups$NH4_ugL[i] <- np_nodups$NH4_ugLAVG[i]
    np_nodups$PO4_ugL[i] <- np_nodups$PO4_ugLAVG[i]
    np_nodups$NO3NO2_ugL[i] <-  np_nodups$NO3NO2_ugLAVG[i]
  }
}

# and get rid of the average column since that is now in the normal data column
np_nodups <- np_nodups %>% select(-c(NH4_ugLAVG, PO4_ugLAVG, NO3NO2_ugLAVG))
# call it np again for coding ease
np <- np_nodups

#manually average the one weird sample that was accidentally run 3 times...
np$NH4_ugL[np$DateTime=="9/30/20 0:00" & np$Depth_m==9.0] <- 849
np$PO4_ugL[np$DateTime=="9/30/20 0:00" & np$Depth_m==9.0] <- 5.2
np$NO3NO2_ugL[np$DateTime=="9/30/20 0:00" & np$Depth_m==9.0] <- 21.1

#keep/format rep col
np$Rep <- ifelse(np$Rep=="R2",2,1)

#change 70 flags to 7
np$Flag_NH4[np$Flag_NH4=="70"] <- "7"
np$Flag_PO4[np$Flag_PO4=="70"] <- "7"
np$Flag_NO3NO2[np$Flag_NO3NO2=="70"] <- "7"

#drop 20Jul20 5m because there are two samples with this label when one should be 8m
np <- np[!(np$DateTime=="7/20/20 0:00" & np$Depth_m==5),] 

#####################################################################
#               if below detection, flag as 3                       #
# 27Jan21, using the following MDL's: ("MDL rolling spiked blanks") #
#                      NH4  PO4   NO3                               #
#                      9.6  3.0   4.5                               #
#####################################################################
#note - reran in 2022 w/ updated MDL's! (only PO4 changed a bit)

for (i in 1:nrow(np)) {
  if(np$NH4_ugL[i] <9.6){
    if(np$Flag_NH4[i]>0){
      np$Flag_NH4[i] <- paste0(np$Flag_NH4[i], 3)
      
    }else{np$Flag_NH4[i] <- 3}
  }
}

for (i in 1:nrow(np)) {
  if(np$PO4_ugL[i] <3.7){
    if(np$Flag_PO4[i]>0){
      np$Flag_PO4[i] <- paste0(np$Flag_PO4[i], 3)
      
    }else{np$Flag_PO4[i] <- 3}
  }
}

for (i in 1:nrow(np)) {
  if(np$NO3NO2_ugL[i] < 4.5){
    if(np$Flag_NO3NO2[i]>0){
      np$Flag_NO3NO2[i] <- paste0(np$Flag_NO3NO2[i], 3)
      
    }else{np$Flag_NO3NO2[i] <- 3}
  }
}

#get rid of notes col
np<- np %>% select(-Notes_lachat)

##########################################################
#add demonic intrusion flags for ?? 
#np$Flag_NO3NO2[which(np$DateTime=='2019-05-23 12:00:00' & np$Depth_m==3.0 & np$NO3NO2_ugL==30.5)] <- "5"



###########################
# join nutrients together #
###########################

#rename PO4_ugL to SRP_ugL and Flag_PO4 to Flag_SRP
colnames(np)[which(names(np) == "PO4_ugL")] <- "SRP_ugL"
colnames(np)[which(names(np) == "Flag_PO4")] <- "Flag_SRP"

#new df with solubles, totals, and DOC
solubles_and_DOC <- full_join(np, doc, by = c('Reservoir', 'Site', 'DateTime',  'Depth_m','Rep'))
chem <- full_join(TNTP, solubles_and_DOC, by = c('Reservoir', 'Site', 'DateTime',  'Depth_m', 'Rep'))

#get rid of notes and run date
chem <- chem %>% select(-c(RunDate_DOC,RunDate.x,RunDate.y,Notes_lachat,SampleID_DOC,SampleID_lachat))

chem <- chem %>% mutate(TP_ugL = as.numeric(TP_ugL),
                        TN_ugL = as.numeric(TN_ugL),
                        NH4_ugL = as.numeric(NH4_ugL),
                        SRP_ugL = as.numeric(SRP_ugL),
                        NO3NO2_ugL = as.numeric(NO3NO2_ugL),
                        DOC_mgL = as.numeric(DOC_mgL),
                        DIC_mgL = as.numeric(DIC_mgL),
                        DC_mgL = as.numeric(DC_mgL),
                        DN_mgL = as.numeric(DN_mgL)
)

# Round values to specified precision (based on 2018 data on EDI)
chem_final <-chem %>% mutate(SRP_ugL = round(SRP_ugL, 0), 
                NH4_ugL = round(NH4_ugL, 0),
                NO3NO2_ugL = round(NO3NO2_ugL, 0),
                TP_ugL = round(TP_ugL, 1),  
                TN_ugL = round(TN_ugL, 1),  
                DOC_mgL = round(DOC_mgL, 1),
                DIC_mgL = round(DIC_mgL, 1),
                DC_mgL = round(DC_mgL, 1),
                DN_mgL = round(DN_mgL, 3)) 

chem_final <- chem_final %>% mutate(Flag_TP= as.numeric(Flag_TP),
                        Flag_TN= as.numeric(Flag_TN),
                        Flag_NH4 = as.numeric(Flag_NH4),
                        Flag_SRP = as.numeric(Flag_SRP),
                        Flag_NO3NO2 = as.numeric(Flag_NO3NO2),
                        Flag_DOC = as.numeric(Flag_DOC),
                        Flag_DIC = as.numeric(Flag_DIC),
                        Flag_DC = as.numeric(Flag_DC),
                        Flag_DN = as.numeric(Flag_DN)
)

#add in flag for DateTime
chem_final$Flag_DateTime <- 1

#change all NAs to 0 in flag columns
chem_final$Flag_DateTime <- ifelse(is.na(chem_final$Flag_DateTime), 0, chem_final$Flag_DateTime)
chem_final$Flag_DC <- ifelse(is.na(chem_final$Flag_DC), 0, chem_final$Flag_DC)
chem_final$Flag_DN <- ifelse(is.na(chem_final$Flag_DN), 0, chem_final$Flag_DN)
chem_final$Flag_DIC <- ifelse(is.na(chem_final$Flag_DIC), 0, chem_final$Flag_DIC)
chem_final$Flag_DOC <- ifelse(is.na(chem_final$Flag_DOC), 0, chem_final$Flag_DOC)
chem_final$Flag_TP <- ifelse(is.na(chem_final$Flag_TP), 0, chem_final$Flag_TP)
chem_final$Flag_TN <- ifelse(is.na(chem_final$Flag_TN), 0, chem_final$Flag_TN)
chem_final$Flag_NH4 <- ifelse(is.na(chem_final$Flag_NH4), 0, chem_final$Flag_NH4)
chem_final$Flag_NO3NO2 <- ifelse(is.na(chem_final$Flag_NO3NO2), 0, chem_final$Flag_NO3NO2)
chem_final$Flag_SRP <- ifelse(is.na(chem_final$Flag_SRP), 0, chem_final$Flag_SRP)


chem_final$DateTime <- strptime(chem_final$DateTime,"%m/%d/%y")
chem_final$Hour <- "12:00:00"
#combine hour and datetime
chem_final$DateTime <- as.POSIXct(as.character(paste(chem_final$DateTime, chem_final$Hour)), format="%Y-%m-%d %H:%M:%S")
#drop hour col
chem_final <- chem_final %>% select(-c(Hour)) 

#order chem
chem_final<- chem_final %>% arrange(Reservoir, DateTime, Site, Depth_m)

#replace datetime column with correct times 
times <- read.csv("./collation/2020/chemistry_2020times.csv")
chem_final$DateTime[chem_final$DateTime >= "2020-01-01"] <- times$DateTime[times$DateTime >= "2020-01-01"]
chem_final$Flag_DateTime[chem_final$DateTime >= "2020-01-01"] <- times$Flag_DateTime[times$DateTime >= "2020-01-01"]

#convert date back to character
chem_final$DateTime <- as.character(chem_final$DateTime)

write.csv(chem_final, "./FinalData/2020_chemistry_collation_final_nocommas.csv")
