# 2019 nutrient chemistry collation
#includes TNTP, DOC, and np 2019 
#Note: flags are aggregated without commas for final EDI upload

library(tidyverse)
library(dplyr)

#np <- read.csv('./Data/analytical chemistry/Lachat 2019/2019_collating_soluble_NP.csv')
#doc <- read.csv('./Data/analytical chemistry/TOC Analyzer 2019/2019_collating_TOC.csv')
TNTP <- read.csv("/Users/heatherwander/Documents/VirginiaTech/research/NutrientData/2019 nutrient data collation/2019_TNTP_collation.csv")

#drop samplID col 
TNTP <- TNTP [,!(names(TNTP) %in% c("SampleID"))]

TNTP$DateTime <- strptime(TNTP$DateTime,"%m/%d/%y")
TNTP$Hour <- "12:00:00"
#combine hour and datetime
TNTP$DateTime <- as.POSIXct(as.character(paste(TNTP$DateTime, TNTP$Hour)), format="%Y-%m-%d %H:%M:%S")
#convert back to character
TNTP$DateTime <- as.character(TNTP$DateTime)
#drop hour col
TNTP <- TNTP %>% select(-c(Hour)) 

# set flags for TN & TP
###################################################
# add 7 for rows that will be averaged
###################################################
# create flag columns
# no flag value = 0
TNTP$Flag_TP <- 0
TNTP$Flag_TN <- 0

#order TNTP df
TNTP <- TNTP %>% arrange(Reservoir, DateTime, Site, Depth_m,Rep)

# look for duplicate values and average them, while adding flag = 7
TNTP_dups <- duplicated(TNTP[,1:4]) & is.na(TNTP$Rep)
table(TNTP_dups)['TRUE']

# create col to put average of reps into
TNTP$TP_ugL_AVG <- 'NA'
TNTP$TN_ugL_AVG <- 'NA'

########## AVERAGING REPS ####
#average all rep 1 and rep 2 data
for (i in 1:length(TNTP_dups)) {
  if(TNTP_dups[i]=='TRUE'){
    TNTP$TP_ugL_AVG[i]= mean(c(TNTP$TP_ugL[i], TNTP$TP_ugL[i-1]))
    TNTP$TN_ugL_AVG[i]= mean(c(TNTP$TN_ugL[i], TNTP$TN_ugL[i-1]))
    # assign this to the other duplicate as well
    TNTP$TP_ugL_AVG[i-1]= mean(c(TNTP$TP_ugL[i], TNTP$TP_ugL[i-1]))
    TNTP$TN_ugL_AVG[i-1]= mean(c(TNTP$TN_ugL[i], TNTP$TN_ugL[i-1]))
  
    # flag as 7, average of two reps
    TNTP$Flag_TP[i] <- 7
    TNTP$Flag_TN[i] <- 7
    TNTP$Flag_TP[i-1] <- 7
    TNTP$Flag_TN[i-1] <- 7
  }  
}

# remove dups 
TNTP <- TNTP[!TNTP_dups,]

# move the averaged data over to the original columns
for (i in 1:nrow(TNTP)) {
  if(!TNTP$TP_ugL_AVG[i]=='NA'){
    TNTP$TP_ugL[i] <- TNTP$TP_ugL_AVG[i]
    TNTP$TN_ugL[i] <- TNTP$TN_ugL_AVG[i]
  }
}

#remove avg columns at end 
TNTP <- TNTP[,-c(13,14)]

#####################################################################
# if below detection, flag as 3 
# mean over 2 years  (06Apr20), using the following MDL's: 
#  TP     TN
#  4.1   21.1
#####################################################################

for (i in 1:nrow(TNTP)) {
  ifelse(TNTP$TP_ugL[i] < 4.1 & TNTP$Flag_TP[i]==7,
    TNTP$Flag_TP[i] <- "73",
  ifelse(TNTP$TP_ugL[i] < 4.1,
    TNTP$Flag_TP[i] <- 3, TNTP$Flag_TP[i]))
  }


for (i in 1:nrow(TNTP)) {
  ifelse(TNTP$TN_ugL[i] < 21.1 & TNTP$Flag_TN[i]==7,
    TNTP$Flag_TN[i] <- "73",
  ifelse(TNTP$TN_ugL[i] < 21.1,
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

#delete B_50 3Jun19 11m TP data because way too high so probably due to hitting sediment with Van Dorn
TNTP$TP_ugL[TNTP$DateTime=="2019-06-06 12:00:00" & TNTP$Depth_m==11] <- NA

#keep/format rep col
TNTP$Rep[is.na(TNTP$Rep)] <- 0

TNTP$Rep <- ifelse(TNTP$Rep=="R2",2,1)

##########################################################
##########################################################
#### WMW code for soluble/DOC collation
##########################################################
#read in 2019_chemistry_collation with DOC and solubles so that we can add TN TP
np <- read.csv("/Users/heatherwander/Documents/VirginiaTech/research/NutrientData/2019 nutrient data collation/2019_collating_soluble_NP.csv")
doc <- read.csv("/Users/heatherwander/Documents/VirginiaTech/research/NutrientData/2019 nutrient data collation/2019_collating_TOC.csv")

#function to select rows based on characters from the end
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#convert to character string for subsetting below
np$SampleID_lachat <- as.character(np$SampleID_lachat)
doc$SampleID_DOC <- as.character(doc$SampleID_DOC)

#delete all np magic sensor samples from 14Aug2019
np_samples_with_times <- substrRight(np$SampleID_lachat, 3)
np_magic.rows <- which(np_samples_with_times == ":00" | np_samples_with_times == "00?")
#delete magic rows
np <- np[-c(np_magic.rows),]

#make sure np analyte columns are numeric
np$NH4_ugL <- as.numeric(np$NH4_ugL) 
np$PO4_ugL <- as.numeric(np$PO4_ugL) 
np$NO3NO2_ugL <- as.numeric(np$NO3NO2_ugL) 

#delete all np magic sensor samples from 14Aug2019
doc_samples_with_times <- substrRight(doc$SampleID_DOC, 3)
doc_magic.rows <- which(doc_samples_with_times == ":00" | doc_samples_with_times == "00?")
#delete magic rows
doc <- doc[-c(doc_magic.rows),]

# create flag columns
# no flag value = 0
doc$Flag_DC <- 0
doc$Flag_DIC <- 0
doc$Flag_DOC <- 0
doc$Flag_DN <- 0

###################################################################
### if negative, correct to zero
### flag = 4
###################################################################

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

##########################################
### if same sample was run twice, average
### flag = 7
##########################################
# clean up DOC data
# look for duplicate values and average them, while adding flag = 7
doc_dups <- duplicated(doc[,1:5]) & doc$Rep==""
table(doc_dups)['TRUE']

# create col to put average of reps into
doc$DOC_mgLAVG <- 'NA'
doc$DIC_mgLAVG <- 'NA'
doc$DC_mgLAVG <- 'NA'
doc$DN_mgLAVG <- 'NA'


# calculate the average of the two dups
for (i in 1:length(doc_dups)) {
  if(doc_dups[i]=='TRUE'){
    doc$DOC_mgLAVG[i]= mean(c(doc$DOC_mgL[i], doc$DOC_mgL[i-1]))
    doc$DIC_mgLAVG[i]= mean(c(doc$DIC_mgL[i], doc$DIC_mgL[i-1]))
    doc$DC_mgLAVG[i]= mean(c(doc$DC_mgL[i], doc$DC_mgL[i-1]))
    doc$DN_mgLAVG[i]= mean(c(doc$DN_mgL[i], doc$DN_mgL[i-1]))
    # assign this to the other duplicate as well
    doc$DOC_mgLAVG[i-1]= mean(c(doc$DOC_mgL[i], doc$DOC_mgL[i-1]))
    doc$DIC_mgLAVG[i-1]= mean(c(doc$DIC_mgL[i], doc$DIC_mgL[i-1]))
    doc$DC_mgLAVG[i-1]= mean(c(doc$DC_mgL[i], doc$DC_mgL[i-1]))
    doc$DN_mgLAVG[i-1]= mean(c(doc$DN_mgL[i], doc$DN_mgL[i-1]))
    # flag as 7, average of two reps
    ifelse(doc$Flag_DOC[i]==4, doc$Flag_DOC[i] <- 74,doc$Flag_DOC[i] <- 7)
    ifelse(doc$Flag_DIC[i]==4, doc$Flag_DIC[i] <- 74,doc$Flag_DIC[i] <- 7)
    ifelse(doc$Flag_DC[i]==4,  doc$Flag_DC[i] <- 74,doc$Flag_DC[i] <- 7)
    ifelse(doc$Flag_DN[i]==4,  doc$Flag_DN[i] <- 74,doc$Flag_DN[i] <- 7)
    ifelse(doc$Flag_DOC[i]==4, doc$Flag_DOC[i-1] <- doc$Flag_DOC[i], doc$Flag_DOC[i-1] <- doc$Flag_DOC[i])
    ifelse(doc$Flag_DIC[i]==4, doc$Flag_DIC[i-1] <- doc$Flag_DIC[i], doc$Flag_DIC[i-1] <- doc$Flag_DIC[i])
    ifelse(doc$Flag_DC[i]==4,  doc$Flag_DC[i-1] <- doc$Flag_DC[i], doc$Flag_DC[i-1] <- doc$Flag_DC[i])
    ifelse(doc$Flag_DN[i]==4,  doc$Flag_DN[i-1] <- doc$Flag_DN[i], doc$Flag_DN[i-1] <- doc$Flag_DN[i])
  }  
}

# get rid of dups
doc_nodups <- doc[!doc_dups,]

# move the averaged data over to the original columns
for (i in 1:nrow(doc_nodups)) {
  if(!doc_nodups$DOC_mgLAVG[i]=='NA'){
    doc_nodups$DOC_mgL[i] <- doc_nodups$DOC_mgLAVG[i]
    doc_nodups$DIC_mgL[i] <- doc_nodups$DIC_mgLAVG[i]
    doc_nodups$DC_mgL[i] <-  doc_nodups$DC_mgLAVG[i]
    doc_nodups$DN_mgL[i] <-  doc_nodups$DN_mgLAVG[i]
  }
}

# and get rid of the average column since that is now in the normal data column
doc_nodups <- doc_nodups %>% select(-c(DOC_mgLAVG, DIC_mgLAVG, DC_mgLAVG, DN_mgLAVG))
# call is doc again for coding ease
doc <- doc_nodups

## a few more flag = 7 cases to do manually
#doc$Flag_DC[which(doc$Reservoir=='BVR' & doc$Site=='100' &  doc$DateTime=="2019-01-21 12:00:00" & doc$Depth_m=='0.1'  & doc$Rep=='R2')] <- 7
#doc$Flag_DN[which(doc$Reservoir=='BVR' & doc$Site=='100' &  doc$DateTime=="2019-01-21 12:00:00" & doc$Depth_m=='0.1'  & doc$Rep=='R2')] <- 7
#doc$Flag_DOC[which(doc$Reservoir=='BVR' & doc$Site=='100' & doc$DateTime=="2019-01-21 12:00:00" & doc$Depth_m=='0.1' & doc$Rep=='R2')] <- 7
#doc$Flag_DIC[which(doc$Reservoir=='BVR' & doc$Site=='100' & doc$DateTime=="2019-01-21 12:00:00" & doc$Depth_m=='0.1' & doc$Rep=='R2')] <- 7
#
#doc$Flag_DC[which(doc$Reservoir== 'FCR' & doc$Site=='50' & doc$DateTime=="2019-04-01 12:00:00" & doc$Depth_m=='6.2' )] <- 7
#doc$Flag_DN[which(doc$Reservoir== 'FCR' & doc$Site=='50' & doc$DateTime=="2019-04-01 12:00:00" & doc$Depth_m=='6.2' )] <- 7
#doc$Flag_DOC[which(doc$Reservoir=='FCR' & doc$Site=='50' & doc$DateTime=="2019-04-01 12:00:00" & doc$Depth_m=='6.2' )] <- 7
#doc$Flag_DIC[which(doc$Reservoir=='FCR' & doc$Site=='50' & doc$DateTime=="2019-04-01 12:00:00" & doc$Depth_m=='6.2' )] <- 7
#
#doc$Flag_DC[which(doc$Reservoir== 'FCR' & doc$Site=='101' & doc$DateTime=="2019-07-18 12:00:00" & doc$Depth_m=='0.1'& doc$Rep=='R1')] <- 7
#doc$Flag_DN[which(doc$Reservoir== 'FCR' & doc$Site=='101' & doc$DateTime=="2019-07-18 12:00:00" & doc$Depth_m=='0.1'& doc$Rep=='R1')] <- 7
#doc$Flag_DOC[which(doc$Reservoir=='FCR' & doc$Site=='101' & doc$DateTime=="2019-07-18 12:00:00" & doc$Depth_m=='0.1'& doc$Rep=='R1')] <- 7
#doc$Flag_DIC[which(doc$Reservoir=='FCR' & doc$Site=='101' & doc$DateTime=="2019-07-18 12:00:00" & doc$Depth_m=='0.1'& doc$Rep=='R1')] <- 7
#
#doc$Flag_DC[which(doc$Reservoir== 'FCR' & doc$Site=='50' & doc$DateTime=="2019-06-03 12:00:00" & doc$Depth_m=='0.1')] <- 7
#doc$Flag_DN[which(doc$Reservoir== 'FCR' & doc$Site=='50' & doc$DateTime=="2019-06-03 12:00:00" & doc$Depth_m=='0.1')] <- 7
#doc$Flag_DOC[which(doc$Reservoir=='FCR' & doc$Site=='50' & doc$DateTime=="2019-06-03 12:00:00" & doc$Depth_m=='0.1')] <- 7
#doc$Flag_DIC[which(doc$Reservoir=='FCR' & doc$Site=='50' & doc$DateTime=="2019-06-03 12:00:00" & doc$Depth_m=='0.1')] <- 7
#
#delete 8Jun19 FCR 50 0.1 m data (wrong date/reservoir so unknown sample ID)
doc <- doc[!(doc$DateTime=='6/8/19 12:00'),]
#delete 15Oct FCR 1.6 m because this is the wrong date so unknown sample ID
doc <- doc[!(doc$DateTime=='10/15/19 12:00'),]
#delete 11Jun B_50 6 m because wrong date so unknown sample ID
doc <- doc[!(doc$DateTime=='6/11/19 12:00'),]
#delete 27May19 FCR 50 3.8 m data because wrong date so unknown sample ID
doc <- doc[!(doc$DateTime=='3/27/19 12:00'),]


###################################################################
### if below detection, flag = 3
###################################################################
# MDLS (in mg/L)
# DIC     DOC     DC      DN
# 0.168   0.110   0.185   0.051

# DIC
for (i in 1:nrow(doc)) {
  if(doc$DIC_mgL[i] <0.168){
    if(doc$Flag_DIC[i]>0){
      doc$Flag_DIC[i] <- paste0(doc$Flag_DIC[i], 3)
      
    }else{doc$Flag_DIC[i] <- 3}
  }
}

# DOC
for (i in 1:nrow(doc)) {
  if(doc$DOC_mgL[i] <0.110){
    if(doc$Flag_DOC[i]>0){
      doc$Flag_DOC[i] <- paste0(doc$Flag_DOC[i], 3)
      
    }
    else{doc$Flag_DOC[i] <- 3}
  }
}

# DC
for (i in 1:nrow(doc)) {
  if(doc$DC_mgL[i] < 0.185){
    if(doc$Flag_DC[i]>0){
      doc$Flag_DC[i] <- paste0(doc$Flag_DOC[i], 3)
      
    }else{doc$Flag_DC[i] <- 3}
  }
}

# DN
for (i in 1:nrow(doc)) {
  if(doc$DN_mgL[i] <0.051){
    if(doc$Flag_DN[i]>0){
      doc$Flag_DN[i] <- paste0(doc$Flag_DN[i], 3)
      
    }else{doc$Flag_DN[i] <- 3}
  }
}

#keep/format rep col
doc$Rep <- ifelse(doc$Rep=="R2",2,1)

###############################################################################################################################################################################
#clean up soluble N & P data
# soluble N & P checking dups for individual cases

# look for dups
np_dups_list <- np[duplicated(np[,1:5]),] # duplicates

# F 18Mar19 F100 0.1m run twice within same run
# using second reading because first reading presumably had messy integrations
np <- np[-33,]

# 22Apr19 F50 8.0m run twice on different days, average the two samples, done below
# 03Jun19 F200 0.1m run twice on different days, average the two samples, done below
# 18Jul19 B50 0.1m R1 run twice on different days, average the two samples, done below


# 14Aug19 F50 5.0m run twice on different days, realllly different NH4 data, rack map says unclear label, delete Nov 18 sample run
np <- np[!(np$DateTime=='8/14/19 12:00' & np$RunDate=='11/18/19'),]

# F 04Oct19 F20 0.1 run twice within same run
# using second reading because first reading had messy integrations
np <- np[!(np$DateTime=='10/4/19 12:00' & np$Site==20 & np$NH4_ugL==9.100),]

###################################################
# set flags for N & P
###################################################
# if negative, set to zero and set flag to 4
###################################################
# initialize flag columns
# no flag value = 0
np$Flag_NH4 <- 0
np$Flag_PO4 <- 0
np$Flag_NO3NO2 <- 0


for (i in 1:nrow(np)) {
  if(np$NH4_ugL[i] <0){
    np$NH4_ugL[i] <- 0
    if(np$Flag_NH4[i]>0){
      np$Flag_NH4[i] <- paste0(np$Flag_NH4[i], 4)
      
    }else{np$Flag_NH4[i] <- 4}
  }
}

for (i in 1:nrow(np)) {
  if(np$PO4_ugL[i] <0){
    np$PO4_ugL[i] <- 0
    if(np$Flag_PO4[i]>0){
      np$Flag_PO4[i] <- paste0(np$Flag_PO4[i], 4)
      
    }else{np$Flag_PO4[i] <- 4}
    
    
  }
}

for (i in 1:nrow(np)) {
  if(np$NO3NO2_ugL[i] <0){
    np$NO3NO2_ugL[i] <- 0
    if(np$Flag_NO3NO2[i]>0){
      np$Flag_NO3NO2[i] <- paste0(np$Flag_NO3NO2[i], 4)
      
    }else{np$Flag_NO3NO2[i] <- 4}
  }
}

# 25Oct19 F50 5.0m, run twice on different days, average the two samples, done below
# 14Aug19 F50 0.1m run twice on different days, mostly similar btw runs
# 14Aug19 F50 1.6m run twice on different days, two different samples run on same day different times?
# 14Aug19 F50 9.0m run twice on different days, really different NH4 data btw samples


# now average the remaining dups and those with two reps
np_dups <- duplicated(np[,1:5]) & np$Rep=="" # duplicates without reps
table(np_dups)['TRUE']

# create col to put average of reps into
np$NH4_ugLAVG <- 'NA'
np$PO4_ugLAVG <- 'NA'
np$NO3NO2_ugLAVG <- 'NA'

# calculate the average of the two dups
for (i in 1:length(np_dups)) {
  if(np_dups[i]=='TRUE'){
    np$NH4_ugLAVG[i]= mean(c(np$NH4_ugL[i], np$NH4_ugL[i-1]))
    np$PO4_ugLAVG[i]= mean(c(np$PO4_ugL[i], np$PO4_ugL[i-1]))
    np$NO3NO2_ugLAVG[i]= mean(c(np$NO3NO2_ugL[i], np$NO3NO2_ugL[i-1]))
    # assign this to the other duplicate as well
    np$NH4_ugLAVG[i-1]= mean(c(np$NH4_ugL[i], np$NH4_ugL[i-1]))
    np$PO4_ugLAVG[i-1]= mean(c(np$PO4_ugL[i], np$PO4_ugL[i-1]))
    np$NO3NO2_ugLAVG[i-1]= mean(c(np$NO3NO2_ugL[i], np$NO3NO2_ugL[i-1]))
    # flag as 7, average of two reps
    ifelse(np$Flag_NH4[i]==4, np$Flag_NH4[i] <- 74, np$Flag_NH4[i] <- 7)
    ifelse(np$Flag_PO4[i]==4, np$Flag_PO4[i] <- 74, np$Flag_PO4[i] <- 7)
    ifelse(np$Flag_NO3NO2[i]==4, np$Flag_NO3NO2[i] <- 74, np$Flag_NO3NO2[i] <- 7)
    ifelse(np$Flag_NH4[i]==4, np$Flag_NH4[i-1] <- np$Flag_NH4[i], np$Flag_NH4[i-1] <- np$Flag_NH4[i])
    ifelse(np$Flag_PO4[i]==4, np$Flag_PO4[i-1] <- np$Flag_PO4[i], np$Flag_PO4[i-1] <- np$Flag_PO4[i])
    ifelse(np$Flag_NO3NO2[i]==4, np$Flag_NO3NO2[i-1] <- np$Flag_NO3NO2[i], np$Flag_NO3NO2[i-1] <- np$Flag_NO3NO2[i])
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
# call is doc again for coding ease
np <- np_nodups

#####################################################################
# if below detection, flag as 3
# 10-Mar-20, using the following MDL's:  (these should be updated)
#  NH4  PO4   NO3
#  3.7  2.1   2.9 
#####################################################################

for (i in 1:nrow(np)) {
  if(np$NH4_ugL[i] <3.7){
    if(np$Flag_NH4[i]>0){
      np$Flag_NH4[i] <- paste0(np$Flag_NH4[i], 3)
      
    }else{np$Flag_NH4[i] <- 3}
  }
}

for (i in 1:nrow(np)) {
  if(np$PO4_ugL[i] <2.1){
    if(np$Flag_PO4[i]>0){
      np$Flag_PO4[i] <- paste0(np$Flag_PO4[i], 3)
      
    }else{np$Flag_PO4[i] <- 3}
  }
}

for (i in 1:nrow(np)) {
  if(np$NO3NO2_ugL[i] < 2.9){
    if(np$Flag_NO3NO2[i]>0){
      np$Flag_NO3NO2[i] <- paste0(np$Flag_NO3NO2[i], 3)
      
    }else{np$Flag_NO3NO2[i] <- 3}
  }
}

##########################################################
#add demonic intrusion flag for B_50 23May 3m NO3NO2 data (abnormally high compared to other epi depths near this time, but nothing in run to suggest data is not real)
np$Flag_NO3NO2[which(np$DateTime=='5/23/19 12:00' & np$Depth_m==3.0 & np$NO3NO2_ugL==30.5)] <- "5"

#keep/format rep col
np$Rep <- ifelse(np$Rep=="R2",2,1)

##########################################################
##########################################################
########################################################################
# join nutrients together
#######################################################################
#rename PO4_ugL to SRP_ugL and Flag_PO4 to Flag_SRP
colnames(np)[which(names(np) == "PO4_ugL")] <- "SRP_ugL"
colnames(np)[which(names(np) == "Flag_PO4")] <- "Flag_SRP"

#new df with solubles, totals, and DOC
solubles_and_DOC <- full_join(np, doc, by = c('Reservoir', 'Site', 'DateTime',  'Depth_m', 'Rep'))
chem <- full_join(TNTP, solubles_and_DOC, by = c('Reservoir', 'Site', 'DateTime',  'Depth_m','Rep'))

#change Site ISCO to Site 100.1
chem$Site <- as.factor(chem$Site)
levels(chem$Site)[levels(chem$Site)=="ISCO"] <- "100.1"

#delete rows where site number is s and s 0 s
chem <- chem[!(chem$Site== "s" | chem$Site=="s 0 s"),] 

#get rid of notes and run date
chem <- chem %>% select(-c(RunDate.x, RunDate.y, RunDate_DOC, Notes_lachat, Notes_DOC,
                           Notes_TP, Notes_TN, SampleID_DOC, SampleID_lachat))

#change NA flags to 0
chem$Flag_DC[is.na(chem$Flag_DC)] <- 0
chem$Flag_DOC[is.na(chem$Flag_DOC)] <- 0
chem$Flag_DN[is.na(chem$Flag_DN)] <- 0
chem$Flag_DIC[is.na(chem$Flag_DIC)] <- 0
chem$Flag_NH4[is.na(chem$Flag_NH4)] <- 0
chem$Flag_NO3NO2[is.na(chem$Flag_NO3NO2)] <- 0
chem$Flag_SRP[is.na(chem$Flag_SRP)] <- 0
chem$Flag_TN[is.na(chem$Flag_TN)] <- 0
chem$Flag_TP[is.na(chem$Flag_TP)] <- 0

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

#order chem
 chem_final<- chem_final %>% arrange(Reservoir, DateTime, Site, Depth_m)

 #read in 2019 file with flags for datetime (note that the data is not to be trusted!)
 flag_datetime_2019 <- read.csv("/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2019/2019_chemistry_collation_final_nocommas_OLD.csv")
 
 #add flag_datetime to chem_final
 chem_final$Flag_DateTime <- flag_datetime_2019$Flag_DateTime
 
 #replace time with actual time when available
 chem_final$DateTime <- flag_datetime_2019$DateTime
 
write.csv(chem_final, "./2019_chemistry_collation_final_nocommas.csv")
