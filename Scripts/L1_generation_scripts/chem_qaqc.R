# Chemistry QAQC/collation
# last edited: HLW
# 2023-12-19

#install.packages('pacman') ## Run this line if you don't have "pacman" package installed
pacman::p_load(tidyverse, lubridate, dplyr) 

#list of all data flags
# 0 - NOT SUSPECT
# 1 - SAMPLE NOT TAKEN
# 2 - INSTRUMENT MALFUNCTION
# 3 - SAMPLE BELOW DETECTION
# 4 - NEGATIVE VALUE SET TO ZERO
# 5 - DEMONIC INTRUSION
# 6 - NON-STANDARD METHOD
# 7 - SAMPLE RUN MULTIPLE TIMES AND VALUES AVERAGED
# 8 - SAMPLE RUN USING NPOC METHOD DUE TO HIGH IC VALUES
# 9 - SUSPECT SAMPLE

#------------------------------------------------------------------------------#
#read in TNTP
TNTP <- read_csv("./Data/DataNotYetUploadedToEDI/NutrientData/collation/2023/2023_TNTP_collation.csv")

#drop samplID col 
TNTP <- TNTP [,!(names(TNTP) %in% c("SampleID_lachat"))]

#drop defrost tag column
TNTP <- TNTP [,!(names(TNTP) %in% c("Notes_defrost.tag"))]

#drop rows with NA values
TNTP <- TNTP[!is.na(TNTP$TP_ugL) | !is.na(TNTP$TN_ugL) ,]

#add DateTime flag
TNTP$DateTime <- as.POSIXct(TNTP$DateTime, format="%m/%d/%y %H:%M")
#TNTP$DateTime <- mdy_hm(TNTP$DateTime)
TNTP$Flag_DateTime <- ifelse(TNTP$Notes_lachat=="Flag_DateTime", TNTP$Flag_DateTime<- 1, TNTP$Flag_DateTime <- 0)

#back to character date
TNTP$DateTime <- as.character(TNTP$DateTime)

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
TNTP_dups <- duplicated(TNTP[,1:4]) 
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

# remove dups (3 in 2022)
TNTP <- TNTP[!TNTP_dups,]

# move the averaged data over to the original columns
for (i in 1:nrow(TNTP)) {
  if(!TNTP$TP_ugL_AVG[i]=='NA'){
    TNTP$TP_ugL[i] <- TNTP$TP_ugL_AVG[i]
    TNTP$TN_ugL[i] <- TNTP$TN_ugL_AVG[i]
  }
}

#remove avg columns at end 
TNTP <- TNTP |> select(-c(TP_ugL_AVG, TN_ugL_AVG))

#keep/format rep col
TNTP$Rep <- ifelse(TNTP$Rep=="R2",2,1)

############################################################
#            if below detection, flag as 3                 #
#          independent digestion for each run              # 
#   averaged across all runs for most recent field season  #
#                "rolling spiked blank 250"                #
#                      TP      TN                          #  
#                     3.5      56                          #
############################################################  
## MDL's come from TNTP MDL 2016 +... 24mar23 excel sheet.
##Using rolling spike blanks for runs in 2023: 22 Feb - 22 March were the runs that went into this years data 

##################################
#        Historical MDL's:       #
#    2020: TP = 6.8; TN = 72.2   #
#    2021: TP = 10; TN = 76.4    #
#    2022: TP = 3.5; TN = 56     #
#    2023: TP = ?; TN = ?        #
##################################


for (i in 1:nrow(TNTP)) {
  ifelse(TNTP$TP_ugL[i] < 3.5 & TNTP$Flag_TP[i]==7,
    TNTP$Flag_TP[i] <- "73",
  ifelse(TNTP$TP_ugL[i] < 3.5,
    TNTP$Flag_TP[i] <- 3, TNTP$Flag_TP[i]))
  }


for (i in 1:nrow(TNTP)) {
  ifelse(TNTP$TN_ugL[i] < 56 & TNTP$Flag_TN[i]==7,
    TNTP$Flag_TN[i] <- "73",
  ifelse(TNTP$TN_ugL[i] < 56,
    TNTP$Flag_TN[i] <- 3, TNTP$Flag_TN[i]))
}

###################################################
# if negative, set to zero and set flag to 4
###################################################

for (i in 1:nrow(TNTP)) {
  if(TNTP$TP_ugL[i] < 0) {TNTP$TP_ugL[i] <- 0}
  
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

#-----------------------------------------------------------------------------#
#Dissolved organic carbon

doc <- read.csv("./Data/DataNotYetUploadedToEDI/NutrientData/collation/2023/2023_TOC_collation.csv")

# create flag columns
# no flag value = 0
doc$Flag_DC <- 0
doc$Flag_DIC <- 0
doc$Flag_DOC <- 0
doc$Flag_DN <- 0

## Flag rows where we used NPOC for CCR tunnel sites (400, 500, 501)
#using Flag 8 for NPOC denotation 
doc <- doc %>% 
  mutate(Flag_DOC = ifelse(Notes_DOC == "run_NPOC", 8, Flag_DOC ) )

#order doc df
doc <- doc %>% arrange(Reservoir, DateTime, Site, Depth_m)

#deleting CCR 14Oct22 site 400 bc is being rerun at a later date (hopefully!)
doc <- doc[!(doc$Notes_DOC=="rerun so delete"),]

doc$DateTime <- as.POSIXct(doc$DateTime, format="%m/%d/%y %H:%M")

#add DateTime flag (also need to count from end bc one sample has average and datetime flag!)
doc$Flag_DateTime <- 0
doc$Flag_DateTime[grep("Flag_DateTime", doc$Notes_DOC)] <- 1

#get rid of notes col
doc<- doc %>% select(-Notes_DOC, Date.NOTES)

#remove one ISCO sample that accidentally got run as a soluble
doc <- doc %>% filter(doc$Site!=100.1) 

#function to select rows based on characters from the end
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#make sure all analytes are numeric
doc$DOC_mgL <- as.numeric(doc$DOC_mgL)
doc$DIC_mgL <- as.numeric(doc$DIC_mgL)
doc$DC_mgL <- as.numeric(doc$DC_mgL)
doc$DN_mgL <- as.numeric(doc$DN_mgL)

#set datetime back to character
doc$DateTime <- as.character(doc$DateTime)

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
  if(doc$DIC_mgL[i] < 0 & !is.na(doc$DIC_mgL[i])){  #added is.na part because on NAs introduced in NPOC data 
    doc$DIC_mgL[i] <- 0
    if(doc$Flag_DIC[i]>0){
      doc$Flag_DIC[i] <- paste0(doc$Flag_DIC[i], 4)
      
    }else{doc$Flag_DIC[i] <- 4}
    
  }
}


for (i in 1:nrow(doc)) {
  if(doc$DC_mgL[i] < 0 & !is.na(doc$DIC_mgL[i])){   #added is.na part because of NAs introduced in NPOC data 
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
#switching order here so that we don't average negative values

# clean up DOC data
# look for duplicate values and average them, while adding flag = 7
doc_dups <- duplicated(doc[,1:4],fromLast = FALSE) 
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
doc$Rep <- ifelse(!is.na(doc$Rep) & doc$Rep=="R2",2,1)

#################################################################
#      rolling spiked blank for most recent field season        #
#                (TIC TC TNb rolling 06oct22.xlsx)              #
#                 if below detection, flag = 3                  #
#     2022 MDLS (in mg/L) from 'rolling spiked blank' tab:      # 
#                    DIC     DOC     DC    DN                   #
#                    0.67   0.76   0.98   0.05                  #
#################################################################
#    Historical MDL's:                                     #
#    2020: DIC = 0.97; DOC = 0.76 ; DC = 0.63; DN = 0.05   #
#    2021: DIC = 0.47; DOC = 0.45; DC = 0.69; DN = 0.11    #
#    2022: DIC = 0.67; DOC = 0.76; DC = 0.98; DN = 0.05    #
#    2023: DIC = ?; DOC = ?; DC = ?; DN = ?                #
############################################################

# DIC
for (i in 1:nrow(doc)) {
  if(doc$DIC_mgL[i] <0.67 & !is.na(doc$DIC_mgL[i])){
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
  if(doc$DC_mgL[i] < 0.98 & !is.na(doc$DC_mgL[i])){
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

#-----------------------------------------------------------------------------#
# Soluble N + P

np <- read.csv("./Data/DataNotYetUploadedToEDI/NutrientData/collation/2023/2023_soluble_NP_collation.csv")

#convert to character string for subsetting below
np$SampleID_lachat <- as.character(np$SampleID_lachat)
doc$SampleID_DOC <- as.character(doc$SampleID_DOC)

#order np df
np <- np %>% arrange(Reservoir, DateTime, Site, Depth_m)

#drop rows with NA values
np <- np[!is.na(np$NH4_ugL) | !is.na(np$PO4_ugL) | !is.na(np$NO3NO2_ugL),]

np$DateTime <- as.POSIXct(np$DateTime, format="%m/%d/%y %H:%M")

#add DateTime flag (also need to count from end bc one sample has average and datetime flag!)
np$Flag_DateTime <- 0
np$Flag_DateTime[grep("Flag_DateTime", np$Notes_lachat)] <- 1

#remove one ISCO sample that accidentally got run as a soluble
np <- np[np$Site!=100.1,]

#set datetime back to character
np$DateTime <- as.character(np$DateTime)

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
np_dups <- duplicated(np[,1:4]) 
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

#change 70 flags to 7
np$Flag_NH4[np$Flag_NH4=="70"] <- "7"
np$Flag_PO4[np$Flag_PO4=="70"] <- "7"
np$Flag_NO3NO2[np$Flag_NO3NO2=="70"] <- "7"

#add rep col
np$Rep <- ifelse(np$Rep=="R2" & !is.na(np$Rep),2,1)

##################################################################
#    2022 field season average: if below detection, flag as 3    #
#    using the following New Style MDL's from last batch csv.    #          
#   (Summary solubles batch 6 24mar23) - n=18 for 2022 samples   #
#                      NH4   PO4   NO3                           # 
#                      4.3   3.0   3.8                           #                        
##################################################################
#    Historical MDL's:                        #
#    2020: NH4 = 9.6; PO4 = 3.0; NO3 =  4.5   #
#    2021: NH4 = 7.3; PO4 = 3.1; NO3 =  3.7   # 
#    2022: NH4 = 4.3; PO4 = 3.0; NO3 =  3.8   #
#    2023: NH4 = ?; PO4 = ?; NO3 = ?          #
###############################################

for (i in 1:nrow(np)) {
  if(np$NH4_ugL[i] <4.3){
    if(np$Flag_NH4[i]>0){
      np$Flag_NH4[i] <- paste0(np$Flag_NH4[i], 3)
      
    }else{np$Flag_NH4[i] <- 3}
  }
}

for (i in 1:nrow(np)) {
  if(np$PO4_ugL[i] <3.0){
    if(np$Flag_PO4[i]>0){
      np$Flag_PO4[i] <- paste0(np$Flag_PO4[i], 3)
      
    }else{np$Flag_PO4[i] <- 3}
  }
}

for (i in 1:nrow(np)) {
  if(np$NO3NO2_ugL[i] < 3.8){
    if(np$Flag_NO3NO2[i]>0){
      np$Flag_NO3NO2[i] <- paste0(np$Flag_NO3NO2[i], 3)
      
    }else{np$Flag_NO3NO2[i] <- 3}
  }
}

#-----------------------------------------------------------------------------#
# join nutrients together 
#rename PO4_ugL to SRP_ugL and Flag_PO4 to Flag_SRP
colnames(np)[which(names(np) == "PO4_ugL")] <- "SRP_ugL"
colnames(np)[which(names(np) == "Flag_PO4")] <- "Flag_SRP_ugL"

#add units to other flag columns
colnames(np)[c(13,15)] <- c("Flag_NH4_ugL","Flag_NO3NO2_ugL")
colnames(doc)[c(13:16)] <- c("Flag_DC_mgL","Flag_DIC_mgL","Flag_DOC_mgL","Flag_DN_mgL")


#make sure all are in same datetime format 
TNTP <- TNTP %>% 
  mutate(DateTime = ymd_hms(DateTime))

doc <- doc %>% 
  mutate(DateTime = ymd_hms(DateTime))

np <- np %>% 
  mutate(DateTime = ymd_hms(DateTime))

#new df with solubles, totals, and DOC
solubles_and_DOC <- full_join(np, doc, by = c('Reservoir', 'Site',
                                              'DateTime',  'Depth_m','Rep'))
chem <- full_join(TNTP, solubles_and_DOC, by = c('Reservoir', 'Site', 
                                                 'DateTime',  'Depth_m', 'Rep'))

## ADD MAINTENANCE LOG FLAGS (manual edits to the data for suspect samples or human error)
maintenance_file <- 'Data/DataNotYetUploadedToEDI/NutrientData/maintenance_log.csv'
log_read <- read_csv(maintenance_file, col_types = cols(
  .default = col_character(),
  TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  flag = col_integer()
))

log <- log_read

for(i in 1:nrow(log)){
  ### Assign variables based on lines in the maintenance log. 
  
  ### get start and end time of one maintenance event
  start <- force_tz(as.POSIXct(log$TIMESTAMP_start[i]), tzone = "America/New_York")
  end <- force_tz(as.POSIXct(log$TIMESTAMP_end[i]), tzone = "America/New_York")
  
  ### Get the Reservoir Name
  Reservoir <- log$Reservoir[i]
  
  ### Get the Site Number
  Site <- as.numeric(log$Site[i])
  
  ### Get the depth value
  Depth <- as.numeric(log$Depth[i]) 
  
  ### Get the Run date 
  RunDate <- as.character(log$RunDate[i])
  
  ### Get the Maintenance Flag 
  flag <- log$flag[i]
  
  ### Get the new value for a column or an offset
  update_value <- as.numeric(log$update_value[i])
  
  ### Get the names of the columns affected by maintenance
  colname_start <- log$start_parameter[i]
  colname_end <- log$end_parameter[i]
  
  ### if it is only one parameter parameter then only one column will be selected
  
  if(is.na(colname_start)){
    
    maintenance_cols <- colnames(update_profiles%>%select(colname_end)) 
    
  }else if(is.na(colname_end)){
    
    maintenance_cols <- colnames(update_profiles%>%select(colname_start))
    
  }else{
    maintenance_cols <- colnames(update_profiles%>%select(colname_start:colname_end))
  }
  
  if(is.na(end)){
    # If there the maintenance is on going then the columns will be removed until
    # and end date is added
    Time <- update_profiles |> filter(DateTime >= start) |> select(DateTime)
    
  }else if (is.na(start)){
    # If there is only an end date change columns from beginning of data frame until end date
    Time <- update_profiles |> filter(DateTime <= end) |> select(DateTime)
    
  }else {
    Time <- update_profiles |> filter(DateTime >= start & DateTime <= end) |> select(DateTime)
  }
  
  ### This is where information in the maintenance log gets updated 
  
  if(flag %in% c(9,5)){ 
    # UPDATE THE MANUAL ISSUE FLAGS (BAD SAMPLE / USER ERROR) AND SET TO NEW VALUE
    
    update_profiles[c(which(update_profiles[,'Site'] == Site & update_profiles$DateTime %in% Time$DateTime)),paste0("Flag_",maintenance_cols)] <- as.numeric(flag)
    update_profiles[c(which(update_profiles[,'Site'] == Site & update_profiles$DateTime %in% Time$DateTime)),maintenance_cols] <- as.numeric(update_value)
    
  }else{
    warning("Flag not coded in the L1 script. See Austin or Adrienne")
  }
}
#### END MAINTENANCE LOG CODE

#get rid of notes and run date
chem <- chem %>% select(-c(RunDate_DOC,SampleID_DOC, RunDate.x, 
                           RunDate.y, Notes_lachat.x, Notes_lachat.y, 
                           SampleID_lachat, Flag_DateTime.x, 
                           Flag_DateTime.y, Date.NOTES))

chem <- chem %>%
  # rename(Flag_DateTime = Flag_DateTime.x) %>% 
  mutate(TP_ugL = as.numeric(TP_ugL),
         TN_ugL = as.numeric(TN_ugL),
         NH4_ugL = as.numeric(NH4_ugL),
         SRP_ugL = as.numeric(SRP_ugL),
         NO3NO2_ugL = as.numeric(NO3NO2_ugL),
         DOC_mgL = as.numeric(DOC_mgL),
         DIC_mgL = as.numeric(DIC_mgL),
         DC_mgL = as.numeric(DC_mgL),
         DN_mgL = as.numeric(DN_mgL))

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

chem_final <- chem_final %>% 
  rename(Flag_TP_ugL = Flag_TP,
         Flag_TN_ugL = Flag_TN) %>% 
  mutate(Flag_TP_ugL= as.numeric(Flag_TP_ugL),
         Flag_TN_ugL= as.numeric(Flag_TN_ugL),
         Flag_NH4_ugL = as.numeric(Flag_NH4_ugL),
         Flag_SRP_ugL = as.numeric(Flag_SRP_ugL),
         Flag_NO3NO2_ugL = as.numeric(Flag_NO3NO2_ugL),
         Flag_DOC_mgL = as.numeric(Flag_DOC_mgL),
         Flag_DIC_mgL = as.numeric(Flag_DIC_mgL),
         Flag_DC_mgL = as.numeric(Flag_DC_mgL),
         Flag_DN_mgL = as.numeric(Flag_DN_mgL))


#change all NAs to 0 in flag columns
chem_final$Flag_DateTime <- ifelse(is.na(chem_final$Flag_DateTime), 0, 
                                   chem_final$Flag_DateTime)
chem_final$Flag_DC_mgL <- ifelse(is.na(chem_final$Flag_DC_mgL), 0, 
                                 chem_final$Flag_DC_mgL)
chem_final$Flag_DN_mgL <- ifelse(is.na(chem_final$Flag_DN_mgL), 0, 
                                 chem_final$Flag_DN_mgL)
chem_final$Flag_DIC_mgL <- ifelse(is.na(chem_final$Flag_DIC_mgL), 0, 
                                  chem_final$Flag_DIC_mgL)
chem_final$Flag_DOC_mgL <- ifelse(is.na(chem_final$Flag_DOC_mgL), 0, 
                                  chem_final$Flag_DOC_mgL)
chem_final$Flag_TP_ugL <- ifelse(is.na(chem_final$Flag_TP_ugL), 0, 
                                 chem_final$Flag_TP_ugL)
chem_final$Flag_TN_ugL <- ifelse(is.na(chem_final$Flag_TN_ugL), 0, 
                                 chem_final$Flag_TN_ugL)
chem_final$Flag_NH4_ugL <- ifelse(is.na(chem_final$Flag_NH4_ugL), 0, 
                                  chem_final$Flag_NH4_ugL)
chem_final$Flag_NO3NO2_ugL <- ifelse(is.na(chem_final$Flag_NO3NO2_ugL), 0, 
                                     chem_final$Flag_NO3NO2_ugL)
chem_final$Flag_SRP_ugL <- ifelse(is.na(chem_final$Flag_SRP_ugL), 0, 
                                  chem_final$Flag_SRP_ugL)

#order chem
chem_final<- chem_final %>% arrange(Reservoir, DateTime, Site, Depth_m)

#drop sun samples
chem_final <- chem_final[chem_final$Reservoir!="SUNP",]

#remove NA rows
chem_final <- chem_final[!is.na(chem_final$Reservoir),]

#remove ISCO samples because these will be a separate data product one day
chem_final <- chem_final[chem_final$Site!=100.1,]

#save final df
write.csv(chem_final, "./Data/DataNotYetUploadedToEDI/NutrientData/chem_L1.csv.csv")

