### GHG for EDI - script to load in previous data from EDI, attach new data
### from current year, and clean data for publication to EDI

### Following: GHG_forEDI.Rmd by Abby Lewis
### Adapted by: A. Hounshell, 29 November 2021

##############################################################################

## Clear workspace
rm(list = ls())

## Load necessary pacakges
pacman::p_load(tidyverse, lubridate, readxl)

## Load previous data from EDI (2015-2020)

# Package ID: edi.551.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: Time series of dissolved methane and carbon dioxide concentrations for Falling Creek Reservoir and Beaverdam Reservoir in southwestern Virginia, USA during 2015-2020.
# Data set creator:  Cayelan Carey - Virginia Tech 
# Data set creator:  Ryan McClure - Virginia Tech 
# Data set creator:  Barbara Niederlehner - Virginia Tech 
# Data set creator:  Mary Lofton - Virginia Tech 
# Data set creator:  Alexandria Hounshell - Virginia Tech 
# Data set creator:  Abigail Lewis - Virginia Tech 
# Contact:  Cayelan Carey -  Virginia Tech  - Cayelan@vt.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/551/5/38d72673295864956cccd6bbba99a1a3" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Site",     
                 "DateTime",     
                 "Depth_m",     
                 "Rep",     
                 "ch4_umolL",     
                 "co2_umolL",     
                 "flag_ch4",     
                 "flag_co2"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Reservoir)!="factor") dt1$Reservoir<- as.factor(dt1$Reservoir)
if (class(dt1$Site)=="factor") dt1$Site <-as.numeric(levels(dt1$Site))[as.integer(dt1$Site) ]               
if (class(dt1$Site)=="character") dt1$Site <-as.numeric(dt1$Site)                                   
# attempting to convert dt1$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1DateTime<-as.POSIXct(dt1$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DateTime) == length(tmp1DateTime[!is.na(tmp1DateTime)])){dt1$DateTime <- tmp1DateTime } else {print("Date conversion failed for dt1$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DateTime) 
if (class(dt1$Depth_m)=="factor") dt1$Depth_m <-as.numeric(levels(dt1$Depth_m))[as.integer(dt1$Depth_m) ]               
if (class(dt1$Depth_m)=="character") dt1$Depth_m <-as.numeric(dt1$Depth_m)
if (class(dt1$Rep)=="factor") dt1$Rep <-as.numeric(levels(dt1$Rep))[as.integer(dt1$Rep) ]               
if (class(dt1$Rep)=="character") dt1$Rep <-as.numeric(dt1$Rep)
if (class(dt1$ch4_umolL)=="factor") dt1$ch4_umolL <-as.numeric(levels(dt1$ch4_umolL))[as.integer(dt1$ch4_umolL) ]               
if (class(dt1$ch4_umolL)=="character") dt1$ch4_umolL <-as.numeric(dt1$ch4_umolL)
if (class(dt1$co2_umolL)=="factor") dt1$co2_umolL <-as.numeric(levels(dt1$co2_umolL))[as.integer(dt1$co2_umolL) ]               
if (class(dt1$co2_umolL)=="character") dt1$co2_umolL <-as.numeric(dt1$co2_umolL)
if (class(dt1$flag_ch4)=="factor") dt1$flag_ch4 <-as.numeric(levels(dt1$flag_ch4))[as.integer(dt1$flag_ch4) ]               
if (class(dt1$flag_ch4)=="character") dt1$flag_ch4 <-as.numeric(dt1$flag_ch4)
if (class(dt1$flag_co2)=="factor") dt1$flag_co2 <-as.numeric(levels(dt1$flag_co2))[as.integer(dt1$flag_co2) ]               
if (class(dt1$flag_co2)=="character") dt1$flag_co2 <-as.numeric(dt1$flag_co2)

# Convert Missing Values to NA for non-dates

dt1$Reservoir <- as.factor(ifelse((trimws(as.character(dt1$Reservoir))==trimws("NA")),NA,as.character(dt1$Reservoir)))
dt1$Site <- ifelse((trimws(as.character(dt1$Site))==trimws("NA")),NA,dt1$Site)               
suppressWarnings(dt1$Site <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Site))==as.character(as.numeric("NA"))),NA,dt1$Site))
dt1$Depth_m <- ifelse((trimws(as.character(dt1$Depth_m))==trimws("NA")),NA,dt1$Depth_m)               
suppressWarnings(dt1$Depth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Depth_m))==as.character(as.numeric("NA"))),NA,dt1$Depth_m))
dt1$Rep <- ifelse((trimws(as.character(dt1$Rep))==trimws("NA")),NA,dt1$Rep)               
suppressWarnings(dt1$Rep <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Rep))==as.character(as.numeric("NA"))),NA,dt1$Rep))
dt1$ch4_umolL <- ifelse((trimws(as.character(dt1$ch4_umolL))==trimws("NA")),NA,dt1$ch4_umolL)               
suppressWarnings(dt1$ch4_umolL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ch4_umolL))==as.character(as.numeric("NA"))),NA,dt1$ch4_umolL))
dt1$co2_umolL <- ifelse((trimws(as.character(dt1$co2_umolL))==trimws("NA")),NA,dt1$co2_umolL)               
suppressWarnings(dt1$co2_umolL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$co2_umolL))==as.character(as.numeric("NA"))),NA,dt1$co2_umolL))
dt1$flag_ch4 <- ifelse((trimws(as.character(dt1$flag_ch4))==trimws("NA")),NA,dt1$flag_ch4)               
suppressWarnings(dt1$flag_ch4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$flag_ch4))==as.character(as.numeric("NA"))),NA,dt1$flag_ch4))
dt1$flag_co2 <- ifelse((trimws(as.character(dt1$flag_co2))==trimws("NA")),NA,dt1$flag_co2)               
suppressWarnings(dt1$flag_co2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$flag_co2))==as.character(as.numeric("NA"))),NA,dt1$flag_co2))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Reservoir)
summary(Site)
summary(DateTime)
summary(Depth_m)
summary(Rep)
summary(ch4_umolL)
summary(co2_umolL)
summary(flag_ch4)
summary(flag_co2) 
# Get more details on character variables

summary(as.factor(dt1$Reservoir))
detach(dt1)   

#### FOR 2021 DATA ONLY! ####
# Add in times for historical data 2015-2020
# Change Station 300 to 1.1 (ToeDrain for FCR and BVR!)

# Following chemistry dataset
# NOTE: 12:00 = No recorded time

# Load in Chemistry data from 2013-2020
# Package ID: edi.199.9 Cataloging System:https://pasta.edirepository.org.
# Data set title: Water chemistry time series for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2020.
# Data set creator:  Cayelan Carey - Virginia Tech 
# Data set creator:  Heather Wander - Virginia Tech 
# Data set creator:  Whitney Woelmer - Virginia Tech 
# Data set creator:  Mary Lofton - Virginia Tech 
# Data set creator:  Adrienne Breef-Pilz - Virginia Tech 
# Data set creator:  Jonathan Doubek - Virginia Tech 
# Data set creator:  Alexandra Gerling - Virginia Tech 
# Data set creator:  Alexandria Hounshell - Virginia Tech 
# Data set creator:  Ryan McClure - Virginia Tech 
# Data set creator:  Barbara Niederlehner - Virginia Tech 
# Contact:  Cayelan Carey -  Virginia Tech  - Cayelan@vt.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/199/9/fe500aac19d1a0d78bb2cb1d196cdbd7" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


chem <-read.csv(infile1,header=F 
                ,skip=1
                ,sep=","  
                , col.names=c(
                  "Reservoir",     
                  "Site",     
                  "DateTime",     
                  "Depth_m",     
                  "Rep",     
                  "TN_ugL",     
                  "TP_ugL",     
                  "NH4_ugL",     
                  "NO3NO2_ugL",     
                  "SRP_ugL",     
                  "DOC_mgL",     
                  "DIC_mgL",     
                  "DC_mgL",     
                  "DN_mgL",     
                  "Flag_DateTime",     
                  "Flag_TN",     
                  "Flag_TP",     
                  "Flag_NH4",     
                  "Flag_NO3NO2",     
                  "Flag_SRP",     
                  "Flag_DOC",     
                  "Flag_DIC",     
                  "Flag_DC",     
                  "Flag_DN"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(chem$Reservoir)!="factor") chem$Reservoir<- as.factor(chem$Reservoir)
if (class(chem$Site)=="factor") chem$Site <-as.numeric(levels(chem$Site))[as.integer(chem$Site) ]               
if (class(chem$Site)=="character") chem$Site <-as.numeric(chem$Site)                                   
# attempting to convert chem$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1DateTime<-as.POSIXct(chem$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DateTime) == length(tmp1DateTime[!is.na(tmp1DateTime)])){chem$DateTime <- tmp1DateTime } else {print("Date conversion failed for chem$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DateTime) 
if (class(chem$Depth_m)=="factor") chem$Depth_m <-as.numeric(levels(chem$Depth_m))[as.integer(chem$Depth_m) ]               
if (class(chem$Depth_m)=="character") chem$Depth_m <-as.numeric(chem$Depth_m)
if (class(chem$Rep)=="factor") chem$Rep <-as.numeric(levels(chem$Rep))[as.integer(chem$Rep) ]               
if (class(chem$Rep)=="character") chem$Rep <-as.numeric(chem$Rep)
if (class(chem$TN_ugL)=="factor") chem$TN_ugL <-as.numeric(levels(chem$TN_ugL))[as.integer(chem$TN_ugL) ]               
if (class(chem$TN_ugL)=="character") chem$TN_ugL <-as.numeric(chem$TN_ugL)
if (class(chem$TP_ugL)=="factor") chem$TP_ugL <-as.numeric(levels(chem$TP_ugL))[as.integer(chem$TP_ugL) ]               
if (class(chem$TP_ugL)=="character") chem$TP_ugL <-as.numeric(chem$TP_ugL)
if (class(chem$NH4_ugL)=="factor") chem$NH4_ugL <-as.numeric(levels(chem$NH4_ugL))[as.integer(chem$NH4_ugL) ]               
if (class(chem$NH4_ugL)=="character") chem$NH4_ugL <-as.numeric(chem$NH4_ugL)
if (class(chem$NO3NO2_ugL)=="factor") chem$NO3NO2_ugL <-as.numeric(levels(chem$NO3NO2_ugL))[as.integer(chem$NO3NO2_ugL) ]               
if (class(chem$NO3NO2_ugL)=="character") chem$NO3NO2_ugL <-as.numeric(chem$NO3NO2_ugL)
if (class(chem$SRP_ugL)=="factor") chem$SRP_ugL <-as.numeric(levels(chem$SRP_ugL))[as.integer(chem$SRP_ugL) ]               
if (class(chem$SRP_ugL)=="character") chem$SRP_ugL <-as.numeric(chem$SRP_ugL)
if (class(chem$DOC_mgL)=="factor") chem$DOC_mgL <-as.numeric(levels(chem$DOC_mgL))[as.integer(chem$DOC_mgL) ]               
if (class(chem$DOC_mgL)=="character") chem$DOC_mgL <-as.numeric(chem$DOC_mgL)
if (class(chem$DIC_mgL)=="factor") chem$DIC_mgL <-as.numeric(levels(chem$DIC_mgL))[as.integer(chem$DIC_mgL) ]               
if (class(chem$DIC_mgL)=="character") chem$DIC_mgL <-as.numeric(chem$DIC_mgL)
if (class(chem$DC_mgL)=="factor") chem$DC_mgL <-as.numeric(levels(chem$DC_mgL))[as.integer(chem$DC_mgL) ]               
if (class(chem$DC_mgL)=="character") chem$DC_mgL <-as.numeric(chem$DC_mgL)
if (class(chem$DN_mgL)=="factor") chem$DN_mgL <-as.numeric(levels(chem$DN_mgL))[as.integer(chem$DN_mgL) ]               
if (class(chem$DN_mgL)=="character") chem$DN_mgL <-as.numeric(chem$DN_mgL)
if (class(chem$Flag_DateTime)=="factor") chem$Flag_DateTime <-as.numeric(levels(chem$Flag_DateTime))[as.integer(chem$Flag_DateTime) ]               
if (class(chem$Flag_DateTime)=="character") chem$Flag_DateTime <-as.numeric(chem$Flag_DateTime)
if (class(chem$Flag_TN)=="factor") chem$Flag_TN <-as.numeric(levels(chem$Flag_TN))[as.integer(chem$Flag_TN) ]               
if (class(chem$Flag_TN)=="character") chem$Flag_TN <-as.numeric(chem$Flag_TN)
if (class(chem$Flag_TP)=="factor") chem$Flag_TP <-as.numeric(levels(chem$Flag_TP))[as.integer(chem$Flag_TP) ]               
if (class(chem$Flag_TP)=="character") chem$Flag_TP <-as.numeric(chem$Flag_TP)
if (class(chem$Flag_NH4)=="factor") chem$Flag_NH4 <-as.numeric(levels(chem$Flag_NH4))[as.integer(chem$Flag_NH4) ]               
if (class(chem$Flag_NH4)=="character") chem$Flag_NH4 <-as.numeric(chem$Flag_NH4)
if (class(chem$Flag_NO3NO2)=="factor") chem$Flag_NO3NO2 <-as.numeric(levels(chem$Flag_NO3NO2))[as.integer(chem$Flag_NO3NO2) ]               
if (class(chem$Flag_NO3NO2)=="character") chem$Flag_NO3NO2 <-as.numeric(chem$Flag_NO3NO2)
if (class(chem$Flag_SRP)=="factor") chem$Flag_SRP <-as.numeric(levels(chem$Flag_SRP))[as.integer(chem$Flag_SRP) ]               
if (class(chem$Flag_SRP)=="character") chem$Flag_SRP <-as.numeric(chem$Flag_SRP)
if (class(chem$Flag_DOC)=="factor") chem$Flag_DOC <-as.numeric(levels(chem$Flag_DOC))[as.integer(chem$Flag_DOC) ]               
if (class(chem$Flag_DOC)=="character") chem$Flag_DOC <-as.numeric(chem$Flag_DOC)
if (class(chem$Flag_DIC)=="factor") chem$Flag_DIC <-as.numeric(levels(chem$Flag_DIC))[as.integer(chem$Flag_DIC) ]               
if (class(chem$Flag_DIC)=="character") chem$Flag_DIC <-as.numeric(chem$Flag_DIC)
if (class(chem$Flag_DC)=="factor") chem$Flag_DC <-as.numeric(levels(chem$Flag_DC))[as.integer(chem$Flag_DC) ]               
if (class(chem$Flag_DC)=="character") chem$Flag_DC <-as.numeric(chem$Flag_DC)
if (class(chem$Flag_DN)=="factor") chem$Flag_DN <-as.numeric(levels(chem$Flag_DN))[as.integer(chem$Flag_DN) ]               
if (class(chem$Flag_DN)=="character") chem$Flag_DN <-as.numeric(chem$Flag_DN)

# Convert Missing Values to NA for non-dates

chem$Reservoir <- as.factor(ifelse((trimws(as.character(chem$Reservoir))==trimws("NA")),NA,as.character(chem$Reservoir)))
chem$Site <- ifelse((trimws(as.character(chem$Site))==trimws("NA")),NA,chem$Site)               
suppressWarnings(chem$Site <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$Site))==as.character(as.numeric("NA"))),NA,chem$Site))
chem$Depth_m <- ifelse((trimws(as.character(chem$Depth_m))==trimws("NA")),NA,chem$Depth_m)               
suppressWarnings(chem$Depth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$Depth_m))==as.character(as.numeric("NA"))),NA,chem$Depth_m))
chem$Rep <- ifelse((trimws(as.character(chem$Rep))==trimws("NA")),NA,chem$Rep)               
suppressWarnings(chem$Rep <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$Rep))==as.character(as.numeric("NA"))),NA,chem$Rep))
chem$TN_ugL <- ifelse((trimws(as.character(chem$TN_ugL))==trimws("NA")),NA,chem$TN_ugL)               
suppressWarnings(chem$TN_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$TN_ugL))==as.character(as.numeric("NA"))),NA,chem$TN_ugL))
chem$TP_ugL <- ifelse((trimws(as.character(chem$TP_ugL))==trimws("NA")),NA,chem$TP_ugL)               
suppressWarnings(chem$TP_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$TP_ugL))==as.character(as.numeric("NA"))),NA,chem$TP_ugL))
chem$NH4_ugL <- ifelse((trimws(as.character(chem$NH4_ugL))==trimws("NA")),NA,chem$NH4_ugL)               
suppressWarnings(chem$NH4_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$NH4_ugL))==as.character(as.numeric("NA"))),NA,chem$NH4_ugL))
chem$NO3NO2_ugL <- ifelse((trimws(as.character(chem$NO3NO2_ugL))==trimws("NA")),NA,chem$NO3NO2_ugL)               
suppressWarnings(chem$NO3NO2_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$NO3NO2_ugL))==as.character(as.numeric("NA"))),NA,chem$NO3NO2_ugL))
chem$SRP_ugL <- ifelse((trimws(as.character(chem$SRP_ugL))==trimws("NA")),NA,chem$SRP_ugL)               
suppressWarnings(chem$SRP_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$SRP_ugL))==as.character(as.numeric("NA"))),NA,chem$SRP_ugL))
chem$DOC_mgL <- ifelse((trimws(as.character(chem$DOC_mgL))==trimws("NA")),NA,chem$DOC_mgL)               
suppressWarnings(chem$DOC_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$DOC_mgL))==as.character(as.numeric("NA"))),NA,chem$DOC_mgL))
chem$DIC_mgL <- ifelse((trimws(as.character(chem$DIC_mgL))==trimws("NA")),NA,chem$DIC_mgL)               
suppressWarnings(chem$DIC_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$DIC_mgL))==as.character(as.numeric("NA"))),NA,chem$DIC_mgL))
chem$DC_mgL <- ifelse((trimws(as.character(chem$DC_mgL))==trimws("NA")),NA,chem$DC_mgL)               
suppressWarnings(chem$DC_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$DC_mgL))==as.character(as.numeric("NA"))),NA,chem$DC_mgL))
chem$DN_mgL <- ifelse((trimws(as.character(chem$DN_mgL))==trimws("NA")),NA,chem$DN_mgL)               
suppressWarnings(chem$DN_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$DN_mgL))==as.character(as.numeric("NA"))),NA,chem$DN_mgL))
chem$Flag_DateTime <- ifelse((trimws(as.character(chem$Flag_DateTime))==trimws("NA")),NA,chem$Flag_DateTime)               
suppressWarnings(chem$Flag_DateTime <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$Flag_DateTime))==as.character(as.numeric("NA"))),NA,chem$Flag_DateTime))
chem$Flag_TN <- ifelse((trimws(as.character(chem$Flag_TN))==trimws("NA")),NA,chem$Flag_TN)               
suppressWarnings(chem$Flag_TN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$Flag_TN))==as.character(as.numeric("NA"))),NA,chem$Flag_TN))
chem$Flag_TP <- ifelse((trimws(as.character(chem$Flag_TP))==trimws("NA")),NA,chem$Flag_TP)               
suppressWarnings(chem$Flag_TP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$Flag_TP))==as.character(as.numeric("NA"))),NA,chem$Flag_TP))
chem$Flag_NH4 <- ifelse((trimws(as.character(chem$Flag_NH4))==trimws("NA")),NA,chem$Flag_NH4)               
suppressWarnings(chem$Flag_NH4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$Flag_NH4))==as.character(as.numeric("NA"))),NA,chem$Flag_NH4))
chem$Flag_NO3NO2 <- ifelse((trimws(as.character(chem$Flag_NO3NO2))==trimws("NA")),NA,chem$Flag_NO3NO2)               
suppressWarnings(chem$Flag_NO3NO2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$Flag_NO3NO2))==as.character(as.numeric("NA"))),NA,chem$Flag_NO3NO2))
chem$Flag_SRP <- ifelse((trimws(as.character(chem$Flag_SRP))==trimws("NA")),NA,chem$Flag_SRP)               
suppressWarnings(chem$Flag_SRP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$Flag_SRP))==as.character(as.numeric("NA"))),NA,chem$Flag_SRP))
chem$Flag_DOC <- ifelse((trimws(as.character(chem$Flag_DOC))==trimws("NA")),NA,chem$Flag_DOC)               
suppressWarnings(chem$Flag_DOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$Flag_DOC))==as.character(as.numeric("NA"))),NA,chem$Flag_DOC))
chem$Flag_DIC <- ifelse((trimws(as.character(chem$Flag_DIC))==trimws("NA")),NA,chem$Flag_DIC)               
suppressWarnings(chem$Flag_DIC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$Flag_DIC))==as.character(as.numeric("NA"))),NA,chem$Flag_DIC))
chem$Flag_DC <- ifelse((trimws(as.character(chem$Flag_DC))==trimws("NA")),NA,chem$Flag_DC)               
suppressWarnings(chem$Flag_DC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$Flag_DC))==as.character(as.numeric("NA"))),NA,chem$Flag_DC))
chem$Flag_DN <- ifelse((trimws(as.character(chem$Flag_DN))==trimws("NA")),NA,chem$Flag_DN)               
suppressWarnings(chem$Flag_DN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(chem$Flag_DN))==as.character(as.numeric("NA"))),NA,chem$Flag_DN))


# Here is the structure of the input data frame:
str(chem)                            
attach(chem)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Reservoir)
summary(Site)
summary(DateTime)
summary(Depth_m)
summary(Rep)
summary(TN_ugL)
summary(TP_ugL)
summary(NH4_ugL)
summary(NO3NO2_ugL)
summary(SRP_ugL)
summary(DOC_mgL)
summary(DIC_mgL)
summary(DC_mgL)
summary(DN_mgL)
summary(Flag_DateTime)
summary(Flag_TN)
summary(Flag_TP)
summary(Flag_NH4)
summary(Flag_NO3NO2)
summary(Flag_SRP)
summary(Flag_DOC)
summary(Flag_DIC)
summary(Flag_DC)
summary(Flag_DN) 
# Get more details on character variables

summary(as.factor(chem$Reservoir))
detach(chem)   

# Separate Date and Time
chem <- chem %>%
  mutate(DateTime = as.POSIXct(DateTime, "%Y-%m-%d %HH:%MM", tz="EST"), #Make sure columns are the correct type
         Date = as.Date(DateTime), #Separate date
         Time = format(DateTime,"%H:%M"), #Separate time
         Depth_m = as.numeric(Depth_m), # CHANGE TO as.numeric AFTER NAMING IS FINALIZED
         Reservoir = as.factor(Reservoir)) %>% 
  select(Reservoir, Site, Date, Time, Depth_m) #Select only the columns we are using

ghg_hist_time <- dt1 %>% 
  rename(Date = DateTime) %>% 
  mutate(Date = as.Date(Date))

ghg_hist_time <- left_join(ghg_hist_time,chem,by=c("Reservoir","Site","Date","Depth_m"))

# Create a loop
for (i in 1:length(ghg_hist_time$Time)){
  if (is.na(ghg_hist_time$Time[i])){
    ghg_hist_time$Time[i] =  ghg_hist_time$Time[1]
  }
  ghg_hist_time$Time[i] = ghg_hist_time$Time[i]
}

# Merge Date and Time
ghg_hist_time <- ghg_hist_time %>% 
  mutate(DateTime = as.POSIXct(paste(Date,Time),format = "%Y-%m-%d %H:%M", tz = "EST")) %>% 
  select(Reservoir,Site,Depth_m,Rep,ch4_umolL,co2_umolL,flag_ch4,flag_co2,DateTime) %>% 
  relocate(DateTime,.before=Depth_m)

# Change Station 300 to 1.1
ghg_hist_time <- ghg_hist_time %>% 
  mutate(Site = ifelse(Site == 300, 1.1, Site))

#############################################################################

## Load in data from 2021 - via the MEGA GHG Datasheet
#Load this year's data
ghgs <- read_excel("./Data/DataNotYetUploadedToEDI/Raw_GHG/2021/GHG_MEGA_GC_SHEET_EXCEL_2021.xlsx", sheet = 2, skip = 6)

#Change column names to match EDI standard
colnames(ghgs)[1:4] <- c("DateTime","Depth_m","Reservoir","Rep")
colnames(ghgs)[13:14] <- c("ch4_umolL","co2_umolL")

ghgs <- ghgs%>%
  select(DateTime,Depth_m,Reservoir,Rep,ch4_umolL,co2_umolL)%>% #Select only the columns we are using
  mutate(DateTime = as.POSIXct(DateTime, "%Y-%m-%d %HH:%MM", tz="EST"), #Make sure columns are the correct type
         Depth_m = as.numeric(Depth_m), # CHANGE TO as.numeric AFTER NAMING IS FINALIZED
         Reservoir = as.factor(Reservoir)) %>%
  mutate(DateTime = force_tz(DateTime,tzone = "EST")) %>% 
  filter(!is.na(Depth_m)) #Depth cannot be missing

#############################################################################
### Make some graphs!

# Plot CH4 at Site 50 in FCR
ghgs%>% 
  filter(Depth_m<100,
         Reservoir == "FCR")%>%
  ggplot(aes(x = DateTime, y = ch4_umolL, col = as.factor(Depth_m)))+
  geom_point()

# Plot CO2 at Site 50 in FCR
ghgs%>% 
  filter(Depth_m<100,
         Reservoir == "FCR")%>%
  ggplot(aes(x = DateTime, y = co2_umolL, col = as.factor(Depth_m)))+
  geom_point()

# Plot CH4 at Site 50 in BVR
ghgs%>% 
  filter(Depth_m<100,
         Reservoir == "BVR")%>%
  ggplot(aes(x = DateTime, y = ch4_umolL, col = as.factor(Depth_m)))+
  geom_point()

# Plot CO2 at Site 50 in BVR
ghgs%>% 
  filter(Depth_m<100,
         Reservoir == "BVR")%>%
  ggplot(aes(x = DateTime, y = co2_umolL, col = as.factor(Depth_m)))+
  geom_point()

##############################################################################

# Calculate % difference between replicates
# Flag 1 = Sample not collected
# Flag 2 = Sample below MDL (for 2021: 0.00252 umol/L CH4; 4.36 umol/L CO2)
# Flag 3 = Difference between samples >LOQ and percent difference >30% but <50%
# Flag 4 = Difference between samples >LOQ and percent difference >50%
# NOTE: All flagged reps were retained in the dataset

## Separate into rep 1 and rep2
ghgs_rep1 <- ghgs %>% 
  filter(Rep == "1") %>% 
  rename(ch4_umolL_rep1 = ch4_umolL, co2_umolL_rep1 = co2_umolL)

ghgs_rep2 <- ghgs %>% 
  filter(Rep == "2") %>% 
  rename(ch4_umolL_rep2 = ch4_umolL, co2_umolL_rep2 = co2_umolL)

ghgs_reps <- left_join(ghgs_rep1,ghgs_rep2,by=c("DateTime","Depth_m","Reservoir"))

# Add '2' when rep 2 is NA
ghgs_reps <- ghgs_reps %>% 
  mutate(Rep.y = ifelse(is.na(Rep.y),2,Rep.y))

## Calculate percent difference between reps
ghgs_reps <- ghgs_reps %>% 
  mutate(ch4_pdiff = round((abs(ch4_umolL_rep1-ch4_umolL_rep2)/(abs(ch4_umolL_rep1+ch4_umolL_rep2)/2))*100)) %>% 
  mutate(ch4_diff = abs(ch4_umolL_rep1-ch4_umolL_rep2))

ghgs_reps <- ghgs_reps %>% 
  mutate(co2_pdiff = round((abs(co2_umolL_rep1-co2_umolL_rep2)/(abs(co2_umolL_rep1+co2_umolL_rep2)/2))*100)) %>% 
  mutate(co2_diff = abs(co2_umolL_rep1 - co2_umolL_rep2))

# Pivot_longer
ghg_rep1 <- ghgs_reps %>% 
  select(DateTime,Depth_m,Reservoir,ch4_umolL_rep1,co2_umolL_rep1,ch4_pdiff,ch4_diff,co2_pdiff,co2_diff) %>% 
  mutate(Rep = "1") %>% 
  rename(ch4_umolL = ch4_umolL_rep1,
         co2_umolL = co2_umolL_rep1)

ghg_rep2 <- ghgs_reps %>% 
  select(DateTime,Depth_m,Reservoir,ch4_umolL_rep2,co2_umolL_rep2,ch4_pdiff,ch4_diff,co2_pdiff,co2_diff) %>% 
  mutate(Rep = "2") %>% 
  rename(ch4_umolL = ch4_umolL_rep2,
         co2_umolL = co2_umolL_rep2)

ghg_all <- rbind(ghg_rep1,ghg_rep2)

ghg_all <-  ghg_all %>% 
  arrange(DateTime,Reservoir,Depth_m)

## Flag replicates: 
# Flag 1 = Sample not collected
# Flag 2 = Sample below MDL (for 2021: 0.00252 umol/L CH4; 4.36 umol/L CO2)
# Flag 3 = Difference between samples >LOQ and percent difference >30% but <50%
# Flag 4 = Difference between samples >LOQ and percent difference >50%
ghg_all <- ghg_all %>% 
  mutate(flag_ch4 = ifelse(ch4_pdiff>=50 & ch4_diff>=0.00252*3,4,
                           ifelse(ch4_pdiff<=50 & ch4_pdiff>=30 & ch4_diff>=0.00252*3,3,
                                         NA)),
         flag_co2 = ifelse(co2_pdiff>=50 & co2_diff>=4.36*3,4,
                           ifelse(co2_pdiff<=50 & co2_pdiff>=30 & co2_diff>=4.36*3,3,
                                         NA)))

ghg_all <- ghg_all %>% 
  mutate(flag_ch4 = ifelse(ch4_umolL <= 0.00252,2,flag_ch4),
         flag_co2 = ifelse(co2_umolL <= 4.36,2,flag_co2))

ghg_all <- ghg_all %>% 
  mutate(flag_ch4 = ifelse(is.na(ch4_umolL), 1, flag_ch4),
         flag_co2 = ifelse(is.na(co2_umolL), 1, flag_co2))

ghg_all <- ghg_all %>% 
  mutate(flag_ch4 = ifelse(is.na(flag_ch4), 0, flag_ch4),
         flag_co2 = ifelse(is.na(flag_co2), 0, flag_co2))

### Separate site and depth
ghg_all <- ghg_all %>% 
  mutate(Site = ifelse(Reservoir == "FCR" & Depth_m %in% c(0.1, 1.6, 3.8, 5.0, 6.2, 8.0, 9.0), 50,
                       ifelse(Reservoir == "BVR" & Depth_m %in% c(0.1, 3.0, 6.0, 9.0, 11.0), 50, Depth_m)))

ghg_all <- ghg_all %>% 
  mutate(Depth_m = ifelse(Depth_m == Site, 0.1, Depth_m))

### Re-order to join with historical data
ghg_all <- ghg_all %>% 
  select(DateTime,Depth_m,Reservoir,ch4_umolL,co2_umolL,flag_ch4,flag_co2,Site,Rep)

col_order <- c("Reservoir","Site","DateTime","Depth_m","Rep","ch4_umolL","co2_umolL","flag_ch4","flag_co2")

ghg_all <- ghg_all[,(col_order)]

### Combine historical and new data
final <- rbind(ghg_hist_time,ghg_all)

final <- final %>% 
  arrange(DateTime,Reservoir,Site,Depth_m)

### Create a DateTime Flag for non-recorded times (i.e., 12:00)
final <- final %>% 
  mutate(Time = format(DateTime,"%H:%M:%S")) %>% 
  mutate(flag_DateTime = ifelse(Time == "12:00:00", 1, 0)) %>% 
  select(-Time) %>% 
  relocate(flag_DateTime,.before=flag_ch4)

# Some depths are funky
final <- final %>% 
  mutate(Site = ifelse(Depth_m == 100, 100,
                       ifelse(Depth_m == 200, 200, Site))) %>% 
  mutate(Depth_m = ifelse(Depth_m %in% c(100,200), 0.1, Depth_m))

### Save GHG file!
write.csv(final,"./Data/DataNotYetUploadedToEDI/Raw_GHG/2021/final_GHG_2015-2021.csv",row.names = FALSE)

### Final plots - just to check : )
# FCR Site 50 - CH4
final %>% 
  filter(Reservoir == "FCR", Site == 50) %>% 
  ggplot(mapping=aes(x=DateTime,y=ch4_umolL,color=as.factor(Depth_m)))+
  geom_point()

# FCR NOT Site 50 - CH4
final %>% 
  filter(Reservoir == "FCR", Site != 50) %>% 
  ggplot(mapping=aes(x=DateTime,y=ch4_umolL,color=as.factor(Site)))+
  geom_point()

# FCR Site 50 - CO2
final %>% 
  filter(Reservoir == "FCR", Site == 50) %>% 
  ggplot(mapping=aes(x=DateTime,y=co2_umolL,color=as.factor(Depth_m)))+
  geom_point()

# FCR NOT Site 50 - CO2
final %>% 
  filter(Reservoir == "FCR", Site != 50) %>% 
  ggplot(mapping=aes(x=DateTime,y=co2_umolL,color=as.factor(Site)))+
  geom_point()

# BVR Site 50 - CH4
final %>% 
  filter(Reservoir == "BVR", Site == 50) %>% 
  ggplot(mapping=aes(x=DateTime,y=ch4_umolL,color=as.factor(Depth_m)))+
  geom_point()

# BVR NOT Site 50 - CH4
final %>% 
  filter(Reservoir == "BVR", Site != 50) %>% 
  ggplot(mapping=aes(x=DateTime,y=ch4_umolL,color=as.factor(Site)))+
  geom_point()

# BVR Site 50 - CO2
final %>% 
  filter(Reservoir == "BVR", Site == 50) %>% 
  ggplot(mapping=aes(x=DateTime,y=co2_umolL,color=as.factor(Depth_m)))+
  geom_point()

# BVR NOT Site 50 - CO2
final %>% 
  filter(Reservoir == "BVR", Site != 50) %>% 
  ggplot(mapping=aes(x=DateTime,y=co2_umolL,color=as.factor(Site)))+
  geom_point()
