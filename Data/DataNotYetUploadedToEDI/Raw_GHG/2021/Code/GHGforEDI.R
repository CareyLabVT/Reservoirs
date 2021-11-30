### GHG for EDI - script to load in previous data from EDI, attach new data
### from current year, and clean data for publication to EDI

### Following: GHG_forEDI.Rmd by Abby Lewis
### Adapted by: A. Hounshell, 29 November 2021

##############################################################################

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

#############################################################################

## Load in data from 2021 - via the MEGA GHG Datasheet
#Load this year's data
ghgs <- read_excel("./Data/DataNotYetUploadedToEDI/Raw_GHG/2021/GHG_MEGA_GC_SHEET_EXCEL_2021.xlsx", sheet = 2, skip = 6)

#Change column names to match EDI standard
colnames(ghgs)[1:4] <- c("DateTime","Depth_m","Reservoir","Rep")
colnames(ghgs)[13:14] <- c("ch4_umolL","co2_umolL")

ghgs=ghgs%>%
  select(DateTime,Depth_m,Reservoir,Rep,ch4_umolL,co2_umolL)%>% #Select only the columns we are using
  mutate(DateTime = as.POSIXct(DateTime, "%Y-%m-%d %HH:%MM", tz="EST"), #Make sure columns are the correct type
         Date = as.Date(DateTime), #Separate date
         Time = format(DateTime,"%H:%M"), #Separate time
         Depth_m = as.numeric(Depth_m), # CHANGE TO as.numeric AFTER NAMING IS FINALIZED
         Reservoir = as.factor(Reservoir)) %>%
  filter(!is.na(Depth_m)) #Depth cannot be missing

# If time = 00:00, then time = NA
ghgs <- ghgs %>% 
  mutate(Time = ifelse(Time == "00:00", NA, Time)) %>% 
  select(Date,Time,Depth_m,Reservoir,Rep,ch4_umolL,co2_umolL) %>% 
  relocate(Date,.before=Depth_m) %>% 
  relocate(Time,.before=Depth_m)

#############################################################################

# Make some graphs!

ghgs%>% #Plot CH4 at Site 50 in FCR
  filter(Depth_m<100,
         Reservoir == "FCR")%>%
  ggplot(aes(x = Date, y = ch4_umolL, col = as.factor(Depth_m)))+
  geom_point()

ghgs%>% #Plot CO2 at Site 50 in FCR
  filter(Depth_m<100,
         Reservoir == "FCR")%>%
  ggplot(aes(x = Date, y = co2_umolL, col = as.factor(Depth_m)))+
  geom_point()

ghgs%>% #Plot CH4 at Site 50 in BVR
  filter(Depth_m<100,
         Reservoir == "BVR")%>%
  ggplot(aes(x = Date, y = ch4_umolL, col = as.factor(Depth_m)))+
  geom_point()

ghgs%>% #Plot CO2 at Site 50 in BVR
  filter(Depth_m<100,
         Reservoir == "BVR")%>%
  ggplot(aes(x = Date, y = co2_umolL, col = as.factor(Depth_m)))+
  geom_point()

##############################################################################

# This section calculates % difference between replicates
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

ghgs_reps <- left_join(ghgs_rep1,ghgs_rep2,by=c("Date","Time","Depth_m","Reservoir"))

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

## Flag replicates: 
# Flag 3 = Difference between samples >LOQ and percent difference >30% but <50%
# Flag 4 = Difference between samples >LOQ and percent difference >50%
ghgs_reps <- ghgs_reps %>% 
  mutate(flag_ch4 = ifelse(ch4_pdiff>=50 & ch4_diff>=0.00252*3,4,
                           ifelse(ch4_pdiff<=50 & ch4_pdiff>=30 & ch4_diff>=0.00252*3,3,
                                  NA)))

ghgs_reps <- ghgs_reps %>% 
  mutate(flag_co2 = ifelse(co2_pdiff>=50 & co2_diff>=4.36*3,4,
                           ifelse(co2_pdiff<=50 & co2_pdiff>=30 & co2_diff>=4.36*3,3,
                                  NA)))
