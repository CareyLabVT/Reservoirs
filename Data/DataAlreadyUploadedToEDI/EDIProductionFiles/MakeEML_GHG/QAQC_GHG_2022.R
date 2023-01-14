### GHG for EDI - script to load in previous data from EDI, attach new data
### from current year, and clean data for publication to EDI

### Following: GHG_forEDI.Rmd by Abby Lewis
### Adapted by: A. Hounshell, 29 November 2021
### Updated for 2022 data: A. Hounshell, 16 June 2022
### Updated with new MDLs from Bobbie; 30 June 2022

##############################################################################

## Clear workspace
rm(list = ls())

## Load necessary pacakges
pacman::p_load(tidyverse, lubridate, readxl)

##############################################################################

## Load previous data from EDI (2015-2021)

# Package ID: edi.551.6 Cataloging System:https://pasta.edirepository.org.
# Data set title: Time series of dissolved methane and carbon dioxide concentrations for Falling Creek Reservoir and Beaverdam Reservoir in southwestern Virginia, USA during 2015-2021.
# Data set creator:  Cayelan Carey - Virginia Tech 
# Data set creator:  Alexandria Hounshell - Virginia Tech 
# Data set creator:  Ryan McClure - Virginia Tech 
# Data set creator:  Alexandra Gerling - Virginia Tech 
# Data set creator:  Abigail Lewis - Virginia Tech 
# Data set creator:  Barbara Niederlehner - Virginia Tech 
# Contact:  Cayelan Carey -  Virginia Tech  - Cayelan@vt.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/551/6/38d72673295864956cccd6bbba99a1a3" 
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
                 "flag_DateTime",     
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
if (class(dt1$flag_DateTime)=="factor") dt1$flag_DateTime <-as.numeric(levels(dt1$flag_DateTime))[as.integer(dt1$flag_DateTime) ]               
if (class(dt1$flag_DateTime)=="character") dt1$flag_DateTime <-as.numeric(dt1$flag_DateTime)
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
dt1$flag_DateTime <- ifelse((trimws(as.character(dt1$flag_DateTime))==trimws("NA")),NA,dt1$flag_DateTime)               
suppressWarnings(dt1$flag_DateTime <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$flag_DateTime))==as.character(as.numeric("NA"))),NA,dt1$flag_DateTime))
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
summary(flag_DateTime)
summary(flag_ch4)
summary(flag_co2) 
# Get more details on character variables

summary(as.factor(dt1$Reservoir))
detach(dt1)      

dt1 = dt1 %>%
  mutate(Depth_m = ifelse(Site==6,6,Depth_m),
         Site = ifelse(Site==6,50,Site),
         Depth_m = ifelse(Site==10&Reservoir=="BVR",10,Depth_m),
         Site = ifelse(Site==10&Reservoir=="BVR",50,Site),
         Site = ifelse(Depth_m==100,100,Site),
         Depth_m = ifelse(Depth_m==100,0.1,Depth_m),
         Site = ifelse(Depth_m==200,200,Site),
         Depth_m = ifelse(Depth_m==200,0.1,Depth_m))#Some issues with 2021 data

#############################################################################

## Load in data from 2022 - via the MEGA GHG Datasheet
#Load this year's data
ghgs <- read_excel("./Data/DataNotYetUploadedToEDI/Raw_GHG/2022/GHG_MEGA_GC_SHEET_EXCEL_2022.xlsx",sheet=2, na = c("","NA"))

#Change column names to match EDI standard
colnames(ghgs)[1:5] <- c("DateTime","Station","Depth_m","Reservoir","Rep")
colnames(ghgs)[14:15] <- c("ch4_umolL","co2_umolL")

#Rename Pipe/Pool stations following 2021 EDI publication
# Pipe1 = 1.1
# Pipe2 = 1.2
# Pipe3 = 1.3
# Pool3 = 1.4

#For 2022 data publication ONLY:
#Rename BVR 1 -> BVR 40

ghgs <- ghgs %>% 
  mutate(Station = ifelse(Station == "Pool3", 1.4, 
                          ifelse(Station == "Pipe3", 1.3, 
                                 ifelse(Station == "Pipe2", 1.2, 
                                        ifelse(Station == "Pipe1", 1.1, Station))))) %>% 
  mutate(Station = ifelse(Reservoir == "BVR" & Station == 1, 40, Station))

ghgs <- ghgs%>%
  select(DateTime,Station,Depth_m,Reservoir,Rep,ch4_umolL,co2_umolL)%>% #Select only the columns we are using
  filter(!is.na(Depth_m), !Depth_m=="NA")%>%
  mutate(DateTime = as.POSIXct(DateTime, "%Y-%m-%d %HH:%MM", tz="EST"), #Make sure columns are the correct type
         Depth_m = as.numeric(Depth_m), # CHANGE TO as.numeric AFTER NAMING IS FINALIZED
         Reservoir = as.factor(Reservoir)) %>%
  mutate(DateTime = force_tz(DateTime,tzone = "EST")) %>% 
  filter(!is.na(Depth_m)) #Depth cannot be missing

ghgs$ch4_umolL <- as.numeric(ghgs$ch4_umolL)
ghgs$co2_umolL <- as.numeric(ghgs$co2_umolL)

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

# Calculate % difference between replicates for current year
# Flag 1 = Sample not collected
# Flag 2 = Sample below MDL (for partial 2022: 0.0123 umol/L CH4; 4.81 umol/L CO2)
# Flag 3 = Difference between samples >LOQ and percent difference >30% but <50%
# Flag 4 = Difference between samples >LOQ and percent difference >50%
# NOTE: All flagged reps were retained in the dataset

## REMOVE SAMPLES WITH NEGATIVE VALUES -> 0
ghgs <- ghgs %>% 
  mutate(ch4_umolL = ifelse(ch4_umolL < 0, 0, ch4_umolL),
         co2_umolL = ifelse(co2_umolL < 0, 0, co2_umolL))

## Separate into rep 1 and rep2
ghgs_rep1 <- ghgs %>% 
  filter(Rep == "1") %>% 
  rename(ch4_umolL_rep1 = ch4_umolL, co2_umolL_rep1 = co2_umolL)

ghgs_rep2 <- ghgs %>% 
  filter(Rep == "2") %>% 
  rename(ch4_umolL_rep2 = ch4_umolL, co2_umolL_rep2 = co2_umolL)

ghgs_reps <- left_join(ghgs_rep1,ghgs_rep2,by=c("DateTime","Station","Depth_m","Reservoir"))

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
  select(DateTime,Station,Depth_m,Reservoir,ch4_umolL_rep1,co2_umolL_rep1,ch4_pdiff,ch4_diff,co2_pdiff,co2_diff) %>% 
  mutate(Rep = "1") %>% 
  rename(ch4_umolL = ch4_umolL_rep1,
         co2_umolL = co2_umolL_rep1)

ghg_rep2 <- ghgs_reps %>% 
  select(DateTime,Station,Depth_m,Reservoir,ch4_umolL_rep2,co2_umolL_rep2,ch4_pdiff,ch4_diff,co2_pdiff,co2_diff) %>% 
  mutate(Rep = "2") %>% 
  rename(ch4_umolL = ch4_umolL_rep2,
         co2_umolL = co2_umolL_rep2)

ghg_all <- rbind(ghg_rep1,ghg_rep2)

ghg_all <-  ghg_all %>% 
  arrange(DateTime,Reservoir,Station,Depth_m)

## Flag replicates: 

## Will need to be updated for 2022!!

# Flag 1 = Sample not collected
# Flag 2 = Sample below MDL (for partial 2022: 0.0123 umol/L CH4; 4.81 umol/L CO2)
# Flag 3 = Difference between samples >LOQ and percent difference >30% but <50%
# Flag 4 = Difference between samples >LOQ and percent difference >50%
ghg_all <- ghg_all %>% 
  mutate(flag_ch4 = ifelse(ch4_pdiff>=50 & ch4_diff>=0.0123*3,4,
                           ifelse(ch4_pdiff<=50 & ch4_pdiff>=30 & ch4_diff>=0.0123*3,3,
                                         NA)),
         flag_co2 = ifelse(co2_pdiff>=50 & co2_diff>=4.81*3,4,
                           ifelse(co2_pdiff<=50 & co2_pdiff>=30 & co2_diff>=4.81*3,3,
                                         NA)))

ghg_all <- ghg_all %>% 
  mutate(flag_ch4 = ifelse(ch4_umolL <= 0.0123,2,flag_ch4),
         flag_co2 = ifelse(co2_umolL <= 4.81,2,flag_co2))

ghg_all <- ghg_all %>% 
  mutate(flag_ch4 = ifelse(is.na(ch4_umolL), 1, flag_ch4),
         flag_co2 = ifelse(is.na(co2_umolL), 1, flag_co2))

ghg_all <- ghg_all %>% 
  mutate(flag_ch4 = ifelse(is.na(flag_ch4), 0, flag_ch4),
         flag_co2 = ifelse(is.na(flag_co2), 0, flag_co2))

### Create a DateTime Flag for non-recorded times (i.e., 12:00)
ghg_all <- ghg_all %>% 
  mutate(Time = format(DateTime,"%H:%M:%S")) %>% 
  mutate(flag_DateTime = ifelse(Time == "12:00:00", 1, 0)) %>% 
  select(-Time) %>% 
  relocate(flag_DateTime,.before=flag_ch4)

### Re-order to join with historical data
ghg_all <- ghg_all %>% 
  select(DateTime,Station,Depth_m,Reservoir,ch4_umolL,co2_umolL,flag_DateTime,flag_ch4,flag_co2,Rep) %>% 
  rename(Site = Station)

col_order <- c("Reservoir","Site","DateTime","Depth_m","Rep","ch4_umolL","co2_umolL","flag_DateTime","flag_ch4","flag_co2")

ghg_all <- ghg_all[,(col_order)]

### For 2022 only - update MDLs with Bobbie's running MDL calculations ###
# Follows standardized methods for calculating MDL using running blanks throughout each year
# Continue to follow this method going forward!

# First, change negative values -> 0 following comments from Bobbie
final_wide <- dt1 %>% 
  mutate(ch4_umolL = ifelse(ch4_umolL < 0, 0, ch4_umolL),
         co2_umolL = ifelse(co2_umolL < 0, 0, co2_umolL)) %>% 
  pivot_wider(names_from = Rep, values_from = c(ch4_umolL, co2_umolL), values_fn = mean)

final_wide <- final_wide %>% 
  mutate(year = year(DateTime),
         ch4_diff = abs(ch4_umolL_1-ch4_umolL_2),
         ch4_pdiff= round((abs(ch4_umolL_1-ch4_umolL_2)/(abs(ch4_umolL_1+ch4_umolL_2)/2))*100),
         co2_diff = abs(co2_umolL_1-co2_umolL_2),
         co2_pdiff = round((abs(co2_umolL_1-co2_umolL_2)/(abs(co2_umolL_1+co2_umolL_2)/2))*100))

# Define flagging for each year
# Flag 1 = Sample not collected
# Flag 2 = Sample below MDL (for 2022: 0.0107 umol/L CH4; 3.848 umol/L CO2)
# Flag 3 = Difference between samples >LOQ and percent difference >30% but <50%
# Flag 4 = Difference between samples >LOQ and percent difference >50%

## Start with 2015
final_wide_2015 <- final_wide %>% 
  filter(year==2015) %>% 
  distinct_at(vars(Reservoir,Site,DateTime,Depth_m,flag_DateTime),.keep_all=TRUE) %>% 
  mutate(flag_ch4 = ifelse(ch4_pdiff>=50 & ch4_diff>0.001*3,4,
                           ifelse(ch4_pdiff<=50 & ch4_pdiff>=30 & ch4_diff>0.001*3,3,
                                  NA)),
         flag_co2 = ifelse(co2_pdiff>=50 & co2_diff>2.13*3,4,
                           ifelse(co2_pdiff<=50 & co2_pdiff>=30 & co2_diff>2.13*3,3,
                                  NA)))

final_2015_co2 <- final_wide_2015 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_co2,co2_umolL_1,co2_umolL_2,co2_umolL_3,co2_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_co2,names_to="Rep",values_to="co2_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "co2_umolL_1", 1, 
                      ifelse(Rep == "co2_umolL_2",2,
                             ifelse(Rep == "co2_umolL_3", 3,
                                    ifelse(Rep == "co2_umolL_4", 4, NA)))))

final_2015_ch4 <- final_wide_2015 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_ch4,ch4_umolL_1,ch4_umolL_2,ch4_umolL_3,ch4_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_ch4,names_to="Rep",values_to="ch4_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "ch4_umolL_1", 1, 
                      ifelse(Rep == "ch4_umolL_2",2,
                             ifelse(Rep == "ch4_umolL_3", 3,
                                    ifelse(Rep == "ch4_umolL_4", 4, NA)))))

final_2015 <- left_join(final_2015_co2,final_2015_ch4,by=c("Reservoir","Site","DateTime","Depth_m","flag_DateTime","Rep")) %>% 
  mutate(Rep = ifelse(Rep == 3 & is.na(co2_umolL) & is.na(ch4_umolL) | Rep == 4 & is.na(co2_umolL) & is.na(ch4_umolL), NA, Rep)) %>% 
  drop_na(Rep)

final_2015 <- final_2015 %>% 
  mutate(flag_ch4 = ifelse(ch4_umolL < 0.001, 2, flag_ch4),
         flag_co2 = ifelse(co2_umolL < 2.13, 2, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(ch4_umolL), 1, flag_ch4),
         flag_co2 = ifelse(is.na(co2_umolL), 1, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(flag_ch4), 0, flag_ch4),
         flag_co2 = ifelse(is.na(flag_co2), 0, flag_co2))

## 2016
final_wide_2016 <- final_wide %>% 
  filter(year==2016) %>% 
  distinct_at(vars(Reservoir,Site,DateTime,Depth_m,flag_DateTime),.keep_all=TRUE) %>% 
  mutate(flag_ch4 = ifelse(ch4_pdiff>=50 & ch4_diff>0.006*3,4,
                           ifelse(ch4_pdiff<=50 & ch4_pdiff>=30 & ch4_diff>0.006*3,3,
                                  NA)),
         flag_co2 = ifelse(co2_pdiff>=50 & co2_diff>4.45*3,4,
                           ifelse(co2_pdiff<=50 & co2_pdiff>=30 & co2_diff>4.45*3,3,
                                  NA)))  

final_2016_co2 <- final_wide_2016 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_co2,co2_umolL_1,co2_umolL_2,co2_umolL_3,co2_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_co2,names_to="Rep",values_to="co2_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "co2_umolL_1", 1, 
                      ifelse(Rep == "co2_umolL_2",2,
                             ifelse(Rep == "co2_umolL_3", 3,
                                    ifelse(Rep == "co2_umolL_4", 4, NA)))))

final_2016_ch4 <- final_wide_2016 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_ch4,ch4_umolL_1,ch4_umolL_2,ch4_umolL_3,ch4_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_ch4,names_to="Rep",values_to="ch4_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "ch4_umolL_1", 1, 
                      ifelse(Rep == "ch4_umolL_2",2,
                             ifelse(Rep == "ch4_umolL_3", 3,
                                    ifelse(Rep == "ch4_umolL_4", 4, NA)))))

final_2016 <- left_join(final_2016_co2,final_2016_ch4,by=c("Reservoir","Site","DateTime","Depth_m","flag_DateTime","Rep")) %>% 
  mutate(Rep = ifelse(Rep == 3 & is.na(co2_umolL) & is.na(ch4_umolL) | Rep == 4 & is.na(co2_umolL) & is.na(ch4_umolL), NA, Rep)) %>% 
  drop_na(Rep)

final_2016 <- final_2016 %>% 
  mutate(flag_ch4 = ifelse(ch4_umolL < 0.006, 2, flag_ch4),
         flag_co2 = ifelse(co2_umolL < 4.45, 2, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(ch4_umolL), 1, flag_ch4),
         flag_co2 = ifelse(is.na(co2_umolL), 1, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(flag_ch4), 0, flag_ch4),
         flag_co2 = ifelse(is.na(flag_co2), 0, flag_co2))

## 2017
final_wide_2017 <- final_wide %>% 
  filter(year==2017) %>% 
  distinct_at(vars(Reservoir,Site,DateTime,Depth_m,flag_DateTime),.keep_all=TRUE) %>% 
  mutate(flag_ch4 = ifelse(ch4_pdiff>=50 & ch4_diff>0.005*3,4,
                           ifelse(ch4_pdiff<=50 & ch4_pdiff>=30 & ch4_diff>0.005*3,3,
                                  NA)),
         flag_co2 = ifelse(co2_pdiff>=50 & co2_pdiff>12.70*3,4,
                           ifelse(co2_pdiff<=50 & co2_pdiff>=30 & co2_diff>12.70*3,3,
                                  NA)))

final_2017_co2 <- final_wide_2017 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_co2,co2_umolL_1,co2_umolL_2,co2_umolL_3,co2_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_co2,names_to="Rep",values_to="co2_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "co2_umolL_1", 1, 
                      ifelse(Rep == "co2_umolL_2",2,
                             ifelse(Rep == "co2_umolL_3", 3,
                                    ifelse(Rep == "co2_umolL_4", 4, NA)))))

final_2017_ch4 <- final_wide_2017 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_ch4,ch4_umolL_1,ch4_umolL_2,ch4_umolL_3,ch4_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_ch4,names_to="Rep",values_to="ch4_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "ch4_umolL_1", 1, 
                      ifelse(Rep == "ch4_umolL_2",2,
                             ifelse(Rep == "ch4_umolL_3", 3,
                                    ifelse(Rep == "ch4_umolL_4", 4, NA)))))

final_2017 <- left_join(final_2017_co2,final_2017_ch4,by=c("Reservoir","Site","DateTime","Depth_m","flag_DateTime","Rep")) %>% 
  mutate(Rep = ifelse(Rep == 3 & is.na(co2_umolL) & is.na(ch4_umolL) | Rep == 4 & is.na(co2_umolL) & is.na(ch4_umolL), NA, Rep)) %>% 
  drop_na(Rep)

final_2017 <- final_2017 %>% 
  mutate(flag_ch4 = ifelse(ch4_umolL < 0.005, 2, flag_ch4),
         flag_co2 = ifelse(co2_umolL < 12.70, 2, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(ch4_umolL), 1, flag_ch4),
         flag_co2 = ifelse(is.na(co2_umolL), 1, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(flag_ch4), 0, flag_ch4),
         flag_co2 = ifelse(is.na(flag_co2), 0, flag_co2))

## 2018
final_wide_2018 <- final_wide %>% 
  filter(year==2018) %>% 
  distinct_at(vars(Reservoir,Site,DateTime,Depth_m,flag_DateTime),.keep_all=TRUE) %>% 
  mutate(flag_ch4 = ifelse(ch4_pdiff>=50 & ch4_diff>0.005*3,4,
                           ifelse(ch4_pdiff<=50 & ch4_pdiff>=30 & ch4_diff>0.005*3,3,
                                  NA)),
         flag_co2 = ifelse(co2_pdiff>=50 & co2_diff>21.84*3,4,
                           ifelse(co2_pdiff<=50 & co2_pdiff>=30 & co2_diff>21.84*3,3,
                                  NA)))

final_2018_co2 <- final_wide_2018 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_co2,co2_umolL_1,co2_umolL_2,co2_umolL_3,co2_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_co2,names_to="Rep",values_to="co2_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "co2_umolL_1", 1, 
                      ifelse(Rep == "co2_umolL_2",2,
                             ifelse(Rep == "co2_umolL_3", 3,
                                    ifelse(Rep == "co2_umolL_4", 4, NA)))))

final_2018_ch4 <- final_wide_2018 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_ch4,ch4_umolL_1,ch4_umolL_2,ch4_umolL_3,ch4_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_ch4,names_to="Rep",values_to="ch4_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "ch4_umolL_1", 1, 
                      ifelse(Rep == "ch4_umolL_2",2,
                             ifelse(Rep == "ch4_umolL_3", 3,
                                    ifelse(Rep == "ch4_umolL_4", 4, NA)))))

final_2018 <- left_join(final_2018_co2,final_2018_ch4,by=c("Reservoir","Site","DateTime","Depth_m","flag_DateTime","Rep")) %>% 
  mutate(Rep = ifelse(Rep == 3 & is.na(co2_umolL) & is.na(ch4_umolL) | Rep == 4 & is.na(co2_umolL) & is.na(ch4_umolL), NA, Rep)) %>% 
  drop_na(Rep)

final_2018 <- final_2018 %>% 
  mutate(flag_ch4 = ifelse(ch4_umolL < 0.005, 2, flag_ch4),
         flag_co2 = ifelse(co2_umolL < 21.84, 2, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(ch4_umolL), 1, flag_ch4),
         flag_co2 = ifelse(is.na(co2_umolL), 1, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(flag_ch4), 0, flag_ch4),
         flag_co2 = ifelse(is.na(flag_co2), 0, flag_co2))

## 2019
final_wide_2019 <- final_wide %>% 
  filter(year==2019) %>% 
  distinct_at(vars(Reservoir,Site,DateTime,Depth_m,flag_DateTime),.keep_all=TRUE) %>% 
  mutate(flag_ch4 = ifelse(ch4_pdiff>=50 & ch4_diff>0.011*3,4,
                           ifelse(ch4_pdiff<=50 & ch4_pdiff>=30 & ch4_diff>0.011*3,3,
                                  NA)),
         flag_co2 = ifelse(co2_pdiff>=50 & co2_diff>5.08*3,4,
                           ifelse(co2_pdiff<=50 & co2_pdiff>=30 & co2_diff>5.08*3,3,
                                  NA)))

final_2019_co2 <- final_wide_2019 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_co2,co2_umolL_1,co2_umolL_2,co2_umolL_3,co2_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_co2,names_to="Rep",values_to="co2_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "co2_umolL_1", 1, 
                      ifelse(Rep == "co2_umolL_2",2,
                             ifelse(Rep == "co2_umolL_3", 3,
                                    ifelse(Rep == "co2_umolL_4", 4, NA)))))

final_2019_ch4 <- final_wide_2019 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_ch4,ch4_umolL_1,ch4_umolL_2,ch4_umolL_3,ch4_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_ch4,names_to="Rep",values_to="ch4_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "ch4_umolL_1", 1, 
                      ifelse(Rep == "ch4_umolL_2",2,
                             ifelse(Rep == "ch4_umolL_3", 3,
                                    ifelse(Rep == "ch4_umolL_4", 4, NA)))))

final_2019 <- left_join(final_2019_co2,final_2019_ch4,by=c("Reservoir","Site","DateTime","Depth_m","flag_DateTime","Rep")) %>% 
  mutate(Rep = ifelse(Rep == 3 & is.na(co2_umolL) & is.na(ch4_umolL) | Rep == 4 & is.na(co2_umolL) & is.na(ch4_umolL), NA, Rep)) %>% 
  drop_na(Rep)

final_2019 <- final_2019 %>% 
  mutate(flag_ch4 = ifelse(ch4_umolL < 0.011, 2, flag_ch4),
         flag_co2 = ifelse(co2_umolL < 5.08, 2, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(ch4_umolL), 1, flag_ch4),
         flag_co2 = ifelse(is.na(co2_umolL), 1, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(flag_ch4), 0, flag_ch4),
         flag_co2 = ifelse(is.na(flag_co2), 0, flag_co2))

## 2020
final_wide_2020 <- final_wide %>% 
  filter(year==2020) %>% 
  distinct_at(vars(Reservoir,Site,DateTime,Depth_m,flag_DateTime),.keep_all=TRUE) %>% 
  mutate(flag_ch4 = ifelse(ch4_pdiff>=50 & ch4_diff>0.013*3,4,
                           ifelse(ch4_pdiff<=50 & ch4_pdiff>=30 & ch4_diff>0.013*3,3,
                                  NA)),
         flag_co2 = ifelse(co2_pdiff>=50 & co2_diff>6.17*3,4,
                           ifelse(co2_pdiff<=50 & co2_pdiff>=30 & co2_diff>6.17*3,3,
                                  NA)))

final_2020_co2 <- final_wide_2020 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_co2,co2_umolL_1,co2_umolL_2,co2_umolL_3,co2_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_co2,names_to="Rep",values_to="co2_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "co2_umolL_1", 1, 
                      ifelse(Rep == "co2_umolL_2",2,
                             ifelse(Rep == "co2_umolL_3", 3,
                                    ifelse(Rep == "co2_umolL_4", 4, NA)))))

final_2020_ch4 <- final_wide_2020 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_ch4,ch4_umolL_1,ch4_umolL_2,ch4_umolL_3,ch4_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_ch4,names_to="Rep",values_to="ch4_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "ch4_umolL_1", 1, 
                      ifelse(Rep == "ch4_umolL_2",2,
                             ifelse(Rep == "ch4_umolL_3", 3,
                                    ifelse(Rep == "ch4_umolL_4", 4, NA)))))

final_2020 <- left_join(final_2020_co2,final_2020_ch4,by=c("Reservoir","Site","DateTime","Depth_m","flag_DateTime","Rep")) %>% 
  mutate(Rep = ifelse(Rep == 3 & is.na(co2_umolL) & is.na(ch4_umolL) | Rep == 4 & is.na(co2_umolL) & is.na(ch4_umolL), NA, Rep)) %>% 
  drop_na(Rep)

final_2020 <- final_2020 %>% 
  mutate(flag_ch4 = ifelse(ch4_umolL < 0.013, 2, flag_ch4),
         flag_co2 = ifelse(co2_umolL < 6.17, 2, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(ch4_umolL), 1, flag_ch4),
         flag_co2 = ifelse(is.na(co2_umolL), 1, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(flag_ch4), 0, flag_ch4),
         flag_co2 = ifelse(is.na(flag_co2), 0, flag_co2))

## 2021
final_wide_2021 <- final_wide %>% 
  filter(year==2021) %>% 
  distinct_at(vars(Reservoir,Site,DateTime,Depth_m,flag_DateTime),.keep_all=TRUE) %>% 
  mutate(flag_ch4 = ifelse(ch4_pdiff>=50 & ch4_diff>0.021*3,4,
                           ifelse(ch4_pdiff<=50 & ch4_pdiff>=30 & ch4_diff>0.021*3,3,
                                  NA)),
         flag_co2 = ifelse(co2_pdiff>=50 & co2_diff>5.74*3,4,
                           ifelse(co2_pdiff<=50 & co2_pdiff>=30 & co2_diff>5.74*3,3,
                                  NA)))

final_2021_co2 <- final_wide_2021 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_co2,co2_umolL_1,co2_umolL_2,co2_umolL_3,co2_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_co2,names_to="Rep",values_to="co2_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "co2_umolL_1", 1, 
                      ifelse(Rep == "co2_umolL_2",2,
                             ifelse(Rep == "co2_umolL_3", 3,
                                    ifelse(Rep == "co2_umolL_4", 4, NA)))))

final_2021_ch4 <- final_wide_2021 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_ch4,ch4_umolL_1,ch4_umolL_2,ch4_umolL_3,ch4_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_ch4,names_to="Rep",values_to="ch4_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "ch4_umolL_1", 1, 
                      ifelse(Rep == "ch4_umolL_2",2,
                             ifelse(Rep == "ch4_umolL_3", 3,
                                    ifelse(Rep == "ch4_umolL_4", 4, NA)))))

final_2021 <- left_join(final_2021_co2,final_2021_ch4,by=c("Reservoir","Site","DateTime","Depth_m","flag_DateTime","Rep")) %>% 
  mutate(Rep = ifelse(Rep == 3 & is.na(co2_umolL) & is.na(ch4_umolL) | Rep == 4 & is.na(co2_umolL) & is.na(ch4_umolL), NA, Rep)) %>% 
  drop_na(Rep)

final_2021 <- final_2021 %>% 
  mutate(flag_ch4 = ifelse(ch4_umolL < 0.021, 2, flag_ch4),
         flag_co2 = ifelse(co2_umolL < 5.74, 2, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(ch4_umolL), 1, flag_ch4),
         flag_co2 = ifelse(is.na(co2_umolL), 1, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(flag_ch4), 0, flag_ch4),
         flag_co2 = ifelse(is.na(flag_co2), 0, flag_co2))

## 2022
final_wide_2022 <- final_wide %>% 
  filter(year==2022) %>% 
  distinct_at(vars(Reservoir,Site,DateTime,Depth_m,flag_DateTime),.keep_all=TRUE) %>% 
  mutate(flag_ch4 = ifelse(ch4_pdiff>=50 & ch4_diff>0.0107*3,4,
                           ifelse(ch4_pdiff<=50 & ch4_pdiff>=30 & ch4_diff>0.0107*3,3,
                                  NA)),
         flag_co2 = ifelse(co2_pdiff>=50 & co2_diff>3.848*3,4,
                           ifelse(co2_pdiff<=50 & co2_pdiff>=30 & co2_diff>3.848*3,3,
                                  NA)))

final_2021_co2 <- final_wide_2021 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_co2,co2_umolL_1,co2_umolL_2,co2_umolL_3,co2_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_co2,names_to="Rep",values_to="co2_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "co2_umolL_1", 1, 
                      ifelse(Rep == "co2_umolL_2",2,
                             ifelse(Rep == "co2_umolL_3", 3,
                                    ifelse(Rep == "co2_umolL_4", 4, NA)))))

final_2021_ch4 <- final_wide_2021 %>% 
  select(Reservoir,Site,DateTime,Depth_m,flag_DateTime,flag_ch4,ch4_umolL_1,ch4_umolL_2,ch4_umolL_3,ch4_umolL_4) %>% 
  pivot_longer(!Reservoir:flag_ch4,names_to="Rep",values_to="ch4_umolL")  %>% 
  mutate(Rep = ifelse(Rep == "ch4_umolL_1", 1, 
                      ifelse(Rep == "ch4_umolL_2",2,
                             ifelse(Rep == "ch4_umolL_3", 3,
                                    ifelse(Rep == "ch4_umolL_4", 4, NA)))))

final_2021 <- left_join(final_2021_co2,final_2021_ch4,by=c("Reservoir","Site","DateTime","Depth_m","flag_DateTime","Rep")) %>% 
  mutate(Rep = ifelse(Rep == 3 & is.na(co2_umolL) & is.na(ch4_umolL) | Rep == 4 & is.na(co2_umolL) & is.na(ch4_umolL), NA, Rep)) %>% 
  drop_na(Rep)

final_2021 <- final_2021 %>% 
  mutate(flag_ch4 = ifelse(ch4_umolL < 0.0107, 2, flag_ch4),
         flag_co2 = ifelse(co2_umolL < 3.848, 2, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(ch4_umolL), 1, flag_ch4),
         flag_co2 = ifelse(is.na(co2_umolL), 1, flag_co2)) %>% 
  mutate(flag_ch4 = ifelse(is.na(flag_ch4), 0, flag_ch4),
         flag_co2 = ifelse(is.na(flag_co2), 0, flag_co2))

## Combine all historical data - now with updated MDLs!
historical <- rbind(final_2015,final_2016,final_2017,final_2018,final_2019,final_2020,final_2021)

historical <- historical[,(col_order)]

###############################################################################

### Combine historical and new data

final <- rbind(historical,ghg_all)

final <- final %>% 
  arrange(DateTime,Reservoir,Site,Depth_m)

## For 2022 data publication ONLY
# Change BVR 1 -> BVR 40
final <- final %>% 
  mutate(Site = ifelse(Reservoir == "BVR" & Site == 1, 40, Site))%>%
  rename(CH4_umolL = ch4_umolL,
         CO2_umolL = co2_umolL,
         Flag_DateTime = flag_DateTime,
         Flag_CH4_umolL = flag_ch4,
         Flag_CO2_umolL = flag_co2)

### Save GHG file!
write.csv(final,"./Data/DataNotYetUploadedToEDI/Raw_GHG/2022/Dissolved_GHG_2015_2022.csv",row.names = FALSE)

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
