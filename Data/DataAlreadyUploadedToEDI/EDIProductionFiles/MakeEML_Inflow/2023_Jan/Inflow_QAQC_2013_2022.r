# Title: Prepare FCR inflow data for publication to EDI
# Author: Mary Lofton
# Date 18DEC18

###############################################################################

# Updated: 17Dec19 by R. Corrigan and A. Hounshell
#   Added script to incorporate Diana (VT) data for publication
#   Updated flags for v-notch weir

###############################################################################

# Updated: 07Feb20 by A. Hounshell to included WVWA data through 31 Jan 2020

###############################################################################

# Updated: 20Feb20 by A. Hounshell - not correctly flagging low-flow data for 
# the v-notch weir; as of 07 Jun 2019, pressure sensors should be at the same 
# location as the bottom of the weir

###############################################################################

# Updated: 06Mar20 by A. Hounshell - updated V-notch weir calculations with 
# rating curve calculated from correlations between gauge height and pressure 
# for the WVWA and VT pressure sensor from 7 Jun 2019 to present. Updates to this
# relationship will be needed for 2020 EDI push. Relationships is documented in 
# EDI metadata and with associated data file with data for the gage height vs. 
# pressure relationships. The gage relationship updates when the weir is too 
# low/over-topped for the period AFTER 6 Jun 2019. Nothing was changed for data 
# prior to 6 Jun 2019.

###############################################################################

# Updated: 04Jan21 by A. Hounshell - updated for EDI 2021 publishing. 
# A few key notes: 
# 1. The weir was 'renovated' in August 2020 which resulted in changes to 
# sensor (WVWA and VT) location. 
# A new rating curve has been calculated and will be applied to data collected 
# after September 2020. 
# 2. Removed the down-correction originally applied for all data after 18 Apr 
# 2016 for data collected using the v-notch weir

###############################################################################

# Updated: 22Oct21 by A. Hounshell
# A few key notes:
# 1. Updated weir rating curve after 02 Sep 2020 using additional data from:
#      2020_WeirWaterLevel: https://docs.google.com/spreadsheets/d/14XlOGqaUZYJO6aOLvMPDrvNAxooOaThK17PU8w7LCIE/edit#gid=0
#      2021_WeirWaterLevel: https://docs.google.com/spreadsheets/d/1w5i30nNg4T2nuPkcKDmC5Pp2Hly8Ddy6gtS6vR1znGk/edit#gid=0
# 2. Incorporated rating curve calculations into the script - no longer need
# additional .csv's for rating curves
# 3. Additional updates will be needed for data after July 2021

#################Notes Jan 2022##############################################################

# Updated: 12Jan22 by W. Woelmer
# added newest VT and WVWA data, updated file naming convention to Inflow_BEGINYEAR_ENDYEAR.csv, e.g. Inflow_2013_2022.csv
# updated flags so '0' indicates no flag and 'NA' indicates no data, so no corresponding flag

#############Notes May 2022##################################################################

# Updated: 17May22 by A. Hounshell
# added newest VT and WVWA data
# NOTES: SOMETHING HAPPENED TO WVWA PRESSURE DATA AFTER OCT 2021; ALSO HAVE SOME FUNKY SURFACE PRESSURE IN MAR 2022
# NEED TO ADDRESS PRIOR TO PUBLICATION!!!!

#############Notes Jan 2023##################################################################

# Updated 06 Jan 23 by A. Breef-Pilz
# Added 2022 weir water level file from Google Drive:
# 2022_WeirWaterLevel: https://docs.google.com/spreadsheets/d/1KpYXbfSP2m8diI_RIBVAEKRrUw1BoUY3ro0KE14qoro/edit#gid=0
# added a water level observations to RatingCurveDates.csv
# Updated rating curves after the sensors were moved for maintenance in: 2021-11-08,and 2022-05-09
#
# Fixed timestamp in WVWA sensor so all in EST. 

# Added automated rating curve so updates with new data

# ABP overhaul the script and add the maintenance log in so when issues arise we can put it in the 
# maintenance log
#
###############################################################################
# Install packages
pacman::p_load(tidyverse, lubridate, magrittr, ggplot2)

#plotting theme
mytheme = theme(axis.title = element_text(size = 16),
                axis.text = element_text(size = 16))

######### read in Data from pressure transducer at the weir (WVWA pressure data)#######
mydir = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Inflow_CSV"
myfiles = list.files(path=mydir, patter="FCR_15min_Inf*", full.names=TRUE)#list the files from BVR platform


#create dataframe for the for loop
out.file<-""

#combine all of the files and make sure all times are in EST
for(k in 1:length(myfiles)){
  files<-read.csv(myfiles[k],skip= 28, header=T) #get header minus wonky Campbell rows
  
  # convert date time so we can determine if it is in EST or EDT
  Date = parse_date_time(files[1,"Date.Time"], 'dmy HMS', tz="America/New_York")
  
  # Check if the first observation was in EST or EDT. 
  # If FALSE then it is in EST, if TRUE then EDT
  if(dst(Date)==T){
    print(Date)
    print("Daylight savings")
    
    # Give datetime the EST timestamp before converting
    files$DateTime = parse_date_time(files$Date.Time, 'dmy HMS', tz="Etc/GMT+5")
    
    # Data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set
    files$DateTime[c(1:nrow(files))]<-with_tz(force_tz(files$DateTime[c(1:nrow(files))],"Etc/GMT+4"), "Etc/GMT+5") 
    
    # Now strip the time so it stays in EST but give it a UTC timestamp to make life easier
    #files$DateTime<-as.POSIXct(strptime(files$DateTime, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
    files$DateTime<-as.character(files$DateTime)
    
    #files = files[,-c(1:2)] #limits data to necessary columns
    
  }else{# This is already in EST. Yea!!
    print(Date)
    print("NOT Daylight Savings Time")
    
    # Convert to yyyy-mm-dd hh:mm:ss
    files$DateTime = parse_date_time(files$Date.Time, 'dmy HMS', tz="Etc/GMT+5")
    
    # Makes binding rows easier for now
    files$DateTime<-as.character(files$DateTime)
    
  }
  # Now bind the rows together after they are all in EST
  out.file=rbind(out.file, files)
}


##A wee bit o' data wrangling to get column names and formats in good shape
# All data collected in EST but not defining a timezone here 
inflow_pressure <- out.file %>%
  select(-Date.Time, -Rec..) %>%
  filter(DateTime!="")%>%
  mutate(DateTime=ymd_hms(DateTime))%>%
  rename(Pressure_psi = Pressure.psi.,
         Temp_C = Temperature.degC.)%>%
  mutate(Pressure_psi = as.double(Pressure_psi),
         Temp_C = as.double(Temp_C))%>%
  mutate(DateTime = round_date(DateTime,"15 minutes"))%>%
  arrange(DateTime)%>%
  distinct()%>%
  select(DateTime, Temp_C, Pressure_psi)

#### Load in data from the catwalk air pressure sensor#####

## Read in catwalk pressure data: from WVWA instruments
#pressure_a4d <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Barometric_CSV", pattern = "FCR_BV*") %>% 
#  map_df(~read_csv(file.path(path = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Barometric_CSV", .), col_types = cols(.default = "c"), skip = 28))
#pressure_a4d = pressure_a4d[,-c(1,3)]

mydir2 = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Barometric_CSV"
myfiles2 = list.files(path=mydir2, patter="FCR_BV*", full.names=TRUE)#list the files from BVR platform

#create dataframe for the for loop
out.file<-""

#combine all of the files and make sure all times are in EST
for(j in 1:length(myfiles2)){
  files<-read.csv(myfiles2[j],skip= 28, header=T) #get header minus wonky Campbell rows
  
  # convert date time so we can determine if it is in EST or EDT
  Date = parse_date_time(files[1,"Date.Time"], 'dmy HMS', tz="America/New_York")
  
  # Check if the first observation was in EST or EDT. 
  # If FALSE then it is in EST, if TRUE then EDT
  if(dst(Date)==T){
    print(Date)
    print("Daylight savings")
    
    # Give datetime the EST timestamp before converting
    files$DateTime = parse_date_time(files$Date.Time, 'dmy HMS', tz="Etc/GMT+5")
    
    # Data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set
    files$DateTime[c(1:nrow(files))]<-with_tz(force_tz(files$DateTime[c(1:nrow(files))],"Etc/GMT+4"), "Etc/GMT+5") 
    
    # Now strip the time so it stays in EST but give it a UTC timestamp to make life easier
    #files$DateTime<-as.POSIXct(strptime(files$DateTime, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
    files$DateTime<-as.character(files$DateTime)
    
    #files = files[,-c(1:2)] #limits data to necessary columns
    
  }else{# This is already in EST. Yea!!
    print(Date)
    print("NOT Daylight Savings Time")
    
    # Convert to yyyy-mm-dd hh:mm:ss
    files$DateTime = parse_date_time(files$Date.Time, 'dmy HMS', tz="Etc/GMT+5")
    
    # Makes binding rows easier for now
    files$DateTime<-as.character(files$DateTime)
    
  }
  # Now bind the rows together after they are all in EST
  out.file=rbind(out.file, files)
}


##A wee bit o' data wrangling to get column names and formats in good shape
# All data collected in EST but not defining a timezone here 
pressure_a4d <- out.file %>%
  select(-Date.Time, -Rec..,-Temperature.degC.) %>%
  filter(DateTime!="")%>%
  mutate(DateTime=ymd_hms(DateTime))%>%
  rename(Baro_pressure_psi = Pressure.psi.)%>%
  mutate(Baro_pressure_psi = as.double(Baro_pressure_psi))%>%
  mutate(DateTime = round_date(DateTime,"15 minutes"))%>%
  arrange(DateTime)%>%
  distinct()%>%
  select(DateTime,Baro_pressure_psi)


## Data wrangling to get columns in correct format and combine data from senvu.net and Aqua4Plus
pressure <- read_csv("./Data/DataNotYetUploadedToEDI/WVWA_DO_sondes/FCR_DOsonde_2012to2017.csv",col_types=list("c","d","d","d","d","d","d","d","l","l","l","l","l","l","l","l"))

baro_pressure = pressure %>%
  select(Date, `Barometric Pressure Pressure (PSI)`)%>%
  mutate(DateTime = ymd_hms(Date))%>%
  dplyr::rename(Baro_pressure_psi = `Barometric Pressure Pressure (PSI)`) %>%
  select(-Date) %>%
  mutate(Baro_pressure_psi = as.double(Baro_pressure_psi))%>%
  filter(!is.na(Baro_pressure_psi)) %>%
  bind_rows(.,pressure_a4d)%>%
  arrange(DateTime)%>%
  select(DateTime, Baro_pressure_psi)
  
# Take out duplicates 
baro_pressure=baro_pressure[!duplicated(baro_pressure$DateTime), ]
  

#### Combine inflow and baro pressure####

#merge inflow and barometric pressures to do differencing
diff = left_join(baro_pressure, inflow_pressure, by = "DateTime")%>% 
  select(DateTime, Temp_C, Pressure_psi, Baro_pressure_psi)%>%
  dplyr::rename('WVWA_Temp_C'='Temp_C', 
                'WVWA_Pressure_psi'='Pressure_psi', 
                'WVWA_Baro_pressure_psi'='Baro_pressure_psi')

# Find the pressure of the water by correcting with atmospheric pressure
diff$WVWA_Pressure_psia=diff$WVWA_Pressure_psi-diff$WVWA_Baro_pressure_psi
  
# Take out NAs when one value is missing
diff<-diff%>%drop_na(WVWA_Pressure_psia)

diff$WVWA_Flow_cms <- NA

### Read in and process VT pressure sensor ####

VTsens <- read_csv(file.path('https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-weir-data/FCRweir.csv'),skip=4,col_names=c("TimeStamp","Record","BattV","PTemp_C","AirTemp_C","Lvl_psi","wtr_weir"))
VTsens$DateTime <- ymd_hms(VTsens$TimeStamp)

# Make sure we aren't missing any data

#Check for record gaps and day gpas

#order data by timestamp
VTsens=VTsens[order(VTsens$DateTime),]
VTsens$DOY=yday(VTsens$DateTime)


#check record for gaps
#daily record gaps by day of year
for(i in 2:nrow(VTsens)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(VTsens$DOY[i]-VTsens$DOY[i-1]>1){
    print(c(VTsens$DateTime[i-1],VTsens$DateTime[i]))
  }
}
# #sub-daily record gaps by record number
for(i in 2:length(VTsens$Record)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(abs(VTsens$Record[i]-VTsens$Record[i-1])>1){
    print(c(VTsens$DateTime[i-1],VTsens$DateTime[i]))
  }
}

# Select the columns you want
VTdat <- VTsens[,c(8,6,7)]
# Rename the columns
colnames(VTdat) <- c('DateTime', 'VT_Pressure_psia', 'VT_Temp_C')


# Add a column to calculate flow
VTdat$VT_Flow_cms <- NA

### Combine the WVWA sensor and the VT sensor ######
All_Inflow <- merge(diff, VTdat, by='DateTime', all=TRUE)

# subset to publish just last year's data
All_Inflow <- All_Inflow%>%
  filter(DateTime<ymd_hms("2023-01-01 00:00:00"))

# Rearrange the columns

All_Inflow<-All_Inflow%>%
  select("DateTime", "WVWA_Pressure_psi", "WVWA_Baro_pressure_psi", "WVWA_Pressure_psia",
                     "WVWA_Flow_cms", "WVWA_Temp_C", "VT_Pressure_psia", "VT_Flow_cms", "VT_Temp_C")

#### Add Flag columns ### 
# for loop to create flag columns
for(j in c(2:9)) { #for loop to create new columns in data frame
  All_Inflow[,paste0("Flag_",colnames(All_Inflow[j]))] <- 0 #creates flag column + name of variable
}


#### QAQC with the Maintenance Log ####

### Read in Maintenance/Rating DateTimes ###
log <- read_csv(file.path('https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-weir-data/Weir_MaintenanceLog.txt'),skip=0, col_types = cols(
  .default = col_character(),
  TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  flag = col_integer()
)) 

# separate the maintenance time from the dates and times for rating curves

main_log <- log%>%
  filter(DataStream=="WEIR")

### QAQC based on Maintenance log ###

# modify catdata based on the information in the log   

for(i in 1:nrow(main_log))
{
  # get start and end time of one maintenance event
  start <- main_log$TIMESTAMP_start[i]
  end <- main_log$TIMESTAMP_end[i]
  
  
  # get indices of columns affected by maintenance
  if(grepl("^\\d+$", main_log$colnumber[i])) # single num
  {
    maintenance_cols <- intersect(c(2:9), as.integer(main_log$colnumber[i]))
  }
  
  else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", main_log$colnumber[i])) # c(x;y;...)
  {
    maintenance_cols <- intersect(c(2:9), as.integer(unlist(regmatches(main_log$colnumber[i],
                                                                        gregexpr("\\d+", main_log$colnumber[i])))))
  }
  else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", main_log$colnumber[i])) # c(x:y)
  {
    bounds <- as.integer(unlist(regmatches(main_log$colnumber[i], gregexpr("\\d+", main_log$colnumber[i]))))
    maintenance_cols <- intersect(c(2:9), c(bounds[1]:bounds[2]))
  }
  else
  {
    warning(paste("Could not parse column colnumber in row", i, "of the maintenance main_log. Skipping maintenance for",
                  "that row. The value of colnumber should be in one of three formats: a single number (\"47\"), a",
                  "semicolon-separated list of numbers in c() (\"c(47;48;49)\"), or a range of numbers in c() (\"c(47:74)\").",
                  "Other values (even valid calls to c()) will not be parsed properly."))
    next
  }
  
  if(length(maintenance_cols) == 0)
  {
    warning(paste("Did not parse any valid data columns in row", i, "of the maintenance main_log. Valid columns have",
                  "indices 2 through 9. Skipping maintenance for that row."))
    next
  }
  
  #index the Flag columns
  if(grepl("^\\d+$", main_log$flagcol[i])) # single num
  {
    flag_cols <- intersect(c(10:17), as.integer(main_log$flagcol[i]))
    
  }
  else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", main_log$flagcol[i])) # c(x;y;...)
  {
    flag_cols <- intersect(c(10:17), as.integer(unlist(regmatches(main_log$flagcol[i],
                                                                  gregexpr("\\d+", main_log$flagcol[i])))))
  }
  
  else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", main_log$flagcol[i])) # c(x:y)
  {
    bounds_flag <- as.integer(unlist(regmatches(main_log$flagcol[i], gregexpr("\\d+", main_log$flagcol[i]))))
    flag_cols <- intersect(c(10:17), c(bounds_flag[1]:bounds_flag[2]))
  }
  else
  {
    warning(paste("Could not parse column flagcol in row", i, "of the maintenance log. Skipping maintenance for",
                  "that row. The value of colnumber should be in one of three formats: a single number (\"47\"), a",
                  "semicolon-separated list of numbers in c() (\"c(47;48;49)\"), or a range of numbers in c() (\"c(47:74)\").",
                  "Other values (even valid calls to c()) will not be parsed properly."))
    next
  }
  
  #Get the Maintenance Flag 
  
  flag <- main_log$flag[i]
  
  # replace relevant data with NAs and set flags while maintenance was in effect
  if(flag==1 && maintenance_cols==2){ # for down correcting
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, maintenance_cols] <- All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, maintenance_cols]-0.14
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, flag_cols] <- flag
  
    # correct flags for those that were down corrected and if the flow is too low based on pressure sensor
    All_Inflow <- All_Inflow%>%
      mutate(Flag_WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                                         WVWA_Pressure_psia<0.184,13, Flag_WVWA_Flow_cms))
    
    # correct flags for those that were down corrected and if the flow is too high and over tops the weir
    All_Inflow <- All_Inflow%>%
      mutate(Flag_WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                                         WVWA_Pressure_psia>0.611,16, Flag_WVWA_Flow_cms))
    
  }else if(flag==1 && maintenance_cols==7){ # for up correcting for VT sensor
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, maintenance_cols] <- All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, maintenance_cols]+0.033
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, flag_cols] <- flag
  }
  
  else{ # All other flags and change to NA
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, maintenance_cols] <- NA
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, flag_cols] <- flag
  }
}

#### Read in gague height #####

# Guage height csv
# Load in this spreadsheet and align with atmospheric corrected pressure measured by the WVWA sensor
rating_curve_dates <- read_csv("./Data/DataNotYetUploadedToEDI/Raw_inflow/RatingCurveDates.csv")
rating_curve_dates$DateTime <- as.POSIXct(strptime(rating_curve_dates$DateTime, "%m/%d/%Y %H:%M", tz="UTC"))

# Round time to nearest 15 minutes
rating_curve_dates <- rating_curve_dates %>% 
  mutate(DateTime = round_date(DateTime,"15 minutes")) %>% 
  select(DateTime,GageHeight_cm)

rating_curve <- left_join(rating_curve_dates, All_Inflow, by="DateTime")
rating_curve <- rating_curve%>% 
  select(DateTime,GageHeight_cm, WVWA_Pressure_psia, VT_Pressure_psia)%>%
  drop_na(GageHeight_cm)
# This is the data you will use to update the rating curve!
# We will calculate a rating curve with data from each time period

# Filter out the DateTime for making rating curve
# The flag numbers are used for filtering out in the if statements
rat_log <- log%>%
  filter(DataStream=="RATING")

### Get Dates and Times for rating curve ####

# modify catdata based on the information in the log   
for(i in 1:nrow(rat_log)){
  # get start and end time of one maintenance event
  start <- rat_log$TIMESTAMP_start[i]
  end <- rat_log$TIMESTAMP_end[i]
  
  # get indices of columns affected by maintenance
  
    main_cols <- as.integer(rat_log$colnumber[i])
  
  # get number to identify time and info for rating curve
  
    info_cols <- as.integer(rat_log$flag[i])
  
  # getting liner realtionship between psi and guage height and calculating flow
  if(info_cols==100){
    
    # the old weir equations are taken directly from MEL's Inflow Aggregation script
    # Use for pressure data prior to 2019-06-06: see notes above for description of equations
    # NOTE: Pressure_psia < 0.185 calculates -flows (and are automatically set to NA's)
    # THIS WILL NOT CHANGE EVER AGAIN! KEEP AS IS FOR RECTANGULAR WEIR
    
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, "flow1"]<-All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, "WVWA_Pressure_psia"]* 0.70324961490205 - 0.1603375 + 0.03048 
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, "flow_cfs"]<- (0.62 * (2/3) * (1.1) * 4.43 * (All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, "flow1"] ^ 1.5) * 35.3147)
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, main_cols]<-All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, "flow_cfs"]*0.028316847
    
    # take out columns you don't need any more
    All_Inflow<-subset(All_Inflow, select=-c(flow1, flow_cfs))
    if(main_cols==5){
    # correct flags for flow being too low based on pressure sensor
    All_Inflow <- All_Inflow%>%
      mutate(Flag_WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                                         Flag_WVWA_Flow_cms!=13 & WVWA_Pressure_psia<0.184,3, Flag_WVWA_Flow_cms))
  
    # Make flow NA when psi <= 0.184 (distance between pressure sensor and bottom of weir = 0.1298575 m = 0.18337 psi
    All_Inflow <- All_Inflow%>%
      mutate(WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                                         WVWA_Pressure_psia<0.184,NA, WVWA_Flow_cms))
  
    # Will also need to flag flows when water tops the weir: for the rectangular weir, head = 0.3 m + 0.1298575 m = 0.4298575 m
    # This corresponds to Pressure_psia <= 0.611244557965199
    # diff_pre$Flow_cms = ifelse(diff_pre$Pressure_psia > 0.611, NA, diff_pre$Flow_cms)
    # Decided to keep data from above the weir, but flag!
    All_Inflow <- All_Inflow%>%
      mutate(Flag_WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                          Flag_WVWA_Flow_cms!=16 & WVWA_Pressure_psia>0.611,6, Flag_WVWA_Flow_cms))
    }
    
    if (main_cols==8){
    # correct flags for flow being too low based on pressure sensor
    All_Inflow <- All_Inflow%>%
      mutate(Flag_VT_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(VT_Pressure_psia) &
                                       VT_Pressure_psia<0.184,3, Flag_VT_Flow_cms))
      
      # Make flow NA when psi <= 0.184 (distance between pressure sensor and bottom of weir = 0.1298575 m = 0.18337 psi
      All_Inflow <- All_Inflow%>%
        mutate(VT_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(VT_Pressure_psia) &
                                      VT_Pressure_psia<0.184,NA, VT_Flow_cms))
      
      # Will also need to flag flows when water tops the weir: for the rectangular weir, head = 0.3 m + 0.1298575 m = 0.4298575 m
      # This corresponds to Pressure_psia <= 0.611244557965199
      # diff_pre$Flow_cms = ifelse(diff_pre$Pressure_psia > 0.611, NA, diff_pre$Flow_cms)
      # Decided to keep data from above the weir, but flag!
      All_Inflow <- All_Inflow%>%
        mutate(Flag_VT_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(VT_Pressure_psia) &
                                            VT_Pressure_psia>0.611,6, Flag_VT_Flow_cms))
    }
    # q = 2.391 * H^2.5
    # where H = head in meters above the notch
    # the head was 14.8 cm on June 24 at ~13:30
    #14.8 cm is 0.148 m 
    #14.9cm on Jun 27 at 3:49PM
    # WW: Used scaling relationship to determine head:pressure relationship
    # diff_post <- diff_post %>%  mutate(head = (0.149*Pressure_psia)/0.293) %>% 
    #   mutate(Flow_cms = 2.391* (head^2.5)) %>% 
    #   select(DateTime, Temp_C, Baro_pressure_psi, Pressure_psi, Pressure_psia, Flow_cms)
    
  }
    if(info_cols!=100 && main_cols==5){
    # Can use this for the rest of the time because everything is done here
    # This is for WVWA sensor
    # figure out the liner regression 
    
    # select rating curve based on date time
    sel_WVWA<- rating_curve[rating_curve$DateTime >= start & rating_curve$DateTime <= end,]
    
       # Let's calculate flow for WVWA
      #get linear relationship between pressure gage and 
     fit_WVWA <- lm(sel_WVWA$GageHeight_cm ~ sel_WVWA$WVWA_Pressure_psia)
     plot(sel_WVWA$GageHeight_cm ~ sel_WVWA$WVWA_Pressure_psia)
     # Pull out intercept
     int_WVWA<-as.numeric(fit_WVWA$coefficients[1])  
     
     # Pull out slope
     slo_WVWA<-as.numeric(fit_WVWA$coefficients[2])
     
     # Calculate the Flow
     All_Inflow <- All_Inflow %>% mutate(head = ((slo_WVWA*WVWA_Pressure_psia)+int_WVWA)/100) %>% 
       mutate(WVWA_Flow_cms = 2.391 * (head^2.5)) %>% 
       select(-c(head))
     
     # take out values when flow is too low to detect
     # figure out when that is 
     low_WVWA<-(-int_WVWA)/slo_WVWA
    
       # take out the low values and flag
       # According to gage height vs. pressure relationship as calculated above
     
     All_Inflow<-All_Inflow%>%
       mutate(Flag_WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                                          WVWA_Pressure_psia<low_WVWA,3, Flag_WVWA_Flow_cms))%>%
       mutate(WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                                     WVWA_Pressure_psia<low_WVWA,NA, WVWA_Flow_cms))
       
       
     # flag when water is over top of the weir at 27.5 cm but not taking it out
      # figure out at what psia that is at 
     high_WVWA=(27.5-int_WVWA)/slo_WVWA
     
     # Print the slope and intercept 
     cat("WVWA_Sensor.","Start Date",as.character(start), "End date" ,as.character(end),"Slope is" ,slo_WVWA, ".Intercept is", int_WVWA, ".Low psi is ", low_WVWA, ".High psi is", high_WVWA)
     
     # flag those values but don't remove them
     All_Inflow<-All_Inflow%>%
     mutate(Flag_WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                                        WVWA_Pressure_psia>high_WVWA,6, Flag_WVWA_Flow_cms))
     
      }else if (info_cols!=100 && main_cols==8){# Let's do it for VT Sensor
        
        # select rating curve based on date time
        sel_VT<- rating_curve[rating_curve$DateTime >= start & rating_curve$DateTime <= end,]
        
       # What to do about if there is only one observation 
        
      fit_VT <- lm(sel_VT$GageHeight_cm ~ sel_VT$VT_Pressure_psia)
      plot(sel_VT$GageHeight_cm ~ sel_VT$VT_Pressure_psia)
      
      # Pull out intercept
      int_VT<-as.numeric(fit_VT$coefficients[1])  
      
      # Pull out slope
      slo_VT<-as.numeric(fit_VT$coefficients[2])
      
    
      # Calculate Flow
      All_Inflow <- All_Inflow %>% mutate(head_VT = ((slo_VT*VT_Pressure_psia)+int_VT)/100) %>% 
        mutate(VT_Flow_cms = 2.391 * (head_VT^2.5)) %>% 
        select(-c(head_VT))
      
      # take out values when flow is too low to detect
      # figure out when that is 
      low_VT<-(-int_VT)/slo_VT
      
      # take out the low values and flag
      # According to gage height vs. pressure relationship as calculated above
      
      All_Inflow<-All_Inflow%>%
        mutate(Flag_VT_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(VT_Pressure_psia) &
                                         VT_Pressure_psia<low_VT,3, Flag_VT_Flow_cms))%>%
        mutate(VT_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(VT_Pressure_psia) &
                                      VT_Pressure_psia<low_VT,NA, VT_Flow_cms))
        
      
      # flag when water is over top of the weir at 27.5 cm but not taking it out
      # figure out at what psia that is at 
      high_VT=(27.5-int_VT)/slo_VT
      
      # Print the slope and intercept 
      cat("VT_Sensor.","Start Date",as.character(start), "End date" ,as.character(end),"Slope is" ,slo_VT, ".Intercept is", int_VT, ".Low psi is ", low_VT, ".High psi is", high_VT)
      
      
      # flag those values
      All_Inflow<-All_Inflow%>%
        mutate(Flag_VT_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(VT_Pressure_psia) &
                                           VT_Pressure_psia>high_VT,6, Flag_VT_Flow_cms))
      }else 
      print(paste("Could not parse column flagcol in row", i, "of the rating log."))
    next
  }
   
### Add flags for missing values ####
# for loop to create flag columns
for(j in c(2:9)) { #for loop to create new columns in data frame
  All_Inflow[c(which(is.na(All_Inflow[,j]) & (All_Inflow[,paste0("Flag_",colnames(All_Inflow[j]))]==0))), paste0("Flag_",colnames(All_Inflow[j]))]=7#s #puts in flag 7 if value not collected
}


#### GRAPH #####
#### Check out to see what the flow looks like ######
b=All_Inflow%>%
  #filter(DateTime>as.Date("2022-12-01"))%>%
  ggplot(., aes(x=DateTime))+
  geom_point(aes(y=WVWA_Flow_cms), color="blue")+
  geom_point(aes(y=VT_Flow_cms), color="red")

### For WVWA_Pressure_psi
## Preliminary visualization of raw pressure data from inflow transducer

daily<- All_Inflow %>% 
  group_by( Date = as.Date(DateTime)) %>% 
  summarise(across(c(WVWA_Pressure_psi, WVWA_Baro_pressure_psi, WVWA_Pressure_psia,WVWA_Flow_cms,WVWA_Temp_C,
                     VT_Pressure_psia,VT_Flow_cms,VT_Temp_C), mean))%>%
  mutate(Year = as.factor(year(Date)),
         Month = month(Date))

rawplot = ggplot(data = daily, aes(x = Date, y = WVWA_Pressure_psi))+
  geom_point()+
  ylab("Daily avg. inflow pressure (psi)")+
  geom_vline(xintercept = as.Date('2016-04-18'))+ # Date when downcorrection started
  theme_bw()
rawplot

# Look at daily average WVWA pressure sensor. Change the data to look at days that are funky
ggplot(data = daily, aes(x = Date, y = WVWA_Pressure_psi))+
  geom_point()+
  ylab("Daily avg. inflow pressure (psi)")+
  theme_bw() +
  xlim(c(as.Date("2022-01-01"),as.Date("2022-12-31")))

# Look at all the readings to look at certain low days. 
ggplot(data = All_Inflow, aes(x = DateTime, y = WVWA_Pressure_psi))+
  geom_point()+
  ylab("Daily avg. inflow pressure (psi)")+
  theme_bw() +
  xlim(c(ymd_hms("2022-01-01 00:00:00"),ymd_hms("2022-01-31 23:50:00")))


pressure_hist = ggplot(data = daily, aes(x = WVWA_Pressure_psi, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. inflow pressure (psi)")+
  theme_bw()
pressure_hist
#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_inflow_pressure_histogram.png", pressure_hist, device = "png")

pressure_boxplot = ggplot(data = daily, aes(x = Year, y = WVWA_Pressures_psi, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. inflow pressure (psi)")+
  theme_bw()
pressure_boxplot
#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_inflow_pressure_boxplot.png", pressure_boxplot, device = "png")


###Preliminary visualization of raw pressure data from catwalk transducer####

rawplot = ggplot(data = daily)+
  geom_point(aes(x = Date, y = WVWA_Baro_pressure_psi))+
  ylab("Daily avg. catwalk pressure (psi)")+
  theme_bw()
rawplot

# Let's look at 2022 surface pressure 
ggplot(data = All_Inflow, aes(x = DateTime, y = WVWA_Baro_pressure_psi))+
  geom_point()+
  ylab("Daily avg. inflow pressure (psi)")+
  theme_bw() +
  xlim(c(ymd_hms("2022-01-01 00:00:00"),ymd_hms("2022-12-31 00:00:00")))


pressure_hist = ggplot(data = daily, aes(x = WVWA_Baro_pressure_psi, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. catwalk pressure (psi)")+
  theme_bw()
pressure_hist


pressure_boxplot = ggplot(data = daily, aes(x = Year, y = WVWA_Baro_pressure_psi, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. catwalk pressure (psi)")+
  theme_bw()
pressure_boxplot

### Look at VT pressure sensor readings ####

rawplot = ggplot(data = daily)+
  geom_point(aes(x = Date, y = VT_Pressure_psia))+
  ylab("Daily avg. VT pressure (psi)")+
  theme_bw()
rawplot

# Let's look at 2022 surface pressure 
ggplot(data = daily, aes(x = Date, y = VT_Pressure_psia))+
  geom_point()+
  ylab("Daily avg. VT pressure (psi)")+
  theme_bw() +
  xlim(c(as.Date("2021-01-01"),as.Date("2022-12-31")))


pressure_hist = ggplot(data = daily, aes(x = VT_Pressure_psia, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. VT pressure (psi)")+
  theme_bw()
pressure_hist

pressure_boxplot = ggplot(data = daily, aes(x = Year, y = VT_Pressure_psia, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. VT pressure (psi)")+
  theme_bw()
pressure_boxplot


###visualization of inflow####

inflow_hist = ggplot(data = daily, aes(x = WVWA_Flow_cms, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. inflow WVWA (cms)")+
  xlim(0,0.5)+
  theme_bw()
inflow_hist


inflow_boxplot = ggplot(data = daily, aes(x = Year, y = WVWA_Flow_cms, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. inflow WVWA (cms)")+
  ylim(0,0.3)+
  theme_bw()+
  mytheme
inflow_boxplot


inflow_hist = ggplot(data = daily, aes(x = VT_Flow_cms, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. inflow VT (cms)")+
  xlim(0,0.5)+
  theme_bw()
inflow_hist


inflow_boxplot = ggplot(data = daily, aes(x = Year, y = VT_Flow_cms, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. inflow VT (cms)")+
  ylim(0,0.3)+
  theme_bw()+
  mytheme
inflow_boxplot


##visualization of temp

temp2 = ggplot(daily, aes(x = Date, y = WVWA_Temp_C))+
  geom_line(size = 1)+
  ylab("Avg. daily WVWA temp (C)")+
  theme_bw()
temp2

temp_hist = ggplot(data = daily, aes(x = WVWA_Temp_C, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. WVWA temp (C)")+
  theme_bw()
temp_hist

temp_boxplot = ggplot(data = daily, aes(x = Year, y = WVWA_Temp_C, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. WVWA temp (C)")+
  theme_bw()
temp_boxplot

# VT Temp sensor check
temp2 = ggplot(daily, aes(x = Date, y = VT_Temp_C))+
  geom_line(size = 1)+
  ylab("Avg. daily VT temp (C)")+
  theme_bw()
temp2

temp_hist = ggplot(data = daily, aes(x = VT_Temp_C, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. VT temp (C)")+
  theme_bw()
temp_hist

temp_boxplot = ggplot(data = daily, aes(x = Year, y = VT_Temp_C, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. VT temp (C)")+
  theme_bw()
temp_boxplot


#### Make sure No NAs in Flag column ####
#prints table of flag frequency and make sure no NAs in Flag column

for(b in 10:17){
  print(colnames(All_Inflow[b]))
  print(table(All_Inflow[,b],useNA="always"))
}

##### This should be it ####
# Get it ready for publishing and get the maintenance log ready. 
#creating columns for EDI
All_Inflow$Reservoir <- "FCR" #creates reservoir column to match other data sets
All_Inflow$Site <- 100  #creates site column to match other data sets

Final<-All_Inflow%>%
  select(Reservoir, Site, everything())

##### Clean up the Maintenance Log and Rating Curve log #####
# Maintenance Log
names(log) = c("Station", "DateTime_start","DateTime_end", "Parameter", "ColumnNumber", "Flag", "FlagColumn","Notes") #finalized column names
log$Reservoir= "FCR"#add reservoir name for EDI archiving
log$Site=100 #add site column for EDI archiving

Final_log=log[,c(9:10,1:8)]

# Observed Gauge Height 
Final_GaugeHeight <- rating_curve_dates%>%
  mutate(Reservoir="FCR")%>%
  mutate(Site=100)%>%
  select(Reservoir, Site, DateTime, GageHeight_cm)


# Write to CSV
folder = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_Inflow/2023_Jan/"
write.csv(Final, paste0(folder,"Final_Inflow_2013_2022.csv"), row.names=F, quote=F)
write.csv(Final_log, paste0(folder, "Inflow_Maintenance_RatingCurveLog_2013_2022.csv"), row.names=F, quote=F)
write.csv(Final_GaugeHeight, paste0(folder, "Inflow_GaugeHeight_2013_2022.csv"), row.names=F, quote=F)
