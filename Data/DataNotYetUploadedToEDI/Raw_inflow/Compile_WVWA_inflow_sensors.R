# Combine old WVWA sensor data into one file so can read it into the weir QAQC function. 
# By: Adrienne Breef-Pilz

#### Load Packages ####
pacman::p_load("RCurl","tidyverse","lubridate")

### Read in current WVWA sensor files ####

current<- read_csv("./Data/DataNotYetUploadedToEDI/Raw_inflow/WVWA_pressure_readings_2013_current.csv")

# Function used to read in the files from the WVWA sensors and convert all times to EST
EST_convert<-function(data) {
  files<-read.csv(data,skip= 28, header=T) #get header minus wonky Campbell rows
  
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
    return(files)
  }else{# This is already in EST. Yea!!
    print(Date)
    print("NOT Daylight Savings Time")
    
    # Convert to yyyy-mm-dd hh:mm:ss
    files$DateTime = parse_date_time(files$Date.Time, 'dmy HMS', tz="Etc/GMT+5")
    
    # Makes binding rows easier for now
    files$DateTime<-as.character(files$DateTime)
    return(files)
  }
}

# Compile all the inflow pressure data from the weir into one file and clean it up 
inflow_pressure<- list.files(path="./Data/DataNotYetUploadedToEDI/Raw_inflow/Inflow_CSV", patter="FCR_15min_Inf*", full.names=TRUE)%>%
  map_df(EST_convert)%>%
  select(-Date.Time, -Rec..) %>%
  mutate(DateTime=ymd_hms(DateTime))%>%
  dplyr::rename(Pressure_psi = Pressure.psi.,
         Temp_C = Temperature.degC.)%>%
  mutate(Pressure_psi = as.double(Pressure_psi),
         Temp_C = as.double(Temp_C))%>%
  mutate(DateTime = round_date(DateTime,"15 minutes"))%>%
  arrange(DateTime)%>%
  distinct()%>%
  select(DateTime, Temp_C, Pressure_psi)

# Compile all the bp pressure data from the catwalk into one file and clean it up
pressure_a4d<-list.files(path="./Data/DataNotYetUploadedToEDI/Raw_inflow/Barometric_CSV", patter="FCR_BV*", full.names=TRUE)%>%
  map_df(EST_convert)%>%
  select(-Date.Time, -Rec..,-Temperature.degC.) %>%
  mutate(DateTime=ymd_hms(DateTime))%>%
  dplyr::rename(Baro_pressure_psi = Pressure.psi.)%>%
  mutate(Baro_pressure_psi = as.double(Baro_pressure_psi))%>%
  mutate(DateTime = round_date(DateTime,"15 minutes"))%>%
  arrange(DateTime)%>%
  distinct()%>%
  select(DateTime,Baro_pressure_psi)


# This is already in the current data frame that I orignially compiled so it is commented out
## Data wrangling to get columns in correct format and combine data from senvu.net and Aqua4Plus
#pressure <- read_csv("./Data/DataNotYetUploadedToEDI/WVWA_DO_sondes/FCR_DOsonde_2012to2017.csv",col_types=list("c","d","d","d","d","d","d","d","l","l","l","l","l","l","l","l"))

#baro_pressure = pressure %>%
#  select(Date, `Barometric Pressure Pressure (PSI)`)%>%
#  mutate(DateTime = ymd_hms(Date))%>%
#  dplyr::rename(Baro_pressure_psi = `Barometric Pressure Pressure (PSI)`) %>%
#  select(-Date) %>%
#  mutate(Baro_pressure_psi = as.double(Baro_pressure_psi))%>%
#  filter(!is.na(Baro_pressure_psi)) %>%
#  bind_rows(.,pressure_a4d)%>%
#  arrange(DateTime)%>%
#  select(DateTime, Baro_pressure_psi)

# Take out duplicates 
#baro_pressure=baro_pressure[!duplicated(baro_pressure$DateTime), ]

#merge inflow and barometric pressures to do differencing
diff = left_join(pressure_a4d, inflow_pressure, by = "DateTime")%>% 
  select(DateTime, Temp_C, Pressure_psi, Baro_pressure_psi)%>%
  dplyr::rename('WVWA_Temp_C'='Temp_C', 
                'WVWA_Pressure_psi'='Pressure_psi', 
                'WVWA_Baro_pressure_psi'='Baro_pressure_psi')

# Find the pressure of the water by correcting with atmospheric pressure
diff$WVWA_Pressure_psia=diff$WVWA_Pressure_psi-diff$WVWA_Baro_pressure_psi

# Take out NAs when one value is missing
diff<-diff%>%drop_na(WVWA_Pressure_psia)

# combine old and new together
final <- rbind(current, diff)

folder <- "./Data/DataNotYetUploadedToEDI/Raw_inflow/"
# Write to csv

write.csv(final, paste0(folder,"WVWA_pressure_readings_2013_current.csv"), row.names=F)

