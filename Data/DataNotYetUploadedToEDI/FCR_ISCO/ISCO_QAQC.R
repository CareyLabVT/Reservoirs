#### ISCO QAQC Script ####

### Purpose: read in ISCO water level files from Github, combine into single dataset for each year, 
### plot to visualize, and check for missing data
### Then, combine all years into a single dataframe and write to csv.

## Created by: Nick Hammond
## Last updated: 12 August 2022

### NOTES: raw ISCO water level files are located on GitHub -> CareyLabVT -> Reservoirs -> Data -> DataNotYetUploadedToEDI -> FCR_ISCO

#### Load Packages ####

#packages needed
library(lubridate)
library(tidyverse)
library(magrittr)
library(gganimate)
library(gifski)
require(transformr)
library(stringr)
library(readxl)



#set working directory
setwd('C:/Reservoirs/Data/DataNotYetUploadedToEDI/FCR_ISCO')



#### 2019 ####
# 2019 water level data is already combined into a single csv, so no need to read in multiple files and stack them
# Read in csv file and format properly
ISCO_19 = read_csv('190601_191201_ISCO_FCRWeir_Level.csv', skip = 6)
colnames(ISCO_19) <- c("DateTime","WaterLevel_m")
ISCO_19$DateTime = mdy_hm(ISCO_19$DateTime, tz="America/New_York")
ISCO_19 = ISCO_19[-nrow(ISCO_19),] # remove NA's at the end
ISCO_19$WaterLevel_m = as.numeric(ISCO_19$WaterLevel_m)


# Ensure Dataset completeness (check for gaps in data)

# Identify dates with missing data

ISCO_19 = ISCO_19 %>% mutate(Date = date(DateTime)) # add new column with just the dates
date_range = seq.Date(from = min(ISCO_19$Date,na.rm = T),to = max(ISCO_19$Date,na.rm = T), by = "day") #create vector of dates for entire range
date_range[!date_range %in% ISCO_19$Date] # print out dates that are not present


# Plot to visualize
ISCO_19 %>% group_by(Date) %>% summarize(daily_flow = mean(WaterLevel_m)) %>% 
  ggplot() +
  geom_point(aes(x =Date, y = daily_flow))




#### 2020 ####


# 2020 water level data is already combined into a single csv, so no need to read in multiple files and stack them
# Read in csv file and format properly
ISCO_20 = read_csv('2020_full_ISCO_FCRWEIR_WaterLevel.csv', skip = 5)
colnames(ISCO_20) <- c("DateTime","WaterLevel_m")
ISCO_20$DateTime = mdy_hms(ISCO_20$DateTime, tz="America/New_York")
ISCO_20 = ISCO_20[-nrow(ISCO_20),] # remove NA's at the end
ISCO_20$WaterLevel_m = as.numeric(ISCO_20$WaterLevel_m)
ISCO_20 = ISCO_20[-c(1),] # remove first entry, which seems like an error

# Ensure Dataset completeness (check for gaps in data)

# Identify dates with missing data

ISCO_20 = ISCO_20 %>% mutate(Date = date(DateTime)) # add new column with just the dates
date_range = seq.Date(from = min(ISCO_20$Date,na.rm = T),to = max(ISCO_20$Date,na.rm = T), by = "day") #create vector of dates for entire range
date_range[!date_range %in% ISCO_20$Date] # print out dates that are not present


# Plot to visualize
ISCO_20 %>% group_by(Date) %>% summarize(daily_flow = mean(WaterLevel_m)) %>% 
ggplot() +
  geom_point(aes(x =Date, y = daily_flow))





#### 2021 ####


# 2021 water level data is in multiple files on github, so we first need to read them all in and combine them

# Create a character string of all 2020 ISCO files 
files_21<-list.files(path="./", pattern = "^21")

# Prepare data frame w/column headers 
ISCO_colnames = c("DateTime","WaterLevel_m")
obs2 <- as.data.frame(matrix(NA,0,length(ISCO_colnames)))
names(obs2) <- ISCO_colnames
obs2$DateTime=ymd_hms(obs2$DateTime, tz="America/New_York")

#Read in all files within folder in Github
for(i in 1:length(files_21)){
  if(file.size(paste0("./",files_21[i]))>4000){
    temp<-read_csv(file=paste0("./",files_21[i]),skip=5)
    names(temp) <- ISCO_colnames
    temp$DateTime=mdy_hms(temp$DateTime, tz="America/New_York")
    obs2<-rbind(obs2,temp)
    #print(i)
  }
}

# Remove duplicated rows (resulting from overlap in files)
obs2 = unique(obs2)

# Rename dataframe
ISCO_21 = obs2


# Ensure Dataset completeness (check for gaps in data)

# Identify dates with missing data

ISCO_21 = ISCO_21 %>% mutate(Date = date(DateTime)) # add new column with just the dates
date_range = seq.Date(from = min(ISCO_21$Date,na.rm = T),to = max(ISCO_21$Date,na.rm = T), by = "day") #create vector of dates for entire range
date_range[!date_range %in% ISCO_21$Date] # print out dates that are not present


# Plot to visualize
ISCO_21 %>% group_by(Date) %>% summarize(daily_flow = mean(WaterLevel_m)) %>% 
  ggplot() +
  geom_point(aes(x =Date, y = daily_flow))




#### 2022 ####

# 2022 water level data is already combined into a single csv, so no need to read in multiple files and stack them
# Read in csv file and format properly
ISCO_22 = read_csv('220803_ISCO_FCRWEIR_WaterLevel.csv', skip = 5)
colnames(ISCO_22) <- c("DateTime","WaterLevel_m")
ISCO_22$DateTime = mdy_hms(ISCO_22$DateTime, tz="America/New_York")
ISCO_22$WaterLevel_m = as.numeric(ISCO_22$WaterLevel_m)

# Ensure Dataset completeness (check for gaps in data)

# Identify dates with missing data

ISCO_22 = ISCO_22 %>% mutate(Date = date(DateTime)) # add new column with just the dates
date_range = seq.Date(from = min(ISCO_22$Date,na.rm = T),to = max(ISCO_22$Date,na.rm = T), by = "day") #create vector of dates for entire range
date_range[!date_range %in% ISCO_22$Date] # print out dates that are not present


# Plot to visualize
ISCO_22 %>% group_by(Date) %>% summarize(daily_flow = mean(WaterLevel_m)) %>% 
  ggplot() +
  geom_point(aes(x =Date, y = daily_flow))



#### Combine all years into a single file ####
ISCO_19_22 = bind_rows(ISCO_19,ISCO_20,ISCO_21,ISCO_22)

# Plot to visualize
ISCO_19_22 %>% group_by(Date) %>% summarize(daily_flow = mean(WaterLevel_m)) %>% 
  ggplot() +
  geom_point(aes(x =Date, y = daily_flow))

ISCO_19_22 %>% select(DateTime,WaterLevel_m) %>% 
  write_excel_csv(file="2019_2022_ISCO_FCRWEIR_WaterLevel.csv")
