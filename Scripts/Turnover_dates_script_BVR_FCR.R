### Title: Script to calculate turnover in Falling Creek and Beaverdam
### Author: Adrienne Breef-Pilz
### 27 Jan 2025

### This script takes the function that determines if the reservoir is mixed or stratified based on density. The function was written by Freya Olsson. Take the output from the function and filter by mixed dates (observation =1) and then find the time that happens for the first time after September 1st. because we want to identify fall mixing. 

## load packages


## load function from GitHub

source("https://raw.githubusercontent.com/LTREB-reservoirs/vera4cast/refs/heads/main/targets/target_functions/target_generation_mixed_binary_daily.R")


### Turnover dates for Beaverdam Reservoir

## the link to the current Beaverdam data
bvr_current <- c("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv")

## link to the historical Beaverdam data on EDI. Make sure this is the most up to date data from EDI. 
bvr_historic <- c("https://pasta.lternet.edu/package/data/eml/edi/725/5/f649de0e8a468922b40dcfa34285055e")


## Run the function and the output if each day is mixed or stratified. 
a <- target_generation_mixed_binary_daily(bvr_current, bvr_historic)

## For each year get the date of fall mixing
bvr_turnover <- a|>
  mutate(Year = year(datetime),
         DOY = yday(datetime))|>
  filter(observation==1 & DOY>200)|>
  group_by(Year)|>
  slice_min(datetime, n = 1)


### Get turnover for FCR

## the link to the current Falling Creek data
fcr_historic <- "https://pasta.lternet.edu/package/data/eml/edi/271/9/f23d27b67f71c25cb8e6232af739f986"

## link to the historical Falling Creek data on EDI. Make sure this is the most up to date data from EDI. 
fcr_current <- "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/refs/heads/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv"


## Run the function and the output if each day is mixed or stratified. 
ag <- target_generation_mixed_binary_daily(fcr_current, fcr_historic)


## For each year get the date of fall mixing
fcr_turnover <- ag|>
  mutate(Year = year(datetime),
         DOY = yday(datetime))|>
  filter(observation==1 & DOY>200)|>
  group_by(Year)|>
  slice_min(datetime, n = 1)


### bind the BVR files and FCR files together and rename the Sites
turnover <- dplyr::bind_rows(fcr_turnover,bvr_turnover)|>
  mutate(Reservoir=ifelse(site_id=="fcre", "FCR", "BVR"))|>
 # filter(datetime<as.Date("2024-01-01"))|> # filter time if you want to 
  select(datetime, Reservoir, Year)

# Add in BVR observations from Cece Wood which are most likely from Mary Lofton
turn <- data.frame(
  datetime = c(as.Date("2018-10-24"), as.Date("2019-11-02")),
  Reservoir = c("BVR", "BVR"),
  Year = c(2018, 2019)
)

# add in the manual dates to the dates from the sensors
turnover <- dplyr::bind_rows(turn, turnover)
