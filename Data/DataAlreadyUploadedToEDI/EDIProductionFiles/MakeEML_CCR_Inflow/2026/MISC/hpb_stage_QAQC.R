#### QAQC CCR HPB pressure sensor to get stage data
## Dexter Howard - Jan 2026
#adapted from: https://github.com/dwh77/RHESSys_Tutorial/blob/main/Data_PreProcessing/HPB_stage_Q.Rmd

#load packages
library(tidyverse)
# library(ggpmisc) #stat poly line
# library(plotly)


#### Read in HPB stage data ####

#### 1st HOBO water level
hpb_hobo1_16sep24 <- read_csv("https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/CCR_manual_downloads/CCR_Hobos/HPB_waterlevel/hobo_hpb_16sep24.csv", skip = 1) |> 
  slice(1:(n() - 2)) |> #remove logger disconnect info at bottom
  select(2:4)

hpb_hobo1_17dec24 <- read_csv("https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/CCR_manual_downloads/CCR_Hobos/HPB_waterlevel/hobo_hpb_17dec24_2.csv", skip = 1) |> 
  slice(1:(n() - 5)) |> #remove logger disconnect info at bottom
  select(2:4)
    
hpb_hobo1 <- rbind(hpb_hobo1_16sep24, hpb_hobo1_17dec24) |> 
  rename(DateTime_EDT = 1, 
         HOBO_Abs_Pres_kPa = 2,
         Temp_C = 3)

hpb_hobo1 |> 
  ggplot(aes(x = DateTime_EDT, y = HOBO_Abs_Pres_kPa))+
  geom_point()

#### 2nd HOBO sensor water level

## Read in csv and trim excess
## skip 11jun and 9jul becasue 20aug has same data downloaded too
# hpb_hobo2_11jun25 <- read_csv("https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/refs/heads/master/CCR_manual_downloads/CCR_Hobos/HPB_waterlevel/CCR_HPB_20250611.csv", skip = 1) |> 
#   select(2:4)
# 
# hpb_hobo2_9jul25 <- read_csv("https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/refs/heads/master/CCR_manual_downloads/CCR_Hobos/HPB_waterlevel/CCR_HPB_20250709.csv", skip = 1) |> 
#   select(2:4)

# hpb_hobo2_20aug25 <- read_csv("https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/refs/heads/master/CCR_manual_downloads/CCR_Hobos/HPB_waterlevel/CCR_HPB_20250820.csv", skip = 1) |> 
#   select(2:4)

hpb_hobo2_14oct25 <- read_csv("https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/refs/heads/master/CCR_manual_downloads/CCR_Hobos/HPB_waterlevel/CCR_HPB_20251014.csv", skip = 1) |> 
  select(2:4)

# hpb_hobo2_11nov25 <- read_csv("https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/refs/heads/master/CCR_manual_downloads/CCR_Hobos/HPB_waterlevel/CCR_HPB_20251111.csv", skip = 1) |> 
#   select(2:4)

hpb_hobo2_2dec25 <- read_csv("https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/refs/heads/master/CCR_manual_downloads/CCR_Hobos/HPB_waterlevel/CCR_HPB_20251202.csv", skip = 1) |> 
  slice(1:(n() - 2)) |> #remove logger disconnect info at bottom
  select(2:4)

hpb_hobo2 <- rbind(hpb_hobo2_14oct25, hpb_hobo2_2dec25) |> 
  rename(DateTime_EDT = 1, 
         HOBO_Abs_Pres_kPa = 2,
         Temp_C = 3)

hpb_hobo2 |> 
  ggplot(aes(x = DateTime_EDT, y = HOBO_Abs_Pres_kPa))+
  geom_point()




#### Bind HOBO dataframes and convert time to EST ####

#helpful time link: https://stackoverflow.com/questions/37053620/dealing-with-eastern-standard-time-est-and-eastern-daylight-savings-edt-in-r

###bind df's
hobo_bind1 <- rbind(hpb_hobo1, hpb_hobo2)

###fix timezone
##first make correctin ymdhms format
hobo_bind2 <- hobo_bind1 |> 
  mutate(DateTime_EDT = mdy_hms(DateTime_EDT))

#then force timezone to be UTC-4 since that's what the sensor was set at. Using 'American/Virgin' since EDT wasn't recognized and Virgin Islands don't use daylight savings so are always UTC-4
hobo_bind2$DateTime_EDT <- force_tz(as.POSIXct(hobo_bind2$DateTime_EDT), tzone = "America/Virgin")

#check that it worked
attr(hobo_bind2$DateTime_EDT, "tzone")

#now need to convert from EDT to EST to line up with meteo data
hobo_bind2$DateTime_EST <- as.POSIXct(hobo_bind2$DateTime_EDT, tz = "EST")

#check that it worked
attr(hobo_bind2$DateTime_EST, "tzone")

#Clean up data frame to prep for next steps
hobo_bind <- hobo_bind2 |> 
  select(DateTime_EST, HOBO_Abs_Pres_kPa, Temp_C)

#final check on timezone
attr(hobo_bind$DateTime_EST, "tzone")



#### QAQC sensors for when out of water for download ####

##look at timeseries before removing mainteance points
hobo_bind |> 
  select(1:3) |> 
  pivot_longer(-1) |> 
  ggplot(aes(x = DateTime_EST, y = value))+
  geom_point()+
  facet_wrap(~name, scales = "free_y", ncol = 1)


## Maintenance log 

# log <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCR_Inflow/hpb_maintenancelog_2024.csv")
# 
# #force tz check 
# log$TIMESTAMP_start <- force_tz(as.POSIXct(log$TIMESTAMP_start), tzone = "EST")
# log$TIMESTAMP_end <- force_tz(as.POSIXct(log$TIMESTAMP_end), tzone = "EST")

#set time where sensor may have been out of the water
#The 2024 times can from DWHs field sheets
#2025 times DWH just visualized plot below and added these and confirmed we were in field to have taken sensor out of water, may need to add some more times
maintenance <- c(seq(ymd_hms("2024-05-22 14:30:00"), ymd_hms("2024-05-22 15:30:00"), by = "10 min"),
                 seq(ymd_hms("2024-06-19 10:30:00"), ymd_hms("2024-06-19 10:50:00"), by = "10 min"),
                 seq(ymd_hms("2024-07-11 11:00:00"), ymd_hms("2024-07-11 12:00:00"), by = "10 min"),
                 seq(ymd_hms("2024-09-16 13:00:00"), ymd_hms("2024-09-16 14:00:00"), by = "10 min"),
                 seq(ymd_hms("2024-09-30 11:30:00"), ymd_hms("2024-09-30 12:30:00"), by = "10 min"),
                 seq(ymd_hms("2024-10-28 12:30:00"), ymd_hms("2024-10-28 12:50:00"), by = "10 min"),
                 seq(ymd_hms("2024-12-17 13:30:00"), ymd_hms("2024-12-17 14:00:00"), by = "10 min"),
                 
                 seq(ymd_hms("2025-04-16 11:30:00"), ymd_hms("2025-04-16 12:10:00"), by = "10 min"),
                 seq(ymd_hms("2025-07-09 10:00:00"), ymd_hms("2025-07-09 10:00:00"), by = "10 min"),
                 seq(ymd_hms("2025-08-20 11:00:00"), ymd_hms("2025-08-20 11:00:00"), by = "10 min"),
                 seq(ymd_hms("2025-12-02 12:30:00"), ymd_hms("2025-12-02 12:40:00"), by = "10 min")
)

maintenance <- force_tz(maintenance, tzone = "EST")
attr(maintenance, "tzone")

##remove points
hobo_qaqc <- hobo_bind |>  
  #remove odd datetimes that are NAs when sensor was out for download
  filter(!is.na(HOBO_Abs_Pres_kPa)) |> 
  #set times where sensor out of water to NA
  # filter(!DateTime_EST %in% maintenance) |> #old code that just threw out time in that range
  mutate(HOBO_Abs_Pres_kPa = ifelse(DateTime_EST %in% maintenance, NA, HOBO_Abs_Pres_kPa),
         Flag_HOBO_Abs_Pres_kPa = ifelse(DateTime_EST %in% maintenance, 1, 0),
         Temp_C = ifelse(DateTime_EST %in% maintenance, NA, Temp_C),
         Flag_Temp_C = ifelse(DateTime_EST %in% maintenance, 1, 0)) 


#plot raw pressure and temp to update maint log
raw_pressure_ts <- hobo_qaqc |> 
  select(1:3) |> 
  pivot_longer(-1) |> 
  ggplot(aes(x = DateTime_EST, y = value))+
  geom_point()+
  facet_wrap(~name, scales = "free_y", ncol = 1)

raw_pressure_ts
# plotly::ggplotly(raw_pressure_ts)


#### Read in CCR meteorology for pressure correction ####

### DWH having issues reading in 2025 met EDI, so using 2024 edi and L1 for now 
# ccr_met_edi <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1105/3/df0ce4fc90f220b65c400b997abae37b" )
# ccr_met_git <- read_csv("https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/refs/heads/ccre-dam-data-qaqc/ccre_met_L1.csv") 
# 
# ccr_met <- ccr_met_edi |>
#   select(-starts_with("Note")) |> #remove note columns to bind
#   rbind(ccr_met_git)
# 
# write.csv(ccr_met, "C:/Users/dwh18/Downloads/ccrmet_edi_git_hold.csv", row.names = F)
# ccr_met <- read_csv("C:/Users/dwh18/Downloads/ccrmet_edi_git_hold.csv")

##format for bind 
ccr_met_bind <- ccr_met |> 
  select(DateTime, BP_Average_kPa, Flag_BP_Average_kPa) |> #check BP flag
  mutate(Date = as.Date(DateTime),
         Hour = hour(DateTime),
         Min = minute(DateTime)) |> 
  filter(Min %in% c(0,10,20,30,40,50)) |>  #get just minutes to line up w/ HOBO
  rename(DateTime_EST = DateTime) |> 
  select(DateTime_EST, BP_Average_kPa) 

#check timezone
attr(ccr_met_bind$DateTime_EST, "tzone")

#fix so it's actually EST
ccr_met_bind$DateTime_EST <- force_tz(as.POSIXct(ccr_met_bind$DateTime_EST), tzone = "EST")

#check that it worked
attr(ccr_met_bind$DateTime_EST, "tzone")




#### Pressure correct Hobo sensor ####

waterlevel_correct <- left_join(hobo_qaqc, ccr_met_bind, by = "DateTime_EST") |> 
  mutate(corrected_Pres_kPa = HOBO_Abs_Pres_kPa - BP_Average_kPa) |> 
  mutate(waterlevel_cm = corrected_Pres_kPa * 10.1972)


## look at data
waterlevel_correct |> 
  ggplot()+
  geom_line(aes(x=DateTime_EST, y = BP_Average_kPa, color = "MET"))+  
  geom_line(aes(x=DateTime_EST, y = HOBO_Abs_Pres_kPa, color = "HOBO_raw"))+
  geom_line(aes(x=DateTime_EST, y = corrected_Pres_kPa + 95, color = "HOBO_correct + 95"))

waterlevel_correct |> 
  ggplot(aes(x = DateTime_EST, y = waterlevel_cm))+
  geom_point()


##compare precip to level data
dailyrain <- ccr_met |> 
  mutate(Date = as.Date(DateTime)) |> 
  filter(Date > ymd("2024-04-01"))  |> 
  group_by(Date) |> 
  summarise(dailyrain = sum(Rain_Total_mm, na.rm = T))


daily_waterlevel <- waterlevel_correct |> 
  mutate(Date = as.Date(DateTime_EST)) |> 
  group_by(Date) |> 
  summarise(level = mean(waterlevel_cm, na.rm = T))

daily_waterlevel |> 
  ggplot(aes(x = Date, y = level))+
  geom_col(data = dailyrain, aes(x = Date, y = dailyrain), color = "lightblue")+
  geom_point()+
  theme_bw()


####### writing csv for now here: some things to think about about that I look at in hpb_Q_calcs_trials.R
# correcting sensor stage to manualy measured stage
# Q calcs
# should we set these negative stage to zero for publication or just flag? Doing nothing currently 


##write csv
# write.csv(waterlevel_correct, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCR_Inflow/2024/HPB_stage_2024_2025.csv", row.names = F )








