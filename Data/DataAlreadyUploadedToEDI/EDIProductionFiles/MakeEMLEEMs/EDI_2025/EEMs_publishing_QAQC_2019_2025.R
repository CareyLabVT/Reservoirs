#### EEMs Compilation and QAQC for EDI script
## DWH January 2026


#### Get packages 
library(tidyverse)


##################################### Format 2021 - 2025 to append to prior publication ----

#### Read in EEMs data and correct dates that were incorrect on sample labels
#These samples were labeled for field day that was rescheduled 
eems_21_25 <- read_csv("./EEMs_Results_2021_2025.csv") |> 
  mutate(Date = mdy(Date)) |> 
  mutate(Date = ifelse(Date == ymd("2024-08-15"), ymd("2024-08-14"), Date),
         Date = ifelse(Date == ymd("2025-02-17"), ymd("2025-02-26"), Date),
         DateTime = as.Date(Date))


#### Remove flagged data and order data frame 
eems_21_25_clean <- eems_21_25 |> 
  #removed suspect flagged scans
  filter(is.na(FLAG)) |> 
  #Add Abs and EEMs flags for remaining data
  mutate(abs_Flag = ifelse(is.na(a254_m),1,0),
         fl_Flag = ifelse(is.na(Max_FL_Ex),1,0)) |> 
  select(-Date, -FLAG, -Notes_flag)


##make file that has sample descriptions for building pfile description folder 
pfile_metadata_21_25 <- eems_21_25_clean |> 
  select(`Sample Name`, Reservoir, Site, DateTime, Depth_m, Rep, Dilution)

#write.csv(pfile_metadata_21_25, "./EDI_2025/pfile_table_for_21-25.csv", row.names = F)






##################################### Bind to prior publication ----

#prior EEMs pub
eems_19_20_edi <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/841/1/f63272976bcd151f8e879cbd14d9a9ce" ) |> 
  mutate(DateTime = mdy(DateTime))


#bind data, add flags and remove unneeded columns
eems_19_25 <- plyr::rbind.fill(eems_19_20_edi, eems_21_25_clean) |> 
  #remove uneeded column from sample name
  select(-`Sample Name`)



##################################### Add times to datetime stamp ----

#### Read in chemistry data to get times
chem_edi <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/199/13/3f09a3d23b7b5dd32ed7d28e9bc1b081" )

chem_times <- chem_edi |> 
  dplyr::select(Reservoir, Site, DateTime, Depth_m) |> 
  unique() |> 
  mutate(Date = as.Date(DateTime), 
         Time = format(DateTime, format = "%H:%M:%S")) |> 
  filter(year(Date) >= 2019)


#### Get times for EEMs
eems_19_25_time <- eems_19_25 |> 
  rename(Date = DateTime) |> 
  #join known times together
  left_join(chem_times, by = c("Reservoir", "Site", "Date", "Depth_m")) |> 
  #remove duplicated rows
  slice(-c(379, 432, 453)) |> 
  #format times form EEMs overnight sampling
  mutate(TIME_eems_msn = ifelse(TIME == "6am", "06:00:00", NA),
         TIME_eems_msn = ifelse(TIME == "midnight", "00:00:00", TIME_eems_msn),
         TIME_eems_msn = ifelse(TIME == "noon", "12:00:00", TIME_eems_msn)
         ) |> 
  #append EEM overnight times 
  mutate(Time = ifelse(is.na(Time), TIME_eems_msn, Time)) |> 
  #set unknown times to noon
  mutate(Time = ifelse(is.na(Time), "12:00:00", Time))

#clean up datetime and removed unneeded columns
eems_time_final <- eems_19_25_time |> 
  mutate(DateTime_fin = ymd_hms(paste(Date, Time, sep = " "))) |> 
  select(Reservoir, Site, DateTime_fin, Depth_m, everything()) |> 
  select(-c(Date, TIME, DateTime, Time, TIME_eems_msn)) |> 
  rename(DateTime = DateTime_fin) |> 
  arrange(Reservoir, DateTime, Site, Depth_m, Rep)


#write final csv
#write.csv(eems_time_final, "./EDI_2025/OpticalData_2019_2025.csv", row.names = F)





