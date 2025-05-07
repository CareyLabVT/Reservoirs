## Title: ISCO load calc and QAQC
## Authors: Adrienne Breef-Pilz, Nick Hammond, and Carly Bauer
## Adapter from ISCO_load_calcs.Rmd
## Date: 17 Oct 2024
## Updated: 07 May 2025 updated for staging but still need to make it a function


# isco_qaqc <- function(
#     water_level_dir,
#     weir_staff_gauge,
#     chem_dir,
#     metals_dir,
#     maintenance_log,
#     start_date,
#     end_date){

 # load packages
  pacman::p_load(tidyverse, readxl)
  
  # Use the metals function to get the ISCO files
  source('https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Scripts/L1_functions/metals_create.R')
  
  
  
  # named variables for running the function manually and QAQCing
  water_level_dir = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/Raw_water_level/"
  chem_dir = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/Chem_ISCO_files/"
  ISCO_outfile = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/isco_2019_2024.csv"
  #metals_dir
  #maintenance_log
  #start_date
  #end_date
  
  
 
  

waterlevel <- list.files(path = water_level_dir, pattern = "", full.names = T)%>% 
  map_df(~ read_csv(.x,
                    col_names = c("DateTime", "WaterLevel_m"), col_types = cols(.default = "c"), skip = 6))

# get all the dates in the same format
waterlevel_2<-waterlevel%>%
  mutate(DateTime=parse_date_time(DateTime, c("mdy HMS","mdy HM", "mdy HMS p", "mdy HM p")),
         WaterLevel_m=as.numeric(WaterLevel_m))%>%
  #### add this as the start date 
  filter(DateTime>ymd_hms("2019-06-06 14:51:00"))%>%
  
  filter(WaterLevel_m<2)%>%
  mutate(Date = as.Date(DateTime),
         Year=year(DateTime),
         DOY=yday(DateTime))%>%
  #filter(DOY>50)%>%
  drop_na()%>%
  distinct()

### This will be added to the maintenance log. Clearing out the weir. 
# SHOULD MAKE A MAINTENANCE LOG OF TIMES TO EXCLUDE WATER LEVEL? Maybe we are all set by setting time to 0?
waterlevel_ISCO<-waterlevel_2%>%
  select(DateTime, WaterLevel_m)%>%
  mutate(
    WaterLevel_m=ifelse(DateTime>ymd_hms("2020-07-20 10:00:00")&DateTime<ymd_hms("2020-08-24 14:50:00"), NA, WaterLevel_m), 
    WaterLevel_m=ifelse(DateTime==ymd_hms("2020-02-20 12:27:00"), NA, WaterLevel_m),
    WaterLevel_m=ifelse(DateTime==ymd_hms("2020-08-24 15:15:00"), NA, WaterLevel_m),
    WaterLevel_m=ifelse(DateTime>ymd_hms("2020-09-02 14:17:00")&DateTime<ymd_hms("2020-09-02 14:37:00"), NA, WaterLevel_m),
    WaterLevel_m=ifelse(DateTime>ymd_hms("2020-11-09 14:20:00")&DateTime<ymd_hms("2020-11-09 14:25:00"), NA, WaterLevel_m),
    WaterLevel_m=ifelse(DateTime==ymd_hms("2020-09-30 14:45:00"), NA, WaterLevel_m),
    WaterLevel_m=ifelse(DateTime==ymd_hms("2023-05-08 09:45:00"), NA, WaterLevel_m),
    WaterLevel_m=ifelse(DateTime==ymd_hms("2023-08-07 09:01:00"), NA, WaterLevel_m),
    WaterLevel_m=ifelse(DateTime>ymd_hms("2023-08-14 09:18:00")&DateTime<ymd_hms("2023-08-14 09:24:00"), NA, WaterLevel_m),
    WaterLevel_m=ifelse(DateTime>ymd_hms("2023-12-04 14:02:00")&DateTime<ymd_hms("2020-12-04 14:13:00"), NA, WaterLevel_m),
         WaterLevel_m = ifelse(WaterLevel_m==0, NA, WaterLevel_m))

## Need to take out so low water level in 2020 and 2023- probably add to a mainteance log
# 2020-02-29 12:27
# 2020-08-24 15:15
# 2020-09-02 14:19-14:31
# 2020-09-30 14:45

# 2023-05-08 09:45
# 2023-08-07 09:01
# 2023-08-14 09:22-09:23

# Need to take out 2020-07-20 10:00:00 EST, 2020-08-24 14:49:00 EST for when the weir was blown out



# Now let's calculate discharge from the weir using the water level from the ISCO. 

# The v-notched weir is 27.5 cm tall so what should we do when water goes over the top?
#  Let's compare the two different methods. 
# 1) discharge if you disregard the flow over the top
# 2) calculate discharge that goes through the weir. Then assume the discharge over the top is a rectangle. 



# Step number 1
waterlevel_ISCO <- waterlevel_ISCO %>% 
  mutate(Flow_cms_v = 2.391*(WaterLevel_m^2.5))%>% 
 select(DateTime, WaterLevel_m, Flow_cms_v)|>
  arrange(DateTime)

# Step number 2
# V-notch weir equation + rectangular weir equation for flow above 0.275 m #    (flow over-topped the weir at 27.5 cm)
B = 0.953 + 1.99 + 1.59 # channel width in meters (see 'WeirMeasurements.jpg')

waterlevel_ISCO2 <- waterlevel_ISCO %>% 
  mutate(Flow_cms_r = ifelse(WaterLevel_m > 0.275, 
2.391*(0.275^2.5) + (1.84*B*((WaterLevel_m-0.275)^1.5)), 2.391*(WaterLevel_m^2.5)))
  #select(DateTime, WaterLevel_m, Flow_cms_r)

# Take the maximum value between the two calculated flows. The values that are 12 mm or less above the weir have a lower flow when adding the rectangle than not including the rectangle above the weir. 

# take the difference between the two calculated values to show that the rectangle above the weir does underestimate flow for values less than 12 mm above 
waterlevel_ISCO2$dif <- waterlevel_ISCO2$Flow_cms_r-waterlevel_ISCO2$Flow_cms_v

waterlevel_ISCO2$Flow_cms <-  pmax(waterlevel_ISCO2$Flow_cms_v, waterlevel_ISCO2$Flow_cms_r)


waterlevel_ISCO3 <- waterlevel_ISCO2|>
  select(DateTime, WaterLevel_m, Flow_cms)|>
  mutate(
    Flag_WaterLevel_m = ifelse(WaterLevel_m>0.275, 1, 0)
  )

  #dplyr::rename(Flow_cms = Flow_cms_r)
 # drop_na(Flow_cms) # drop when there are NAs so we don't count those

# Add a flag if waterlevel is over the top of the weir

##Next section is looking a total discharge over sampling events. 

# Read in the ICPMS (metals) data  and nutrients data from the ISCO.

# Determine the start and end for each ISCO sample to get the discharge over that time period


 # Read in ISCO metals file

# Read in the metals files and clean them up. 

# Use the function from the metals files that is sourced from GitHub

sed <-metals_qaqc(directory = "./Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/",
                  historic = 'https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/FCR_ISCO/Data/ISCO_metals_2019.csv',
                  sample_ID_key = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Scripts/Metals_Sample_Depth.csv",
                  maintenance_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Metals_Maintenance_Log.csv",
                  sample_time = "https://docs.google.com/spreadsheets/d/1MbSN2G_NyKyXQUEzfMHmxEgZYI_s-VDVizOZM8qPpdg/edit#gid=0",
                  MRL_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/MRL_metals.txt",
                  metals_save = F, 
                  metals_outfile = "./Data/DataNotYetUploadedToEDI/Metals_Data/metals_L12.csv",
                  ISCO_save = T,
                  ISCO_outfile = NULL,
                  start_date = NULL, # change when we update to read date from EDI
                  end_date = NULL)

# Take out the soluble samples because we only have totals samples
sedf <- sed|>
  select(-c(starts_with("S")|starts_with("Flag_S")))|>
  mutate(Date = as.Date(DateTime)) # Make a Date column to bind with the nutrients data


# Read in Nutrients files and clean then up. Average across duplicates

# Read in the chemistry files and select on TN and TP columns

nuts <- list.files(path = chem_dir, pattern = "", full.names = T)%>%  
    map_df(~ read_csv(.x,
                  col_select =c("DateTime", "Depth_m", "Rep", "TN_ugL", "TP_ugL"
                                , "Flag_DateTime", "Flag_TN", "Flag_TP")))|>
  mutate(Date= as.Date(DateTime))

# Take out flags columns and then filter so only one flag per an observation

flag<- nuts|>
  filter(Rep==1)|>
  select(Date, Flag_TN, Flag_TP)|>
  dplyr::rename(
    "Flag_TN_mgL" = Flag_TN,
    "Flag_TP_mgL" = Flag_TP)

# Take the mean of TN and TP

mean_nuts<- nuts |>
  select(Date, TN_ugL, TP_ugL)|>
  group_by(Date)|>
  summarise(across(c("TN_ugL", "TP_ugL"), ~ (mean(.x, na.rm = TRUE)/1000)))|>
  ungroup()|>
  dplyr::rename(
    "TN_mgL" = TN_ugL,
    "TP_mgL" = TP_ugL)
  

# Bind the obs with the flags

mean_nuts <- merge(mean_nuts, flag, by= "Date")

# Right now take out because we want observations to have the same time
# # Add in a DateTime column but all times are set to 12:00:00
# mean_nuts <- mean_nuts|>
#   mutate(DateTime = ymd_hms(paste0(Date," ","12:00:00")))|>
#   select(-Date)

# Filter a start and end date. FIGURE OUT WHEN/WHERE TO ADD THIS IN 
# ### identify the date subsetting for the data
# if (!is.null(start_date)){
#   #force tz check
#   start_date <- force_tz(as.POSIXct(start_date), tzone = "America/New_York")
#   
#   mean_nuts <- mean_nuts %>%
#     filter(DateTime >= start_date)
#   
# }
# 
# if(!is.null(end_date)){
#   #force tz check
#   end_date <- force_tz(as.POSIXct(end_date), tzone = "America/New_York")
#   
#   mean_nuts <- mean_nuts %>%
#     filter(DateTime <= end_date)
#   
# }


# bind the metals and nutrients files together. Maybe they should be separate files??
# We should bind them to at least get the times samples were taken

nuts_mets <- merge(sedf, mean_nuts, by="Date", all = T)|>
  mutate(Reservoir = ifelse(is.na(Reservoir), "FCR", Reservoir),
         Depth_m = ifelse(is.na(Depth_m), 0.1, Depth_m),
         DateTime = as.character(DateTime),
         DateTime = ifelse(is.na(DateTime), paste0(Date, " 12:00:00"), DateTime),
         DateTime = ymd_hms(DateTime))


# Make a data frame of when samples were taken

ISCO_samp_date <- nuts_mets|>
  select(DateTime)
 
# flag of 1 is sample not taken


# Using the waterlevel_ISCO2 file let's figure out the start and end date for each sampling. 

 # First we are going to determine when the ISCO was first put out in the field each year. Then when it was taken in.

 # Get first and last date for the ISCO each year

startdate<-waterlevel_ISCO3%>%
  mutate(Year=year(DateTime))%>%
  group_by(Year)%>%
  dplyr::summarise(DateTime=first(DateTime))%>%
  mutate(Start_date="START",
         Date=as.Date(DateTime))%>%
  select(DateTime,Date, Start_date)

lastdate<-waterlevel_ISCO3%>%
  mutate(Year=year(DateTime))%>%
  group_by(Year)%>%
  dplyr::summarise(DateTime=last(DateTime))%>%
  mutate(Start_date="END",
         Date=as.Date(DateTime))%>%
  select(DateTime,Date, Start_date)%>%
  rbind(., startdate)%>%
  drop_na(DateTime)

# 

# add the start dates in 

all_samples2=merge(ISCO_samp_date, lastdate, by="DateTime", all=T)

# drop a few of the observation
# 2020-11-09 13:14:00 
# 2021-12-06 12:00:00 - find this one- have to find the water level until that point in the software!!!! This is missing :(
# 2022-12-12 11:04:00 - change the name

# remove some of these observations to make it nice. Will need to change some dates later. 
all_samples22 <- all_samples2%>%
  filter(DateTime!=ymd_hms("2020-11-09 13:14:00") & DateTime!=ymd_hms("2021-12-06 12:00:00") & 
           DateTime!=ymd_hms("2022-12-12 11:04:00") & DateTime != ymd_hms("2019-11-20 12:00:00") &
           DateTime != ymd_hms("2021-11-22 12:00:00") & DateTime != ymd_hms("2023-12-04 13:53:00") &
           DateTime != ymd_hms("2024-12-03 14:30:00"))


# merge with the ISCO discharge data to calculate discharge per an event

total_dis <- merge(waterlevel_ISCO3, all_samples22, by="DateTime", all =T)


# Subset Q based on sampling times
# Start with making a data frame of all of the sampling dates
time = data.frame(all_samples22$DateTime, all_samples22$DateTime, all_samples22$Start_date)

# Rename the columns
colnames(time) = c("time1", "time2", "Start_date")

# shift time 2 up one row. Then remove the rows when start_date=END
time2 <- time %>% 
  mutate(time2 = lead(time2, 1, default = NA))|>
  filter(Start_date=="START"|is.na(Start_date))|>
  select(time1, time2)|>
  dplyr::rename("Start_time"="time1",
                "End_time"="time2")

# # now drop the NA waterlevel observations. It might not matter
waterlevel_ISCO3 <- waterlevel_ISCO3|>
  drop_na(Flow_cms)

# create the lists of discharge based on the sample duration of deployments. When the ISCO started collecting the sample
# until it ended. 
sample = list()
for(i in 1:nrow(time2)){
  sample[[i]] = waterlevel_ISCO3[waterlevel_ISCO3$DateTime>time2[i,1] & waterlevel_ISCO3$DateTime<time2[i,2],]
}

# Multiply each minute Q by 60 s to get a cumulative volume of flow per minute
for(i in 1:length(sample)){
  sample[[i]]$cum_flow = sample[[i]]$Flow_cms*60
}

# Sum up all cum_flow for each sampling period to get a total cumulative flow
for(i in 1:nrow(time2)){
  time2$cum_flow[i] = sum(sample[[i]]$cum_flow, na.rm = T)
  time2$count[i] = nrow(sample[[i]]) # number of flow observations in the cumulative flow
  time2$Flag_high[i] = sum(sample[[i]]$Flag_WaterLevel_m, na.rm=T) # sum the number of flags to see how many obs over topped the weir
  
}

# add a Date for the end to make it easier to join the end of the isco sample by the end of the sample take
time2$Date_end <- as.Date(time2$End_time)

# Now that we have calculated cumulative flow as captured by the ISCO for each sampling interval, let's calculate loads.

# To make it easy to multiple the concentration by the cumulative flow we are going to put our wide dataframe with concentrations into a long format. Figure out what to do with the flags. Right now just take them out. 

long_metals <- nuts_mets%>%
  select(-Date)|>
  select(-starts_with("Flag"))%>%
  #pivot_longer(!cDateTime, names_to = "source", values_to = "observations")%>%
  pivot_longer(!c(Reservoir, DateTime, Depth_m), names_to = "variable", values_to = "concentration")
  
# get the Flags in a column
long_flag <- nuts_mets%>%
  select(-Date)|>
  select(-starts_with("T"))%>%
  #pivot_longer(!cDateTime, names_to = "source", values_to = "observations")%>%
  pivot_longer(!c(Reservoir, DateTime, Depth_m), names_to = "variable", values_to = "Flag")
  
long_flag$variable <- gsub('Flag_', '', long_flag$variable) # take out the flag name

long_flag <- long_flag|>
  filter(variable != "DateTime") # take out the the datetime flag and add it back in later

# bring them back together

long_df2 <- full_join(long_metals, long_flag, by=c("Reservoir", "DateTime", "Depth_m", "variable"))|>
  mutate(Date_end = as.Date(DateTime))


# join the cumulative flow and the metals observation by the closest observations at the end time. The end time is when the samples were taken
#by <- join_by(closest(End_time, DateTime))
Calc_dis_metals <- full_join(time2, long_df2, by="Date_end")|>
  mutate(
    diff_time = difftime(End_time, Start_time, units='mins') -1,
    missing_waterlevel_obs = as.numeric(diff_time) - count
  )|>
  # drop the samples if there was no waterlevel file for now
  drop_na(cum_flow)|>
  dplyr::rename("Flag_Concentration_mgL" = Flag)

# make a data frame of observations that are greater than a 1000 obs missing

# low_cal_flow <- Calc_dis_metals|>
#   select(Start_time, End_time, cum_flow, count, exp_count, dif)|>
#   distinct()
#   filter(dif > 1000)
  

# add in the flags
Calc_dis_metals2 <- Calc_dis_metals|>
  mutate(
    Flag_Cumulative_flow_L = ifelse(cum_flow==0, 1, 0),
    # if any waterlevel reading is over the weir flag as water over top of weir
    Flag_Cumulative_flow_L = ifelse(Flag_high>0, paste0(Flag_Cumulative_flow_L, 2), Flag_Cumulative_flow_L),
    # flag if there were more than 1000 missing water level observations. Maybe this isn't right. 
    Flag_Cumulative_flow_L = ifelse(missing_waterlevel_obs>1000, paste0(Flag_Cumulative_flow_L,3), 
                                    Flag_Cumulative_flow_L),
    # reflag one because 1 overrides everything else since value was not collected.
    Flag_Concentration_mgL = ifelse(is.na(concentration), 1, Flag_Concentration_mgL),
    Flag_Cumulative_flow_L = as.numeric(Flag_Cumulative_flow_L)
  )


# Calculate loads

calc_load <- Calc_dis_metals2%>%
  # convert m3 to L
  mutate(cum_v_L = cum_flow * 1000,
         # cumulative volume (L) * metals concentration (mg/L) = load (mg)/ divide by 1000000 to get (kg)
         ISCO_load_v_kg = (cum_v_L*concentration)/1000000,
         #diff_time = difftime(End_time, Start_time, units='mins') -1,
         ISCO_load_v_kg_d = (ISCO_load_v_kg/as.numeric(diff_time))*(60*24))
         # Use the number of observation to calculate time because water level observation was taken every minute so if the ISCO wasn't working, no samples were taken and no water level was recorded. 
         #ISCO_load_v_kgd_count = (ISCO_load_v_kg/count) *(60*24))


#### What do we want the data file to look like?

# final data frame. Rename and reorder

final_df <- calc_load|>
  mutate(Site = 100.1)|>
  dplyr::rename(
    "Load_kg" = ISCO_load_v_kg,
    "Load_kgD" = ISCO_load_v_kg_d,
    "Collection_start_time" = Start_time,
    "Collection_end_time" = End_time,
    "Cumulative_flow_L" = cum_v_L,
    "Element_name" = variable,
    "Concentration_mgL" = concentration,
    "Total_waterlevel_obs" = count)|>
  select(Reservoir, Site, Depth_m, Collection_start_time, Collection_end_time, Cumulative_flow_L, Total_waterlevel_obs, Element_name, Concentration_mgL, Load_kg, Load_kgD, Flag_Cumulative_flow_L, Flag_Concentration_mgL)


# save date time columns as characters and save the file

# If there is an outfile argument, that is where the data are saved
final_df$Collection_start_time <- as.character(format(final_df$Collection_start_time)) # convert DateTime to character
final_df$Collection_end_time <- as.character(format(final_df$Collection_end_time))

# Write the L1 file
write_csv(final_df, ISCO_outfile)

        

#}


