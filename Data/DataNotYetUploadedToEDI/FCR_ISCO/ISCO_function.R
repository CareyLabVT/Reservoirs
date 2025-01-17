## Title: ISCO load calc and QAQC
## Authors: Adrienne Breef-Pilz, Nick Hammond, and Carly Bauer
## Adapter from ISCO_load_calcs.Rmd
## Date: 17 Oct 2024


isco_qaqc <- function(
    water_level_dir,
    weir_staff_gauge,
    chem_dir,
    metals_dir,
    maintenance_log,
    start_date,
    end_date){
  
  # Use the metals function to get the ISCO files
  source('https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Scripts/L1_functions/metals_create.R')
  
  # named variables for running the function manually and QAQCing
  #water_level_dir = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/Raw_water_level/"
  weir_staff_gauge = "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-weir-data-qaqc/Inflow_Gauge_Height_at_Weir.csv"
  chem_dir
  metals_dir
  maintenance_log
  start_date
  end_date
  
  
  pacman::p_load(tidyverse, readxl, scattermore, plotly)
  

waterlevel <- list.files(path = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/Raw_water_level/", pattern = "", full.names = T)%>% 
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
  mutate(WaterLevel_m=ifelse(DateTime>ymd_hms("2020-07-20 10:00:00")&DateTime<ymd_hms("2020-08-24 14:50:00"), NA, WaterLevel_m),
         WaterLevel_m = ifelse(WaterLevel_m==0, NA, WaterLevel_m))

# Need to take out 2020-07-20 10:00:00 EST, 2020-08-24 14:49:00 EST for when the weir was blown out

# read in the staff gauge observations from GitHub
# WE SHOULDN'T NEED THIS

# staff_gauge <- read_csv(weir_staff_gauge)%>%
#   mutate(GageHeight_cm=GageHeight_cm/100)



# Now let's calculate discharge from the weir using the water level from the ISCO. 

# The v-notched weir is 27.5 cm tall so what should we do when water goes over the top?
#  Let's compare the two different methods. 
# 1) discharge if you disregard the flow over the top
# 2) calculate discharge that goes through the weir. Then assume the discharge over the top is a rectangle. 



# Step number 1
waterlevel_ISCO <- waterlevel_ISCO %>% mutate(Flow_cms_v = 2.391*(WaterLevel_m^2.5))%>% 
 select(DateTime, WaterLevel_m, Flow_cms_v)|>
  arrange(DateTime)

# Step number 2
# V-notch weir equation + rectangular weir equation for flow above 0.275 m #    (flow over-topped the weir at 27.5 cm)
B = 0.953 + 1.99 + 1.59 # channel width in meters (see 'WeirMeasurements.jpg')

waterlevel_ISCO2 <- waterlevel_ISCO %>% 
  mutate(Flow_cms_r = ifelse(WaterLevel_m > 0.275, 
2.391*(0.275^2.5) + (1.84*B*((WaterLevel_m-0.275)^1.5)), 2.391*(WaterLevel_m^2.5)))%>%
  select(DateTime, WaterLevel_m, Flow_cms_r)%>%
  dplyr::rename(Flow_cms = Flow_cms_r)



##Next section is looking a total discharge over sampling events. 

# Read in the ICPMS (metals) data  and nutrients data from the ISCO.

# Determine the start and end for each ISCO sample to get the discharge over that time period


 # Read in ISCO metals file

# Read in the metals files and clean them up. 

# Use the function from the metals files that is sourced from GitHub

sed <-metals_qaqc(directory = "./Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/",
                  historic = './Data/DataNotYetUploadedToEDI/FCR_ISCO/Data/ISCO_metals_2019.csv',
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
  select(-c(starts_with("S")|starts_with("Flag_S")))


# Read in Nutrients files and clean then up. Average across duplicates

# Read in the chemistry files and select on TN and TP columns

nuts <- list.files(path = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/Chem_ISCO_files/", pattern = "", full.names = T)%>%  
    map_df(~ read_csv(.x,
                  col_select =c("DateTime", "Depth_m", "Rep", "TN_ugL", "TP_ugL"
                                , "Flag_DateTime", "Flag_TN", "Flag_TP")))|>
  mutate(Date= as.Date(DateTime))

# Take out flags columns and then filter so only one flag per an observation

flag<- nuts|>
  filter(Rep==1)|>
  select(Date, Flag_TN, Flag_TP)

# Take the mean of TN and TP

mean_nuts<- nuts |>
  select(Date, TN_ugL, TP_ugL)|>
  group_by(Date)|>
  summarise(across(c("TN_ugL", "TP_ugL"), ~ mean(.x, na.rm = TRUE)))|>
  ungroup()

# Bind the obs with the flags

mean_nuts <- merge(mean_nuts, flag, by= "Date")

# Add in a DateTime column but all times are set to 12:00:00
mean_nuts <- mean_nuts|>
  mutate(DateTime = ymd_hms(paste0(Date," ","12:00:00")))|>
  select(-Date)

# Filter a start and end date 
### identify the date subsetting for the data
if (!is.null(start_date)){
  #force tz check
  start_date <- force_tz(as.POSIXct(start_date), tzone = "America/New_York")
  
  mean_nuts <- mean_nuts %>%
    filter(DateTime >= start_date)
  
}

if(!is.null(end_date)){
  #force tz check
  end_date <- force_tz(as.POSIXct(end_date), tzone = "America/New_York")
  
  mean_nuts <- mean_nuts %>%
    filter(DateTime <= end_date)
  
}


# bind the metals and nutrients files together. Maybe they should be separate files??
# We should bind them to at least get the times samples were taken

nuts_mets <- merge(sedf, mean_nuts, by="DateTime", all = T)


# Make a data frame of when samples were taken

ISCO_samp_date <- nuts_mets|>
  select(DateTime)
 



# Using the waterlevel_ISCO file let's figure out the start and end date for each sampling. 

 # First we are going to determine when the ISCO was first put out in the field each year. Then when it was taken in.

 # Get first and last date for the ISCO each year

startdate<-waterlevel_ISCO%>%
  mutate(Year=year(DateTime))%>%
  group_by(Year)%>%
  dplyr::summarise(DateTime=first(DateTime))%>%
  mutate(Start_date="START",
         Date=as.Date(DateTime))%>%
  select(DateTime,Date, Start_date)

lastdate<-waterlevel_ISCO%>%
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
# 2021-12-06 12:00:00 - find this one- have to find the water level until that point in the software!!!!
# 2022-12-12 11:04:00 - change the name
# 2023-11-14 10:30:00 - change the name
# 2023-12-04 13:53:00 - find this one- have to find it in the software!!!

# remove some of these observations to make it nice. Will need to change some dates later. 
all_samples22 <- all_samples2%>%
  filter(DateTime!=ymd_hms("2021-12-06 12:00:00") & DateTime!=ymd_hms("2022-12-12 11:04:00") & 
           DateTime!=ymd_hms("2023-11-14 10:30:00") & DateTime!=ymd_hms("2023-12-04 13:53:00"))


# merge with the ISCO discharge data to calculate discharge per an event

total_dis <- merge(waterlevel_ISCO, all_samples22, by="DateTime", all =T)


#Subset Q based on sampling times
#int = data.frame(metals$Date)
#colnames(int) = c("interval")
time = data.frame(all_samples22$DateTime, all_samples22$DateTime, all_samples22$Start_date)
colnames(time) = c("time1", "time2", "Start_date")

# shift time 2 up one row. Then remove the rows when start_date=END
time2=time %>% 
  mutate_at(("time2"), funs(lead), n = 1)%>%
  filter(Start_date=="START"|is.na(Start_date))%>%
  select(time1, time2)%>%
  dplyr::rename("Start_time"="time1",
                "End_time"="time2")

options(scipen = 100, digits = 4)

# create the lists of discharge based on the sample duration of deployments. When the ISCO started collecting the sample
# until it ended. 
sample = list()
for(i in 1:nrow(time2)){
  sample[[i]] = waterlevel_ISCO[waterlevel_ISCO$DateTime>time2[i,1] & waterlevel_ISCO$DateTime<time2[i,2],]
}

# Multiply each minute Q by 60 s to get a cumulative volume of flow per minute
for(i in 1:length(sample)){
  sample[[i]]$cum_flow_v = sample[[i]]$Flow_cms_v*60
  sample[[i]]$cum_flow_r = sample[[i]]$Flow_cms_r*60
}

# Sum up all cum_flow for each sampling period to get a total cumulative flow
for(i in 1:nrow(time2)){
  time2$cum_v[i] = sum(sample[[i]]$cum_flow_v, na.rm = T)
  time2$cum_r[i] = sum(sample[[i]]$cum_flow_r, na.rm = T)
}


# Now let's calculate the cumulative flow for the VT sensors

# merge with the ISCO discharge data to calculate discharge per an event

total_dis_VT <- merge(weir_discharge, all_samples22, by="DateTime", all =T)%>%
  filter(DateTime>"2019-06-06 10:45:00")

# shift time 2 up one row. Then remove the rows when start_date=END
time_VT=time %>% 
  mutate_at(("time2"), funs(lead), n = 1)%>%
  filter(Start_date=="START"|is.na(Start_date))%>%
  select(time1, time2)%>%
  dplyr::rename("Start_time"="time1",
                "End_time"="time2")

options(scipen = 100, digits = 4)

# create the lists of discharge based on the sample duration of deployments. When the ISCO started collecting the sample
# until it ended. 
sample = list()
for(i in 1:nrow(time_VT)){
  sample[[i]] = weir_discharge[weir_discharge$DateTime>time_VT[i,1] & weir_discharge$DateTime<time_VT[i,2],]
}

# Multiply each minute Q by 60 s to get a cumulative volume of flow per minute and then by 15 because we record a discahrge event every 15 minutes
for(i in 1:length(sample)){
  sample[[i]]$VT_cum_flow_v = sample[[i]]$mean_flow*(60*15)
}

# Sum up all cum_flow for each sampling period to get a total cumulative flow
for(i in 1:nrow(time_VT)){
  time_VT$VT_cum_v[i] = sum(sample[[i]]$VT_cum_flow_v, na.rm = T)
}

```


Then compare the calculated discharge from the ISCO with v-notched, ISCO with v-notched and rectangle, VT pressure transducer, recorded discharge from the ISCO.

The discharge from the ISCO is the gold standard. This will determine what is the best way to calculate discharge when it goes over the weir. 

```{r Plot cumulative discharge}

# merge the two data frames so we have VT discharge and ISCO calculated discharge 

calc_discharge <- merge(time2, time_VT, by= c("Start_time", "End_time"))

a <- ggplot(calc_discharge, aes(x = Start_time))+
  geom_point(aes(y= cum_v))+
  geom_point(aes(y=cum_r), color= "red")+
  geom_point(aes(y=VT_cum_v), color="darkorange")+
  theme_bw()

ggplotly(a)

```



Once we determine the best method for discharge then we can use that to calculate loads. 

```{r Calculate Load}

# make metals easy to work with 
long_metals <- ISCO_metals%>%
  select(-starts_with("Flag"))%>%
  #pivot_longer(!DateTime, names_to = "source", values_to = "observations")%>%
  pivot_longer(!c(Reservoir, Site, DateTime), names_to = "variable", values_to = "observation")%>%
  drop_na(observation)


# join the cumulative flow and the metals observation by the closes observations
by <- join_by(closest(End_time <= DateTime))
Calc_dis_metals <- full_join(calc_discharge, long_metals, by)


# Calculate loads

calc_load <- Calc_dis_metals%>%
  # convert m3 to L
  mutate(cum_v_L = cum_v * 1000,
         cum_r_L = cum_r * 1000,
         VT_cum_v_L = VT_cum_v * 1000,
         # cumulative volume (L) * metals concentration (mg/L) = load (mg)/ divide by 1000000 to get (kg)
         ISCO_load_v_kg = (cum_v_L*observation)/1000000,
         ISCO_load_r_kg = (cum_r_L*observation)/1000000,
         VT_load_v_kg = (VT_cum_v_L*observation)/1000000,
         diff_time = difftime(End_time, Start_time, units='mins'),
         ISCO_load_v_kg_d = (ISCO_load_v_kg/as.numeric(diff_time))*(60*24),
         ISCO_load_r_kg_d = (ISCO_load_r_kg/as.numeric(diff_time))*(60*24),
         VT_load_v_kg_d = (VT_load_v_kg/as.numeric(diff_time))*(60*24),
         Reservoir = "FCR",
         Site = 100.1)%>%
  select(Reservoir, Site, Start_time, End_time, variable, observation, cum_v, cum_r, VT_cum_v,
         cum_v_L, cum_r_L, VT_cum_v_L, ISCO_load_v_kg, ISCO_load_r_kg, VT_load_v_kg, diff_time,
         ISCO_load_v_kg_d, ISCO_load_r_kg_d, VT_load_v_kg_d)

}


