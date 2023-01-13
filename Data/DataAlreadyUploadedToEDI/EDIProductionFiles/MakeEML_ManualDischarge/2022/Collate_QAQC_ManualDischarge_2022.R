# script to collate and qaqc data prior to publication on edi
library(tidyverse)
library(ggplot2)
#Install the required googlesheets4 package
#install.packages('googlesheets4')
#Load the library 
library(googlesheets4)

# read in data from previous year's publication
olddata <- read.csv('./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ManualDischarge/2021/ManualDischarge_2019_2021.csv')
olddata$Date <- as.Date(olddata$Date)

#####################################################################################
# calculate discharge
#####################################################################################
# download raw flowmate data from google drive
flow <- read_sheet('https://docs.google.com/spreadsheets/d/1Vsly-gKL0YsrNLlGpDJdTLTTlfEnxOIV1rmOahk_rck/edit#gid=0')

# calculate discharge
# first convert the depth to m in a new column (it is always measured in cm in the field)
flow$Depth_m <- flow$Depth_cm/100

# now convert the velocity to m/s (the flowmeter measures in ft/s)
flow$Velocity_m.s <- ifelse(flow$Velocity_unit=="ft_s", flow$Velocity*0.3048, flow$Velocity)

# lastly calculate discharge for each interval
flow$Discharge <- flow$Depth_m*flow$Velocity_m.s*flow$WidthInterval_m

# now sum by site and date to get the total discharge for that day/site
flow <-  flow %>% group_by(Site, Date) %>% mutate(Discharge_m3_s = sum(Discharge, na.rm = TRUE))

# now subset out only the unique discharge measurements
discharge <- flow %>% 
  select(Date, Site, Discharge_m3_s, Notes) %>% 
  distinct(Date, Site, .keep_all = TRUE)

write.csv(discharge, './Data/DataNotYetUploadedToEDI/Raw_inflow/Manual_Discharge/2022/Calculated_Discharge_Flowmate_Data_2022.csv', row.names = FALSE)

#####################################################################################
# collate 2022 data
#####################################################################################
newdata <- read.csv('./Data/DataNotYetUploadedToEDI/Raw_inflow/Manual_Discharge/2022/Calculated_Discharge_Flowmate_Data_2022.csv')
newdata$Date <- as.Date(newdata$Date)
newdata$Reservoir <- NA
newdata$Method <- 'F'
newdata$Flag_Flow <- 0

for(i in 1:nrow(newdata)){
  if(newdata$Site[i]=='F200'){
    newdata$Reservoir[i] <- 'FCR'
    newdata$Site[i] <- 200
  }else{
    newdata$Reservoir[i] <- 'CCR'
  }
}

newdata <- newdata %>% select(Reservoir, Site, Date, Discharge_m3_s, Method, Flag_Flow)
colnames(newdata) <- c('Reservoir', 'Site', 'Date', 'Flow_cms', 'Method', 'Flag_Flow')

ggplot(data = newdata, aes(x = Date, y = Flow_cms)) + 
  geom_line() +
  facet_wrap(~Reservoir)

# read in bucket and float velocity data
bucket <- read.csv('./Data/DataNotYetUploadedToEDI/Raw_inflow/Manual_Discharge/CCR_bucket_data_2021.csv')
bucket <- bucket %>% select(Reservoir, Site, DateTime, Discharge_cms, Method)
bucket$Flag_Flow <- 0
colnames(bucket) <- c('Reservoir', 'Site', 'Date', 'Flow_cms', 'Method', 'Flag_Flow')
bucket$Date <- as.Date(bucket$Date)

# combine with previous published data
edi <- rbind(olddata, newdata)
edi <- rbind(edi, bucket)
edi$Date <- as.Date(edi$Date)

# sort by date
edi <- edi[order(as.Date(edi$Date)),]

# plot data
ggplot(data = edi, aes(x = Date, y = Flow_cms)) + 
  geom_point(aes(color = Site)) +
  facet_wrap(~Reservoir, scales = 'free_y')

edi$Flag_Flow <- as.character(edi$Flag_Flow)

write.csv(edi, './Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ManualDischarge/2021/ManualDischarge_2019_2021.csv', row.names = FALSE)
