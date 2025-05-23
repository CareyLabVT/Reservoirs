# script to calculate discharge from digitized stream width/depth/velocity measurements
#install.packages("tidyverse")
library(tidyverse)

flow_20 <- read.csv("./Data/DataNotYetUploadedToEDI/Raw_inflow/Manual_Discharge/2020_Discharge_Flowmate.csv") # the location of the discharge_digitized.csv, comes from Reservoir Group google drive
flow_20$Date <- as.Date(flow_20$Date)

flow_21 <- read.csv("./Data/DataNotYetUploadedToEDI/Raw_inflow/Manual_Discharge/2021_Discharge_Flowmate.csv") # the location of the discharge_digitized.csv, comes from Reservoir Group google drive
flow_21$Date <- as.Date(flow_21$Date)

flow <- rbind(flow_20, flow_21)

# first convert the depth to m in a new column (it is always measured in cm in the field)
flow$Depth_m <- flow$Depth_cm/100

# now convert the velocity to m/s (the flowmeter measures in ft/s)
#flow$Velocity <- as.numeric(flow$Velocity)
flow$Velocity_m.s <- ifelse(flow$Velocity_unit=="ft_s", flow$Velocity*0.3048, flow$Velocity)


# lastly calculate discharge for each interval
flow$Discharge <- flow$Depth_m*flow$Velocity_m.s*flow$WidthInterval_m

# now sum by site and date to get the total discharge for that day/site
flow <-  flow %>% group_by(Site, Date) %>% mutate(Discharge_m3_s = sum(Discharge, na.rm = TRUE))


# now subset out only the unique discharge measurements
discharge <- flow %>% select(Date, Site, Discharge_m3_s, FlowmeterSensorID, Notes)
discharge <- discharge[!duplicated(discharge[1:3]),]

write.csv(discharge, './Data/DataNotYetUploadedToEDI/Raw_inflow/Manual_Discharge/Calculated_Discharge_Flowmate_Data_2020_2021.csv', row.names = FALSE)
