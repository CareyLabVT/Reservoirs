# script to collate and qaqc data prior to publication on edi
library(tidyverse)
library(ggplot2)

# read in new data, format, and plot for QAQC
newdata <- read.csv('./Data/DataNotYetUploadedToEDI/Raw_inflow/Manual_Discharge/Calculated_Discharge_Flowmate_Data_2020_2021.csv')
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
olddata <- read.csv('./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ManualDischarge/2019/2019_Continuum_Discharge.csv')
olddata$Date <- as.Date(olddata$Date)

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
