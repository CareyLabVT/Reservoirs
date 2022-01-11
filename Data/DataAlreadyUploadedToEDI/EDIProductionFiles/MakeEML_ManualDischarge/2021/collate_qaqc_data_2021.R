# script to collate and qaqc data prior to publication on edi
library(tidyverse)
library(ggplot2)

# read in new data, format, and plot for QAQC
newdata <- read.csv('./Data/DataNotYetUploadedToEDI/Raw_inflow/Wetland_Discharge_Data_2020_2021.csv')
newdata$Date <- as.Date(newdata$Date)
newdata$Reservoir <- NA
newdata$Method <- NA
newdata$Flag_Flow <- 0

for(i in 1:nrow(newdata)){
  if(newdata$Site[i]=='F200'){
    newdata$Reservoir <- 'FCR'
    newdata$Site[i] <- 200
  }
  if(newdata$FlowmeterSensorID[i]=='flowmate new'){
    newdata$Method[i] <- F
  }
}

newdata <- newdata %>% select(Reservoir, Site, Date, Discharge_m3_s, Method, Flag_Flow)
colnames(newdata) <- c('Reservoir', 'Site', 'Date', 'Flow_cms', 'Method', 'Flag_Flow')

ggplot(data = newdata, aes(x = Date, y = Flow_cms)) + 
  geom_line() 

# combine with previous published data
olddata <- read.csv('./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ManualDischarge/2019/2019_Continuum_Discharge.csv')

edi <- rbind(olddata, newdata)
write.csv(edi, './Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ManualDischarge/2021/ManualDischarge_2019_2021.csv', row.names = FALSE)
