library(dplyr)
library(tidyr)
library(lubridate)
library(rLakeAnalyzer)

setwd("~/Dropbox/2017/CTD_2017")
# This reads all the files into the R environment
layer <- read.csv("GWR_temp_profiles.csv", header = T)
layer$datetime <-as.POSIXct(strptime(layer$datetime, "%Y-%m-%d %H:%M:%S", tz="EST"))

df.final<-data.frame()

layer1<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 0.1)))
layer2<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 1)))
layer3<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 2)))
layer4<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 3)))
layer5<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 4)))
layer6<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 5)))
layer7<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 6)))
layer8<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 7)))
layer9<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 8)))
layer10<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 9)))
layer11<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 10)))
layer12<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 11)))
layer13<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 12)))
layer14<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 13)))

df.final = rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,layer11,layer12,layer13,layer14)

gwr_layers <- arrange(df.final, datetime)
gwr_layers$depth <- round(as.numeric(gwr_layers$depth), digits = .5)

gwr_2016_new <- gwr_layers %>%
  select(datetime,depth,wtp)%>%
  spread(depth,wtp)

colnames(gwr_2016_new)[-1] = paste0('wtr_',colnames(gwr_2016_new)[-1])
names(gwr_2016_new)[1] <- "datetime"

GWRthermo_2016 <- ts.thermo.depth(gwr_2016_new)
GWRmeta_2016 <- ts.meta.depths(gwr_2016_new)

write.csv(GWRthermo_2016,'GWR_ThermoDepth_2016.csv', row.names = F)
write.csv(GWRmeta_2016,'GWR_MetaDepths_2016.csv', row.names = F)
