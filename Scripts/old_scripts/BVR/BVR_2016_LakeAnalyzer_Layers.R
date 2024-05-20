library(dplyr)
library(tidyr)
library(lubridate)
library(rLakeAnalyzer)

setwd("~/Dropbox/2017/CTD_2017")
# This reads all the files into the R environment
layer <- read.csv("BVR_temp_profiles.csv", header = T)
layer$datetime <-as.POSIXct(strptime(bvr_temp$datetime, "%Y-%m-%d %H:%M:%S", tz="EST"))

df.final<-data.frame()

layer1<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 0.1)))
layer2<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 0.5)))
layer3<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 1)))
layer4<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 1.5)))
layer5<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 2)))
layer6<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 2.5)))
layer7<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 3)))
layer8<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 3.5)))
layer9<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 4)))
layer10<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 4.5)))
layer11<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 5)))
layer12<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 5.5)))
layer13<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 6)))
layer14<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 6.5)))
layer15<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 7)))
layer16<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 7.5)))
layer17<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 8)))
layer18<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 8.5)))
layer19<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 9)))
layer20<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 9.5)))
layer21<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 10)))
layer22<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 10.5)))
layer23<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 11)))


df.final = rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,layer11,layer12,layer13,layer14,layer15,layer16,layer17,layer18,layer19,
                 layer20,layer21,layer22,layer23)

bvr_layers <- arrange(df.final, datetime)
bvr_layers$depth <- round(as.numeric(bvr_layers$depth), digits = .5)

for(i in 1:length(bvr_layers$depth)){
  if(bvr_layers$depth[i] == 0.2){
    bvr_layers$depth[i] = 0.1
  }
  if(bvr_layers$depth[i] == 0.3){
    bvr_layers$depth[i] = 0.1
  }
  if(bvr_layers$depth[i] == 10.8){
    bvr_layers$depth[i] = 11
  }
  if(bvr_layers$depth[i] == 10.9){
    bvr_layers$depth[i] = 11
  }
}

bvr_2016_new <- bvr_layers %>%
  select(datetime,depth,wtp)%>%
  spread(depth,wtp)

colnames(bvr_2016_new)[-1] = paste0('wtr_',colnames(bvr_2016_new)[-1])
names(bvr_2016_new)[1] <- "datetime"

BVRthermo_2016 <- ts.thermo.depth(bvr_2016_new)
BVRmeta_2016 <- ts.meta.depths(bvr_2016_new)

write.csv(BVRthermo_2016,'BVR_ThermoDepth_2016.csv', row.names = F)
write.csv(BVRmeta_2016,'BVR_MetaDepths_2016.csv', row.names = F)

