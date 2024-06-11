library(dplyr)
library(tidyr)
library(lubridate)
library(rLakeAnalyzer)

setwd("~/Dropbox/2017/CTD_2017")
# This reads all the files into the R environment
layer <- read.csv("CCR_temp_profiles.csv", header = T)
layer$datetime <-as.POSIXct(strptime(layer$datetime, "%Y-%m-%d %H:%M:%S", tz="EST"))

df.final<-data.frame()


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
layer24<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 11.5)))
layer25<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 12)))
layer26<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 12.5)))
layer27<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 13)))
layer28<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 13.5)))
layer29<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 14)))
layer30<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 14.5)))
layer31<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 15)))
layer32<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 15.5)))
layer33<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 16)))
layer34<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 16.5)))
layer35<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 17)))
layer36<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 17.5)))
layer37<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 18)))
layer38<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 18.5)))
layer39<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 19)))
layer40<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 19.5)))
layer41<-layer %>% group_by(datetime) %>% slice(which.min(abs(as.numeric(depth) - 20)))


df.final = rbind(layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,layer11,layer12,layer13,layer14,layer15,layer16,layer17,layer18,layer19,
                 layer20,layer21,layer22,layer23,layer24,layer25,layer26,layer27,layer28,layer29,layer30,layer31,layer32,layer33,layer34,layer35,layer36,layer37,
                 layer38,layer39,layer40,layer41)

ccr_layers <- arrange(df.final, datetime)
ccr_layers$depth <- round(as.numeric(ccr_layers$depth), digits = .5)

for(i in 1:length(ccr_layers$depth)){
  if(ccr_layers$depth[i] == 0.7){
    ccr_layers$depth[i] = 0.5
  }
  if(ccr_layers$depth[i] == 0.8){
    ccr_layers$depth[i] = 0.5
  }
  if(ccr_layers$depth[i] == 0.9){
    ccr_layers$depth[i] = 0.5
  }
}

ccr_2016_new <- ccr_layers %>%
  select(datetime,depth,wtp)%>%
  spread(depth,wtp)

colnames(ccr_2016_new)[-1] = paste0('wtr_',colnames(ccr_2016_new)[-1])
names(ccr_2016_new)[1] <- "datetime"

CCRthermo_2016 <- ts.thermo.depth(ccr_2016_new)
CCRmeta_2016 <- ts.meta.depths(ccr_2016_new)

write.csv(CCRthermo_2016,'CCR_ThermoDepth_2016.csv', row.names = F)
write.csv(CCRmeta_2016,'CCR_MetaDepths_2016.csv', row.names = F)

