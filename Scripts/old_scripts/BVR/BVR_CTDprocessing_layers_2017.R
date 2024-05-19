#################################
#################################
###### BVR_CTD PROCESSING #######
#################################
#################################

setwd("~/Dropbox/2017/CTD_2017")

bvr = read.table("061517_bvr.asc", header = T, row.names = NULL)

names(bvr)[1] <- "depth_m"
names(bvr)[2] <- "do_mgL"
names(bvr)[3] <- "do_sat"
names(bvr)[4] <- "pH"
names(bvr)[5] <- "ORP_mV"
names(bvr)[6] <- "cond_uScm"
names(bvr)[7] <- "temp_C"
names(bvr)[8] <- "NAN"


for(i in 1:length(bvr$depth_m)){
  if(bvr$depth_m[i]<0){
    bvr$temp_C[i] = NA
  }
  if(bvr$do_mgL[i]<0){
    bvr$temp_C[i] = NA
  }
  if(bvr$ORP_mV[i]<0){
    bvr$temp_C[i] = NA
  }
  if(bvr$cond_uScm[i]<0){
    bvr$temp_C[i] = NA
  }
}

site = na.omit(bvr)

site = site[,c(1,2,3,4,5,6,7)]

layer = data.frame(site, row.names = NULL)

#layer = layer[,-1]

df.final<-data.frame()

### Run the for loop across the depths ###
for(i in 1:length(layer[,1])){
  layer1<-layer[order(abs(layer[,1] - 0.1)), ][1,]
  layer2<-layer[order(abs(layer[,1] - 0.4)), ][1,]
  layer3<-layer[order(abs(layer[,1] - 0.7)), ][1,]
  layer4<-layer[order(abs(layer[,1] - 1)), ][1,]
  layer5<-layer[order(abs(layer[,1] - 1.3)), ][1,]
  layer6<-layer[order(abs(layer[,1] - 1.6)), ][1,]
  layer7<-layer[order(abs(layer[,1] - 1.9)), ][1,]
  layer8<-layer[order(abs(layer[,1] - 2.3)), ][1,]
  layer9<-layer[order(abs(layer[,1] - 2.6)), ][1,]
  layer10<-layer[order(abs(layer[,1] - 2.9)), ][1,]
  layer11<-layer[order(abs(layer[,1] - 3.2)), ][1,]
  layer12<-layer[order(abs(layer[,1] - 3.5)), ][1,]
  layer13<-layer[order(abs(layer[,1] - 3.8)), ][1,]
  layer14<-layer[order(abs(layer[,1] - 4.1)), ][1,]
  layer15<-layer[order(abs(layer[,1] - 4.4)), ][1,]
  layer16<-layer[order(abs(layer[,1] - 4.7)), ][1,]
  layer17<-layer[order(abs(layer[,1] - 5)), ][1,]
  layer18<-layer[order(abs(layer[,1] - 5.3)), ][1,]
  layer19<-layer[order(abs(layer[,1] - 5.6)), ][1,]
  layer20<-layer[order(abs(layer[,1] - 5.9)), ][1,]
  layer21<-layer[order(abs(layer[,1] - 6.2)), ][1,]
  layer22<-layer[order(abs(layer[,1] - 6.5)), ][1,]
  layer23<-layer[order(abs(layer[,1] - 6.8)), ][1,]
  layer24<-layer[order(abs(layer[,1] - 7.1)), ][1,]
  layer25<-layer[order(abs(layer[,1] - 7.4)), ][1,]
  layer26<-layer[order(abs(layer[,1] - 7.7)), ][1,]
  layer27<-layer[order(abs(layer[,1] - 8)), ][1,]
  layer28<-layer[order(abs(layer[,1] - 8.3)), ][1,]
  layer29<-layer[order(abs(layer[,1] - 8.7)), ][1,]
  layer30<-layer[order(abs(layer[,1] - 9)), ][1,]
  layer31<-layer[order(abs(layer[,1] - 9.3)), ][1,]
  layer32<-layer[order(abs(layer[,1] - 9.6)), ][1,]
  layer33<-layer[order(abs(layer[,1] - 9.9)), ][1,]
  layer34<-layer[order(abs(layer[,1] - 10.2)), ][1,]
  layer35<-layer[order(abs(layer[,1] - 10.5)), ][1,]
  layer36<-layer[order(abs(layer[,1] - 10.8)), ][1,]
  }

df.final = rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,layer11,layer12,layer13,layer14,layer15,layer16,layer17,layer18,layer19,
                 layer20,layer21,layer22,layer23,layer24,layer25,layer26,layer27,layer28,layer29,layer30,layer31,layer32,layer33,layer34,layer35,layer36)

write.table(df.final, "bvrlayer_061517.csv", row.names = F, col.names = TRUE, sep = ',')

plot(df.final$do_mgL, -df.final$depth_m, type = "l", lwd = 2)
plot(df.final$do_sat, -df.final$depth_m, type = "l", lwd = 2)
plot(df.final$pH, -df.final$depth_m, type = "l", lwd = 2)
plot(df.final$ORP_mV, -df.final$depth_m, type = "l", lwd = 2)
plot(df.final$cond_uScm, -df.final$depth_m, type = "l", lwd = 2)
plot(df.final$temp_C, -df.final$depth_m, type = "l", lwd = 2)

dev.copy2pdf(file="BVR_CTD_061517.pdf")

# load libraries
library(readr)
library(dplyr)
library(tidyr)
library(rLakeAnalyzer)
library(lubridate)
library(LakeMetabolizer)

# Setwd WD  (Working Directory)
setwd("~/Dropbox/Data/2016 Data/BVR_layers")#This is the WD that all the files are in, you will need to adapt
 # this is the output file named old. if you run this line, all the data should be stacked into a single dataset
### AFter this, write your own .csv output script, this will be the final code from your layers scrits, only with a different name and WD.


files = list.files(path = "~/Dropbox/Data/2016 Data/BVR_layers") # This reads all the files into the R environment
files # run this to make sure you have all the files
old = read.csv(files[1])

old = read.csv(files[1], header = T, row.names = NULL)
newdate <- sapply(strsplit(files[1], "_"), "[[", 1)
newdate <- mdy(newdate)
date <- seq(from = newdate,to = newdate,length.out = nrow(old))
old <- cbind(date,old)

for (i in 2:length(files)){
  new = read.csv(files[i], stringsAsFactors = F, header = T, row.names = NULL)
  newdate <- sapply(strsplit(files[i], "_"), "[[", 1)
  newdate <- mdy(newdate)
  date <- seq(from = newdate,to = newdate,length.out = nrow(new)) # !! NKW changed this
  new <- cbind(date,new)
  old = rbind(old,new, deparse.level = 1)
}

layers <- round(as.numeric(old$depth),digits = .5)

bvrLayer = cbind(layers,old, deparse.level = 1)

write.table(bvrLayer, "BVR_Layers_2016.csv", row.names = F, sep = ',')
BVR_Layers_2016 <- read.csv("BVR_Layers_2016.csv", header = T)

BVR_2016_New <- BVR_Layers_2016 %>%
  select(sampletime,depth,wTemp)%>%
  spread(depth,wTemp)

names(BVR_2016_New)

colnames(BVR_2016_New)[-1] = paste0('wtr_',colnames(BVR_2016_New)[-1])
names(BVR_2016_New)[1] <- "datetime"

BVRthermo_2016 <- ts.thermo.depth(BVR_2016_New)
BVRmeta_2016 <- ts.meta.depths(BVR_2016_New)

write.table(BVR_2016_New, "beaver.wtr", sep = '\t', row.names = F)

wtr.path <- system.file('extdata', 'beaver.wtr', package = "rLakeAnalyzer")

wtr = load.ts(wtr.path)

wtr.heat.map(wtr)
wtr.heatmap.layers(wtr)

write.table(BVRthermo_2016, "BVR_Thermo_2016.csv", row.names = F, sep = ',')
write.table(BVRmeta_2016, "BVR_Meta_2016.csv", row.names = F, sep = ',')
################################################################################################
################################################################################################
################################################################################################


files = list.files(path = "~/Dropbox/Data/2016 Data/BVR_MSN2_Layers") # This reads all the files into the R environment
files # run this to make sure you have all the files
old = read.csv(files[1])

old = read.csv(files[1], header = T, row.names = NULL)
newdate <- sapply(strsplit(files[1], "_"), "[[", 1)
newdate <- mdy(newdate)
date <- seq(from = newdate,to = newdate,length.out = nrow(old))
old <- cbind(date,old)

for (i in 2:length(files)){
  new = read.csv(files[i], stringsAsFactors = F, header = T, row.names = NULL)
  newdate <- sapply(strsplit(files[i], "_"), "[[", 1)
  newdate <- mdy(newdate)
  date <- seq(from = newdate,to = newdate,length.out = nrow(new)) # !! NKW changed this
  new <- cbind(date,new)
  old = rbind(old,new, deparse.level = 1)
}

layers <- round(as.numeric(old$depth),digits = .5)



bvrLayer = cbind(layers,old, deparse.level = 1)

write.table(bvrLayer, "BVR_MSN2_Layers_2016.csv", row.names = F, col.names = T, sep = ',')
MSN2_Layers_2016 <- read.csv("BVR_MSN2_Layers_2016.csv", header = T)



MSN2_BVR_2016_New <- MSN2_Layers_2016 %>%
  select(sampletime,depth,oxygen)%>%
  spread(depth,oxygen)

names(MSN2_BVR_2016_New)

colnames(MSN2_BVR_2016_New)[-1] = paste0('wtr_',colnames(MSN2_BVR_2016_New)[-1])
names(MSN2_BVR_2016_New)[1] <- "datetime"

write.table(MSN2_BVR_2016_New, "beaver2.wtr", sep = '\t', row.names = F)
wtr.path2 <- system.file('extdata', 'beaver3.wtr', package = "rLakeAnalyzer")

wtr2 = load.ts(wtr.path2)

wtr.heat.map(wtr2)
wtr.heatmap.layers(wtr2)





# 
# ### Buoy hourly wide ####
# 
# BVR2014_Wide <- old %>%
#   spread(depth,wTemp)
# ### plotting buoy data
# names(buoyHourlyWide)
# colnames(buoyHourlyWide)[-1] = paste0('wtr_',
#                                       colnames(buoyHourlyWide)[-1])
# wtr.heat.map(buoyHourlyWide)
# ts.thermo.depth(buoyHourlyWide)
# 



MSN1_BVRthermo_2016 <- ts.thermo.depth(MSN1_BVR_2016_New)
MSN1_BVRmeta_2016 <- ts.meta.depths(MSN1_BVR_2016_New)

write.table(MSN1_BVRthermo_2016,'BVR_ThermoDepth_MSN1_2016.csv', row.names = F, sep = ',')
write.table(MSN1_BVRmeta_2016,'BVR_MetaDepths_MSN1_2016.csv', row.names = F, sep = ',')

wtr.heatmap.layers(MSN1_BVR_2016_New)
wtr.heat.map(bvrLayer)
