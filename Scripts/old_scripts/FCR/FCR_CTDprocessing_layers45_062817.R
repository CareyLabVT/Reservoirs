### CTD CLLEAN UP FILES ###
#FCR site 10 CTD data processing for the entire year
# Author RPM
# CTD site 10 Layer processing
# 062817

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

setwd("")
# This reads all the files into the R environment
files = list.files(path = "")

# file name format = MMDDYY_resX.asc (X = site, example: 051416_fcr50.asc)
# run this to make sure you have all the files
files 
old = read.table(files[1], header = T, row.names = NULL)

# Stripsplit the date in the ctd file to rbind to the dataset  

newdate <- sapply(strsplit(files[1], "_"), "[[", 1)
newdate <- mdy(newdate)
date <- seq(from = newdate,to = newdate,length.out = nrow(old))
old <- cbind(date,old)

for (i in 2:length(files)){
  new = read.table(files[i], stringsAsFactors = F, header = T, row.names = NULL)
  newdate <- sapply(strsplit(files[i], "_"), "[[", 1)
  newdate <- mdy(newdate)
  date <- seq(from = newdate,to = newdate,length.out = nrow(new))
  new <- cbind(date,new)
  old = rbind(old,new, deparse.level = 1)
}

names(old)[1] <- "date"
names(old)[2] <- "depth"
names(old)[3] <- "wTemp"
names(old)[4] <- "chla"
names(old)[5] <- "turb"
names(old)[6] <- "depth_f"
names(old)[7] <- "cond"
names(old)[8] <- "oxygen"
names(old)[9] <- "saturate"

for(i in 1:length(old$depth)){
  if(old$depth[i]<0){
    old$wTemp[i] = NA
  }
  if(old$oxygen[i]<0){
    old$wTemp[i] = NA
  }
  if(old$cond[i]<0){
    old$wTemp[i] = NA
  }
  if(old$depth_f[i]<0){
    old$wTemp[i] = NA
  }
  if(old$turb[i]<0){
    old$wTemp[i] = NA
  }
  if(old$chla[i]<0){
    old$wTemp[i] = NA
  }
}


layer = na.omit(old)

layer = data.frame(layer, row.names = NULL)

#layer = layer[,-1]

df.final<-data.frame()

layer1<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 0.1)))
layer2<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 0.4)))
layer3<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 0.7)))
layer4<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 1)))
layer5<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 1.3)))
layer6<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 1.6)))
layer7<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 1.9)))
layer8<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 2.3)))
layer9<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 2.6)))
layer10<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 2.9)))
layer11<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 3.2)))
layer12<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 3.5)))
layer13<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 3.8)))
layer14<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 4.1)))
layer15<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 4.4)))
layer16<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 4.7)))
layer17<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 5)))
layer18<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 5.3)))
layer19<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 5.6)))
layer20<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 5.9)))
layer21<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 6.2)))
layer22<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 6.5)))
layer23<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 6.8)))
layer24<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 7.1)))
layer25<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 7.4)))
layer26<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 7.7)))
layer27<-layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 8)))


df.final = rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,layer11,layer12,layer13,layer14,layer15,layer16,layer17,layer18,layer19,
                 layer20,layer21,layer22,layer23,layer24,layer25,layer26,layer27)

fcr45_layers <- arrange(df.final, date)

write.table(df.final, "fcrlayer45_2017.csv", row.names = F, col.names = TRUE, sep = ',')

