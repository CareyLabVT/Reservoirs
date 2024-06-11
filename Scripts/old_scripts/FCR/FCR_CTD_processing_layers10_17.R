#FCR site 10 CTD data processing for the entire year

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

setwd("")
# This reads all the files into the R environment
files = list.files(path = "~/Dropbox/Data/2016 Data/CTD 2016/ASCII/FCR10") 

# run this to make sure you have all the files
files 
old = read.table(files[1], header = T, row.names = NULL)


newdate <- sapply(strsplit(files[1], "_"), "[[", 1)
newdate <- mdy(newdate)
date <- seq(from = newdate,to = newdate,length.out = nrow(old))
old <- cbind(date,old)

for (i in 2:length(files)){
  new = read.table(files[i], stringsAsFactors = F, header = T, row.names = NULL)
  newdate <- sapply(strsplit(files[i], "_"), "[[", 1)
  newdate <- mdy(newdate)
  date <- seq(from = newdate,to = newdate,length.out = nrow(new)) # !! NKW changed this
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


layer = data.frame(old, row.names = NULL)

layer1 <- layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 0.1)))
layer2 <- layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 0.4)))
layer3 <- layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 0.7)))
layer4 <- layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 1)))
layer5 <- layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 1.3)))
layer6 <- layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 1.6)))
layer7 <- layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 1.9)))
layer8 <- layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 2.3)))
layer9 <- layer %>% group_by(date) %>% slice(which.min(abs(as.numeric(depth) - 2.6)))


df.final = rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9)

fcr10_layers <- arrange(df.final, date)

write.csv(fcr10_layers[,c(1,2,3,4,5,7,8,9)], "FCR_site10_layers_2016.csv", row.names=FALSE)
