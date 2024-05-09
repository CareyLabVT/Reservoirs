### Transforming the outputted csv files to a MATLAB format
### Reason I am plotting in R and Not matlab is because MATLAB
### makes for better Heatmap plots

### Primary Author: Ryan McClure
### Date Developed: 09June2019
### Date updated: 09June19

# download pacman if you do not have it
# This makes downloading and opening all other packages way easier if you do not have them
if (!"pacman" %in% installed.packages()) install.packages("pacman")
library(pacman)

# read in packages
pacman::p_load(tidyverse, zoo, lubridate, rLakeAnalyzer,reshape2,akima,dplyr,gridExtra,grid,colorRamps)

# This reads all the files into the R environment
files = list.files(path = "./CTD_CASTS_CSV")
#files = list.files(path = "C:/Users/Owner/Dropbox/2019/CTD_2019/MSN_CTD_DATA")

# run this to make sure you have all the files
files

#This reads the first file in
ctd = read.csv(files[1]) 

# Loop through and pull all the files in
for (i in 2:length(files)){
  new = read.csv(files[i],stringsAsFactors = F) 
  ctd = rbind(ctd,new)
}

write_csv(ctd, "CTD_notmatlab_ready_2019_fcr50.csv")

# Convert dates to MatLab dates
POSIXt2matlabUTC = function(x) {
  if (class(x)[1] == "POSIXct") {
    x = as.POSIXlt(x, tz = "UTC") #convert to UTC time zone
    days = as.numeric(x) / 86400 #convert to days
    datenum = days + 719529 #convert to MATLAB datenum
  } else if (class(x)[1] == "POSIXlt") {
    x = as.POSIXlt(x, tz = "UTC") #convert to UTC time zone
    days = as.numeric(x) / 86400  #convert to days
    datenum = days + 719529 #convert to MATLAB datenum
  } else {stop("POSIXct or POSIXlt object required for input")}
  return(datenum)
}

ctd_matlab <- ctd
# Get dates into a reasonable format for MATLAB outputs
ctd_matlab$Date <- ymd_hms(ctd_matlab$Date)
ctd_matlab$Date <- as.POSIXct(strptime(ctd_matlab$Date, "%Y-%m-%d %H:%M:%S", tz="EST"))

#Convert dates into matlab format
ctd_matlab$Date <- POSIXt2matlabUTC(ctd_matlab$Date)


write_csv(ctd_matlab, "CTD_matlab_ready_2019_fcr50.csv", col_names = F)


# filter out depths in the CTD cast that are closest to these specified values for FLARE - AED 
df.final<-data.frame()

ctd1<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
ctd2<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.33)))
ctd3<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.66)))
ctd4<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1)))
ctd5<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.33)))
ctd6<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.6)))
ctd7<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.0)))
ctd8<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.33)))
ctd9<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.66)))
ctd10<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.0)))
ctd11<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.33)))
ctd12<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.66)))
ctd13<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.0)))
ctd14<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.33)))
ctd15<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.66)))
ctd16<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.0)))
ctd17<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.33)))
ctd18<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.66)))
ctd19<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.0)))
ctd20<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.33)))
ctd21<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.66)))
ctd22<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.0)))
ctd23<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.33)))
ctd24<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.66)))
ctd25<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.0)))
ctd26<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.33)))
ctd27<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.66)))
ctd28<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.0)))
ctd29<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.33)))


# Bind each of the data layers together.
df.final = rbind(ctd1,ctd2,ctd3,ctd4,ctd5,ctd6,ctd7,ctd8,ctd9,ctd10,ctd11,ctd12,ctd13,ctd14,ctd15,ctd16,ctd17,ctd18,ctd19,
                 ctd20,ctd21,ctd22,ctd23,ctd24,ctd25,ctd26,ctd27,ctd28,ctd29)

# Re-arrange the data frame by date
ctd_new <- arrange(df.final, Date)

# Round each extracted depth to the nearest 10th. 
ctd_new$Depth_m <- round(as.numeric(ctd_new$Depth_m), digits = 0.5)
ctd_new$Date <- ymd_hms(ctd_new$Date)

write_csv(ctd_new, "ctd_short_for_GLM_AED_2019.csv")


# # Pulling just temp, depth and date and going from long to wide. 
# temp_RLA <- ctd_new %>%
#   select(Date,Depth_m,Temp_C)%>%
#   spread(Depth_m,Temp_C)
# 
# # renaming the column names to include wtr_ 
# # Otherwise, rLakeAnaylzer will not run!
# colnames(temp_RLA)[-1] = paste0('wtr_',colnames(temp_RLA)[-1])
# 
# # rename the first column to "datetime"
# names(temp_RLA)[1] <- "datetime"
# 
# # Calculate thermocline depth
# FCR_thermo_18 <- ts.thermo.depth(temp_RLA)
# 
# #rename the datetime name back to Date
# names(FCR_thermo_18)[1] <- "Date"
