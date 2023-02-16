fcr_data_wrangling <- function(){
  ### Transforming the outputted csv files to a MATLAB format
  ### Reason I am plotting in R and Not matlab is because MATLAB
  ### makes for better Heatmap plots
  
  ### Primary Author: Ryan McClure
  ### Date Developed: 09June2019
  ### Date updated: 09June19
  
  # This reads all the files into the R environment
  files = list.files(path = "./CTD_CASTS_CSV")
  #files = list.files(path = "C:/Users/Owner/Dropbox/2019/CTD_2019/MSN_CTD_DATA")
  
  # run this to make sure you have all the files
  #files
  
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
  
  
  # filter out depths in the CTD cast that are closest to the specified values for FLARE - AED 
  depths <- round(c(0.1,seq(0.33,9.33, by = 1/3)),2) #These are the depths we are using as of 02 Aug 19. 
  newDepths <- sort(c(0.1,seq(1,9, by = 1), seq(0.3,9.3, by = 1), seq(0.6,8.6, by = 1))) 
  # I am reassigning a depth based on the numbers we are currently using for flare, etc
  # (Depths Can be changed as necessary)
  df.final<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - depths[1]))) #Create a new dataframe
  df.final$Depth_m <- newDepths[1]
  for (i in 2:length(depths)){ #loop through all depths and add the closest values to the final dataframe
    ctd_atThisDepth <- ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - depths[i])))
    ctd_atThisDepth$Depth_m <- newDepths[i]
    df.final <- rbind(df.final,ctd_atThisDepth)
  }
  
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
  print("Success! Matlab files and glm aed files have been created. Double check they look fine in dropbox")
  return(ctd_new)
}




ctd_vs_catwalk <- function(ctd_new, on,off,startDate = "2019-06-01 12:00:00"){
  # Compare the CTD and catwalk data. 
  # RPM 18June2019
  
  pacman::p_load(tidyverse, rLakeAnalyzer)
  cat <- read_csv(file = getURL("https://raw.githubusercontent.com/CareyLabVT/SCCData/mia-data/Catwalk.csv"),skip = 1)
  
  cat_sum_19 <- cat %>% filter(TIMESTAMP >= startDate) %>%
    select(TIMESTAMP, doobs_1, doobs_5, doobs_9) %>%
    filter(doobs_1 != "NAN") %>%
    filter(doobs_5 != "NAN") %>%
    filter(doobs_9 != "NAN") %>%
    filter(TIMESTAMP != "NAN") %>%
    filter(TIMESTAMP != "YYYY_MM_DD_HH_MM_SS")
  
  ctd_1.0 <- ctd_new %>% filter(Depth_m == 1.6)
  
  ctd_5.0 <- ctd_new %>% filter(Depth_m == 5.0)
  
  ctd_9.0 <- ctd_new %>% filter(Depth_m == 9.0)
  
  pdf("SEASONAL_CATWALK_CTD_COMPARE_DO_2019.pdf", width=14, height=8)
  plot(as.POSIXct(cat_sum_19$TIMESTAMP), cat_sum_19$doobs_1, type = "l", ylim = c(0,14), xlab = "", ylab = "DO (mg/L)")
  lines(as.POSIXct(cat_sum_19$TIMESTAMP), cat_sum_19$doobs_5, type = "l", ylim = c(0,14), col = "blue")
  lines(as.POSIXct(cat_sum_19$TIMESTAMP), cat_sum_19$doobs_9, type = "l", ylim = c(0,14), col = "magenta")
  points(ctd_1.0$Date, ctd_1.0$DO_mgL, type = "p", pch = 21, col = "black", bg = "black", cex = 2, lwd = 2)
  points(ctd_5.0$Date, ctd_5.0$DO_mgL, type = "p", pch = 21, col = "black", bg = "blue", cex = 2, lwd = 2)
  points(ctd_9.0$Date, ctd_9.0$DO_mgL, type = "p", pch = 21, col = "black", bg = "magenta", cex = 2, lwd = 2)
  abline(v=on, lwd = 1.5)
  abline(v=off,lty=2, lwd = 1.5)
  legend("topright", legend=c("SSS on", "SSS off"), lty = c(1,2), bg = "white")
  dev.off()
  
  ### THERE IS NO dotemp_1 ???
  cat_sum_19_temp <- cat %>% filter(TIMESTAMP >= "2019-06-01 12:00:00") %>%
    select(TIMESTAMP, wtr_1, dotemp_5, dotemp_9) %>%
    filter(wtr_1 != "NAN") %>%
    filter(dotemp_5 != "NAN") %>%
    filter(dotemp_9 != "NAN") %>%
    filter(TIMESTAMP != "NAN") %>%
    filter(TIMESTAMP != "YYYY_MM_DD_HH_MM_SS")
  
  
  pdf("SEASONAL_CATWALK_CTD_COMPARE_TEMP_2019.pdf", width=14, height=8)
  plot(as.POSIXct(cat_sum_19_temp$TIMESTAMP), cat_sum_19_temp$wtr_1, type = "l", ylim = c(4,33), xlab = "", ylab = "Temp (C)")
  lines(as.POSIXct(cat_sum_19_temp$TIMESTAMP), cat_sum_19_temp$dotemp_5, type = "l", ylim = c(4,33), col = "blue")
  lines(as.POSIXct(cat_sum_19_temp$TIMESTAMP), cat_sum_19_temp$dotemp_9, type = "l", ylim = c(4,33), col = "magenta")
  points(ctd_1.0$Date, ctd_1.0$Temp_C, type = "p", pch = 21, col = "black", bg = "black", cex = 2, lwd = 2)
  points(ctd_5.0$Date, ctd_5.0$Temp_C, type = "p", pch = 21, col = "black", bg = "blue", cex = 2, lwd = 2)
  points(ctd_9.0$Date, ctd_9.0$Temp_C, type = "p", pch = 21, col = "black", bg = "magenta", cex = 2, lwd = 2)
  abline(v=on, lwd = 1.5)
  abline(v=off,lty=2, lwd = 1.5)
  legend("topright", legend=c("SSS on", "SSS off"), lty = c(1,2), bg = "white")
  dev.off()
  
  cat_sum_19_chla <- cat %>% filter(TIMESTAMP >= "2019-06-01 12:00:00") %>%
    select(TIMESTAMP, Chla_1 ) %>%
    filter(Chla_1 != "NAN") %>%
    filter(TIMESTAMP != "NAN") %>%
    filter(TIMESTAMP != "YYYY_MM_DD_HH_MM_SS")
  
  pdf("SEASONAL_CATWALK_CTD_COMPARE_CHLA_2019.pdf", width=14, height=8)
  plot(as.POSIXct(cat_sum_19_chla$TIMESTAMP), cat_sum_19_chla$Chla_1, type = "l", ylim = c(0,50), xlab = "", ylab = "chla (ug/L)")
  points(ctd_1.0$Date, ctd_1.0$Chla_ugL, type = "p", pch = 21, col = "black", bg = "green", cex = 2, lwd = 2)
  dev.off()
  
  print("Success! Catwalk comparison figures have been created. Double check they look good on dropbox")
}
