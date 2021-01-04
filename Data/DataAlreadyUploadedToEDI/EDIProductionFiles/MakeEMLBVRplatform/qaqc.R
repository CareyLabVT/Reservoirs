
#qaqc of BVRdata for EDI
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
library(lubridate)
library(plyr)
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)



#time to now play with BVR data!
#Gateway has missing data sections so combine manual data for EDI
#upload the current BVR data from GitHub
download.file('https://github.com/FLARE-forecast/BVRE-data/raw/bvre-platform-data/BVRplatform.csv', "BVRplatform.csv") 
download.file('https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/BVRplatform_manual_2020.csv', "BVRmanualplatform.csv")

#Read in csv of data from pushed to github
bvrheader1<-read.csv("BVRplatform.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
bvrdata1<-read.csv("BVRplatform.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(bvrdata1)<-names(bvrheader1) #combine the names to deal with Campbell logger formatting

#Read in csv of manual uploaded data
bvrdata3<-read.csv("BVRmanualplatform.csv")

#delete column that was added when uploaded 
bvrdata3=select(bvrdata3, -X)

#combine manual and most recent files
bvrdata4=rbind(bvrdata3, bvrdata1)

#taking out the duplicate values  
obs1=bvrdata4[!duplicated(bvrdata4$TIMESTAMP), ]


#change the date from character to unknown making it easier to graph
obs1$TIMESTAMP <- as.POSIXct(obs1$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+4") 




#check record for gaps
#order data by timestamp
obs1=obs1[order(obs1$TIMESTAMP),]
obs1$DOY=yday(obs1$TIMESTAMP)

#daily record gaps by day of year
for(i in 2:nrow(obs1)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(obs1$DOY[i]-obs1$DOY[i-1]>1){
    print(c(obs1$TIMESTAMP[i-1],obs1$TIMESTAMP[i]))
  }
}
#sub-daily record gaps by record number
for(i in 2:length(obs1$RECORD)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(abs(obs1$RECORD[i]-obs1$RECORD[i-1])>1){
    print(c(obs1$TIMESTAMP[i-1],obs1$TIMESTAMP[i]))
  }
}


qaqc <- function(data_file, maintenance_file, output_file)
{
  BVRDATA_COL_NAMES = c("DateTime", "RECORD", "CR6_Batt_V", "CR6Panel_Temp_C", "ThermistorTemp_C_12.5",
                        "ThermistorTemp_C_11.5", "ThermistorTemp_C_10.5", "ThermistorTemp_C_9.5", "ThermistorTemp_C_8.5",
                        "ThermistorTemp_C_7.5", "ThermistorTemp_C__6.5", "ThermistorTemp_C_5.5", "ThermistorTemp_C_4.5",
                        "ThermistorTemp_C_3.5","ThermistorTemp_C_2.5","ThermistorTemp_C_1.5","ThermistorTemp_C_0.5",
                        "RDO_mgL_7.5", "RDOsat_percent_7.5", "RDOTemp_C_7.5", "RDO_mgL_0.5",
                        "RDOsat_percent_0.5", "RDOTemp_C_0.5", "EXO_Date", "EXO_Time", "EXOTemp_C_1.5", "EXOCond_uScm_1.5",
                        "EXOSpCond_uScm_1.5", "EXOTDS_mgL_1.5", "EXODOsat_percent_1.5", "EXODO_mgL_1.5", "EXOChla_RFU_1",
                        "EXOChla_ugL_1.5", "EXOBGAPC_RFU_1.5", "EXOBGAPC_ugL_1.5", "EXOfDOM_RFU_1.5", "EXOfDOM_QSU_1.5",
                        "EXO_pressure_1.5", "EXO_depth", "EXO_battery", "EXO_cablepower", "EXO_wiper", "Lvl_psi_0.5", "LvlTemp_C_0.5")
  
  # after maintenance, DO values will continue to be replaced by NA until DO_mgL returns within this threshold (in mg/L)
  # of the pre-maintenance value
  DO_RECOVERY_THRESHOLD <- 1
  
  # columns where certain values are stored
  DO_MGL_COLS <- c(18, 21, 31)
  DO_SAT_COLS <- c(19, 22, 30)
  DO_FLAG_COLS <- c(45, 46, 47)
  
  # depths at which DO is measured
  #what do I say for DO depths
  DO_DEPTHS <- c(1.5, 7.5, 0.5)
  
  # EXO sonde sensor data that differs from the mean by more than the standard deviation multiplied by this factor will
  # either be replaced with NA and flagged (if between 2018-10-01 and 2019-03-01) or just flagged (otherwise)
  EXO_FOULING_FACTOR <- 4
  
  
  
  # read catwalk data and maintenance log
  # NOTE: date-times throughout this script are processed as UTC
  #bvrdata <- read_csv(data_file, skip = 4, col_names = CATDATA_COL_NAMES,
                      #col_types = cols(.default = col_double(), DateTime = col_datetime()))
  bvrdata=data_file

  log <- read_csv(maintenance_file, col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  ))
  
  # remove NaN data at beginning
  #catdata <- catdata %>% filter(DateTime >= ymd_hms("2018-07-05 14:50:00"))
  
  # add flag columns
  bvrdata$Flag_All <- 0
  bvrdata$Flag_DO_1.5 <- 0
  bvrdata$Flag_DO_7.5 <- 0
  bvrdata$Flag_DO_0.5 <- 0
  bvrdata$Flag_Chla <- 0
  bvrdata$Flag_Phyco <- 0
  bvrdata$Flag_TDS <- 0
  
  # replace negative DO values with 0
  bvrdata <- bvrdata %>%
    mutate(Flag_DO_1.5 = ifelse((! is.na(EXODO_mgL_1.5) & EXODO_mgL_1.5 < 0)
                            | (! is.na(EXODOsat_percent_1) & EXODOsat_percent_1.5 < 0), 3, Flag_DO_1.5)) %>%
    mutate(EXODO_mgL_1.5 = ifelse(EXODO_mgL_1.5 < 0, 0, EXODO_mgL_1.5)) %>%
    mutate(EXODOsat_percent_1.5 = ifelse(EXODOsat_percent_1.5 <0, 0, EXODOsat_percent_1.5))
  
  bvrdata <- bvrdata %>%
    mutate(Flag_DO_7.5 = ifelse((! is.na(RDO_mgL_7.5) & RDO_mgL_5 < 0)
                            | (! is.na(RDOsat_percent_7.5) & RDOsat_percent_7.5 < 0), 3, Flag_DO_7.5)) %>%
    mutate(RDO_mgL_7.5 = ifelse(RDO_mgL_7.5 < 0, 0, RDO_mgL_7.5)) %>%
    mutate(RDOsat_percent_7.5 = ifelse(RDOsat_percent_7.5 < 0, 0, RDOsat_percent_7.5))

  bvrdata <- bvrdata %>%
    mutate(Flag_DO_0.5 = ifelse((! is.na(RDO_mgL_0.5) & RDO_mgL_0.5 < 0)
                            | (! is.na(RDOsat_percent_0.5) & RDOsat_percent_0.5 < 0), 3, Flag_DO_9)) %>%
    mutate(RDO_mgL_0.5 = ifelse(RDO_mgL_0.5 < 0, 0, RDO_mgL_0.5)) %>%
    mutate(RDOsat_percent_0.5 = ifelse(RDOsat_percent_0.5 < 0, 0, RDOsat_percent_0.5))
  
  # modify bvrdata based on the information in the log
  for(i in 1:nrow(log))
  {
    # get start and end time of one maintenance event
    start <- log$TIMESTAMP_start[i]
    end <- log$TIMESTAMP_end[i]
    
    # get indices of columns affected by maintenance
    if(grepl("^\\d+$", log$colnumber[i])) # single num
    {
      maintenance_cols <- intersect(c(2:44), as.integer(log$colnumber[i]))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", log$colnumber[i])) # c(x;y;...)
    {
      maintenance_cols <- intersect(c(2:44), as.integer(unlist(regmatches(log$colnumber[i],
                                                                          gregexpr("\\d+", log$colnumber[i])))))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", log$colnumber[i])) # c(x:y)
    {
      bounds <- as.integer(unlist(regmatches(log$colnumber[i], gregexpr("\\d+", log$colnumber[i]))))
      maintenance_cols <- intersect(c(2:44), c(bounds[1]:bounds[2]))
    }
    else
    {
      warning(paste("Could not parse column colnumber in row", i, "of the maintenance log. Skipping maintenance for",
        "that row. The value of colnumber should be in one of three formats: a single number (\"47\"), a",
        "semicolon-separated list of numbers in c() (\"c(47;48;49)\"), or a range of numbers in c() (\"c(47:74)\").",
        "Other values (even valid calls to c()) will not be parsed properly."))
      next
    }
    
    # remove EXO_Date and EXO_Time columns from the list of maintenance columns, because they will be deleted later
    maintenance_cols <- setdiff(maintenance_cols, c(24, 25))
    
    if(length(maintenance_cols) == 0)
    {
      warning(paste("Did not parse any valid data columns in row", i, "of the maintenance log. Valid columns have",
        "indices 2 through 39, excluding 21 and 22, which are deleted by this script. Skipping maintenance for that row."))
      next
    }
    
    # replace relevant data with NAs and set "all" flag while maintenance was in effect
    bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, maintenance_cols] <- NA
    bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, "Flag_All"] <- 1
    
    # if DO data was affected by maintenance, set the appropriate DO flags, and replace DO data with NAs after maintenance
    # was in effect until value returns to within a threshold of the value when maintenance began, because the sensors take
    # time to re-adjust to ambient conditions
    last_row_before_maintenance <- tail(bvrdata %>% filter(DateTime < start), 1)
    for(j in 1:3)
    {
      # if maintenance was not in effect on DO data, then skip
      if(! (DO_MGL_COLS[j] %in% maintenance_cols | DO_SAT_COLS[j] %in% maintenance_cols))
      {
        next
      }
      
      # set the appropriate DO flag while maintenance was in effect
      bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, DO_FLAG_COLS[j]] <- 1
      
      last_DO_before_maintenance <- last_row_before_maintenance[[DO_MGL_COLS[j]]][1]
      if(is.na(last_DO_before_maintenance))
      {
        warning(paste("For row", i, "of the maintenance log, the pre-maintenance DO value at depth", DO_DEPTHS[j],
          "could not be found. Not replacing DO values after the end of maintenance. This could occur because the start",
          "date-time for maintenance is at or before the first date-time in the data, or simply because the value was",
          "missing or replaced in prior maintenance."))
      }
      else
      {
        DO_recovery_time <- (bvrdata %>%
                               filter(DateTime > end &
                                      abs(bvrdata[[DO_MGL_COLS[j]]] - last_DO_before_maintenance) <= DO_RECOVERY_THRESHOLD)
                            )$DateTime[1]
        
        # if the recovery time cannot be found, then raise a warning and replace post-maintenance DO values until the end of
        # the file
        if(is.na(DO_recovery_time))
        {
          warning(paste("For row", i, "of the maintenance log, post-maintenance DO levels at depth", DO_DEPTHS[j],
            "never returned within the given threshold of the pre-maintenance DO value. All post-maintenance DO values",
            "have been replaced with NA. This could occur because the end date-time for maintenance is at or after the",
            "last date-time in the data, or simply because post-maintenance levels never returned within the threshold."))
          bvrdata[bvrdata$DateTime > end, intersect(maintenance_cols, c(DO_MGL_COLS[j], DO_SAT_COLS[j]))] <- NA
          bvrdata[bvrdata$DateTime > end, DO_FLAG_COLS[j]] <- 1
        }
        else
        {
          bvrdata[bvrdata$DateTime > end & bvrdata$DateTime < DO_recovery_time,
                  intersect(maintenance_cols, c(DO_MGL_COLS[j], DO_SAT_COLS[j]))] <- NA
          bvrdata[catdata$DateTime > end & bvrdata$DateTime < DO_recovery_time, DO_FLAG_COLS[j]] <- 1
        }
      }
    }
  }
  
  # find EXO sonde sensor data that differs from the mean by more than the standard deviation times a given factor, and
  # replace with NAs between October and March, due to sensor fouling
  Chla_RFU_1.5_mean <- mean(bvrdata$EXOChla_RFU_1.5, na.rm = TRUE)
  Chla_ugL_1.5_mean <- mean(bvrdata$EXOChla_ugL_1.5, na.rm = TRUE)
  BGAPC_RFU_1.5_mean <- mean(bvrdata$EXOBGAPC_RFU_1.5, na.rm = TRUE)
  BGAPC_ugL_1.5_mean <- mean(bvrdata$EXOBGAPC_ugL_1.5, na.rm = TRUE)
  Chla_RFU_1.5_threshold <- EXO_FOULING_FACTOR * sd(bvrdata$EXOChla_RFU_1.5, na.rm = TRUE)
  Chla_ugL_1.5_threshold <- EXO_FOULING_FACTOR * sd(bvrdata$EXOChla_ugL_1.5, na.rm = TRUE)
  BGAPC_RFU_1.5_threshold <- EXO_FOULING_FACTOR * sd(bvrdata$EXOBGAPC_RFU_1.5, na.rm = TRUE)
  BGAPC_ugL_1.5_threshold <- EXO_FOULING_FACTOR * sd(bvrdata$EXOBGAPC_ugL_1.5, na.rm = TRUE)

  bvrdata <- bvrdata %>%
    mutate(Flag_Chla = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                (! is.na(EXOChla_RFU_1.5) & abs(EXOChla_RFU_1.5 - Chla_RFU_1.5_mean) > Chla_RFU_1.5_threshold |
                                 ! is.na(EXOChla_ugL_1.5) & abs(EXOChla_ugL_1.5 - Chla_ugL_1.5_mean) > Chla_ugL_1.5_threshold),
                              4, Flag_Chla)) %>%
    mutate(Flag_Phyco = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                 (! is.na(EXOBGAPC_RFU_1.5) & abs(EXOBGAPC_RFU_1.5 - BGAPC_RFU_1.5_mean) > BGAPC_RFU_1.5_threshold |
                                  ! is.na(EXOBGAPC_ugL_1.5) & abs(EXOBGAPC_ugL_1.5 - BGAPC_ugL_1.5_mean) > BGAPC_ugL_1.5_threshold),
                               4, Flag_Phyco)) %>%
    mutate(EXOChla_RFU_1.5 = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                  abs(EXOChla_RFU_1.5 - Chla_RFU_1.5_mean) > Chla_RFU_1.5_threshold, NA, EXOChla_RFU_1.5)) %>%
    mutate(EXOChla_ugL_1.5 = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                  abs(EXOChla_ugL_1.5 - Chla_ugL_1.5_mean) > Chla_ugL_1.5_threshold, NA, EXOChla_ugL_1.5)) %>%
    mutate(EXOBGAPC_RFU_1.5 = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                   abs(EXOBGAPC_RFU_1.5 - BGAPC_RFU_1.5_mean) > BGAPC_RFU_1.5_threshold, NA, EXOBGAPC_RFU_1.5)) %>%
    mutate(EXOBGAPC_ugL_1.5 = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                   abs(EXOBGAPC_ugL_1.5 - BGAPC_ugL_1.5_mean) > BGAPC_ugL_1.5_threshold, NA, EXOBGAPC_ugL_1.5))
  
  # flag EXO sonde sensor data of value above 4 * standard deviation at other times
  bvrdata <- bvrdata %>%
    mutate(Flag_Phyco = ifelse(! is.na(EXOBGAPC_RFU_1.5) & abs(EXOBGAPC_RFU_1.5 - BGAPC_RFU_1.5_mean) > BGAPC_RFU_1.5_threshold |
                               ! is.na(EXOBGAPC_ugL_1.5) & abs(EXOBGAPC_ugL_1.5 - BGAPC_ugL_1.5_mean) > BGAPC_ugL_1.5_threshold,
                               5, Flag_Phyco))
  
  # delete EXO_Date and EXO_Time columns
  bvrdata <- bvrdata %>% select(-EXO_Date, -EXO_Time)
  
  # add Reservoir and Site columns
  bvrdata$Reservoir <- "BVR"
  bvrdata$Site <- "50"
  
  # reorder columns
  bvrdata <- bvrdata %>% select(Reservoir, Site, -RECORD, -CR6_Batt_V, -CR6Panel_Temp_C, -Flag_All, -Flag_DO_1.5, -Flag_DO_7.5,
                                -Flag_DO_0.5, -Flag_Chla, -Flag_Phyco, -Flag_TDS, everything())
  
  # replace NaNs with NAs
  bvrdata[is.na(bvrdata)] <- NA
  
  # convert datetimes to characters so that they are properly formatted in the output file
  bvrdata$DateTime <- as.character(catdata$DateTime)
  
  # write to output file
  write_csv(bvrdata, output_file)
}

# example usage
qaqc(obs1,
      "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data/BVR_maintenance_log",
     "BVRplatform.csv")
