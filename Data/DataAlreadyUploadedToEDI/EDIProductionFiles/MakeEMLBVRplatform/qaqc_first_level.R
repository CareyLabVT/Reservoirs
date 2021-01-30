
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
#download.file('https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data/BVR_maintenance_log.txt', "maintenance_file.csv")

#Read in csv of data from pushed to github
#bvrheader1<-read.csv("BVRplatform.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
bvrdata1<-read.csv("BVRplatform.csv", skip=4, header=F) #get data minus wonky Campbell rows
colnames(bvrdata1)=c('TIMESTAMP','RECORD','BattV','PTemp_C', 'wtr_1','wtr_2','wtr_3','wtr_4',	
                     'wtr_5','wtr_6','wtr_7','wtr_8','wtr_9','wtr_10','wtr_11','wtr_12',
                     'wtr_13','doobs_6','dosat_6','dotemp_6','doobs_13','dosat_13','dotemp_13',
                     'EXO_Date','EXO_Time','EXO_wtr_1','Cond_1','SpCond_1','TDS_1','dosat_1',
                     'doobs_1',	'Chla_RFU_1',	'Chla_1','BGAPC_RFU_1',	'BGAPC_1','fDOM_RFU_1',
                     'fDOM_QSU_1',	'EXO_pressure','EXO_depth','EXO_battery','EXO_cablepower',
                     'EXO_wiper','Lvl_psi','wtr_pt_13') #combine the names to deal with Campbell logger formatting
					

#Read in csv of manual uploaded data
bvrdata3<-read.csv("BVRmanualplatform.csv")

bvrdata2= bvrdata3%>%
  filter(!X==30772)%>% #delete pesky blank row. Make sure that it actually doesn't change
  select(!X)%>% #delete column that was added when uploaded
  rbind(.,bvrdata1)%>% #combine manual and most recent files
  distinct(TIMESTAMP, .keep_all= TRUE)%>% #taking out the duplicate values 
  filter(!TIMESTAMP=="")%>% #take out the rows with blank timestamps
  filter(TIMESTAMP< ymd_hms("2021-01-01 00:00:00", tz = "Etc/GMT+5")) #filter for 2020

bvrdata2$RECORD=as.numeric(bvrdata2$RECORD)  

#change the date from character to unknown making it easier to graph
bvrdata2$TIMESTAMP <- as.POSIXct(bvrdata2$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+5") 

#check record for gaps
#order data by timestamp
bvrdata2=bvrdata2[order(bvrdata2$TIMESTAMP),]
bvrdata2$DOY=yday(bvrdata2$TIMESTAMP)

#daily record gaps by day of year
for(i in 2:nrow(bvrdata2)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(bvrdata2$DOY[i]-bvrdata2$DOY[i-1]>1){
    print(c(bvrdata2$TIMESTAMP[i-1],bvrdata2$TIMESTAMP[i]))
  }
}
#sub-daily record gaps by record number
for(i in 2:length(bvrdata2$RECORD)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(abs(bvrdata2$RECORD[i]-bvrdata2$RECORD[i-1])>1){
    print(c(bvrdata2$TIMESTAMP[i-1],bvrdata2$TIMESTAMP[i]))
  }
}

# convert datetimes to characters so that they are properly formatted in the output file
#bvrdata$DateTime <- as.character(bvrdata$DateTime)
#remove the DOY column and change to bvrdata
bvrdata2=select(bvrdata2, -DOY)

setwd("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLBVRplatform")
write.csv(bvrdata2, 'bvrdata2020_comb.csv')

qaqc <- function(data_file,maintenance_file,  output_file)
{

#bvrdata=data_file
#change column names
  BVRDATA_COL_NAMES = c("X","DateTime", "RECORD", "CR6_Batt_V", "CR6Panel_Temp_C", "ThermistorTemp_C_1",
                        "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4", "ThermistorTemp_C_5",
                        "ThermistorTemp_C_6", "ThermistorTemp_C_7", "ThermistorTemp_C_8", "ThermistorTemp_C_9",
                        "ThermistorTemp_C_10","ThermistorTemp_C_11","ThermistorTemp_C_12","ThermistorTemp_C_13",
                        "RDO_mgL_6", "RDOsat_percent_6", "RDOTemp_C_6", "RDO_mgL_13",
                        "RDOsat_percent_13", "RDOTemp_C_13", "EXO_Date", "EXO_Time", "EXOTemp_C_1.5", "EXOCond_uScm_1.5",
                        "EXOSpCond_uScm_1.5", "EXOTDS_mgL_1.5", "EXODOsat_percent_1.5", "EXODO_mgL_1.5", "EXOChla_RFU_1.5",
                        "EXOChla_ugL_1.5", "EXOBGAPC_RFU_1.5", "EXOBGAPC_ugL_1.5", "EXOfDOM_RFU_1.5", "EXOfDOM_QSU_1.5",
                        "EXO_pressure_1.5", "EXO_depth", "EXO_battery", "EXO_cablepower", "EXO_wiper", "Lvl_psi_13", "LvlTemp_C_13")
  
 
  # after maintenance, DO values will continue to be replaced by NA until DO_mgL returns within this threshold (in mg/L)
  # of the pre-maintenance value
  DO_RECOVERY_THRESHOLD <- 1
  
  # columns where certain values are stored
  DO_MGL_COLS <- c(18, 21, 31)
  DO_SAT_COLS <- c(19, 22, 30)
  DO_FLAG_COLS <- c(46, 47, 48)
  
  # depths at which DO is measured
  #what do I say for DO depths
  DO_DEPTHS <- c(1.5, 7.5, 0.5)
  
  # EXO sonde sensor data that differs from the mean by more than the standard deviation multiplied by this factor will
  # either be replaced with NA and flagged (if between 2018-10-01 and 2019-03-01) or just flagged (otherwise)
  EXO_FOULING_FACTOR <- 4
  
  #read in data from obs1 above
  
  # read catwalk data and maintenance log
  # NOTE: date-times throughout this script are processed as UTC
  bvrdata <- read_csv(data_file, skip=1, col_names = BVRDATA_COL_NAMES,
                      col_types = cols(.default = col_double(), DateTime = col_datetime()))
 bvrdata$X<-NULL
#read in maintenance log
  log <- read_csv(maintenance_file,
    col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  ))
 
  # add flag columns
  bvrdata$Flag_All <- 0
  bvrdata$Flag_DO_1.5 <- 0
  bvrdata$Flag_DO_6 <- 0
  bvrdata$Flag_DO_13 <- 0
  bvrdata$Flag_Chla <- 0
  bvrdata$Flag_Phyco <- 0
  bvrdata$Flag_TDS <- 0
  bvrdata$Flag_fDOM <- 0
  bvrdata$Flag_Cond <-0
  bvrdata$Flag_Lvl <-0
  bvrdata$Flag_Temp_1 <-0
  bvrdata$Flag_Temp_2 <-0
  bvrdata$Flag_Temp_3 <-0
  bvrdata$Flag_Temp_4 <-0
  bvrdata$Flag_Temp_5 <-0
  bvrdata$Flag_Temp_6 <-0
  bvrdata$Flag_Temp_7 <-0
  bvrdata$Flag_Temp_8 <-0
  bvrdata$Flag_Temp_9 <-0
  bvrdata$Flag_Temp_10 <-0
  bvrdata$Flag_Temp_11 <-0
  bvrdata$Flag_Temp_12 <-0
  bvrdata$Flag_Temp_13 <-0
  
  
  # replace negative DO values with 0
  bvrdata <- bvrdata %>%
    mutate(Flag_DO_1.5 = ifelse((! is.na(EXODO_mgL_1.5) & EXODO_mgL_1.5 < 0)
                            | (! is.na(EXODOsat_percent_1.5) & EXODOsat_percent_1.5 < 0), 3, Flag_DO_1.5)) %>%
    mutate(EXODO_mgL_1.5 = ifelse(EXODO_mgL_1.5 < 0, 0, EXODO_mgL_1.5)) %>%
    mutate(EXODOsat_percent_1.5 = ifelse(EXODOsat_percent_1.5 <0, 0, EXODOsat_percent_1.5))
  
  bvrdata <- bvrdata %>%
    mutate(Flag_DO_6 = ifelse((! is.na(RDO_mgL_6) & RDO_mgL_6 < 0)
                            | (! is.na(RDOsat_percent_6) & RDOsat_percent_6 < 0), 3, Flag_DO_6)) %>%
    mutate(RDO_mgL_6 = ifelse(RDO_mgL_6 < 0, 0, RDO_mgL_6)) %>%
    mutate(RDOsat_percent_6 = ifelse(RDOsat_percent_6 < 0, 0, RDOsat_percent_6))

  bvrdata <- bvrdata %>%
    mutate(Flag_DO_13 = ifelse((! is.na(RDO_mgL_13) & RDO_mgL_13 < 0)
                            | (! is.na(RDOsat_percent_13) & RDOsat_percent_13 < 0), 3, Flag_DO_13)) %>%
    mutate(RDO_mgL_13 = ifelse(RDO_mgL_13 < 0, 0, RDO_mgL_13)) %>%
    mutate(RDOsat_percent_13 = ifelse(RDOsat_percent_13 < 0, 0, RDOsat_percent_13))
  
  
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
          bvrdata[bvrdata$DateTime > end & bvrdata$DateTime < DO_recovery_time, DO_FLAG_COLS[j]] <- 1
        }
      }
    }
  }
  
  # find EXO sonde sensor data that differs from the mean by more than the standard deviation times a given factor, and
  
  # chl and phyco qaqc ----
  # perform qaqc on the entire dataset for chl and phyco
  
  # assign standard deviation thresholds
  sd_4 <- 4*sd(bvrdata$EXOChla_ugL_1.5, na.rm = TRUE)
  threshold <- sd_4
  sd_4_phyco <- 4*sd(bvrdata$EXOBGAPC_ugL_1.5, na.rm = TRUE)
  threshold_phyco <- sd_4_phyco 
  
  # QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
  bvrdata <- bvrdata %>% 
    mutate(Chla = lag(EXOChla_ugL_1.5, 0),
           Chla_lag1.5 = lag(EXOChla_ugL_1.5, 1),
           Chla_lead1.5 = lead(EXOChla_ugL_1.5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_Chla = ifelse(Chla < 0 & !is.na(Chla), 3, Flag_Chla)) %>% 
    mutate(Flag_Chla = ifelse(Chla < 0 & !is.na(Chla), 3, Flag_Chla)) %>% 
    mutate(EXOChla_ugL_1.5 = ifelse(Chla < 0 & !is.na(Chla), 0, EXOChla_ugL_1.5)) %>% 
    mutate(EXOChla_RFU_1.5 = ifelse(Chla < 0 & !is.na(Chla), 0, EXOChla_RFU_1.5)) %>% 
    mutate(EXOChla_ugL_1.5 = ifelse((abs(Chla_lag1.5 - Chla) > (threshold))  & (abs(Chla_lead1.5 - Chla) > (threshold) & !is.na(Chla)), 
                                    NA, EXOChla_ugL_1.5)) %>%   
    mutate(EXOChla_RFU_1.5 = ifelse((abs(Chla_lag1.5 - Chla) > (threshold))  & (abs(Chla_lead1.5 - Chla) > (threshold) & !is.na(Chla)), 
                                    NA, EXOChla_RFU_1.5)) %>% 
    mutate(Flag_Chla = ifelse((abs(Chla_lag1.5 - Chla) > (threshold))  & (abs(Chla_lead1.5 - Chla) > (threshold)) & !is.na(Chla), 
                              2, Flag_Chla)) %>% 
    select(-Chla, -Chla_lag1.5, -Chla_lead1.5)
  
  # QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
  bvrdata <- bvrdata %>% 
    mutate(phyco = lag(EXOBGAPC_ugL_1.5, 0),
           phyco_lag1.5 = lag(EXOBGAPC_ugL_1.5, 1),
           phyco_lead1.5 = lead(EXOBGAPC_ugL_1.5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_Phyco = ifelse(phyco < 0 & !is.na(phyco), 3, Flag_Phyco)) %>% 
    mutate(Flag_Phyco = ifelse(phyco < 0 & !is.na(phyco), 3, Flag_Phyco)) %>% 
    mutate(EXOBGAPC_RFU_1.5 = ifelse(phyco < 0 & !is.na(phyco), 0, EXOBGAPC_RFU_1.5)) %>% 
    mutate(EXOBGAPC_ugL_1.5 = ifelse(phyco < 0 & !is.na(phyco), 0, EXOBGAPC_ugL_1.5)) %>% 
    mutate(EXOBGAPC_ugL_1.5 = ifelse((abs(phyco_lag1.5 - phyco) > (threshold_phyco))  & (abs(phyco_lead1.5 - phyco) > (threshold_phyco) & !is.na(phyco)), 
                                     NA, EXOBGAPC_ugL_1.5)) %>%   
    mutate(EXOBGAPC_RFU_1.5 = ifelse((abs(phyco_lag1.5 - phyco) > (threshold_phyco))  & (abs(phyco_lead1.5 - phyco) > (threshold_phyco) & !is.na(phyco)), 
                                     NA, EXOBGAPC_RFU_1.5)) %>% 
    mutate(Flag_Phyco = ifelse((abs(phyco_lag1.5 - phyco) > (threshold_phyco))  & (abs(phyco_lead1.5 - phyco) > (threshold_phyco) & !is.na(phyco)), 
                               2, Flag_Phyco)) %>%
    select(-phyco, -phyco_lag1.5, -phyco_lead1.5)
  
  #deteriming the standard deviation of fDOM data 
  sd_fDOM <- sd(bvrdata$EXOfDOM_QSU_1.5, na.rm = TRUE) 
  bvrdata <- bvrdata %>% 
    mutate(fDOM = lag(EXOfDOM_QSU_1.5, 0),
           fDOM_lag1.5 = lag(EXOfDOM_QSU_1.5, 1),
           fDOM_lead1.5 = lead(EXOfDOM_QSU_1.5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_fDOM = ifelse(fDOM < 0 & !is.na(fDOM), 3, Flag_fDOM),
           EXOfDOM_QSU_1.5 = ifelse(fDOM < 0 & !is.na(fDOM), NA, EXOfDOM_QSU_1.5),
           EXOfDOM_RFU_1.5 = ifelse(fDOM < 0 & !is.na(fDOM), NA, EXOfDOM_RFU_1.5),
           Flag_fDOM = ifelse(fDOM < 0, 2, Flag_fDOM)   ) %>% #These mutates are QAQCing for negative fDOM QSU values and setting these to NA and making a flag for these. This was done outside of the 2 sd deviation rule because there were two negative points in a row and one was not removed with the follwoing if else statements. 
    mutate(EXOfDOM_QSU_1.5 = ifelse(
      ( abs(fDOM_lag1.5 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1.5 - fDOM) > (2*sd_fDOM)  & !is.na(fDOM) ), NA, EXOfDOM_QSU_1.5
    )) %>%  #QAQC to remove outliers for QSU fDOM data 
    mutate(EXOfDOM_RFU_1.5 = ifelse(
      ( abs(fDOM_lag1.5 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1.5 - fDOM) > (2*sd_fDOM)  & !is.na(fDOM)  ), NA, EXOfDOM_RFU_1.5
    )) %>% #QAQC to remove outliers for RFU fDOM data
    mutate(Flag_fDOM = ifelse(
      ( abs(fDOM_lag1.5 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1.5 - fDOM) > (2*sd_fDOM)  & !is.na(fDOM)  ), 2, Flag_fDOM
    ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
    select(-fDOM, -fDOM_lag1.5, -fDOM_lead1.5)  #This removes the columns used to run ifelse statements since they are no longer needed. 
  
  #create depth column
  bvrdata=bvrdata%>%
    mutate(Depth_m_13=Lvl_psi_13*0.70455)#1psi=2.31ft, 1ft=0.305m
  
  # delete EXO_Date and EXO_Time columns
  bvrdata <- bvrdata %>% select(-EXO_Date, -EXO_Time)
  
  # add Reservoir and Site columns
  bvrdata$Reservoir <- "BVR"
  bvrdata$Site <- "50"
  
  # reorder columns
  bvrdata <- bvrdata %>% select(Reservoir, Site, -RECORD, -CR6_Batt_V, -CR6Panel_Temp_C, -Flag_All, -Flag_DO_1.5, -Flag_DO_6,
                                -Flag_DO_13, -Flag_Chla, -Flag_Phyco, -Flag_TDS, everything())
  
  # replace NaNs with NAs
  bvrdata[is.na(bvrdata)] <- NA
  
  # convert datetimes to characters so that they are properly formatted in the output file
  bvrdata$DateTime <- as.character(bvrdata$DateTime)
 
 
  # write to output file
  write_csv(bvrdata, output_file)
}

# example usage
#qaqc(obs1,
#      "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data/BVR_maintenance_log",
#     "BVRplatform.csv")

qaqc('bvrdata2020_comb.csv',
      "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data/BVR_maintenance_log.txt",
       "BVRplatform_clean.csv")
