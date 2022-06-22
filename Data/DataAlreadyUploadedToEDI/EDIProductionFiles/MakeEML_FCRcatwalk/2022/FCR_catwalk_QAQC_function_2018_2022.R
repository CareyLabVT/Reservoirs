

qaqc <- function(data_file, data2_file, maintenance_file, output_file)
{
 
  CATPRES_COL_NAMES = c("DateTime", "RECORD", "CR6_Batt_V", "CR6Panel_Temp_C", "ThermistorTemp_C_surface",
                        "ThermistorTemp_C_1", "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4",
                        "ThermistorTemp_C_5", "ThermistorTemp_C_6", "ThermistorTemp_C_7", "ThermistorTemp_C_8",
                        "ThermistorTemp_C_9", "RDO_mgL_5", "RDOsat_percent_5", "RDOTemp_C_5", "RDO_mgL_9",
                        "RDOsat_percent_9", "RDOTemp_C_9", "EXO_Date", "EXO_Time", "EXOTemp_C_1", "EXOCond_uScm_1",
                        "EXOSpCond_uScm_1", "EXOTDS_mgL_1", "EXODOsat_percent_1", "EXODO_mgL_1", "EXOChla_RFU_1",
                        "EXOChla_ugL_1", "EXOBGAPC_RFU_1", "EXOBGAPC_ugL_1", "EXOfDOM_RFU_1", "EXOfDOM_QSU_1","EXOTurbidity_FNU_1",
                        "EXO_pressure_psi", "EXO_depth_m", "EXO_battery_V", "EXO_cablepower_V", "EXO_wiper_V","Lvl_psi_9", "LvlTemp_C_9")
  
  # EXO sonde sensor data that differs from the mean by more than the standard deviation multiplied by this factor will
  # either be replaced with NA and flagged (if between 2018-10-01 and 2019-03-01) or just flagged (otherwise)
  EXO_FOULING_FACTOR <- 4
  #EXO_FOULING_FACTORfor2 <- 2
  
  #Adjustment period of time to stabilization after cleaning in seconds
  ADJ_PERIOD = 2*60*60 
  
  # read catwalk data and maintenance log
  # NOTE: date-times throughout this script are processed as UTC
  catdata <- read_csv(data_file, skip = 1, col_names = CATPRES_COL_NAMES,
                      col_types = cols(.default = col_double(), DateTime = col_datetime()))
  
 #read in manual data from the data logger to fill in missing gaps
  
  catdata2 <- read_csv(data2_file, skip = 1, col_names = CATPRES_COL_NAMES,
                       col_types = cols(.default = col_double(), DateTime = col_datetime()))
  
  
  catdata <-rbind(catdata,catdata2)
  
  #get rid of duplicates
  catdata=catdata[!duplicated(catdata$DateTime), ]
  
  #reorder 
  catdata=catdata[order(catdata$DateTime),]
  
####################################################################################################################################
#Fix the timezone issues  
  #time was changed from GMT-4 to GMT-5 on 15 APR 19 at 10:00
  #have to seperate data frame by year and record because when the time was changed 10:00-10:40 were recorded twice
  #once before the time change and once after so have to seperate and assign the right time. 
  before=catdata%>%
    filter(DateTime<"2019-04-15 6:50")%>%
    filter(DateTime<"2019-04-15 6:50" & RECORD < 32879)#Don't know how to change timezones so just subtract 4 from the time we want
    
  
  #now put into GMT-5 from GMT-4
  before$DateTime<-as.POSIXct(strptime(before$DateTime, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5") #get dates aligned
  before$DateTime<-with_tz(force_tz(before$DateTime,"Etc/GMT+4"), "Etc/GMT+5") #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set
  
  
  #filter after the time change 
  after=catdata%>%
    filter(DateTime>"2019-04-15 05:50")%>%
    slice(-c(1,3,5,7,9))
    
  
  after$DateTime<-as.POSIXct(strptime(after$DateTime, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5")#change or else after is off by an hour
  
 
  
  #merge before and after so they are one dataframe in GMT-5
  
  catdata=rbind(before, after)
  
  catdata=catdata[!duplicated(catdata$DateTime), ]
  
  catdata$DateTime<-as.POSIXct(strptime(catdata$DateTime, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
###############################################################################################################################
  #check for gaps and missing data
  #order data by timestamp
  catdata2=catdata
  catdata2=catdata2[order(catdata2$DateTime),]
  catdata2$DOY=yday(catdata2$DateTime)
  
  
  #check record for gaps
  #daily record gaps by day of year
  for(i in 2:nrow(catdata2)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
    if(catdata2$DOY[i]-catdata2$DOY[i-1]>1){
      print(c(catdata2$DateTime[i-1],catdata2$DateTime[i]))
    }
  }
  cat2=catdata2%>%filter(!is.na(RECORD))
  for(i in 2:length(cat2$RECORD)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
    if(abs(cat2$RECORD[i]-cat2$RECORD[i-1])>1){
      print(c(cat2$DateTime[i-1],cat2$DateTime[i]))
    }
  }
############################################################################################################################### 

  # remove NaN data at beginning when data when no sensors were connected to the data logger
  catdata <- catdata %>% filter(DateTime >= ymd_hms("2018-07-05 13:50:00"))
  
  # for loop to create flag columns
  for(j in c(5:20,23:42)) { #for loop to create new columns in data frame
    catdata[,paste0("Flag_",colnames(catdata[j]))] <- 0 #creates flag column + name of variable
    catdata[c(which(is.na(catdata[,j]))),paste0("Flag_",colnames(catdata[j]))] <-7 #puts in flag 7 if value not collected
  }
  
  for(k in c(15:16,18:19,24:35)) { #for loop to create new columns in data frame
    catdata[c(which((catdata[,k]<0))),paste0("Flag_",colnames(catdata[k]))] <- 3
    catdata[c(which((catdata[,k]<0))),k] <- 0 #replaces value with 0
  }
  
  #add in the adjusted DO columns for used below as dates are in the maintenance log
  catdata$RDO_mgL_5_adjusted <-0
  catdata$RDOsat_percent_5_adjusted <-0
  catdata$RDO_mgL_9_adjusted <-0
  catdata$RDOsat_percent_9_adjusted <-0
##############################################################################################################  

  
  #Read in the maintneance log 
  
  log <- read_csv(maintenance_file, col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  )) 
 
  # modify catdata based on the information in the log   
  
  for(i in 1:nrow(log))
  {
    # get start and end time of one maintenance event
    start <- log$TIMESTAMP_start[i]
    end <- log$TIMESTAMP_end[i]
    
    
    # get indices of columns affected by maintenance
    if(grepl("^\\d+$", log$colnumber[i])) # single num
    {
      maintenance_cols <- intersect(c(2:42), as.integer(log$colnumber[i]))
    }
    
    else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", log$colnumber[i])) # c(x;y;...)
    {
      maintenance_cols <- intersect(c(2:42), as.integer(unlist(regmatches(log$colnumber[i],
                                                                          gregexpr("\\d+", log$colnumber[i])))))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", log$colnumber[i])) # c(x:y)
    {
      bounds <- as.integer(unlist(regmatches(log$colnumber[i], gregexpr("\\d+", log$colnumber[i]))))
      maintenance_cols <- intersect(c(2:42), c(bounds[1]:bounds[2]))
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
    maintenance_cols <- setdiff(maintenance_cols, c(21, 22))
    
    if(length(maintenance_cols) == 0)
    {
      warning(paste("Did not parse any valid data columns in row", i, "of the maintenance log. Valid columns have",
                    "indices 2 through 41, excluding 21 and 22, which are deleted by this script. Skipping maintenance for that row."))
      next
    }
    
    #index the Flag columns
    if(grepl("^\\d+$", log$flagcol[i])) # single num
    {
      flag_cols <- intersect(c(43:78), as.integer(log$flagcol[i]))
      
    }
    else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", log$flagcol[i])) # c(x;y;...)
    {
      flag_cols <- intersect(c(43:78), as.integer(unlist(regmatches(log$flagcol[i],
                                                                    gregexpr("\\d+", log$flagcol[i])))))
    }
    
    else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", log$flagcol[i])) # c(x:y)
    {
      bounds_flag <- as.integer(unlist(regmatches(log$flagcol[i], gregexpr("\\d+", log$flagcol[i]))))
      flag_cols <- intersect(c(43:78), c(bounds_flag[1]:bounds_flag[2]))
    }
    else
    {
      warning(paste("Could not parse column flagcol in row", i, "of the maintenance log. Skipping maintenance for",
                    "that row. The value of colnumber should be in one of three formats: a single number (\"47\"), a",
                    "semicolon-separated list of numbers in c() (\"c(47;48;49)\"), or a range of numbers in c() (\"c(47:74)\").",
                    "Other values (even valid calls to c()) will not be parsed properly."))
      next
    }
    
    #Get the Maintenance Flag 
    
    flag <- log$flag[i]
    
    # replace relevant data with NAs and set flags while maintenance was in effect
    if(flag==5)
      {
      catdata[catdata$DateTime >= start & catdata$DateTime <= end, flag_cols] <- flag
      }
    else if(flag==8 && maintenance_cols==6)
      {
      catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] <- catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols]-0.22617
      catdata[catdata$DateTime >= start & catdata$DateTime <= end, flag_cols] <- flag
      }
    else if (flag==8 && maintenance_cols==9)
      {
      catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] <- catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols]-0.18122 
      catdata[catdata$DateTime >= start & catdata$DateTime <= end, flag_cols] <- flag
      }
    else if (flag==6 && maintenance_cols==15) #adjusting the RDO_5_mgL
      {
      dt=catdata[catdata$DateTime >= start & catdata$DateTime <= end, 1]
      
      catdata[catdata$DateTime >= start & catdata$DateTime <= end, 79] = catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] + 
        sqrt(as.numeric(difftime(dt$DateTime, start, units = "mins")))/30
      
      catdata[catdata$DateTime >= start & catdata$DateTime <= end, flag_cols] <- flag
      }
    else if (flag==6 && maintenance_cols==16) #adjusting the RDO_5_sat
      {
        dt=catdata[catdata$DateTime >= start & catdata$DateTime <= end, 1]
        
        catdata[catdata$DateTime >= start & catdata$DateTime <= end, 80] = catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] + 
          sqrt(as.numeric(difftime(dt$DateTime, start, units = "mins")))/30/11.3*100
        
        catdata[catdata$DateTime >= start & catdata$DateTime <= end, flag_cols] <- flag
      }
    else if (flag==6 && maintenance_cols==18) #adjusting the RDO_9_mgl
      {
    dt=catdata[catdata$DateTime >= start & catdata$DateTime <= end, 1]
    
      if(start=="2020-08-19 16:00:00" || start=="2020-08-11 03:00:00") #figure out how to get this to work
      {
        catdata[catdata$DateTime >= start & catdata$DateTime <= end, 81] = catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] + 
        sqrt(as.numeric(difftime(dt$DateTime, start, units = "mins")))/6500
        print(paste("Row",i,"is in the maintenance log for 2020-08-19 20:00:00 or 2020-08-11 07:00:00"))
      }
      else if (start=="2020-08-26 08:00:00")
      {
        catdata[catdata$DateTime >= start & catdata$DateTime <= end, 81] = catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] + 
        sqrt(as.numeric(difftime(dt$DateTime, start, units = "mins")))/10000
        print(paste("Row",i,"is in the maintenance log for 2020-08-26 12:00:00"))
      }
      else if (start=="2020-09-05 02:00:00")
      {
        catdata[catdata$DateTime >= start & catdata$DateTime <= end, 81] = catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] + 
          sqrt(as.numeric(difftime(dt$DateTime, start, units = "mins")))/3000
        print(paste("Row",i,"is in the maintenance log for 2020-09-05 06:00:00"))
      }
      else{
        warning(paste("Make sure row",i,"is in the maintenance log"))
      }
    catdata[catdata$DateTime >= start & catdata$DateTime <= end, flag_cols] <- flag
    }
    else if (flag==6 && maintenance_cols==19) #adjusting the RDO_9_sat
      {
    dt=catdata[catdata$DateTime >= start & catdata$DateTime <= end, 1]
    
        if(start=="2020-08-19 16:00:00" || start=="2020-08-11 03:00:00")
        {
        catdata[catdata$DateTime >= start & catdata$DateTime <= end, 82] = catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] + 
        sqrt(as.numeric(difftime(dt$DateTime, start, units = "mins")))/6500/11.3*100
        print(paste("Row",i,"is in the maintenance log for 2020-08-19 20:00:00 or 2020-08-11 07:00:00"))
        }
        else if (start=="2020-08-26 08:00:00")
        {
        catdata[catdata$DateTime >= start & catdata$DateTime <= end, 82] = catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] + 
        sqrt(as.numeric(difftime(dt$DateTime, start, units = "mins")))/10000/11.3*100
        print(paste("Row",i,"is in the maintenance log for 2020-08-26 12:00:00"))
        }
        else if (start=="2020-09-05 02:00:00") 
        {
        catdata[catdata$DateTime >= start & catdata$DateTime <= end, 82] = catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] + 
        sqrt(as.numeric(difftime(dt$DateTime, start, units = "mins")))/3000/11.3*100
        print(paste("Row",i,"is in the maintenance log for 2020-09-05 06:00:00"))
        }
      else{
      warning(paste("Make sure row",i,"is in the maintenance log"))
        }
      catdata[catdata$DateTime >= start & catdata$DateTime <= end, flag_cols] <- flag
        }
    else 
      {
      catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] <- NA
      catdata[catdata$DateTime >= start & catdata$DateTime <= end, flag_cols] <- flag
      }
    #Add the 2 hour adjustment for DO 
    if (log$colnumber[i]=="c(1:42)" && flag==1){
      DO_col=c("RDO_mgL_5", "RDOsat_percent_5", "RDO_mgL_9","RDOsat_percent_9","EXODOsat_percent_1", "EXODO_mgL_1")
      DO_flag_col=c("Flag_RDO_mgL_5", "Flag_RDOsat_percent_5", "Flag_RDO_mgL_9","Flag_RDOsat_percent_9","Flag_EXODOsat_percent_1", "Flag_EXODO_mgL_1")
      catdata[catdata$DateTime>start&catdata$DateTime<end+ADJ_PERIOD,DO_col] <- NA
      catdata[catdata$DateTime>start&catdata$DateTime<end+ADJ_PERIOD,DO_flag_col] <- flag
    }
    else if(log$colnumber[i] %in% c("15","16") && flag==1){
      DO_col=c("RDO_mgL_5", "RDOsat_percent_5")
      DO_flag_col=c("Flag_RDO_mgL_5", "Flag_RDOsat_percent_5")
      catdata[catdata$DateTime>start&catdata$DateTime<end+ADJ_PERIOD,DO_col] <- NA
      catdata[catdata$DateTime>start&catdata$DateTime<end+ADJ_PERIOD,DO_flag_col] <- flag
    }
    else if(log$colnumber[i] %in% c("18","19") && flag==1){
      DO_col=c("RDO_mgL_9","RDOsat_percent_9")
      DO_flag_col=c("Flag_RDO_mgL_9","Flag_RDOsat_percent_9")
      catdata[catdata$DateTime>start&catdata$DateTime<end+ADJ_PERIOD,DO_col] <- NA
      catdata[catdata$DateTime>start&catdata$DateTime<end+ADJ_PERIOD,DO_flag_col] <- flag
    }
    else if (log$colnumber[i] %in% c("c(21:40","27","28") && flag==1){
      DO_col=c("EXODOsat_percent_1", "EXODO_mgL_1")
      DO_flag_col=c("Flag_EXODOsat_percent_1", "Flag_EXODO_mgL_1")
      catdata[catdata$DateTime>start&catdata$DateTime<end+ADJ_PERIOD,DO_col] <- NA
      catdata[catdata$DateTime>start&catdata$DateTime<end+ADJ_PERIOD,DO_flag_col] <-1
    }
    else{
      warning(paste("No DO time to adjust in row",i,"."))
    }
  }
########################################################################################################################
# Fill in adjusted DO values that didn't get changed. If the value didn't get changed then it is the same as values from the 
# non adjusted columns
  
  catdata=catdata%>%
    mutate(
      RDO_mgL_5_adjusted=ifelse(RDO_mgL_5_adjusted==0, RDO_mgL_5, RDO_mgL_5_adjusted),
      RDOsat_percent_5_adjusted=ifelse(RDOsat_percent_5_adjusted==0, RDOsat_percent_5, RDOsat_percent_5_adjusted),
      RDO_mgL_9_adjusted=ifelse(RDO_mgL_9_adjusted==0, RDO_mgL_9, RDO_mgL_9_adjusted),
      RDOsat_percent_9_adjusted=ifelse(RDOsat_percent_9_adjusted==0, RDOsat_percent_9, RDOsat_percent_9_adjusted)
    )

DO=catdata%>%
  select(DateTime, RDO_mgL_5,RDO_mgL_5_adjusted, RDO_mgL_9, RDO_mgL_9_adjusted, Flag_RDO_mgL_5, Flag_RDO_mgL_9)
  
  
########################################################################################################################  
  # find chla and phyo on the EXO sonde sensor data that differs from the mean by more than the standard deviation times a given factor, and
  # replace with NAs between October 2018 and March 2019, due to sensor fouling
  Chla_RFU_1_mean <- mean(catdata$EXOChla_RFU_1, na.rm = TRUE)
  Chla_ugL_1_mean <- mean(catdata$EXOChla_ugL_1, na.rm = TRUE)
  BGAPC_RFU_1_mean <- mean(catdata$EXOBGAPC_RFU_1, na.rm = TRUE)
  BGAPC_ugL_1_mean <- mean(catdata$EXOBGAPC_ugL_1, na.rm = TRUE)
  Chla_RFU_1_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOChla_RFU_1, na.rm = TRUE)
  Chla_ugL_1_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOChla_ugL_1, na.rm = TRUE)
  BGAPC_RFU_1_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOBGAPC_RFU_1, na.rm = TRUE)
  BGAPC_ugL_1_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOBGAPC_ugL_1, na.rm = TRUE)
  
  
  # QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
  catdata <- catdata %>% 
    mutate(Chla_ugL = lag(EXOChla_ugL_1, 0),
           Chla_ugL_lag1 = lag(EXOChla_ugL_1, 1),
           Chla_ugL_lead1 = lead(EXOChla_ugL_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(EXOChla_ugL_1 = ifelse((abs(Chla_ugL_lag1 - Chla_ugL) > (Chla_ugL_1_threshold))  & (abs(Chla_ugL_lead1 - Chla_ugL) > (Chla_ugL_1_threshold) & !is.na(Chla_ugL)), 
                                  NA, EXOChla_ugL_1)) %>%   
    mutate(Flag_EXOChla_ugL_1 = ifelse((abs(Chla_ugL_lag1 - Chla_ugL) > (Chla_ugL_1_threshold))  & (abs(Chla_ugL_lead1 - Chla_ugL) > (Chla_ugL_1_threshold)) & !is.na(Chla_ugL), 
                              2, Flag_EXOChla_ugL_1)) %>%
    mutate(Flag_EXOChla_ugL_1 = ifelse(is.na(Flag_EXOChla_ugL_1), 2, Flag_EXOChla_ugL_1))%>%
    select(-Chla_ugL, -Chla_ugL_lag1, -Chla_ugL_lead1)
  
  #Chla_RFU QAQC
  catdata <- catdata %>% 
    mutate(Chla_RFU = lag(EXOChla_RFU_1, 0),
           Chla_RFU_lag1 = lag(EXOChla_RFU_1, 1),
           Chla_RFU_lead1 = lead(EXOChla_RFU_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(EXOChla_RFU_1 = ifelse((abs(Chla_RFU_lag1 - Chla_RFU) > (Chla_RFU_1_threshold))  & (abs(Chla_RFU_lead1 - Chla_RFU) > (Chla_RFU_1_threshold) & !is.na(Chla_RFU)), 
                                  NA, EXOChla_RFU_1)) %>%   
    mutate(Flag_EXOChla_RFU_1 = ifelse((abs(Chla_RFU_lag1 - Chla_RFU) > (Chla_RFU_1_threshold))  & (abs(Chla_RFU_lead1 - Chla_RFU) > (Chla_RFU_1_threshold)) & !is.na(Chla_RFU), 
                                  2, Flag_EXOChla_RFU_1)) %>%
    mutate(Flag_EXOChla_RFU_1 = ifelse(is.na(Flag_EXOChla_RFU_1), 2, Flag_EXOChla_RFU_1))%>%
    select(-Chla_RFU, -Chla_RFU_lag1, -Chla_RFU_lead1)
  # QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
  catdata <- catdata %>% 
    mutate(phyco_ugL = lag(EXOBGAPC_ugL_1, 0),
           phyco_ugL_lag1 = lag(EXOBGAPC_ugL_1, 1),
           phyco_ugL_lead1 = lead(EXOBGAPC_ugL_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOBGAPC_ugL_1 = ifelse(phyco_ugL < 0 & !is.na(phyco_ugL), 3, Flag_EXOBGAPC_ugL_1)) %>% 
    mutate(EXOBGAPC_ugL_1 = ifelse(phyco_ugL < 0 & !is.na(phyco_ugL), 0, EXOBGAPC_ugL_1)) %>% 
    mutate(EXOBGAPC_ugL_1 = ifelse((abs(phyco_ugL_lag1 - phyco_ugL) > (BGAPC_ugL_1_threshold))  & (abs(phyco_ugL_lead1 - phyco_ugL) > (BGAPC_ugL_1_threshold) & !is.na(phyco_ugL)), 
                                   NA, EXOBGAPC_ugL_1)) %>%   
    mutate(Flag_EXOBGAPC_ugL_1 = ifelse((abs(phyco_ugL_lag1 - phyco_ugL) > (BGAPC_ugL_1_threshold))  & (abs(phyco_ugL_lead1 - phyco_ugL) > (BGAPC_ugL_1_threshold) & !is.na(phyco_ugL)), 
                               2, Flag_EXOBGAPC_ugL_1)) %>%
    mutate(Flag_EXOBGAPC_ugL_1 = ifelse(is.na(Flag_EXOBGAPC_ugL_1),2,Flag_EXOBGAPC_ugL_1))%>%
    select(-phyco_ugL, -phyco_ugL_lag1, -phyco_ugL_lead1)

#Phyco QAQC for RFU
    catdata <- catdata %>% 
      mutate(phyco_RFU = lag(EXOBGAPC_RFU_1, 0),
             phyco_RFU_lag1 = lag(EXOBGAPC_RFU_1, 1),
             phyco_RFU_lead1 = lead(EXOBGAPC_RFU_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
      mutate(EXOBGAPC_RFU_1 = ifelse((abs(phyco_RFU_lag1 - phyco_RFU) > (BGAPC_RFU_1_threshold))  & (abs(phyco_RFU_lead1 - phyco_RFU) > (BGAPC_RFU_1_threshold) & !is.na(phyco_RFU)), 
                                     NA, EXOBGAPC_RFU_1)) %>%   
      mutate(Flag_EXOBGAPC_RFU_1 = ifelse((abs(phyco_RFU_lag1 - phyco_RFU) > (BGAPC_RFU_1_threshold))  & (abs(phyco_RFU_lead1 - phyco_RFU) > (BGAPC_RFU_1_threshold) & !is.na(phyco_RFU)), 
                                     2, Flag_EXOBGAPC_RFU_1)) %>%
      mutate(Flag_EXOBGAPC_RFU_1 = ifelse(is.na(Flag_EXOBGAPC_RFU_1),2,Flag_EXOBGAPC_RFU_1))%>%
    select(-phyco_RFU, -phyco_RFU_lag1, -phyco_RFU_lead1)
  
  #QAQC major outliers during the winter of 2018 going into 2019 due to fouling that did not get caught above. These points are removed
  catdata <- catdata %>%
    mutate(Flag_EXOChla_RFU_1 = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                (! is.na(EXOChla_RFU_1) & abs(EXOChla_RFU_1 - Chla_RFU_1_mean) > Chla_RFU_1_threshold),
                              4, Flag_EXOChla_RFU_1)) %>%
    mutate(Flag_EXOChla_ugL_1 = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                (! is.na(EXOChla_ugL_1) & abs(EXOChla_ugL_1 - Chla_ugL_1_mean) > Chla_ugL_1_threshold),
                              4, Flag_EXOChla_ugL_1)) %>%
    mutate(Flag_EXOBGAPC_RFU_1 = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                 (! is.na(EXOBGAPC_RFU_1) & abs(EXOBGAPC_RFU_1 - BGAPC_RFU_1_mean) > BGAPC_RFU_1_threshold),
                               4, Flag_EXOBGAPC_RFU_1)) %>%
    mutate(Flag_EXOBGAPC_ugL_1 = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                 (! is.na(EXOBGAPC_ugL_1) & abs(EXOBGAPC_ugL_1 - BGAPC_ugL_1_mean) > BGAPC_ugL_1_threshold),
                               4, Flag_EXOBGAPC_ugL_1)) %>%
    mutate(EXOChla_RFU_1 = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                    abs(EXOChla_RFU_1 - Chla_RFU_1_mean) > Chla_RFU_1_threshold, NA, EXOChla_RFU_1)) %>%
    mutate(EXOChla_ugL_1 = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                    abs(EXOChla_ugL_1 - Chla_ugL_1_mean) > Chla_ugL_1_threshold, NA, EXOChla_ugL_1)) %>%
    mutate(EXOBGAPC_RFU_1 = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                     abs(EXOBGAPC_RFU_1 - BGAPC_RFU_1_mean) > BGAPC_RFU_1_threshold, NA, EXOBGAPC_RFU_1)) %>%
    mutate(EXOBGAPC_ugL_1 = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                     abs(EXOBGAPC_ugL_1 - BGAPC_ugL_1_mean) > BGAPC_ugL_1_threshold, NA, EXOBGAPC_ugL_1))

  # flag EXO sonde sensor data of value above 4 * standard deviation at other times but leave them in the dataset
  catdata <- catdata %>%
    mutate(Flag_EXOBGAPC_RFU_1 = ifelse(! is.na(EXOBGAPC_RFU_1) & abs(EXOBGAPC_RFU_1 - BGAPC_RFU_1_mean) > BGAPC_RFU_1_threshold,
                               5, Flag_EXOBGAPC_RFU_1)) %>%
    mutate(Flag_EXOBGAPC_ugL_1 = ifelse( ! is.na(EXOBGAPC_ugL_1) & abs(EXOBGAPC_ugL_1 - BGAPC_ugL_1_mean) > BGAPC_ugL_1_threshold,
                               5, Flag_EXOBGAPC_ugL_1)) %>%
  mutate(Flag_EXOChla_RFU_1 = ifelse(! is.na(EXOChla_RFU_1) & abs(EXOChla_RFU_1 - Chla_RFU_1_mean) > Chla_RFU_1_threshold,
                             5, Flag_EXOChla_RFU_1)) %>%
    mutate(Flag_EXOChla_ugL_1 = ifelse(! is.na(EXOChla_ugL_1) & abs(EXOChla_ugL_1 - Chla_ugL_1_mean) > Chla_ugL_1_threshold,
                              5, Flag_EXOChla_ugL_1)) 

  
  
####################################################################################################################################  
  # fdom qaqc----
  # QAQC from DWH to remove major outliers from fDOM data that are 2 sd's greater than the previous and following datapoint
  sd_fDOM_QSU <- sd(catdata$EXOfDOM_QSU_1, na.rm = TRUE) #deteriming the standard deviation of fDOM data 
  sd_fDOM_RFU <- sd(catdata$EXOfDOM_RFU_1, na.rm = TRUE)
  mean_fDOM_QSU <- mean(catdata$EXOfDOM_QSU_1, na.rm = TRUE) #deteriming the standard deviation of fDOM data 
  mean_fDOM_RFU <- mean(catdata$EXOfDOM_RFU_1, na.rm = TRUE)
  
  #fDOM QSU QAQC
  catdata <- catdata%>% 
    #mutate(Flag_fDOM = ifelse(is.na(EXOfDOM_QSU_1), 1, 0)) #This creates Flag column for fDOM data, setting all NA's going into QAQC as 1 for missing data, and to 0 for the rest
  mutate(fDOM_QSU = lag(EXOfDOM_QSU_1, 0),
         fDOM_QSU_lag1 = lag(EXOfDOM_QSU_1, 1),
         fDOM_QSU_lead1 = lead(EXOfDOM_QSU_1, 1)) %>%  #These mutates create columns for current fDOM_QSU, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOfDOM_QSU_1 = ifelse(fDOM_QSU < 0 & !is.na(fDOM_QSU), 2, Flag_EXOfDOM_QSU_1),
           EXOfDOM_QSU_1 = ifelse(fDOM_QSU < 0 & !is.na(fDOM_QSU), NA, EXOfDOM_QSU_1))%>%
    mutate(EXOfDOM_QSU_1 = ifelse(
      ( abs(fDOM_QSU_lag1 - fDOM_QSU) > (2*sd_fDOM_QSU)   )  & ( abs(fDOM_QSU_lead1 - fDOM_QSU) > (2*sd_fDOM_QSU)  & !is.na(fDOM_QSU) ), NA, EXOfDOM_QSU_1
    )) %>%  #QAQC to remove outliers for QSU fDOM data 
    mutate(Flag_EXOfDOM_QSU_1 = ifelse(
      ( abs(fDOM_QSU_lag1 - fDOM_QSU) > (2*sd_fDOM_QSU)   )  & ( abs(fDOM_QSU_lead1 - fDOM_QSU) > (2*sd_fDOM_QSU)  & !is.na(fDOM_QSU)  ), 2, Flag_EXOfDOM_QSU_1
    ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
    mutate(Flag_EXOfDOM_QSU_1 = ifelse(is.na(Flag_EXOfDOM_QSU_1),2,Flag_EXOfDOM_QSU_1))%>%
    select(-fDOM_QSU, -fDOM_QSU_lag1, -fDOM_QSU_lead1)#This removes the columns used to run ifelse statements since they are no longer needed.
    
  
  #fDOM QSU QAQC
  catdata <- catdata%>% 
    #mutate(Flag_fDOM = ifelse(is.na(EXOfDOM_RFU_1), 1, 0)) #This creates Flag column for fDOM data, setting all NA's going into QAQC as 1 for missing data, and to 0 for the rest
    mutate(fDOM_RFU = lag(EXOfDOM_RFU_1, 0),
           fDOM_RFU_lag1 = lag(EXOfDOM_RFU_1, 1),
           fDOM_RFU_lead1 = lead(EXOfDOM_RFU_1, 1)) %>%  #These mutates create columns for current fDOM_RFU, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOfDOM_RFU_1 = ifelse(fDOM_RFU < 0 & !is.na(fDOM_RFU), 2, Flag_EXOfDOM_RFU_1),
           EXOfDOM_RFU_1 = ifelse(fDOM_RFU < 0 & !is.na(fDOM_RFU), NA, EXOfDOM_RFU_1))%>%
    mutate(EXOfDOM_RFU_1 = ifelse(
      ( abs(fDOM_RFU_lag1 - fDOM_RFU) > (2*sd_fDOM_RFU)   )  & ( abs(fDOM_RFU_lead1 - fDOM_RFU) > (2*sd_fDOM_RFU)  & !is.na(fDOM_RFU) ), NA, EXOfDOM_RFU_1
    )) %>%  #QAQC to remove outliers for RFU fDOM data 
    mutate(Flag_EXOfDOM_RFU_1 = ifelse(
      ( abs(fDOM_RFU_lag1 - fDOM_RFU) > (2*sd_fDOM_RFU)   )  & ( abs(fDOM_RFU_lead1 - fDOM_RFU) > (2*sd_fDOM_RFU)  & !is.na(fDOM_RFU)  ), 2, Flag_EXOfDOM_RFU_1
    ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
    mutate(Flag_EXOfDOM_RFU_1 = ifelse(is.na(Flag_EXOfDOM_RFU_1),2,Flag_EXOfDOM_RFU_1))%>%
    select(-fDOM_RFU, -fDOM_RFU_lag1, -fDOM_RFU_lead1)#This removes the columns used to run ifelse statements since they are no longer needed.
  
  # flag EXO sonde sensor data of value above 4 * standard deviation at other times but leave them in the dataset. Probably won't flag anything but for consistenacy. 
  catdata <- catdata %>%
    mutate(Flag_EXOfDOM_RFU_1 = ifelse(! is.na(EXOfDOM_RFU_1) & abs(EXOfDOM_RFU_1 - mean_fDOM_RFU) > (4*sd_fDOM_RFU),
                                   5, Flag_EXOfDOM_RFU_1)) %>%
    mutate(Flag_EXOfDOM_QSU_1 = ifelse( ! is.na(EXOfDOM_QSU_1) & abs(EXOfDOM_QSU_1 - mean_fDOM_QSU) > (4*sd_fDOM_QSU),
                                    5, Flag_EXOfDOM_QSU_1)) 
   
#####################################################################################################################################  
#QAQC from DWH to remove major outliers from conductity, specific conductivity and TDS data that are 2 sd's greater than the previous and following datapoint
  

  sd_cond <- sd(catdata$EXOCond_uScm_1, na.rm = TRUE)
  sd_spcond <-sd(catdata$EXOSpCond_uScm_1, na.rm = TRUE)
  sd_TDS <- sd(catdata$EXOTDS_mgL_1, na.rm = TRUE)
  mean_cond <- mean(catdata$EXOCond_uScm_1, na.rm = TRUE)
  mean_spcond <-mean(catdata$EXOSpCond_uScm_1, na.rm = TRUE)
  mean_TDS <- mean(catdata$EXOTDS_mgL_1, na.rm = TRUE)
  
  
  
  # QAQC on major conductivity outliers using DWH's method: datapoint set to NA if data is greater than 2*sd different from both previous and following datapoint
  #Remove any data below 1 because it is an outlier and give it a Flag Code of 2
  catdata <- catdata %>% 
    mutate(Cond = lag(EXOCond_uScm_1, 0),
           Cond_lag1 = lag(EXOCond_uScm_1, 1),
           Cond_lead1 = lead(EXOCond_uScm_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOCond_uScm_1 = ifelse(Cond < 1 & !is.na(Cond), 2, Flag_EXOCond_uScm_1)) %>%#Remove any points less than 1
    mutate(EXOCond_uScm_1 = ifelse(Cond < 1 & !is.na(Cond), NA, EXOCond_uScm_1)) %>%
    mutate(EXOCond_uScm_1 = ifelse((abs(Cond_lag1 - Cond) > (2*sd_cond))  & (abs(Cond_lead1 - Cond) > (2*sd_cond) & !is.na(Cond)), 
                                   NA, EXOCond_uScm_1)) %>%   
    mutate(Flag_EXOCond_uScm_1 = ifelse((abs(Cond_lag1 - Cond) > (2*sd_cond))  & (abs(Cond_lead1 - Cond) > (2*sd_cond) & !is.na(Cond)), 
                              2, Flag_EXOCond_uScm_1)) %>%
    mutate(Flag_EXOCond_uScm_1 = ifelse(is.na(Flag_EXOCond_uScm_1),2,Flag_EXOCond_uScm_1))%>%
    select(-Cond, -Cond_lag1, -Cond_lead1)
  
  # QAQC on major Specific conductivity outliers using DWH's method: datapoint set to NA if data is greater than 2*sd different from both previous and following datapoint
  #Remove any data below 1 because it is an outlier and give it a Flag Code of 2
  catdata <- catdata %>% 
    mutate(SpCond = lag(EXOSpCond_uScm_1, 0),
           SpCond_lag1 = lag(EXOSpCond_uScm_1, 1),
           SpCond_lead1 = lead(EXOSpCond_uScm_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOSpCond_uScm_1 = ifelse(SpCond < 1 & !is.na(SpCond), 2, Flag_EXOSpCond_uScm_1)) %>%
    mutate(EXOSpCond_uScm_1 = ifelse(SpCond < 1 & !is.na(SpCond), NA, EXOSpCond_uScm_1)) %>%
    mutate(EXOSpCond_uScm_1 = ifelse((abs(SpCond_lag1 - SpCond) > (2*sd_spcond))  & (abs(SpCond_lead1 - SpCond) > (2*sd_spcond) & !is.na(SpCond)), 
                                     NA, EXOSpCond_uScm_1)) %>%   
    mutate(Flag_EXOSpCond_uScm_1 = ifelse((abs(SpCond_lag1 - SpCond) > (2*sd_spcond))  & (abs(SpCond_lead1 - SpCond) > (2*sd_spcond)) & !is.na(SpCond), 
                                2, Flag_EXOSpCond_uScm_1)) %>% 
    mutate(Flag_EXOSpCond_uScm_1 = ifelse(is.na(Flag_EXOSpCond_uScm_1),2,Flag_EXOSpCond_uScm_1))%>%
    select(-SpCond, -SpCond_lag1, -SpCond_lead1)
  
  # QAQC on major TDS outliers using DWH's method: datapoint set to NA if data is greater than 2*sd different from both previous and following datapoint
  #Remove any data below 1 because it is an outlier and give it a Flag Code of 2
  catdata <- catdata %>% 
    mutate(TDS = lag(EXOTDS_mgL_1, 0),
           TDS_lag1 = lag(EXOTDS_mgL_1, 1),
           TDS_lead1 = lead(EXOTDS_mgL_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOTDS_mgL_1 = ifelse(TDS < 1 & !is.na(TDS), 2, Flag_EXOTDS_mgL_1)) %>%
    mutate(EXOTDS_mgL_1 = ifelse(TDS < 1 & !is.na(TDS), NA, EXOTDS_mgL_1)) %>% 
    mutate(EXOTDS_mgL_1 = ifelse((abs(TDS_lag1 - TDS) > (2*sd_TDS))  & (abs(TDS_lead1 - TDS) > (2*sd_TDS) & !is.na(TDS)), 
                                 NA, EXOTDS_mgL_1)) %>%   
    mutate(Flag_EXOTDS_mgL_1 = ifelse((abs(TDS_lag1 - TDS) > (2*sd_TDS))  & (abs(TDS_lead1 - TDS) > (2*sd_TDS)) & !is.na(TDS), 
                             2, Flag_EXOTDS_mgL_1)) %>% 
    mutate(Flag_EXOTDS_mgL_1 = ifelse(is.na(Flag_EXOTDS_mgL_1),2,Flag_EXOTDS_mgL_1))%>%
    select(-TDS, -TDS_lag1, -TDS_lead1)
  
  # flag EXO sonde sensor data of value above 4 * standard deviation at other times but leave them in the dataset. Probably won't flag anything but for consistenacy. 
  catdata <- catdata %>%
    mutate(Flag_EXOCond_uScm_1 = ifelse(! is.na(EXOCond_uScm_1) & abs(EXOCond_uScm_1 - mean_cond) > (4*sd_cond),
                                  5, Flag_EXOCond_uScm_1)) %>%
    mutate(Flag_EXOSpCond_uScm_1 = ifelse( ! is.na(EXOSpCond_uScm_1) & abs(EXOSpCond_uScm_1 - mean_spcond) > (4*sd_spcond),
                                   5, Flag_EXOSpCond_uScm_1)) %>%
    mutate(Flag_EXOTDS_mgL_1 = ifelse( ! is.na(EXOTDS_mgL_1) & abs(EXOTDS_mgL_1 - mean_TDS) > (4*sd_TDS),
                                   5, Flag_EXOTDS_mgL_1)) 
    
  
  
  #Flag high conductivity values in 2020 but don't remove them. If want to remove later I will but I am
  # not convinved it is a sensor malfunction
  
  catdata <- catdata%>%
    mutate(
      #DateTime=as.character(DateTime),#change DateTime to as.character so they line up when making changes
      Flag_EXOCond_uScm_1 = ifelse(DateTime >"2020-05-01 00:00" & DateTime < "2020-08-31 23:59" &
                           EXOCond_uScm_1>42 & Flag_EXOCond_uScm_1==0 ,5, Flag_EXOCond_uScm_1),
      Flag_EXOSpCond_uScm_1 = ifelse(DateTime >"2020-05-01 00:00" & DateTime < "2020-08-31 23:59" &
                             EXOSpCond_uScm_1>42 & Flag_EXOSpCond_uScm_1==0 ,5, Flag_EXOSpCond_uScm_1),
      Flag_EXOTDS_mgL_1 = ifelse(DateTime >"2020-05-01 00:00" & DateTime < "2020-08-31 23:59" &
                          EXOTDS_mgL_1>30 & Flag_EXOTDS_mgL_1==0 ,5, Flag_EXOTDS_mgL_1))
  
#####################################################################################################################################
  sd_Turbidity_FNU <- sd(catdata$EXOTurbidity_FNU_1, na.rm = TRUE) #deteriming the standard deviation of fDOM data 
  mean_Turbidity_FNU <- mean(catdata$EXOTurbidity_FNU_1, na.rm = TRUE) #deteriming the standard deviation of fDOM data 
  
  #Turbidity FNU QAQC
  catdata <- catdata%>% 
    #mutate(Flag_fDOM = ifelse(is.na(EXOTurbidity_FNU_1), 1, 0)) #This creates Flag column for fDOM data, setting all NA's going into QAQC as 1 for missing data, and to 0 for the rest
    mutate(Turbidity_FNU = lag(EXOTurbidity_FNU_1, 0),
           Turbidity_FNU_lag1 = lag(EXOTurbidity_FNU_1, 1),
           Turbidity_FNU_lead1 = lead(EXOTurbidity_FNU_1, 1)) %>%  #These mutates create columns for current Turbidity_FNU, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOTurbidity_FNU_1 = ifelse(Turbidity_FNU < 0 & !is.na(Turbidity_FNU), 3, Flag_EXOTurbidity_FNU_1),
           EXOTurbidity_FNU_1 = ifelse(Turbidity_FNU < 0 & !is.na(Turbidity_FNU), 0, EXOTurbidity_FNU_1))%>%
    mutate(EXOTurbidity_FNU_1 = ifelse(
      ( abs(Turbidity_FNU_lag1 - Turbidity_FNU) > (2*sd_Turbidity_FNU)   )  & ( abs(Turbidity_FNU_lead1 - Turbidity_FNU) > (2*sd_Turbidity_FNU)  & !is.na(Turbidity_FNU) ), NA, EXOTurbidity_FNU_1
    )) %>%  #QAQC to remove outliers for QSU fDOM data 
    mutate(Flag_EXOTurbidity_FNU_1 = ifelse(
      ( abs(Turbidity_FNU_lag1 - Turbidity_FNU) > (2*sd_Turbidity_FNU)   )  & ( abs(Turbidity_FNU_lead1 - Turbidity_FNU) > (2*sd_Turbidity_FNU)  & !is.na(Turbidity_FNU)  ), 2, Flag_EXOTurbidity_FNU_1
    ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
    mutate(Flag_EXOTurbidity_FNU_1 = ifelse(is.na(Flag_EXOTurbidity_FNU_1),2,Flag_EXOTurbidity_FNU_1))%>%
    select(-Turbidity_FNU, -Turbidity_FNU_lag1, -Turbidity_FNU_lead1)#This removes the columns used to run ifelse statements since they are no longer needed.
  
#####################################################################################################################################   
  #change EXO values to NA if EXO depth is less than 0.5m and Flag as 2
  
  #index only the colummns with EXO at the beginning
  exo_idx <-grep("^EXO",colnames(catdata))
  
  #create list of the Flag columns that need to be changed to 2
  exo_flag <- c("Flag_EXOTemp_C_1", "Flag_EXOCond_uScm_1","Flag_EXOSpCond_uScm_1","Flag_EXOTDS_mgL_1",'Flag_EXODO_mgL_1',
                "Flag_EXODOsat_percent_1","Flag_EXOChla_RFU_1","Flag_EXOChla_ugL_1","Flag_EXOBGAPC_RFU_1",
                "Flag_EXOBGAPC_ugL_1",'Flag_EXOfDOM_RFU_1','Flag_EXOfDOM_QSU_1', 'Flag_EXOTurbidity_FNU_1')
  
  
  #Change the EXO data to NAs when the EXO is above 0.5m and not due to maintenance
  catdata[which(catdata$EXO_depth_m < 0.55), exo_idx] <- NA
  #Flag the data that was removed with 2 for outliers
  catdata[which(catdata$EXO_depth_m<0.55),exo_flag]<- 2

    
#############################################################################################################################  
   # delete EXO_Date and EXO_Time columns
  catdata <- catdata %>% select(-EXO_Date, -EXO_Time)
  
  # add Reservoir and Site columns
  catdata$Reservoir <- "FCR"
  catdata$Site <- "50"
  
  
  # reorder columns
  catdata <- catdata %>% select(Reservoir, Site, DateTime, ThermistorTemp_C_surface:ThermistorTemp_C_9,
                                RDO_mgL_5, RDOsat_percent_5,RDO_mgL_5_adjusted, RDOsat_percent_5_adjusted, 
                                RDOTemp_C_5, RDO_mgL_9, RDOsat_percent_9, 
                                RDO_mgL_9_adjusted, RDOsat_percent_9_adjusted, RDOTemp_C_9,
                                EXOTemp_C_1, EXOCond_uScm_1, EXOSpCond_uScm_1, EXOTDS_mgL_1, EXODOsat_percent_1,
                                EXODO_mgL_1, EXOChla_RFU_1, EXOChla_ugL_1, EXOBGAPC_RFU_1, EXOBGAPC_ugL_1,
                                EXOfDOM_RFU_1, EXOfDOM_QSU_1,EXOTurbidity_FNU_1, EXO_pressure_psi, EXO_depth_m, EXO_battery_V, EXO_cablepower_V,
                                EXO_wiper_V, Lvl_psi_9, LvlTemp_C_9, RECORD, CR6_Batt_V, CR6Panel_Temp_C,
                                Flag_ThermistorTemp_C_surface:Flag_ThermistorTemp_C_9,Flag_RDO_mgL_5, Flag_RDOsat_percent_5, Flag_RDOTemp_C_5,
                                Flag_RDO_mgL_9, Flag_RDOsat_percent_9, Flag_RDOTemp_C_9,Flag_EXOTemp_C_1, Flag_EXOCond_uScm_1, Flag_EXOSpCond_uScm_1,Flag_EXOTDS_mgL_1,
                                Flag_EXODOsat_percent_1, Flag_EXODO_mgL_1, Flag_EXOChla_RFU_1,Flag_EXOChla_ugL_1, Flag_EXOBGAPC_RFU_1,Flag_EXOBGAPC_ugL_1,
                                Flag_EXOfDOM_RFU_1,Flag_EXOfDOM_QSU_1,Flag_EXOTurbidity_FNU_1, Flag_EXO_pressure_psi, Flag_EXO_depth_m, Flag_EXO_battery_V, Flag_EXO_cablepower_V,
                                Flag_EXO_wiper_V, Flag_Lvl_psi_9, Flag_LvlTemp_C_9)
  
  # replace NaNs with NAs
  catdata[is.na(catdata)] <- NA
  
  
  #order by date and time
  catdata <- catdata[order(catdata$DateTime),]
  
  # convert datetimes to characters so that they are properly formatted in the output file
  catdata$DateTime <- as.character(catdata$DateTime)
  
  # write to output file
  write_csv(catdata, output_file)
}

# example usage
#qaqc("https://raw.githubusercontent.com/CareyLabVT/SCCData/mia-data/Catwalk.csv",
#     'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/FCRWaterLevel.csv',
#     "https://raw.githubusercontent.com/CareyLabVT/SCCData/mia-data/CAT_MaintenanceLog.txt",
#    "Catwalk.csv")


