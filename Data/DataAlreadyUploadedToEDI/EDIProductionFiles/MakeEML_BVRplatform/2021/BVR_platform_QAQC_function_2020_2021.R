


qaqc <- function(data_file, data2_file, 
                 maintenance_file,  output_file)
{

#bvrdata=data_file
#change column names
  BVRDATA_COL_NAMES = c("DateTime", "RECORD", "CR6_Batt_V", "CR6Panel_Temp_C", "ThermistorTemp_C_1",
                        "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4", "ThermistorTemp_C_5",
                        "ThermistorTemp_C_6", "ThermistorTemp_C_7", "ThermistorTemp_C_8", "ThermistorTemp_C_9",
                        "ThermistorTemp_C_10","ThermistorTemp_C_11","ThermistorTemp_C_12","ThermistorTemp_C_13",
                        "RDO_mgL_6", "RDOsat_percent_6", "RDOTemp_C_6", "RDO_mgL_13",
                        "RDOsat_percent_13", "RDOTemp_C_13", "EXO_Date", "EXO_Time", "EXOTemp_C_1_5", "EXOCond_uScm_1_5",
                        "EXOSpCond_uScm_1_5", "EXOTDS_mgL_1_5", "EXODOsat_percent_1_5", "EXODO_mgL_1_5", "EXOChla_RFU_1_5",
                        "EXOChla_ugL_1_5", "EXOBGAPC_RFU_1_5", "EXOBGAPC_ugL_1_5", "EXOfDOM_RFU_1_5", "EXOfDOM_QSU_1_5",
                        "EXOTurbidity_FNU_1_5", "EXOTSS_mg_1_5","EXO_pressure_psi", "EXO_depth_m", "EXO_battery_V",
                         "EXO_cablepower_V", "EXO_wiper_V", "Lvl_psi_13", "LvlTemp_C_13")
 
  
 
  
 #Fouling factor for Chla on EXO
  EXO_FOULING_FACTOR <- 4
  
  #Adjustment period of time to stabilization after cleaning in seconds
  ADJ_PERIOD = 2*60*60 
  
  #read in data from obs1 above
  
  # read bvrwalk data and maintenance log
  # NOTE: date-times throughout this script are processed as UTC
  bvrdata1 <- read_csv(data_file, skip=1, col_names = BVRDATA_COL_NAMES,
                      col_types = cols(.default = col_double(), DateTime = col_datetime()))
 

 bvrdata2<-read_csv(data2_file, skip=1, col_names = BVRDATA_COL_NAMES,
                    col_types = cols(.default = col_double(), DateTime = col_datetime()))
 
 

 
 bvrdata= bvrdata1%>%
   rbind(.,bvrdata2)%>% #combine manual and most recent files
   drop_na(DateTime)%>% #take out the rows with blank timestamps
   distinct(DateTime, .keep_all= TRUE) #taking out the duplicate values 
   
   #bvrdata$DateTime<-as.POSIXct(bvrdata$DateTime,format = "%Y-%m-%d %H:%M:%S", tz="UTC")
   #after$DateTime<-as.POSIXct(strptime(after$DateTime, "%Y-%m-%d %H:%M"), tz = "EST")

###########################################################################################################################
  #check for gaps and missing data
  #order data by timestamp
  BVRdata2=bvrdata
  BVRdata2=BVRdata2[order(BVRdata2$DateTime),]
  BVRdata2$DOY=yday(BVRdata2$DateTime)
  
  
  #check record for gaps
  #daily record gaps by day of year
  for(i in 2:nrow(BVRdata2)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
    if(BVRdata2$DOY[i]-BVRdata2$DOY[i-1]>1){
      print(c(BVRdata2$DateTime[i-1],BVRdata2$DateTime[i]))
    }
  }
  BVR2=BVRdata2%>%filter(!is.na(RECORD))
  for(i in 2:length(BVR2$RECORD)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
    if(abs(BVR2$RECORD[i]-BVR2$RECORD[i-1])>1){
      print(c(BVR2$DateTime[i-1],BVR2$DateTime[i]))
    }
  }
####################################################################################################################################### 
  
  
#read in maintenance log
  log <- read_csv(maintenance_file,
    col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  ))
  
  #Filter out the 7 flag because it is already NAs in the dataset and not maintenance
  log=log%>%filter(flag!=7)
  
  for(j in c(5:23,26:46)) { #for loop to create new columns in data frame
    bvrdata[,paste0("Flag_",colnames(bvrdata[j]))] <- 0 #creates flag column + name of variable
    bvrdata[c(which(is.na(bvrdata[,j]))),paste0("Flag_",colnames(bvrdata[j]))] <-7 #puts in flag 7 if value not collected
  }
  
  # modify bvrdata based on the information in the log
  for(i in 1:nrow(log))
  {
    # get start and end time of one maintenance event
    start <- log$TIMESTAMP_start[i]
    end <- log$TIMESTAMP_end[i]
    
    
    # get indices of columns affected by maintenance
    if(grepl("^\\d+$", log$colnumber[i])) # single num
    {
      maintenance_cols <- intersect(c(2:46), as.integer(log$colnumber[i]))
    }
    
    else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", log$colnumber[i])) # c(x;y;...)
    {
      maintenance_cols <- intersect(c(2:46), as.integer(unlist(regmatches(log$colnumber[i],
                                                                          gregexpr("\\d+", log$colnumber[i])))))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", log$colnumber[i])) # c(x:y)
    {
      bounds <- as.integer(unlist(regmatches(log$colnumber[i], gregexpr("\\d+", log$colnumber[i]))))
      maintenance_cols <- intersect(c(2:46), c(bounds[1]:bounds[2]))
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
                    "indices 2 through 46, excluding 24 and 25, which are deleted by this script. Skipping maintenance for that row."))
      next
    }
    
    #index the Flag columns
    if(grepl("^\\d+$", log$flagcol[i])) # single num
    {
      flag_cols <- intersect(c(47:86), as.integer(log$flagcol[i]))
    
    }
    else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", log$flagcol[i])) # c(x;y;...)
    {
      flag_cols <- intersect(c(47:86), as.integer(unlist(regmatches(log$flagcol[i],
                                                                          gregexpr("\\d+", log$flagcol[i])))))
    }
    
    else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", log$flagcol[i])) # c(x:y)
    {
      bounds_flag <- as.integer(unlist(regmatches(log$flagcol[i], gregexpr("\\d+", log$flagcol[i]))))
      flag_cols <- intersect(c(47:86), c(bounds_flag[1]:bounds_flag[2]))
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
    if(flag!=5)
    {
    bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, maintenance_cols] <- NA
    bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, flag_cols] <- flag
    }
    else
    {
      bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, flag_cols] <- flag
      next
    }
    #Add the 2 hour adjustment for DO 
    if (log$colnumber[i]=="c(1:46)" & flag==1){
      DO_col=c("RDO_mgL_6", "RDOsat_percent_6", "RDO_mgL_13","RDOsat_percent_13","EXODOsat_percent_1_5", "EXODO_mgL_1_5")
      DO_flag_col=c("Flag_RDO_mgL_6", "Flag_RDOsat_percent_6", "Flag_RDO_mgL_13","Flag_RDOsat_percent_13","Flag_EXODOsat_percent_1_5", "Flag_EXODO_mgL_1_5")
        bvrdata[bvrdata$DateTime>start&bvrdata$DateTime<(end+ADJ_PERIOD),DO_col] <- NA
        bvrdata[bvrdata$DateTime>start&bvrdata$DateTime<(end+ADJ_PERIOD),DO_flag_col] <- flag
    }
    else if(log$colnumber[i] %in% c(" 18"," 19") & flag==1){
      DO_col=c(RDO_mgL_6, RDOsat_percent_6)
      DO_flag_col=c(Flag_RDO_mgL_6, Flag_RDOsat_percent_6)
      bvrdata[bvrdata$DateTime>start[i]&bvrdata$DateTime<end[i]+ADJ_PERIOD,DO_col] <- NA
      bvrdata[bvrdata$DateTime>start[i]&bvrdata$DateTime<end[i]+ADJ_PERIOD,DO_flag_col] <- flag
      
    }
    else if(log$colnumber[i] %in% c(" 21"," 22") & flag==1){
      DO_col=c(RDO_mgL_13,RDOsat_percent_13)
      DO_flag_col=c(Flag_RDO_mgL_13,Flag_RDOsat_percent_13)
      bvrdata[bvrdata$DateTime>start[i]&bvrdata$DateTime<end[i]+ADJ_PERIOD,DO_col] <- NA
      bvrdata[bvrdata$DateTime>start[i]&bvrdata$DateTime<end[i]+ADJ_PERIOD,DO_flag_col] <- flag
      
    }
    else if (log$colnumber[i] %in% c(" c(26:44"," 30"," 31") & flag==1){
      DO_col=c(EXODOsat_percent_1_5, EXODO_mgL_1_5)
      DO_flag_col=c(Flag_EXODOsat_percent_1_5, Flag_EXODO_mgL_1_5)
      bvrdata[bvrdata$DateTime>start[i]&bvrdata$DateTime<(end[i]+ADJ_PERIOD),DO_col] <- NA
      bvrdata[bvrdata$DateTime>start[i]&bvrdata$DateTime<(end[i]+ADJ_PERIOD),DO_flag_col] <-1
      
    }
      else{
        warning(paste("No DO to time to adjust"))
        
      }
  }

  
  ##################################################################################################################  
  #Set negative DO values to 0 and Flag_DO for NA values
  bvrdata <- bvrdata %>%  #RDO at 5m
    mutate(Flag_RDO_mgL_6 = ifelse(RDO_mgL_6 < 0 & !is.na(RDO_mgL_6) , 3, Flag_RDO_mgL_6),#Add a flag for DO<0
           Flag_RDOsat_percent_6 = ifelse(RDOsat_percent_6 < 0 & !is.na(RDOsat_percent_6) , 3, Flag_RDOsat_percent_6),
           RDO_mgL_6 = ifelse(RDO_mgL_6 < 0, 0, RDO_mgL_6), #Change negative to 0
           RDOsat_percent_6 = ifelse(RDOsat_percent_6 < 0, 0, RDOsat_percent_6), #Change negative %sat to 0
           
           Flag_RDO_mgL_13 = ifelse(RDO_mgL_13 < 0 & !is.na(RDO_mgL_13), 3, Flag_RDO_mgL_13), #repeat for 13m
           Flag_RDOsat_percent_13 = ifelse(RDOsat_percent_13 < 0 & !is.na(RDOsat_percent_13) , 3, Flag_RDOsat_percent_13),
           RDO_mgL_13 = ifelse(RDO_mgL_13 < 0, 0, RDO_mgL_13),
           RDOsat_percent_13 = ifelse(RDOsat_percent_13 < 0, 0, RDOsat_percent_13),
           
           Flag_EXODO_mgL_1_5 = ifelse(EXODO_mgL_1_5 < 0 & !is.na(EXODO_mgL_1_5), 3, Flag_EXODO_mgL_1_5), #and for 1m
           Flag_EXODOsat_percent_1_5 = ifelse(EXODOsat_percent_1_5 < 0 & !is.na(EXODOsat_percent_1_5) , 3, Flag_EXODOsat_percent_1_5),
           EXODO_mgL_1_5 = ifelse(EXODO_mgL_1_5 < 0, 0, EXODO_mgL_1_5),
           EXODOsat_percent_1_5 = ifelse(EXODOsat_percent_1_5 <0, 0, EXODOsat_percent_1_5)
    )
  
  ########################################################################################################################  
  # find chla and phyo on the EXO sonde sensor data that differs from the mean by more than the standard deviation times a given factor, and
  # replace with NAs between October 2018 and March 2019, due to sensor fouling
  Chla_RFU_1_5_mean <- mean(bvrdata$EXOChla_RFU_1_5, na.rm = TRUE)
  Chla_ugL_1_5_mean <- mean(bvrdata$EXOChla_ugL_1_5, na.rm = TRUE)
  BGAPC_RFU_1_5_mean <- mean(bvrdata$EXOBGAPC_RFU_1_5, na.rm = TRUE)
  BGAPC_ugL_1_5_mean <- mean(bvrdata$EXOBGAPC_ugL_1_5, na.rm = TRUE)
  Chla_RFU_1_5_threshold <- EXO_FOULING_FACTOR * sd(bvrdata$EXOChla_RFU_1_5, na.rm = TRUE)
  Chla_ugL_1_5_threshold <- EXO_FOULING_FACTOR * sd(bvrdata$EXOChla_ugL_1_5, na.rm = TRUE)
  BGAPC_RFU_1_5_threshold <- EXO_FOULING_FACTOR * sd(bvrdata$EXOBGAPC_RFU_1_5, na.rm = TRUE)
  BGAPC_ugL_1_5_threshold <- EXO_FOULING_FACTOR * sd(bvrdata$EXOBGAPC_ugL_1_5, na.rm = TRUE)
  
  
  # QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
  bvrdata <- bvrdata %>% 
    mutate(Chla_ugL = lag(EXOChla_ugL_1_5, 0),
           Chla_ugL_lag1 = lag(EXOChla_ugL_1_5, 1),
           Chla_ugL_lead1 = lead(EXOChla_ugL_1_5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOChla_ugL_1_5 = ifelse(Chla_ugL < 0 & !is.na(Chla_ugL), 3, Flag_EXOChla_ugL_1_5)) %>% 
    mutate(EXOChla_ugL_1_5 = ifelse(Chla_ugL < 0 & !is.na(Chla_ugL), 0, EXOChla_ugL_1_5)) %>% 
    mutate(EXOChla_ugL_1_5 = ifelse((abs(Chla_ugL_lag1 - Chla_ugL) > (Chla_ugL_1_5_threshold))  & (abs(Chla_ugL_lead1 - Chla_ugL) > (Chla_ugL_1_5_threshold) & !is.na(Chla_ugL)), 
                                  NA, EXOChla_ugL_1_5)) %>%   
    mutate(Flag_EXOChla_ugL_1_5 = ifelse((abs(Chla_ugL_lag1 - Chla_ugL) > (Chla_ugL_1_5_threshold))  & (abs(Chla_ugL_lead1 - Chla_ugL) > (Chla_ugL_1_5_threshold)) & !is.na(Chla_ugL), 
                                  2, Flag_EXOChla_ugL_1_5)) %>%
    mutate(Flag_EXOChla_ugL_1_5 = ifelse(is.na(Flag_EXOChla_ugL_1_5), 2, Flag_EXOChla_ugL_1_5))%>%
    select(-Chla_ugL, -Chla_ugL_lag1, -Chla_ugL_lead1)
  
  #Chla_RFU QAQC
  bvrdata <- bvrdata %>% 
    mutate(Chla_RFU = lag(EXOChla_RFU_1_5, 0),
           Chla_RFU_lag1 = lag(EXOChla_RFU_1_5, 1),
           Chla_RFU_lead1 = lead(EXOChla_RFU_1_5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOChla_RFU_1_5 = ifelse(Chla_RFU < 0 & !is.na(Chla_RFU), 3, Flag_EXOChla_RFU_1_5)) %>% 
    mutate(EXOChla_RFU_1_5 = ifelse(Chla_RFU < 0 & !is.na(Chla_RFU), 0, EXOChla_RFU_1_5)) %>% 
    mutate(EXOChla_RFU_1_5 = ifelse((abs(Chla_RFU_lag1 - Chla_RFU) > (Chla_RFU_1_5_threshold))  & (abs(Chla_RFU_lead1 - Chla_RFU) > (Chla_RFU_1_5_threshold) & !is.na(Chla_RFU)), 
                                  NA, EXOChla_RFU_1_5)) %>%   
    mutate(Flag_EXOChla_RFU_1_5 = ifelse((abs(Chla_RFU_lag1 - Chla_RFU) > (Chla_RFU_1_5_threshold))  & (abs(Chla_RFU_lead1 - Chla_RFU) > (Chla_RFU_1_5_threshold)) & !is.na(Chla_RFU), 
                                  2, Flag_EXOChla_RFU_1_5)) %>%
    mutate(Flag_EXOChla_RFU_1_5 = ifelse(is.na(Flag_EXOChla_RFU_1_5), 2, Flag_EXOChla_RFU_1_5))%>%
    select(-Chla_RFU, -Chla_RFU_lag1, -Chla_RFU_lead1)
  # QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
  bvrdata <- bvrdata %>% 
    mutate(phyco_ugL = lag(EXOBGAPC_ugL_1_5, 0),
           phyco_ugL_lag1 = lag(EXOBGAPC_ugL_1_5, 1),
           phyco_ugL_lead1 = lead(EXOBGAPC_ugL_1_5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOBGAPC_ugL_1_5 = ifelse(phyco_ugL < 0 & !is.na(phyco_ugL), 3, Flag_EXOBGAPC_ugL_1_5)) %>% 
    mutate(EXOBGAPC_ugL_1_5 = ifelse(phyco_ugL < 0 & !is.na(phyco_ugL), 0, EXOBGAPC_ugL_1_5)) %>% 
    mutate(EXOBGAPC_ugL_1_5 = ifelse((abs(phyco_ugL_lag1 - phyco_ugL) > (BGAPC_ugL_1_5_threshold))  & (abs(phyco_ugL_lead1 - phyco_ugL) > (BGAPC_ugL_1_5_threshold) & !is.na(phyco_ugL)), 
                                   NA, EXOBGAPC_ugL_1_5)) %>%   
    mutate(Flag_EXOBGAPC_ugL_1_5 = ifelse((abs(phyco_ugL_lag1 - phyco_ugL) > (BGAPC_ugL_1_5_threshold))  & (abs(phyco_ugL_lead1 - phyco_ugL) > (BGAPC_ugL_1_5_threshold) & !is.na(phyco_ugL)), 
                                   2, Flag_EXOBGAPC_ugL_1_5)) %>%
    mutate(Flag_EXOBGAPC_ugL_1_5 = ifelse(is.na(Flag_EXOBGAPC_ugL_1_5),2,Flag_EXOBGAPC_ugL_1_5))%>%
    select(-phyco_ugL, -phyco_ugL_lag1, -phyco_ugL_lead1)
  
  #Phyco QAQC for RFU
  bvrdata <- bvrdata %>% 
    mutate(phyco_RFU = lag(EXOBGAPC_RFU_1_5, 0),
           phyco_RFU_lag1 = lag(EXOBGAPC_RFU_1_5, 1),
           phyco_RFU_lead1 = lead(EXOBGAPC_RFU_1_5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOBGAPC_RFU_1_5 = ifelse(phyco_RFU < 0 & !is.na(phyco_RFU), 3, Flag_EXOBGAPC_RFU_1_5)) %>% 
    mutate(EXOBGAPC_RFU_1_5 = ifelse(phyco_RFU < 0 & !is.na(phyco_RFU), 0, EXOBGAPC_RFU_1_5)) %>% 
    mutate(EXOBGAPC_RFU_1_5 = ifelse((abs(phyco_RFU_lag1 - phyco_RFU) > (BGAPC_RFU_1_5_threshold))  & (abs(phyco_RFU_lead1 - phyco_RFU) > (BGAPC_RFU_1_5_threshold) & !is.na(phyco_RFU)), 
                                   NA, EXOBGAPC_RFU_1_5)) %>%   
    mutate(Flag_EXOBGAPC_RFU_1_5 = ifelse((abs(phyco_RFU_lag1 - phyco_RFU) > (BGAPC_RFU_1_5_threshold))  & (abs(phyco_RFU_lead1 - phyco_RFU) > (BGAPC_RFU_1_5_threshold) & !is.na(phyco_RFU)), 
                                   2, Flag_EXOBGAPC_RFU_1_5)) %>%
    mutate(Flag_EXOBGAPC_RFU_1_5 = ifelse(is.na(Flag_EXOBGAPC_RFU_1_5),2,Flag_EXOBGAPC_RFU_1_5))%>%
    select(-phyco_RFU, -phyco_RFU_lag1, -phyco_RFU_lead1)
  
 
  
  # flag EXO sonde sensor data of value above 4 * standard deviation at other times but leave them in the dataset
  bvrdata <- bvrdata %>%
    mutate(Flag_EXOBGAPC_RFU_1_5 = ifelse(! is.na(EXOBGAPC_RFU_1_5) & abs(EXOBGAPC_RFU_1_5 - BGAPC_RFU_1_5_mean) > BGAPC_RFU_1_5_threshold,
                                   5, Flag_EXOBGAPC_RFU_1_5)) %>%
    mutate(Flag_EXOBGAPC_ugL_1_5 = ifelse( ! is.na(EXOBGAPC_ugL_1_5) & abs(EXOBGAPC_ugL_1_5 - BGAPC_ugL_1_5_mean) > BGAPC_ugL_1_5_threshold,
                                    5, Flag_EXOBGAPC_ugL_1_5)) %>%
    mutate(Flag_EXOChla_RFU_1_5 = ifelse(! is.na(EXOChla_RFU_1_5) & abs(EXOChla_RFU_1_5 - Chla_RFU_1_5_mean) > Chla_RFU_1_5_threshold,
                                  5, Flag_EXOChla_RFU_1_5)) %>%
    mutate(Flag_EXOChla_ugL_1_5 = ifelse(! is.na(EXOChla_ugL_1_5) & abs(EXOChla_ugL_1_5 - Chla_ugL_1_5_mean) > Chla_ugL_1_5_threshold,
                                  5, Flag_EXOChla_ugL_1_5)) 
  
  
  
  ####################################################################################################################################  
  # fdom qaqc----
  # QAQC from DWH to remove major outliers from fDOM data that are 2 sd's greater than the previous and following datapoint
  sd_fDOM_QSU <- sd(bvrdata$EXOfDOM_QSU_1_5, na.rm = TRUE) #deteriming the standard deviation of fDOM data 
  sd_fDOM_RFU <- sd(bvrdata$EXOfDOM_RFU_1_5, na.rm = TRUE)
  mean_fDOM_QSU <- mean(bvrdata$EXOfDOM_QSU_1_5, na.rm = TRUE) #deteriming the standard deviation of fDOM data 
  mean_fDOM_RFU <- mean(bvrdata$EXOfDOM_RFU_1_5, na.rm = TRUE)
  
  #fDOM QSU QAQC
  bvrdata <- bvrdata%>% 
    #mutate(Flag_fDOM = ifelse(is.na(EXOfDOM_QSU_1_5), 1, 0)) #This creates Flag column for fDOM data, setting all NA's going into QAQC as 1 for missing data, and to 0 for the rest
    mutate(fDOM_QSU = lag(EXOfDOM_QSU_1_5, 0),
           fDOM_QSU_lag1 = lag(EXOfDOM_QSU_1_5, 1),
           fDOM_QSU_lead1 = lead(EXOfDOM_QSU_1_5, 1)) %>%  #These mutates create columns for current fDOM_QSU, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOfDOM_QSU_1_5 = ifelse(fDOM_QSU < 0 & !is.na(fDOM_QSU), 3, Flag_EXOfDOM_QSU_1_5),
           EXOfDOM_QSU_1_5 = ifelse(fDOM_QSU < 0 & !is.na(fDOM_QSU), 0, EXOfDOM_QSU_1_5))%>%
    mutate(EXOfDOM_QSU_1_5 = ifelse(
      ( abs(fDOM_QSU_lag1 - fDOM_QSU) > (2*sd_fDOM_QSU)   )  & ( abs(fDOM_QSU_lead1 - fDOM_QSU) > (2*sd_fDOM_QSU)  & !is.na(fDOM_QSU) ), NA, EXOfDOM_QSU_1_5
    )) %>%  #QAQC to remove outliers for QSU fDOM data 
    mutate(Flag_EXOfDOM_QSU_1_5 = ifelse(
      ( abs(fDOM_QSU_lag1 - fDOM_QSU) > (2*sd_fDOM_QSU)   )  & ( abs(fDOM_QSU_lead1 - fDOM_QSU) > (2*sd_fDOM_QSU)  & !is.na(fDOM_QSU)  ), 2, Flag_EXOfDOM_QSU_1_5
    ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
    mutate(Flag_EXOfDOM_QSU_1_5 = ifelse(is.na(Flag_EXOfDOM_QSU_1_5),2,Flag_EXOfDOM_QSU_1_5))%>%
    select(-fDOM_QSU, -fDOM_QSU_lag1, -fDOM_QSU_lead1)#This removes the columns used to run ifelse statements since they are no longer needed.
  
  
  #fDOM QSU QAQC
  bvrdata <- bvrdata%>% 
    #mutate(Flag_fDOM = ifelse(is.na(EXOfDOM_RFU_1_5), 1, 0)) #This creates Flag column for fDOM data, setting all NA's going into QAQC as 1 for missing data, and to 0 for the rest
    mutate(fDOM_RFU = lag(EXOfDOM_RFU_1_5, 0),
           fDOM_RFU_lag1 = lag(EXOfDOM_RFU_1_5, 1),
           fDOM_RFU_lead1 = lead(EXOfDOM_RFU_1_5, 1)) %>%  #These mutates create columns for current fDOM_RFU, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOfDOM_RFU_1_5 = ifelse(fDOM_RFU < 0 & !is.na(fDOM_RFU), 3, Flag_EXOfDOM_RFU_1_5),
           EXOfDOM_RFU_1_5 = ifelse(fDOM_RFU < 0 & !is.na(fDOM_RFU), 0, EXOfDOM_RFU_1_5))%>%
    mutate(EXOfDOM_RFU_1_5 = ifelse(
      ( abs(fDOM_RFU_lag1 - fDOM_RFU) > (2*sd_fDOM_RFU)   )  & ( abs(fDOM_RFU_lead1 - fDOM_RFU) > (2*sd_fDOM_RFU)  & !is.na(fDOM_RFU) ), NA, EXOfDOM_RFU_1_5
    )) %>%  #QAQC to remove outliers for RFU fDOM data 
    mutate(Flag_EXOfDOM_RFU_1_5 = ifelse(
      ( abs(fDOM_RFU_lag1 - fDOM_RFU) > (2*sd_fDOM_RFU)   )  & ( abs(fDOM_RFU_lead1 - fDOM_RFU) > (2*sd_fDOM_RFU)  & !is.na(fDOM_RFU)  ), 2, Flag_EXOfDOM_RFU_1_5
    ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
    mutate(Flag_EXOfDOM_RFU_1_5 = ifelse(is.na(Flag_EXOfDOM_RFU_1_5),2,Flag_EXOfDOM_RFU_1_5))%>%
    select(-fDOM_RFU, -fDOM_RFU_lag1, -fDOM_RFU_lead1)#This removes the columns used to run ifelse statements since they are no longer needed.
  
  # flag EXO sonde sensor data of value above 4 * standard deviation at other times but leave them in the dataset. Probably won't flag anything but for consistenacy. 
  bvrdata <- bvrdata %>%
    mutate(Flag_EXOfDOM_RFU_1_5 = ifelse(! is.na(EXOfDOM_RFU_1_5) & abs(EXOfDOM_RFU_1_5 - mean_fDOM_RFU) > (4*sd_fDOM_RFU),
                                  5, Flag_EXOfDOM_RFU_1_5)) %>%
    mutate(Flag_EXOfDOM_QSU_1_5 = ifelse( ! is.na(EXOfDOM_QSU_1_5) & abs(EXOfDOM_QSU_1_5 - mean_fDOM_QSU) > (4*sd_fDOM_QSU),
                                   5, Flag_EXOfDOM_QSU_1_5)) 
  
  #####################################################################################################################################  
  #QAQC from DWH to remove major outliers from conductity, specific conductivity and TDS data that are 2 sd's greater than the previous and following datapoint
  
  
  sd_cond <- sd(bvrdata$EXOCond_uScm_1_5, na.rm = TRUE)
  sd_spcond <-sd(bvrdata$EXOSpCond_uScm_1_5, na.rm = TRUE)
  sd_TDS <- sd(bvrdata$EXOTDS_mgL_1_5, na.rm = TRUE)
  mean_cond <- mean(bvrdata$EXOCond_uScm_1_5, na.rm = TRUE)
  mean_spcond <-mean(bvrdata$EXOSpCond_uScm_1_5, na.rm = TRUE)
  mean_TDS <- mean(bvrdata$EXOTDS_mgL_1_5, na.rm = TRUE)
  
  
  # QAQC on major conductivity outliers using DWH's method: datapoint set to NA if data is greater than 2*sd different from both previous and following datapoint
  #Remove any data below 1 because it is an outlier and give it a Flag Code of 2
  bvrdata <- bvrdata %>% 
    mutate(Cond = lag(EXOCond_uScm_1_5, 0),
           Cond_lag1 = lag(EXOCond_uScm_1_5, 1),
           Cond_lead1 = lead(EXOCond_uScm_1_5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOCond_uScm_1_5 = ifelse(Cond < 0 & !is.na(Cond), 3, Flag_EXOCond_uScm_1_5)) %>%
    mutate(Flag_EXOCond_uScm_1_5 = ifelse(Cond < 1 & !is.na(Cond), 2, Flag_EXOCond_uScm_1_5)) %>%#Remove any points less than 1
    mutate(EXOCond_uScm_1_5 = ifelse(Cond < 0 & !is.na(Cond), 0, EXOCond_uScm_1_5)) %>%
    mutate(EXOCond_uScm_1_5 = ifelse(Cond < 1 & !is.na(Cond), NA, EXOCond_uScm_1_5)) %>%
    mutate(EXOCond_uScm_1_5 = ifelse((abs(Cond_lag1 - Cond) > (2*sd_cond))  & (abs(Cond_lead1 - Cond) > (2*sd_cond) & !is.na(Cond)), 
                                   NA, EXOCond_uScm_1_5)) %>%   
    mutate(Flag_EXOCond_uScm_1_5 = ifelse((abs(Cond_lag1 - Cond) > (2*sd_cond))  & (abs(Cond_lead1 - Cond) > (2*sd_cond) & !is.na(Cond)), 
                              2, Flag_EXOCond_uScm_1_5)) %>%
    mutate(Flag_EXOCond_uScm_1_5 = ifelse(is.na(Flag_EXOCond_uScm_1_5),2,Flag_EXOCond_uScm_1_5))%>%
    select(-Cond, -Cond_lag1, -Cond_lead1)
  
  # QAQC on major Specific conductivity outliers using DWH's method: datapoint set to NA if data is greater than 2*sd different from both previous and following datapoint
  #Remove any data below 1 because it is an outlier and give it a Flag Code of 2
  bvrdata <- bvrdata %>% 
    mutate(SpCond = lag(EXOSpCond_uScm_1_5, 0),
           SpCond_lag1 = lag(EXOSpCond_uScm_1_5, 1),
           SpCond_lead1 = lead(EXOSpCond_uScm_1_5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOSpCond_uScm_1_5 = ifelse(SpCond < 0 & !is.na(SpCond), 3, Flag_EXOSpCond_uScm_1_5)) %>% 
    mutate(Flag_EXOSpCond_uScm_1_5 = ifelse(SpCond < 1 & !is.na(SpCond), 2, Flag_EXOSpCond_uScm_1_5)) %>%
    mutate(EXOSpCond_uScm_1_5 = ifelse(SpCond < 0 & !is.na(SpCond), 0, EXOSpCond_uScm_1_5)) %>% 
    mutate(EXOSpCond_uScm_1_5 = ifelse(SpCond < 1 & !is.na(SpCond), NA, EXOSpCond_uScm_1_5)) %>%
    mutate(EXOSpCond_uScm_1_5 = ifelse((abs(SpCond_lag1 - SpCond) > (2*sd_spcond))  & (abs(SpCond_lead1 - SpCond) > (2*sd_spcond) & !is.na(SpCond)), 
                                     NA, EXOSpCond_uScm_1_5)) %>%   
    mutate(Flag_EXOSpCond_uScm_1_5 = ifelse((abs(SpCond_lag1 - SpCond) > (2*sd_spcond))  & (abs(SpCond_lead1 - SpCond) > (2*sd_spcond)) & !is.na(SpCond), 
                                2, Flag_EXOSpCond_uScm_1_5)) %>% 
    mutate(Flag_EXOSpCond_uScm_1_5 = ifelse(is.na(Flag_EXOSpCond_uScm_1_5),2,Flag_EXOSpCond_uScm_1_5))%>%
    select(-SpCond, -SpCond_lag1, -SpCond_lead1)
  
  # QAQC on major TDS outliers using DWH's method: datapoint set to NA if data is greater than 2*sd different from both previous and following datapoint
  #Remove any data below 1 because it is an outlier and give it a Flag Code of 2
  bvrdata <- bvrdata %>% 
    mutate(TDS = lag(EXOTDS_mgL_1_5, 0),
           TDS_lag1 = lag(EXOTDS_mgL_1_5, 1),
           TDS_lead1 = lead(EXOTDS_mgL_1_5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOTDS_mgL_1_5 = ifelse(TDS < 0 & !is.na(TDS), 3, Flag_EXOTDS_mgL_1_5)) %>% 
    mutate(Flag_EXOTDS_mgL_1_5 = ifelse(TDS < 1 & !is.na(TDS), 2, Flag_EXOTDS_mgL_1_5)) %>%
    mutate(EXOTDS_mgL_1_5 = ifelse(TDS < 0 & !is.na(TDS), 0, EXOTDS_mgL_1_5)) %>%
    mutate(EXOTDS_mgL_1_5 = ifelse(TDS < 1 & !is.na(TDS), NA, EXOTDS_mgL_1_5)) %>% 
    mutate(EXOTDS_mgL_1_5 = ifelse((abs(TDS_lag1 - TDS) > (2*sd_TDS))  & (abs(TDS_lead1 - TDS) > (2*sd_TDS) & !is.na(TDS)), 
                                 NA, EXOTDS_mgL_1_5)) %>%   
    mutate(Flag_EXOTDS_mgL_1_5 = ifelse((abs(TDS_lag1 - TDS) > (2*sd_TDS))  & (abs(TDS_lead1 - TDS) > (2*sd_TDS)) & !is.na(TDS), 
                             2, Flag_EXOTDS_mgL_1_5)) %>% 
    mutate(Flag_EXOTDS_mgL_1_5 = ifelse(is.na(Flag_EXOTDS_mgL_1_5),2,Flag_EXOTDS_mgL_1_5))%>%
    select(-TDS, -TDS_lag1, -TDS_lead1)
  
  # flag EXO sonde sensor data of value above 4 * standard deviation at other times but leave them in the dataset. Probably won't flag anything but for consistenacy. 
  bvrdata <- bvrdata %>%
    mutate(Flag_EXOCond_uScm_1_5 = ifelse(! is.na(EXOCond_uScm_1_5) & abs(EXOCond_uScm_1_5 - mean_cond) > (4*sd_cond),
                              5, Flag_EXOCond_uScm_1_5)) %>%
    mutate(Flag_EXOSpCond_uScm_1_5 = ifelse( ! is.na(EXOSpCond_uScm_1_5) & abs(EXOSpCond_uScm_1_5 - mean_spcond) > (4*sd_spcond),
                                 5, Flag_EXOSpCond_uScm_1_5)) %>%
    mutate(Flag_EXOTDS_mgL_1_5 = ifelse( ! is.na(EXOTDS_mgL_1_5) & abs(EXOTDS_mgL_1_5 - mean_TDS) > (4*sd_TDS),
                              5, Flag_EXOTDS_mgL_1_5)) 
 ##################################################################################################################################### 
 #QAQC for Turbidity and Total Suspended Solids
  
  sd_Turbidity_FNU <- sd(bvrdata$EXOTurbidity_FNU_1_5, na.rm = TRUE) #deteriming the standard deviation of fDOM data 
  sd_TSS_mg <- sd(bvrdata$EXOTSS_mg_1_5, na.rm = TRUE)
  mean_Turbidity_FNU <- mean(bvrdata$EXOTurbidity_FNU_1_5, na.rm = TRUE) #deteriming the standard deviation of fDOM data 
  mean_TSS_mg <- mean(bvrdata$EXOTSS_mg_1_5, na.rm = TRUE)
  
  #Turbidity FNU QAQC
  bvrdata <- bvrdata%>% 
    #mutate(Flag_fDOM = ifelse(is.na(EXOTurbidity_FNU_1_5), 1, 0)) #This creates Flag column for fDOM data, setting all NA's going into QAQC as 1 for missing data, and to 0 for the rest
    mutate(Turbidity_FNU = lag(EXOTurbidity_FNU_1_5, 0),
           Turbidity_FNU_lag1 = lag(EXOTurbidity_FNU_1_5, 1),
           Turbidity_FNU_lead1 = lead(EXOTurbidity_FNU_1_5, 1)) %>%  #These mutates create columns for current Turbidity_FNU, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOTurbidity_FNU_1_5 = ifelse(Turbidity_FNU < 0 & !is.na(Turbidity_FNU), 3, Flag_EXOTurbidity_FNU_1_5),
           EXOTurbidity_FNU_1_5 = ifelse(Turbidity_FNU < 0 & !is.na(Turbidity_FNU), 0, EXOTurbidity_FNU_1_5))%>%
    mutate(EXOTurbidity_FNU_1_5 = ifelse(
      ( abs(Turbidity_FNU_lag1 - Turbidity_FNU) > (2*sd_Turbidity_FNU)   )  & ( abs(Turbidity_FNU_lead1 - Turbidity_FNU) > (2*sd_Turbidity_FNU)  & !is.na(Turbidity_FNU) ), NA, EXOTurbidity_FNU_1_5
    )) %>%  #QAQC to remove outliers for QSU fDOM data 
    mutate(Flag_EXOTurbidity_FNU_1_5 = ifelse(
      ( abs(Turbidity_FNU_lag1 - Turbidity_FNU) > (2*sd_Turbidity_FNU)   )  & ( abs(Turbidity_FNU_lead1 - Turbidity_FNU) > (2*sd_Turbidity_FNU)  & !is.na(Turbidity_FNU)  ), 2, Flag_EXOTurbidity_FNU_1_5
    ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
    mutate(Flag_EXOTurbidity_FNU_1_5 = ifelse(is.na(Flag_EXOTurbidity_FNU_1_5),2,Flag_EXOTurbidity_FNU_1_5))%>%
    select(-Turbidity_FNU, -Turbidity_FNU_lag1, -Turbidity_FNU_lead1)#This removes the columns used to run ifelse statements since they are no longer needed.
  
  
  #TSS mgL QAQC
  bvrdata <- bvrdata%>% 
    #mutate(Flag_fDOM = ifelse(is.na(EXOTSS_mg_1_5), 1, 0)) #This creates Flag column for fDOM data, setting all NA's going into QAQC as 1 for missing data, and to 0 for the rest
    mutate(TSS_mg = lag(EXOTSS_mg_1_5, 0),
           TSS_mg_lag1 = lag(EXOTSS_mg_1_5, 1),
           TSS_mg_lead1 = lead(EXOTSS_mg_1_5, 1)) %>%  #These mutates create columns for current TSS_mg, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOTSS_mg_1_5 = ifelse(TSS_mg < 0 & !is.na(TSS_mg), 3, Flag_EXOTSS_mg_1_5),
           EXOTSS_mg_1_5 = ifelse(TSS_mg < 0 & !is.na(TSS_mg), 0, EXOTSS_mg_1_5))%>%
    mutate(EXOTSS_mg_1_5 = ifelse(
      ( abs(TSS_mg_lag1 - TSS_mg) > (2*sd_TSS_mg)   )  & ( abs(TSS_mg_lead1 - TSS_mg) > (2*sd_TSS_mg)  & !is.na(TSS_mg) ), NA, EXOTSS_mg_1_5
    )) %>%  #QAQC to remove outliers for RFU fDOM data 
    mutate(Flag_EXOTSS_mg_1_5 = ifelse(
      ( abs(TSS_mg_lag1 - TSS_mg) > (2*sd_TSS_mg)   )  & ( abs(TSS_mg_lead1 - TSS_mg) > (2*sd_TSS_mg)  & !is.na(TSS_mg)  ), 2, Flag_EXOTSS_mg_1_5
    ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
    mutate(Flag_EXOTSS_mg_1_5 = ifelse(is.na(Flag_EXOTSS_mg_1_5),2,Flag_EXOTSS_mg_1_5))%>%
    select(-TSS_mg, -TSS_mg_lag1, -TSS_mg_lead1)#This removes the columns used to run ifelse statements since they are no longer needed.
  
  # flag EXO sonde sensor data of value above 4 * standard deviation at other times but leave them in the dataset. Probably won't flag anything but for consistenacy. 
  bvrdata <- bvrdata %>%
    mutate(Flag_EXOTSS_mg_1_5 = ifelse(! is.na(EXOTSS_mg_1_5) & abs(EXOTSS_mg_1_5 - mean_TSS_mg) > (4*sd_TSS_mg),
                                  5, Flag_EXOTSS_mg_1_5)) %>%
    mutate(Flag_EXOTurbidity_FNU_1_5 = ifelse( ! is.na(EXOTurbidity_FNU_1_5) & abs(EXOTurbidity_FNU_1_5 - mean_Turbidity_FNU) > (4*sd_Turbidity_FNU),
                                   5, Flag_EXOTurbidity_FNU_1_5)) 
  
  
  
  
  #####################################################################################################################################   
  #change EXO values to NA if EXO depth is less than 0.5m and Flag as 2
  
  #index only the colummns with EXO at the beginning
  #exo_idx <-grep("^EXO",colnames(bvrdata))
  
  #create list of the Flag columns that need to be changed to 2
  #exo_flag <- c(66:84)
  
  
  #Change the EXO data to NAs when the EXO is above 0.5m and not due to maintenance
  #bvrdata[which(bvrdata$EXO_depth_m < 0.5), exo_idx] <- NA
  #Flag the data that was removed with 2 for outliers
  #bvrdata[which(bvrdata$EXO_depth_m<0.5),exo_flag]<- 2
  
########################################################################################################################### 
  
  #create depth column
  bvrdata=bvrdata%>%mutate(Depth_m_13=Lvl_psi_13*0.70455)#1psi=2.31ft, 1ft=0.305m
  
  #offsets from BVR_
  bvrdata=bvrdata%>%
    mutate(
    depth_1=Depth_m_13-11.82, #Gets depth of thermistor 1
    depth_2=Depth_m_13-11.478, #Gets depth of thermistor 2
    depth_3=Depth_m_13-10.47, #Gets depth of thermistor 3
    depth_4=Depth_m_13-9.423) #Gets depth of thermistor 4. This will have to be recalculated if/when the thermistor comees out of the water. 
  
  
  #change the temp to NA when the thermistor is clearly out of the water which we used to determine the depth of the temp string
  #negative depths are changed to NA
  #when depth is NA then the depth of the sensors can't be calculated and are set to NA
  
  #for thermistor at position 1 when it was out of the water 
  bvrdata=bvrdata%>%
    mutate(Flag_ThermistorTemp_C_1= ifelse(is.na(Lvl_psi_13) & !is.na(ThermistorTemp_C_1) ,2,Flag_ThermistorTemp_C_1))%>%
    mutate(ThermistorTemp_C_1= ifelse(is.na(Lvl_psi_13) & !is.na(ThermistorTemp_C_1) ,NA,ThermistorTemp_C_1))%>%
    mutate(Flag_ThermistorTemp_C_1= ifelse(!is.na(depth_1) & depth_1<0 ,2,Flag_ThermistorTemp_C_1))%>%
    mutate(ThermistorTemp_C_1=ifelse(!is.na(depth_1) & depth_1<0,NA,ThermistorTemp_C_1))
  
  
  #for thermistor at position 2 when it was out of the water or depth is not recorded
  bvrdata=bvrdata%>%
    mutate(Flag_ThermistorTemp_C_2= ifelse(is.na(Lvl_psi_13) & !is.na(ThermistorTemp_C_2),2,Flag_ThermistorTemp_C_2))%>%#this is when the pressure sensor was unplugged
    mutate(ThermistorTemp_C_2= ifelse(is.na(Lvl_psi_13) & !is.na(ThermistorTemp_C_2),NA,ThermistorTemp_C_2))%>%
    mutate(Flag_ThermistorTemp_C_2= ifelse(!is.na(depth_2) & depth_2<0 ,2,Flag_ThermistorTemp_C_2))%>%
    mutate(ThermistorTemp_C_2=ifelse(!is.na(depth_2) & depth_2<0,NA,ThermistorTemp_C_2))
  
  #for thermistor at position 3 when it was out of the water or depth is not recorded
  bvrdata=bvrdata%>%
    mutate(
    Flag_ThermistorTemp_C_3= ifelse(!is.na(depth_3) & depth_3<0 ,2,Flag_ThermistorTemp_C_3),
    ThermistorTemp_C_3=ifelse(!is.na(depth_3) & depth_3<0,NA,ThermistorTemp_C_3))
  
  #for thermistor at position 3 when it was out of the water 
  bvrdata=bvrdata%>%
    mutate(
    Flag_ThermistorTemp_C_4= ifelse(!is.na(depth_4) & depth_4<0 ,2,Flag_ThermistorTemp_C_4),
    ThermistorTemp_C_4=ifelse(!is.na(depth_4) & depth_4<0,NA,ThermistorTemp_C_4))
  
  
  #take out the depth columns for thermisotrs depths after you set the values to NA
  bvrdata=bvrdata%>%
    select(-depth_1,-depth_2, -depth_3, -depth_4)
  
  
  
################################################################################################################################  
  # delete EXO_Date and EXO_Time columns
  bvrdata <- bvrdata %>% select(-EXO_Date, -EXO_Time)
  
  # add Reservoir and Site columns
  bvrdata$Reservoir="BVR"
  bvrdata$Site=50
    
 
  # reorder columns
  bvrdata <- bvrdata %>% select(Reservoir, Site, DateTime, ThermistorTemp_C_1:ThermistorTemp_C_13,
                                RDO_mgL_6, RDOsat_percent_6,
                                RDOTemp_C_6, RDO_mgL_13, RDOsat_percent_13, RDOTemp_C_13,
                                EXOTemp_C_1_5, EXOCond_uScm_1_5, EXOSpCond_uScm_1_5, EXOTDS_mgL_1_5, EXODOsat_percent_1_5,
                                EXODO_mgL_1_5, EXOChla_RFU_1_5, EXOChla_ugL_1_5, EXOBGAPC_RFU_1_5, EXOBGAPC_ugL_1_5,
                                EXOfDOM_RFU_1_5, EXOfDOM_QSU_1_5,EXOTurbidity_FNU_1_5, EXOTSS_mg_1_5, EXO_pressure_psi, EXO_depth_m, EXO_battery_V, EXO_cablepower_V,
                                EXO_wiper_V, Lvl_psi_13,Depth_m_13, LvlTemp_C_13, RECORD, CR6_Batt_V, CR6Panel_Temp_C,
                                Flag_ThermistorTemp_C_1:Flag_ThermistorTemp_C_13,Flag_RDO_mgL_6, Flag_RDOsat_percent_6, Flag_RDOTemp_C_6,
                                Flag_RDO_mgL_13, Flag_RDOsat_percent_13, Flag_RDOTemp_C_13,Flag_EXOTemp_C_1_5, Flag_EXOCond_uScm_1_5, Flag_EXOSpCond_uScm_1_5,Flag_EXOTDS_mgL_1_5,
                                Flag_EXODOsat_percent_1_5, Flag_EXODO_mgL_1_5, Flag_EXOChla_RFU_1_5,Flag_EXOChla_ugL_1_5, Flag_EXOBGAPC_RFU_1_5,Flag_EXOBGAPC_ugL_1_5,
                                Flag_EXOfDOM_RFU_1_5,Flag_EXOfDOM_QSU_1_5, Flag_EXOTurbidity_FNU_1_5, Flag_EXOTSS_mg_1_5,
                                Flag_EXO_pressure_psi, Flag_EXO_depth_m, Flag_EXO_battery_V, Flag_EXO_cablepower_V,Flag_EXO_wiper_V,Flag_Lvl_psi_13)

  # replace NaNs with NAs
  bvrdata[is.na(bvrdata)] <- NA
  

  #order by date and time
  bvrdata <- bvrdata[order(bvrdata$DateTime),]
  
  
  
  # convert datetimes to characters so that they are properly formatted in the output file
  bvrdata$DateTime <- as.character(bvrdata$DateTime)
 
 
  # write to output file
  write.csv(bvrdata, output_file, row.names = FALSE, quote=FALSE)
  
 
  
  # ###Prep RemoveMet for final file version
  # 
  #   names(log)=c("Station", "DateTime_start","DateTime_end", "Parameter", "ColumnNumber", "Flag", "Notes")
  #   
  #   log=log%>%
  #   mutate(Reservoir="BVR")%>%
  #   mutate(Site=50)%>%
  #   mutate(Station="BVR_sensor_string")%>%
  #   select(Reservoir, Site, Station, DateTime_start, DateTime_end, Parameter, ColumnNumber, Flag, Notes)
  # 
  # 
  # write.csv(log, output2_file, row.names = FALSE, quote=FALSE)
}

# example usage


#qaqc('https://github.com/FLARE-forecast/BVRE-data/raw/bvre-platform-data/BVRplatform.csv',
#      'https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/BVRplatform_manual_2020.csv',
#      "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data/BVR_maintenance_log.txt",
#       "BVRplatform_clean.csv", 
#     "BVR_Maintenance_2020.csv")

