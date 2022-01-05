

qaqc <- function(data_file, maintenance_file, output_file)
{
 
  CATPRES_COL_NAMES = c("DateTime", "Record", "CR3000_Batt_V", "CR3000Panel_Temp_C", 
                        "ThermistorTemp_C_1", "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4",
                        "ThermistorTemp_C_5", "ThermistorTemp_C_6", "ThermistorTemp_C_7", "ThermistorTemp_C_8",
                        "ThermistorTemp_C_9","ThermistorTemp_C_10","ThermistorTemp_C_11", "ThermistorTemp_C_12",
                        "ThermistorTemp_C_13","EXO_Date_1", "EXO_Time_1", "EXOTemp_C_1", "EXOCond_uScm_1",
                        "EXOSpCond_uScm_1", "EXOTDS_mgL_1", "EXODOsat_percent_1", "EXODO_mgL_1", "EXOChla_RFU_1",
                        "EXOChla_ugL_1", "EXOBGAPC_RFU_1", "EXOBGAPC_ugL_1", "EXOfDOM_RFU_1", "EXOfDOM_QSU_1",
                        "EXO_pressure_psi_1", "EXO_depth_m_1", "EXO_battery_V_1", "EXO_cablepower_V_1", "EXO_wiper_V_1",
                        "EXO_Date_9", "EXO_Time_9", "EXOTemp_C_9", "EXOCond_uScm_9",
                        "EXOSpCond_uScm_9", "EXOTDS_mgL_9", "EXODOsat_percent_9", "EXODO_mgL_9", 
                        "EXOfDOM_RFU_9", "EXOfDOM_QSU_9","EXO_pressure_psi_9", "EXO_depth_m_9", "EXO_battery_V_9",
                         "EXO_cablepower_V_9", "EXO_wiper_V_9","Lvl_psi_13", "LvlTemp_C_13")
  
  
  # EXO sonde sensor data that differs from the mean by more than the standard deviation multiplied by this factor will
  # either be replaced with NA and flagged (if between 2018-10-01 and 2019-03-01) or just flagged (otherwise)
  EXO_FOULING_FACTOR <- 4
  #EXO_FOULING_FACTORfor2 <- 2
  
  
  
  
  # read catwalk data and maintenance log
  # NOTE: date-times throughout this script are processed as UTC
  ccrwater <- read_csv(data_file, skip = 1, col_names = CATPRES_COL_NAMES,
                      col_types = cols(.default = col_double(), DateTime = col_datetime()))
  
  ccr2=ccrwater
  
 #read in manual data from the data logger to fill in missing gaps
  
  #ccrwater2 <- read_csv(data2_file, skip = 1, col_names = CATPRES_COL_NAMES,
  #                     col_types = cols(.default = col_double(), DateTime = col_datetime()))
  
  #ccrwater <-rbind(ccrwater,ccrwater2)
  
################################################################################################################
  #check for gaps and missing data
  #order data by timestamp
  ccr2=ccr2[order(ccr2$DateTime),]
  ccr2$DOY=yday(ccr2$DateTime)
  
  
  #check record for gaps
  #daily record gaps by day of year
  for(i in 2:nrow(ccr2)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
    if(ccr2$DOY[i]-ccr2$DOY[i-1]>1){
      print(c(ccr2$DateTime[i-1],ccr2$DateTime[i]))
    }
  }
  # #sub-daily record gaps by record number
  for(i in 2:length(ccr2$Record)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
    if(abs(ccr2$Record[i]-ccr2$Record[i-1])>1){
      print(c(ccr2$DateTime[i-1],ccr2$DateTime[i]))
    }
  }
  
###################################################### 
#Read in the maintneance log 
  
  log <- read_csv(maintenance_file, col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  ))
  
  
  
  # remove NaN data at beginning when data when no sensors were connected to the data logger
  ccrwater <- ccrwater %>% filter(DateTime >= ymd_hms("2021-04-09 14:00:00"))
  
  # add flag columns
  ccrwater$Flag_Temp_1 <- 0
  ccrwater$Flag_Temp_2 <- 0
  ccrwater$Flag_Temp_3 <- 0
  ccrwater$Flag_Temp_4 <- 0
  ccrwater$Flag_Temp_5 <- 0
  ccrwater$Flag_Temp_6 <- 0
  ccrwater$Flag_Temp_7 <- 0
  ccrwater$Flag_Temp_8 <- 0
  ccrwater$Flag_Temp_9 <- 0
  ccrwater$Flag_Temp_10 <- 0
  ccrwater$Flag_Temp_11 <- 0
  ccrwater$Flag_Temp_12 <- 0
  ccrwater$Flag_Temp_13 <- 0
  ccrwater$Flag_Pres_13 <-0
  ccrwater$Flag_EXOTemp_1 <- 0
  ccrwater$Flag_EXOCond_1 <- 0
  ccrwater$Flag_EXOSpCond_1 <- 0
  ccrwater$Flag_EXOTDS_1 <- 0
  ccrwater$Flag_EXODO_sat_1 <- 0
  ccrwater$Flag_EXODO_obs_1 <- 0
  ccrwater$Flag_EXOChla_RFU_1 <- 0
  ccrwater$Flag_EXOChla_ugL_1 <-0
  ccrwater$Flag_EXOPhyco_RFU_1 <- 0
  ccrwater$Flag_EXOPhyco_ugL_1 <- 0
  ccrwater$Flag_EXOfDOM_1 <- 0
  ccrwater$Flag_EXOPres_1 <- 0
  ccrwater$Flag_EXOdep_1 <-0
  ccrwater$Flag_EXObat_1 <-0
  ccrwater$Flag_EXOcab_1 <-0
  ccrwater$Flag_EXOwip_1 <-0
  ccrwater$Flag_EXOTemp_9 <- 0
  ccrwater$Flag_EXOCond_9 <- 0
  ccrwater$Flag_EXOSpCond_9 <- 0
  ccrwater$Flag_EXOTDS_9 <- 0
  ccrwater$Flag_EXODO_sat_9 <- 0
  ccrwater$Flag_EXODO_obs_9 <- 0
  ccrwater$Flag_EXOChla_9 <- 0
  ccrwater$Flag_EXOPhyco_9 <- 0
  ccrwater$Flag_EXOfDOM_9 <- 0
  ccrwater$Flag_EXOPres_9 <- 0
  ccrwater$Flag_EXOdep_9 <-0
  ccrwater$Flag_EXObat_9 <-0
  ccrwater$Flag_EXOcab_9 <-0
  ccrwater$Flag_EXOwip_9 <-0
  
  

  # change the flags if there are NAs values before the maintenance log then the data is missing and flagged as 7 for missing values
  ccrwater <- ccrwater%>%
    mutate(
      Flag_Temp_1 = ifelse(is.na(ThermistorTemp_C_1) & 
                             Flag_Temp_1==0,7, Flag_Temp_1),
      Flag_Temp_2 = ifelse(is.na(ThermistorTemp_C_2) & 
                             Flag_Temp_2==0,7, Flag_Temp_2),
      Flag_Temp_3 = ifelse(is.na(ThermistorTemp_C_3) & 
                             Flag_Temp_3==0,7, Flag_Temp_3),
      Flag_Temp_4 = ifelse(is.na(ThermistorTemp_C_4) & 
                             Flag_Temp_4==0,7, Flag_Temp_4),
      Flag_Temp_5 = ifelse(is.na(ThermistorTemp_C_5) & 
                             Flag_Temp_5==0,7, Flag_Temp_5),
      Flag_Temp_6 = ifelse(is.na(ThermistorTemp_C_6) & 
                             Flag_Temp_6==0,7, Flag_Temp_6),
      Flag_Temp_7 = ifelse(is.na(ThermistorTemp_C_7) & 
                             Flag_Temp_7==0,7, Flag_Temp_7),
      Flag_Temp_8 = ifelse(is.na(ThermistorTemp_C_8) & 
                             Flag_Temp_8==0,7, Flag_Temp_8),
      Flag_Temp_9 = ifelse(is.na(ThermistorTemp_C_9) & 
                             Flag_Temp_9==0,7, Flag_Temp_9),
      Flag_Temp_10 = ifelse(is.na(ThermistorTemp_C_10) & 
                             Flag_Temp_10==0,7, Flag_Temp_10),
      Flag_Temp_11 = ifelse(is.na(ThermistorTemp_C_11) & 
                             Flag_Temp_11==0,7, Flag_Temp_11),
      Flag_Temp_12 = ifelse(is.na(ThermistorTemp_C_12) & 
                             Flag_Temp_12==0,7, Flag_Temp_12),
      Flag_Temp_13 = ifelse(is.na(ThermistorTemp_C_13) & 
                             Flag_Temp_13==0,7, Flag_Temp_13),
      Flag_Pres_13 = ifelse(is.na(Lvl_psi_13) & 
                           Flag_Pres_13==0, 7, Flag_Pres_13),
      Flag_EXODO_obs_1 = ifelse(is.na(EXODO_mgL_1) &
                            Flag_EXODO_obs_1==0, 7, Flag_EXODO_obs_1),
      Flag_EXODO_sat_1 = ifelse(is.na(EXODOsat_percent_1) &
                               Flag_EXODO_sat_1==0, 7, Flag_EXODO_sat_1),
      Flag_EXOTemp_1 = ifelse( is.na(EXOTemp_C_1) & 
                            Flag_EXOTemp_1==0, 7, Flag_EXOTemp_1),
      Flag_EXOChla_RFU_1 = ifelse( is.na(EXOChla_RFU_1)  &
                              Flag_EXOChla_RFU_1==0, 7, Flag_EXOChla_RFU_1),
      Flag_EXOChla_ugL_1 = ifelse(  is.na(EXOChla_ugL_1) &
                                 Flag_EXOChla_ugL_1==0, 7, Flag_EXOChla_ugL_1),
      Flag_EXOPhyco_RFU_1 = ifelse( is.na(EXOBGAPC_RFU_1)  &
                                  Flag_EXOPhyco_RFU_1==0, 7, Flag_EXOPhyco_RFU_1),
      Flag_EXOPhyco_ugL_1 = ifelse( is.na(EXOBGAPC_ugL_1) &
                               Flag_EXOPhyco_ugL_1==0, 7, Flag_EXOPhyco_ugL_1),
      Flag_EXOfDOM_1 = ifelse( is.na(EXOfDOM_RFU_1) & is.na(EXOfDOM_QSU_1) &
                              Flag_EXOfDOM_1==0, 7, Flag_EXOfDOM_1),
      Flag_EXOTDS_1 = ifelse( is.na(EXOTDS_mgL_1) & 
                           Flag_EXOTDS_1==0, 7, Flag_EXOTDS_1),
      Flag_EXOCond_1 = ifelse( is.na(EXOCond_uScm_1) & 
                            Flag_EXOCond_1==0, 7, Flag_EXOCond_1),
      Flag_EXOSpCond_1 = ifelse( is.na(EXOSpCond_uScm_1) & 
                              Flag_EXOSpCond_1==0, 7, Flag_EXOSpCond_1),
      Flag_EXOPres_1 = ifelse( is.na(EXO_pressure_psi_1) &
                                 Flag_EXOPres_1==0, 7, Flag_EXOPres_1),
      Flag_EXOdep_1 = ifelse( is.na(EXO_depth_m_1) & 
                                Flag_EXOdep_1==0, 7, Flag_EXOdep_1),
      Flag_EXObat_1 = ifelse( is.na(EXO_battery_V_1)&
                                Flag_EXObat_1==0, 7, Flag_EXObat_1),
      Flag_EXOcab_1 = ifelse( is.na(EXO_cablepower_V_1) & 
                                Flag_EXOcab_1==0, 7, Flag_EXOcab_1),
      Flag_EXOwip_1 = ifelse( is.na(EXO_wiper_V_1) & 
                                Flag_EXOwip_1==0, 7, Flag_EXOwip_1),
      Flag_EXODO_obs_9 = ifelse(is.na(EXODO_mgL_9) &
                                  Flag_EXODO_obs_9==0, 7, Flag_EXODO_obs_9),
      Flag_EXODO_sat_9 = ifelse(is.na(EXODOsat_percent_9) &
                                  Flag_EXODO_sat_9==0, 7, Flag_EXODO_sat_9),
      Flag_EXOTemp_9 = ifelse( is.na(EXOTemp_C_9) & 
                                 Flag_EXOTemp_9==0, 7, Flag_EXOTemp_9),
      Flag_EXOfDOM_9 = ifelse( is.na(EXOfDOM_RFU_9) & is.na(EXOfDOM_QSU_9) &
                                 Flag_EXOfDOM_9==0, 7, Flag_EXOfDOM_9),
      Flag_EXOTDS_9 = ifelse( is.na(EXOTDS_mgL_9) & 
                                Flag_EXOTDS_9==0, 7, Flag_EXOTDS_9),
      Flag_EXOCond_9 = ifelse( is.na(EXOCond_uScm_9) & 
                                 Flag_EXOCond_9==0, 7, Flag_EXOCond_9),
      Flag_EXOSpCond_9 = ifelse( is.na(EXOSpCond_uScm_9) & 
                                   Flag_EXOSpCond_9==0, 7, Flag_EXOSpCond_9),
      Flag_EXOPres_9 = ifelse( is.na(EXO_pressure_psi_9) &
                                 Flag_EXOPres_9==0, 7, Flag_EXOPres_9),
      Flag_EXOdep_9 = ifelse( is.na(EXO_depth_m_9) & 
                                Flag_EXOdep_9==0, 7, Flag_EXOdep_9),
      Flag_EXObat_9 = ifelse( is.na(EXO_battery_V_9)&
                                Flag_EXObat_9==0, 7, Flag_EXObat_9),
      Flag_EXOcab_9 = ifelse( is.na(EXO_cablepower_V_9) & 
                                Flag_EXOcab_9==0, 7, Flag_EXOcab_9),
      Flag_EXOwip_9 = ifelse( is.na(EXO_wiper_V_9) & 
                                Flag_EXOwip_9==0, 7, Flag_EXOwip_9)
      
    )
  
 
  # modify ccrwater based on the information in the log
  for(i in 1:nrow(log))
  #for(i in 4)  
  {
    # get start and end time of one maintenance event
    start <- log$TIMESTAMP_start[i]
    end <- log$TIMESTAMP_end[i]
    
    
    # get indices of columns affected by maintenance
    if(grepl("^\\d+$", log$colnumber[i])) # single num
    {
      maintenance_cols <- intersect(c(5:53), as.integer(log$colnumber[i]))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", log$colnumber[i])) # c(x;y;...)
    {
      maintenance_cols <- intersect(c(5:53), as.integer(unlist(regmatches(log$colnumber[i],
                                                                          gregexpr("\\d+", log$colnumber[i])))))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", log$colnumber[i])) # c(x:y)
    {
      bounds <- as.integer(unlist(regmatches(log$colnumber[i], gregexpr("\\d+", log$colnumber[i]))))
      maintenance_cols <- intersect(c(5:53), c(bounds[1]:bounds[2]))
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
    maintenance_cols <- setdiff(maintenance_cols, c(18, 19))
    
    if(length(maintenance_cols) == 0)
    {
      warning(paste("Did not parse any valid data columns in row", i, "of the maintenance log. Valid columns have",
                    "indices 2 through 39, excluding 21 and 22, which are deleted by this script. Skipping maintenance for that row."))
      next
    }
  

    
    # replace relevant data with NAs and set "all" flag while maintenance was in effect
    ccrwater[ccrwater$DateTime >= start & ccrwater$DateTime <= end, maintenance_cols] <- NA
    if(39 %in% bounds){
      ccrwater[ccrwater$DateTime >= start & ccrwater$DateTime <= end, 84:97] <- 1
    }
    else if( 5 %in% bounds){
      ccrwater[ccrwater$DateTime >= start & ccrwater$DateTime <= end, 54:66] <- 1
    }
    else if (20 %in% bounds){
      ccrwater[ccrwater$DateTime >= start & ccrwater$DateTime <= end, 68:83] <- 1 
    }
    else if (52 %in% bounds){
      ccrwater[ccrwater$DateTime >= start & ccrwater$DateTime <= end, 67] <- 1 
    }
    else{
      warning(paste("Column row not in bounds identified above. Fix by adding another else if statement",
                    "or find another way to fix it."))
      next
    }
  }
##################################################################################################################
#2 hour adjustment period for DO sensors- Remove values up to 2 hours after temp string has been pulled up
  
#All based on times in the maintenance log
  
    maint = read.csv(maintenance_file)
    maint_all = maint[grepl("EXO",maint$parameter),] #creating file "maint" with all sensor string maintenance
    maint_all = maint_all%>%
    filter(flag==1)%>%
    filter(parameter!="fdom")
    clean_start<-as.POSIXct(maint$TIMESTAMP_start, tz = "UTC")#changed the time tz to make sure there is no conflict
    clean_end <- as.POSIXct(maint$TIMESTAMP_end, tz = "UTC")

    ADJ_PERIOD = 2*60*60 #amount of time to stabilization after cleaning in seconds

    for (i in 1:length(clean_start)){ #Set all data during cleaning and for ADJ_PERIOD after to NA
      if (maint$colnumber[i]==" c(20:36)"){
      ccrwater$EXODO_mgL_1[ccrwater$DateTime>clean_start[i]&ccrwater$DateTime<(clean_end[i]+ADJ_PERIOD)] <- NA
      ccrwater$EXODOsat_percent_1[ccrwater$DateTime>clean_start[i]&ccrwater$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
      ccrwater$Flag_EXODO_obs_1[ccrwater$DateTime>clean_start[i]&ccrwater$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
      ccrwater$Flag_EXODO_sat_1[ccrwater$DateTime>clean_start[i]&ccrwater$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
     
      }
      
      else if(maint$colnumber[i]==" c(39:51)"){
        ccrwater$EXODO_mgL_9[ccrwater$DateTime>clean_start[i]&ccrwater$DateTime<(clean_end[i]+ADJ_PERIOD)] <- NA
        ccrwater$EXODOsat_percent_9[ccrwater$DateTime>clean_start[i]&ccrwater$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
        ccrwater$Flag_EXODO_obs_9[ccrwater$DateTime>clean_start[i]&ccrwater$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
        ccrwater$Flag_EXODO_sat_9[ccrwater$DateTime>clean_start[i]&ccrwater$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
        next
      }
      }
    


##################################################################################################################  
  #Set negative DO values to 0 and Flag_DO for NA values for both EXO at 1m and 9m 
    
  ccrwater <- ccrwater %>%  
    mutate(
           Flag_EXODO_obs_1 = ifelse(EXODO_mgL_1 < 0 & !is.na(EXODO_mgL_1), 3, Flag_EXODO_obs_1), #and for 1m
           Flag_EXODO_sat_1 = ifelse(EXODOsat_percent_1 < 0 & !is.na(EXODOsat_percent_1) , 3, Flag_EXODO_sat_1),
           EXODO_mgL_1 = ifelse(EXODO_mgL_1 < 0, 0, EXODO_mgL_1),
           EXODOsat_percent_1 = ifelse(EXODOsat_percent_1 <0, 0, EXODOsat_percent_1),
           Flag_EXODO_obs_9 = ifelse(EXODO_mgL_9 < 0 & !is.na(EXODO_mgL_9), 3, Flag_EXODO_obs_9), #and for 9m
           Flag_EXODO_sat_9 = ifelse(EXODOsat_percent_9 < 0 & !is.na(EXODOsat_percent_9) , 3, Flag_EXODO_sat_9),
           EXODO_mgL_9 = ifelse(EXODO_mgL_9 < 0, 0, EXODO_mgL_9),
           EXODOsat_percent_9 = ifelse(EXODOsat_percent_9 <0, 0, EXODOsat_percent_9)
    )
  
########################################################################################################################  
    # find chla and phyo on the EXO sonde sensor data that differs from the mean by more than the standard deviation times a given factor, and
    # replace with NAs between October 2018 and March 2019, due to sensor fouling
    Chla_RFU_1_mean <- mean(ccrwater$EXOChla_RFU_1, na.rm = TRUE)
    Chla_ugL_1_mean <- mean(ccrwater$EXOChla_ugL_1, na.rm = TRUE)
    BGAPC_RFU_1_mean <- mean(ccrwater$EXOBGAPC_RFU_1, na.rm = TRUE)
    BGAPC_ugL_1_mean <- mean(ccrwater$EXOBGAPC_ugL_1, na.rm = TRUE)
    Chla_RFU_1_threshold <- EXO_FOULING_FACTOR * sd(ccrwater$EXOChla_RFU_1, na.rm = TRUE)
    Chla_ugL_1_threshold <- EXO_FOULING_FACTOR * sd(ccrwater$EXOChla_ugL_1, na.rm = TRUE)
    BGAPC_RFU_1_threshold <- EXO_FOULING_FACTOR * sd(ccrwater$EXOBGAPC_RFU_1, na.rm = TRUE)
    BGAPC_ugL_1_threshold <- EXO_FOULING_FACTOR * sd(ccrwater$EXOBGAPC_ugL_1, na.rm = TRUE)
  
  # QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
 
    ccrwater <- ccrwater %>% #one for ugL
    mutate(Chla_ugL = lag(EXOChla_ugL_1, 0),
           Chla_lag1_ugL = lag(EXOChla_ugL_1, 1),
           Chla_lead1_ugL = lead(EXOChla_ugL_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOChla_ugL_1 = ifelse(Chla_ugL < 0 & !is.na(Chla_ugL), 3, Flag_EXOChla_ugL_1)) %>% 
    mutate(EXOChla_ugL_1 = ifelse(Chla_ugL < 0 & !is.na(Chla_ugL), 0, EXOChla_ugL_1)) %>% 
    mutate(EXOChla_ugL_1 = ifelse((abs(Chla_lag1_ugL - Chla_ugL) > (Chla_ugL_1_threshold))  & (abs(Chla_lead1_ugL - Chla_ugL) > (Chla_ugL_1_threshold) & !is.na(Chla_ugL)), 
                                  NA, EXOChla_ugL_1)) %>%   
    mutate(Flag_EXOChla_ugL_1 = ifelse((abs(Chla_lag1_ugL - Chla_ugL) > (Chla_ugL_1_threshold))  & (abs(Chla_lead1_ugL - Chla_ugL) > (Chla_ugL_1_threshold)) & !is.na(Chla_ugL), 
                              2, Flag_EXOChla_ugL_1)) %>% 
    select(-Chla_ugL, -Chla_lag1_ugL, -Chla_lead1_ugL)
    
    ccrwater <- ccrwater %>% #one for RFU 
      mutate(Chla_RFU = lag(EXOChla_RFU_1, 0),
             Chla_lag1_RFU = lag(EXOChla_RFU_1, 1),
             Chla_lead1_RFU = lead(EXOChla_RFU_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
      mutate(Flag_EXOChla_RFU_1 = ifelse(Chla_RFU < 0 & !is.na(Chla_RFU), 3, Flag_EXOChla_RFU_1)) %>% 
      mutate(EXOChla_RFU_1 = ifelse(Chla_RFU < 0 & !is.na(Chla_RFU), 0, EXOChla_RFU_1)) %>% 
      mutate(EXOChla_RFU_1 = ifelse((abs(Chla_lag1_RFU - Chla_RFU) > (Chla_RFU_1_threshold))  & (abs(Chla_lead1_RFU - Chla_RFU) > (Chla_RFU_1_threshold) & !is.na(Chla_RFU)), 
                                    NA, EXOChla_RFU_1)) %>%   
      mutate(Flag_EXOChla_RFU_1 = ifelse((abs(Chla_lag1_RFU - Chla_RFU) > (Chla_RFU_1_threshold))  & (abs(Chla_lead1_RFU - Chla_RFU) > (Chla_RFU_1_threshold)) & !is.na(Chla_RFU), 
                                         2, Flag_EXOChla_RFU_1)) %>% 
      select(-Chla_RFU, -Chla_lag1_RFU, -Chla_lead1_RFU)
  
  # QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
  ccrwater <- ccrwater %>% 
    mutate(phyco_ugL = lag(EXOBGAPC_ugL_1, 0),
           phyco_lag1_ugL = lag(EXOBGAPC_ugL_1, 1),
           phyco_lead1_ugL = lead(EXOBGAPC_ugL_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOPhyco_ugL_1 = ifelse(phyco_ugL < 0 & !is.na(phyco_ugL), 3, Flag_EXOPhyco_ugL_1)) %>% 
    mutate(EXOBGAPC_ugL_1 = ifelse(phyco_ugL < 0 & !is.na(phyco_ugL), 0, EXOBGAPC_ugL_1)) %>% 
    mutate(EXOBGAPC_ugL_1 = ifelse((abs(phyco_lag1_ugL - phyco_ugL) > (BGAPC_ugL_1_threshold))  & (abs(phyco_lead1_ugL - phyco_ugL) > (BGAPC_ugL_1_threshold) & !is.na(phyco_ugL)), 
                                   NA, EXOBGAPC_ugL_1)) %>%   
    mutate(Flag_EXOPhyco_ugL_1 = ifelse((abs(phyco_lag1_ugL - phyco_ugL) > (BGAPC_ugL_1_threshold))  & (abs(phyco_lead1_ugL - phyco_ugL) > (BGAPC_ugL_1_threshold) & !is.na(phyco_ugL)), 
                               2, Flag_EXOPhyco_ugL_1)) %>%
    select(-phyco_ugL, -phyco_lag1_ugL, -phyco_lead1_ugL)
  
  ccrwater <- ccrwater %>% 
    mutate(phyco_RFU = lag(EXOBGAPC_RFU_1, 0),
           phyco_lag1_RFU = lag(EXOBGAPC_RFU_1, 1),
           phyco_lead1_RFU = lead(EXOBGAPC_RFU_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOPhyco_RFU_1 = ifelse(phyco_RFU < 0 & !is.na(phyco_RFU), 3, Flag_EXOPhyco_RFU_1)) %>% 
    mutate(EXOBGAPC_RFU_1 = ifelse(phyco_RFU < 0 & !is.na(phyco_RFU), 0, EXOBGAPC_RFU_1)) %>% 
    mutate(EXOBGAPC_RFU_1 = ifelse((abs(phyco_lag1_RFU - phyco_RFU) > (BGAPC_RFU_1_threshold))  & (abs(phyco_lead1_RFU - phyco_RFU) > (BGAPC_RFU_1_threshold) & !is.na(phyco_RFU)), 
                                   NA, EXOBGAPC_RFU_1)) %>%   
    mutate(Flag_EXOPhyco_RFU_1 = ifelse((abs(phyco_lag1_RFU - phyco_RFU) > (BGAPC_RFU_1_threshold))  & (abs(phyco_lead1_RFU - phyco_RFU) > (BGAPC_RFU_1_threshold) & !is.na(phyco_RFU)), 
                                        2, Flag_EXOPhyco_RFU_1)) %>%
    select(-phyco_RFU, -phyco_lag1_RFU, -phyco_lead1_RFU)
  
  

  # flag EXO sonde sensor data of value above 4 * standard deviation at other times but leave them in the dataset
  ccrwater <- ccrwater %>%
    mutate(Flag_EXOPhyco_ugL_1 = ifelse(! is.na(EXOBGAPC_ugL_1) & abs(EXOBGAPC_ugL_1 - BGAPC_ugL_1_mean) > BGAPC_ugL_1_threshold,
                               5, Flag_EXOPhyco_ugL_1),
           Flag_EXOPhyco_RFU_1 = ifelse(! is.na(EXOBGAPC_RFU_1) & abs(EXOBGAPC_RFU_1 - BGAPC_RFU_1_mean) > BGAPC_RFU_1_threshold |
                                    5, Flag_EXOPhyco_RFU_1),
            Flag_EXOChla_ugL_1 = ifelse(! is.na(EXOChla_ugL_1) & abs(EXOChla_ugL_1 - Chla_ugL_1_mean) > Chla_ugL_1_threshold |
                               ! is.na(EXOChla_RFU_1) & abs(EXOChla_RFU_1 - Chla_RFU_1_mean) > Chla_RFU_1_threshold,
                             5, Flag_EXOChla_ugL_1),
           Flag_EXOChla_RFU_1 = ifelse(! is.na(EXOChla_RFU_1) & abs(EXOChla_RFU_1 - Chla_RFU_1_mean) > Chla_RFU_1_threshold,
                                       5, Flag_EXOChla_RFU_1))

  
  


######################################################################################################################  
  # fdom qaqc----
  # QAQC from DWH to remove major outliers from fDOM data that are 2 sd's greater than the previous and following datapoint
  sd_fDOM_1 <- sd(ccrwater$EXOfDOM_QSU_1, na.rm = TRUE) #deteriming the standard deviation of fDOM data 
  sd_fDOM_9 <- sd(ccrwater$EXOfDOM_QSU_9, na.rm = TRUE)
  
  #
  ccrwater <- ccrwater%>% 
  mutate(fDOM1 = lag(EXOfDOM_QSU_1, 0),
         fDOM_lag1 = lag(EXOfDOM_QSU_1, 1),
         fDOM_lead1 = lead(EXOfDOM_QSU_1, 1)) %>%   #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOfDOM_1 = ifelse(fDOM1 < 0 & !is.na(fDOM1), 2, Flag_EXOfDOM_1),
           EXOfDOM_QSU_1 = ifelse(fDOM1 < 0 & !is.na(fDOM1), NA, EXOfDOM_QSU_1),
           EXOfDOM_RFU_1 = ifelse(fDOM1 < 0 & !is.na(fDOM1), NA, EXOfDOM_RFU_1))%>%
           #These mutates are QAQCing for negative fDOM QSU values and setting these to NA and making a flag for these. This was done outside of the 2 sd deviation rule because there were two negative points in a row and one was not removed with the follwoing if else statements. 
    mutate(EXOfDOM_QSU_1 = ifelse(
      ( abs(fDOM_lag1 - fDOM1) > (2*sd_fDOM_1)   )  & ( abs(fDOM_lead1 - fDOM1) > (2*sd_fDOM_1)  & !is.na(fDOM1) ), NA, EXOfDOM_QSU_1
    )) %>%   #QAQC to remove outliers for QSU fDOM data 
    mutate(EXOfDOM_RFU_1 = ifelse(
      ( abs(fDOM_lag1 - fDOM1) > (2*sd_fDOM_1)   )  & ( abs(fDOM_lead1 - fDOM1) > (2*sd_fDOM_1)  & !is.na(fDOM1)  ), NA, EXOfDOM_RFU_1
    )) %>% #QAQC to remove outliers for RFU fDOM data
    mutate(Flag_EXOfDOM_1 = ifelse(
      ( abs(fDOM_lag1 - fDOM1) > (2*sd_fDOM_1)   )  & ( abs(fDOM_lead1 - fDOM1) > (2*sd_fDOM_1)  & !is.na(fDOM1)  ), 2, Flag_EXOfDOM_1
    ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
    select(-fDOM1, -fDOM_lag1, -fDOM_lead1)#This removes the columns used to run ifelse statements since they are no longer needed.
    
  ccrwater <- ccrwater%>%
  mutate(fDOM9 = lag(EXOfDOM_QSU_9, 0),
         fDOM_lag9 = lag(EXOfDOM_QSU_9, 1),
         fDOM_lead9 = lead(EXOfDOM_QSU_9, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOfDOM_9 = ifelse(fDOM9 < 0 & !is.na(fDOM9), 2, Flag_EXOfDOM_9),
           EXOfDOM_QSU_9 = ifelse(fDOM9 < 0 & !is.na(fDOM9), NA, EXOfDOM_QSU_9),
           EXOfDOM_RFU_9 = ifelse(fDOM9 < 0 & !is.na(fDOM9), NA, EXOfDOM_RFU_9))%>%
    #Flag_EXOfDOM_9 = ifelse(fDOM < 0, 2, Flag_EXOfDOM_9)) %>% #These mutates are QAQCing for negative fDOM QSU values and setting these to NA and making a flag for these. This was done outside of the 2 sd deviation rule because there were two negative points in a row and one was not removed with the follwoing if else statements. 
    mutate(EXOfDOM_QSU_9 = ifelse(
      ( abs(fDOM_lag9 - fDOM9) > (2*sd_fDOM_9)   )  & ( abs(fDOM_lead9 - fDOM9) > (2*sd_fDOM_9)  & !is.na(fDOM9) ), NA, EXOfDOM_QSU_9
    )) %>%  #QAQC to remove outliers for QSU fDOM data 
    mutate(EXOfDOM_RFU_9 = ifelse(
      ( abs(fDOM_lag9 - fDOM9) > (2*sd_fDOM_9)   )  & ( abs(fDOM_lead9 - fDOM9) > (2*sd_fDOM_9)  & !is.na(fDOM9)  ), NA, EXOfDOM_RFU_9
    )) %>% #QAQC to remove outliers for RFU fDOM data
    mutate(Flag_EXOfDOM_9 = ifelse(
      ( abs(fDOM_lag9 - fDOM9) > (2*sd_fDOM_9)   )  & ( abs(fDOM_lead9 - fDOM9) > (2*sd_fDOM_9)  & !is.na(fDOM9)  ), 2, Flag_EXOfDOM_9
    ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
    select(-fDOM9, -fDOM_lag9, -fDOM_lead9)#This removes the columns used to run ifelse statements since they are no longer needed.
#####################################################################################################################################  
#QAQC from DWH to remove major outliers from condnuctity, specific conductivity and TDS data that are 2 sd's greater than the previous and following datapoint
  

  sd_2_cond_1 <- 2*sd(ccrwater$EXOCond_uScm_1, na.rm = TRUE)
  sd_2_cond_9 <- 2*sd(ccrwater$EXOCond_uScm_9, na.rm = TRUE)
  sd_2_spcond_1 <- 2*sd(ccrwater$EXOSpCond_uScm_1, na.rm = TRUE)
  sd_2_spcond_9 <- 2*sd(ccrwater$EXOSpCond_uScm_9, na.rm = TRUE)
  sd_2_TDS_1 <- 2*sd(ccrwater$EXOTDS_mgL_1, na.rm = TRUE)
  sd_2_TDS_9 <- 2*sd(ccrwater$EXOTDS_mgL_9, na.rm = TRUE) 
  
  # QAQC on major conductivity outliers using DWH's method: datapoint set to NA if data is greater than 2*sd different from both previous and following datapoint
  #Remove any data below 1 because it is an outlier and give it a Flag Code of 2
  ccrwater <- ccrwater %>% 
    mutate(Cond1 = lag(EXOCond_uScm_1, 0),
           Cond_lag1 = lag(EXOCond_uScm_1, 1),
           Cond_lead1 = lead(EXOCond_uScm_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOCond_1 = ifelse(Cond1 < 0 & !is.na(Cond1), 3, Flag_EXOCond_1)) %>%
    mutate(Flag_EXOCond_1 = ifelse(Cond1 < 1 & !is.na(Cond1), 2, Flag_EXOCond_1)) %>%#Remove any points less than 1
    mutate(EXOCond_uScm_1 = ifelse(Cond1 < 0 & !is.na(Cond1), 0, EXOCond_uScm_1)) %>%
    mutate(EXOCond_uScm_1 = ifelse(Cond1 < 1 & !is.na(Cond1), NA, EXOCond_uScm_1)) %>%
    mutate(EXOCond_uScm_1 = ifelse((abs(Cond_lag1 - Cond1) > (sd_2_cond_1))  & (abs(Cond_lead1 - Cond1) > (sd_2_cond_1) & !is.na(Cond1)), 
                                   NA, EXOCond_uScm_1)) %>%   
    mutate(Flag_EXOCond_1 = ifelse((abs(Cond_lag1 - Cond1) > (sd_2_cond_1))  & (abs(Cond_lead1 - Cond1) > (sd_2_cond_1) & !is.na(Cond1)), 
                              2, Flag_EXOCond_1)) %>% 
    select(-Cond1, -Cond_lag1, -Cond_lead1)
  
  ccrwater <- ccrwater %>% 
    mutate(Cond9 = lag(EXOCond_uScm_9, 0),
           Cond_lag9 = lag(EXOCond_uScm_9, 1),
           Cond_lead9 = lead(EXOCond_uScm_9, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOCond_9 = ifelse(Cond9 < 0 & !is.na(Cond9), 3, Flag_EXOCond_9)) %>%
    mutate(Flag_EXOCond_9 = ifelse(Cond9 < 1 & !is.na(Cond9), 2, Flag_EXOCond_9)) %>%#Remove any points less than 9
    mutate(EXOCond_uScm_9 = ifelse(Cond9 < 0 & !is.na(Cond9), 0, EXOCond_uScm_9)) %>%
    mutate(EXOCond_uScm_9 = ifelse(Cond9 < 1 & !is.na(Cond9), NA, EXOCond_uScm_9)) %>%
    mutate(EXOCond_uScm_9 = ifelse((abs(Cond_lag9 - Cond9) > (sd_2_cond_9))  & (abs(Cond_lead9 - Cond9) > (sd_2_cond_9) & !is.na(Cond9)), 
                                   NA, EXOCond_uScm_9)) %>%   
    mutate(Flag_EXOCond_9 = ifelse((abs(Cond_lag9 - Cond9) > (sd_2_cond_9))  & (abs(Cond_lead9 - Cond9) > (sd_2_cond_9) & !is.na(Cond9)), 
                                   2, Flag_EXOCond_9)) %>% 
    select(-Cond9, -Cond_lag9, -Cond_lead9)
  
  # QAQC on major Specific conductivity outliers using DWH's method: datapoint set to NA if data is greater than 2*sd different from both previous and following datapoint
  #Remove any data below 1 because it is an outlier and give it a Flag Code of 2
  ccrwater <- ccrwater %>% 
    mutate(SpCond1 = lag(EXOSpCond_uScm_1, 0),
           SpCond_lag1 = lag(EXOSpCond_uScm_1, 1),
           SpCond_lead1 = lead(EXOSpCond_uScm_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOSpCond_1 = ifelse(SpCond1 < 0 & !is.na(SpCond1), 3, Flag_EXOSpCond_1)) %>% 
    mutate(Flag_EXOSpCond_1 = ifelse(SpCond1 < 1 & !is.na(SpCond1), 2, Flag_EXOSpCond_1)) %>%
    mutate(EXOSpCond_uScm_1 = ifelse(SpCond1 < 0 & !is.na(SpCond1), 0, EXOSpCond_uScm_1)) %>% 
    mutate(EXOSpCond_uScm_1 = ifelse(SpCond1 < 1 & !is.na(SpCond1), NA, EXOSpCond_uScm_1)) %>%
    mutate(EXOSpCond_uScm_1 = ifelse((abs(SpCond_lag1 - SpCond1) > (sd_2_spcond_1))  & (abs(SpCond_lead1 - SpCond1) > (sd_2_spcond_1) & !is.na(SpCond1)), 
                                     NA, EXOSpCond_uScm_1)) %>%   
    mutate(Flag_EXOSpCond_1 = ifelse((abs(SpCond_lag1 - SpCond1) > (sd_2_spcond_1))  & (abs(SpCond_lead1 - SpCond1) > (sd_2_spcond_1)) & !is.na(SpCond1), 
                                2, Flag_EXOSpCond_1)) %>% 
    select(-SpCond1, -SpCond_lag1, -SpCond_lead1)
  
  ccrwater <- ccrwater %>% 
    mutate(SpCond9 = lag(EXOSpCond_uScm_9, 0),
           SpCond_lag9 = lag(EXOSpCond_uScm_9, 1),
           SpCond_lead9 = lead(EXOSpCond_uScm_9, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOSpCond_9 = ifelse(SpCond9 < 0 & !is.na(SpCond9), 3, Flag_EXOSpCond_9)) %>% 
    mutate(Flag_EXOSpCond_9 = ifelse(SpCond9 < 1 & !is.na(SpCond9), 2, Flag_EXOSpCond_9)) %>%
    mutate(EXOSpCond_uScm_9 = ifelse(SpCond9 < 0 & !is.na(SpCond9), 0, EXOSpCond_uScm_9)) %>% 
    mutate(EXOSpCond_uScm_9 = ifelse(SpCond9 < 1 & !is.na(SpCond9), NA, EXOSpCond_uScm_9)) %>%
    mutate(EXOSpCond_uScm_9 = ifelse((abs(SpCond_lag9 - SpCond9) > (sd_2_spcond_9))  & (abs(SpCond_lead9 - SpCond9) > (sd_2_spcond_9) & !is.na(SpCond9)), 
                                     NA, EXOSpCond_uScm_9)) %>%   
    mutate(Flag_EXOSpCond_9 = ifelse((abs(SpCond_lag9 - SpCond9) > (sd_2_spcond_9))  & (abs(SpCond_lead9 - SpCond9) > (sd_2_spcond_9)) & !is.na(SpCond9), 
                                     2, Flag_EXOSpCond_9)) %>% 
    select(-SpCond9, -SpCond_lag9, -SpCond_lead9)
  
  # QAQC on major TDS outliers using DWH's method: datapoint set to NA if data is greater than 2*sd different from both previous and following datapoint
  #Remove any data below 1 because it is an outlier and give it a Flag Code of 2
  ccrwater <- ccrwater %>% 
    mutate(TDS1 = lag(EXOTDS_mgL_1, 0),
           TDS_lag1 = lag(EXOTDS_mgL_1, 1),
           TDS_lead1 = lead(EXOTDS_mgL_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOTDS_1 = ifelse(TDS1 < 0 & !is.na(TDS1), 3, Flag_EXOTDS_1)) %>% 
    mutate(Flag_EXOTDS_1 = ifelse(TDS1 < 1 & !is.na(TDS1), 2, Flag_EXOTDS_1)) %>%
    mutate(EXOTDS_mgL_1 = ifelse(TDS1 < 0 & !is.na(TDS1), 0, EXOTDS_mgL_1)) %>%
    mutate(EXOTDS_mgL_1 = ifelse(TDS1 < 1 & !is.na(TDS1), NA, EXOTDS_mgL_1)) %>% 
    mutate(EXOTDS_mgL_1 = ifelse((abs(TDS_lag1 - TDS1) > (sd_2_TDS_1))  & (abs(TDS_lead1 - TDS1) > (sd_2_TDS_1) & !is.na(TDS1)), 
                                 NA, EXOTDS_mgL_1)) %>%   
    mutate(Flag_EXOTDS_1 = ifelse((abs(TDS_lag1 - TDS1) > (sd_2_TDS_1))  & (abs(TDS_lead1 - TDS1) > (sd_2_TDS_1)) & !is.na(TDS1), 
                             2, Flag_EXOTDS_1)) %>% 
    select(-TDS1, -TDS_lag1, -TDS_lead1)

  ccrwater <- ccrwater %>% 
    mutate(TDS9 = lag(EXOTDS_mgL_9, 0),
           TDS_lag9 = lag(EXOTDS_mgL_9, 1),
           TDS_lead9 = lead(EXOTDS_mgL_9, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_EXOTDS_9 = ifelse(TDS9 < 0 & !is.na(TDS9), 3, Flag_EXOTDS_9)) %>% 
    mutate(Flag_EXOTDS_9 = ifelse(TDS9 < 1 & !is.na(TDS9), 2, Flag_EXOTDS_9)) %>%
    mutate(EXOTDS_mgL_9 = ifelse(TDS9 < 0 & !is.na(TDS9), 0, EXOTDS_mgL_9)) %>%
    mutate(EXOTDS_mgL_9 = ifelse(TDS9 < 1 & !is.na(TDS9), NA, EXOTDS_mgL_9)) %>% 
    mutate(EXOTDS_mgL_9 = ifelse((abs(TDS_lag9 - TDS9) > (sd_2_TDS_9))  & (abs(TDS_lead9 - TDS9) > (sd_2_TDS_9) & !is.na(TDS9)), 
                                 NA, EXOTDS_mgL_9)) %>%   
    mutate(Flag_EXOTDS_9 = ifelse((abs(TDS_lag9 - TDS9) > (sd_2_TDS_9))  & (abs(TDS_lead9 - TDS9) > (sd_2_TDS_9)) & !is.na(TDS9), 
                                  2, Flag_EXOTDS_9)) %>% 
    select(-TDS9, -TDS_lag9, -TDS_lead9)
#####################################################################################################################################   
  #change EXO_1 at 1.5m values to NA if EXO depth is less than 0.3m and Flag as 2
  
  #index only the colummns with EXO at the beginning
  exo_idx <-grep("^EXO.*_1$",colnames(ccrwater))
  
  #create list of the Flag columns that need to be changed to 2
  exo_flag <- c("Flag_EXOTemp_1", "Flag_EXOCond_1","Flag_EXOSpCond_1","Flag_EXOTDS_1",'Flag_EXODO_obs_1',
                "Flag_EXODO_sat_1","Flag_EXOChla_RFU_1","Flag_EXOChla_ugL_1","Flag_EXOPhyco_RFU_1",
                "Flag_EXOPhyco_ugL_1",'Flag_EXOfDOM_1',"Flag_EXOPres_1",
                "Flag_EXObat_1","Flag_EXOcab_1","Flag_EXOwip_1" )
  
  
  #Change the EXO data to NAs when the EXO is above 0.5m and not due to maintenance
  ccrwater[which(ccrwater$EXO_depth_m_1 < 0.3), exo_idx] <- NA
  #Flag the data that was removed with 2 for outliers
  ccrwater[which(ccrwater$EXO_depth_m_1<0.3),exo_flag]<- 2
  
  #index only the colummns with EXO at the beginning
  exo_idx9 <-grep("^EXO.*_1$",colnames(ccrwater))
  
  #create list of the Flag columns that need to be changed to 2
  exo_flag9 <- c("Flag_EXOTemp_9", "Flag_EXOCond_9","Flag_EXOSpCond_9","Flag_EXOTDS_9",'Flag_EXODO_obs_9',
                "Flag_EXODO_sat_9",'Flag_EXOfDOM_9',"Flag_EXOPres_9",
                "Flag_EXObat_9","Flag_EXOcab_9","Flag_EXOwip_9")
  
  
  #Change the EXO data to NAs when the EXO is above 0.5m and not due to maintenance
  ccrwater[which(ccrwater$EXO_depth_m_9 < 6), exo_idx9] <- NA
  #Flag the data that was removed with 2 for outliers
  ccrwater[which(ccrwater$EXO_depth_m_9 < 6),exo_flag9]<- 2
  
#############################################################################################################################  
   # delete EXO_Date and EXO_Time columns
  ccrwater <- ccrwater %>% select(-EXO_Date_1,-EXO_Date_9,-EXO_Time_1,-EXO_Time_9)
  
#add depth of bottom Thermistor. Subtract 0.15 from the depth because the pressure sensor
#is 0.15m deeper than the bottom Thermistor
  ccrwater=ccrwater%>%mutate(LvlDepth_m_13=(Lvl_psi_13*0.70455)-0.15)#1psi=2.31ft, 1ft=0.305m
  
  # add Reservoir and Site columns
  ccrwater$Reservoir <- "CCR"
  ccrwater$Site <- "50"
  
  # reorder columns
  ccrwater <- ccrwater %>% select(Reservoir, Site, DateTime,  
  ThermistorTemp_C_1, ThermistorTemp_C_2, ThermistorTemp_C_3, ThermistorTemp_C_4,
  ThermistorTemp_C_5, ThermistorTemp_C_6, ThermistorTemp_C_7, ThermistorTemp_C_8,
  ThermistorTemp_C_9,ThermistorTemp_C_10,ThermistorTemp_C_11, ThermistorTemp_C_12,
  ThermistorTemp_C_13, EXOTemp_C_1, EXOCond_uScm_1,
  EXOSpCond_uScm_1, EXOTDS_mgL_1, EXODOsat_percent_1, EXODO_mgL_1, EXOChla_RFU_1,
  EXOChla_ugL_1, EXOBGAPC_RFU_1, EXOBGAPC_ugL_1, EXOfDOM_RFU_1, EXOfDOM_QSU_1,
  EXO_pressure_psi_1, EXO_depth_m_1, EXO_battery_V_1, EXO_cablepower_V_1, EXO_wiper_V_1,
  EXOTemp_C_9, EXOCond_uScm_9,
  EXOSpCond_uScm_9, EXOTDS_mgL_9, EXODOsat_percent_9, EXODO_mgL_9, 
  EXOfDOM_RFU_9, EXOfDOM_QSU_9,EXO_pressure_psi_9, EXO_depth_m_9, EXO_battery_V_9,
  EXO_cablepower_V_9, EXO_wiper_V_9,Lvl_psi_13,LvlDepth_m_13, LvlTemp_C_13, 
  Record, CR3000_Batt_V, CR3000Panel_Temp_C,everything())
  
  
  
  # replace NaNs with NAs
  ccrwater[is.na(ccrwater)] <- NA
  
  # convert datetimes to characters so that they are properly formatted in the output file
  ccrwater$DateTime <- as.character(ccrwater$DateTime)
  
  # write to output file
  write_csv(ccrwater, output_file)
}

# example usage
#qaqc("https://raw.githubusercontent.com/CareyLabVT/SCCData/mia-data/Catwalk.csv",
#     'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/FCRWaterLevel.csv',
#     "https://raw.githubusercontent.com/CareyLabVT/SCCData/mia-data/CAT_MaintenanceLog.txt",
#    "Catwalk.csv")


