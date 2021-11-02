
#

temp_oxy_chla_qaqc <- function(data_file, data2_file, maintenance_file, output_file)
{
 
  CATPRES_COL_NAMES = c("DateTime", "RECORD", "CR6_Batt_V", "CR6Panel_Temp_C", "ThermistorTemp_C_surface",
                        "ThermistorTemp_C_1", "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4",
                        "ThermistorTemp_C_5", "ThermistorTemp_C_6", "ThermistorTemp_C_7", "ThermistorTemp_C_8",
                        "ThermistorTemp_C_9", "RDO_mgL_5", "RDOsat_percent_5", "RDOTemp_C_5", "RDO_mgL_9",
                        "RDOsat_percent_9", "RDOTemp_C_9", "EXO_Date", "EXO_Time", "EXOTemp_C_1", "EXOCond_uScm_1",
                        "EXOSpCond_uScm_1", "EXOTDS_mgL_1", "EXODOsat_percent_1", "EXODO_mgL_1", "EXOChla_RFU_1",
                        "EXOChla_ugL_1", "EXOBGAPC_RFU_1", "EXOBGAPC_ugL_1", "EXOfDOM_RFU_1", "EXOfDOM_QSU_1",
                        "EXO_pressure", "EXO_depth", "EXO_battery", "EXO_cablepower", "EXO_wiper","Lvl_psi_9", "LvlTemp_C_9")
  
  
  # EXO sonde sensor data that differs from the mean by more than the standard deviation multiplied by this factor will
  # either be replaced with NA and flagged (if between 2018-10-01 and 2019-03-01) or just flagged (otherwise)
  EXO_FOULING_FACTOR <- 4
  #EXO_FOULING_FACTORfor2 <- 2
  
  
  
  
  # read catwalk data and maintenance log
  # NOTE: date-times throughout this script are processed as UTC
  catdata <- read_csv(data_file, skip = 7, col_names = CATPRES_COL_NAMES,
                      col_types = cols(.default = col_double(), DateTime = col_datetime()))
  
 #read in manual data from the data logger to fill in missing gaps
  
  catdata2 <- read_csv(data2_file, skip = 1, col_names = CATPRES_COL_NAMES,
                       col_types = cols(.default = col_double(), DateTime = col_datetime()))
  
  catdata <-rbind(catdata,catdata2)
  
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
#Read in the maintneance log 
  
  log <- read_csv(maintenance_file, col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  ))
  
  log=log%>%filter(flag!=5)
  
  # remove NaN data at beginning when data when no sensors were connected to the data logger
  catdata <- catdata %>% filter(DateTime >= ymd_hms("2018-07-05 13:50:00"))
  
  # add flag columns
  catdata$Flag_All <- 0
  catdata$Flag_Temp_Surf <- 0
  catdata$Flag_Temp_1 <- 0
  catdata$Flag_Temp_2 <- 0
  catdata$Flag_Temp_3 <- 0
  catdata$Flag_Temp_4 <- 0
  catdata$Flag_Temp_5 <- 0
  catdata$Flag_Temp_6 <- 0
  catdata$Flag_Temp_7 <- 0
  catdata$Flag_Temp_8 <- 0
  catdata$Flag_Temp_9 <- 0
  catdata$Flag_DO_5_obs <- 0
  catdata$Flag_DO_5_sat <- 0
  catdata$Flag_DO_5_temp <- 0
  catdata$Flag_DO_9_obs <- 0
  catdata$Flag_DO_9_sat <- 0
  catdata$Flag_DO_9_temp <- 0
  catdata$Flag_EXOTemp <- 0
  catdata$Flag_Cond <- 0
  catdata$Flag_SpCond <- 0
  catdata$Flag_TDS <- 0
  catdata$Flag_DO_1_sat <- 0
  catdata$Flag_DO_1_obs <- 0
  catdata$Flag_Chla <- 0
  catdata$Flag_Phyco <- 0
  catdata$Flag_fDOM <- 0
  catdata$Flag_Pres <- 0
  
  

  # change the flags if there are NAs values before the maintenance log then the data is missing and flagged as 7 for missing values
  catdata <- catdata%>%
    mutate(
      Flag_Temp_Surf = ifelse(is.na(ThermistorTemp_C_surface) &
                                Flag_Temp_Surf==0,7, Flag_Temp_Surf),
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
      Flag_Pres = ifelse(is.na(Lvl_psi_9) & 
                           Flag_Pres==0, 7, Flag_Pres),
      Flag_DO_1_obs = ifelse(is.na(EXODO_mgL_1) &
                            Flag_DO_1_obs==0, 7, Flag_DO_1_obs),
      Flag_DO_1_sat = ifelse(is.na(EXODOsat_percent_1) &
                               Flag_DO_1_sat==0, 7, Flag_DO_1_sat),
      Flag_DO_5_obs = ifelse( is.na(RDO_mgL_5) & is.na(RDOsat_percent_5) &
                             Flag_DO_5_obs==0, 7, Flag_DO_5_obs),
      Flag_DO_5_sat = ifelse( is.na(RDOsat_percent_5) &
                                Flag_DO_5_sat==0, 7, Flag_DO_5_sat),
      Flag_Do_5_temp = ifelse( is.na(RDOTemp_C_5) &
                                 Flag_DO_5_temp==0, 7, Flag_DO_5_temp),
      Flag_DO_9_obs = ifelse( is.na(RDO_mgL_9) & is.na(RDOsat_percent_9) &
                             Flag_DO_9_obs==0, 7, Flag_DO_9_obs),
      Flag_DO_9_sat = ifelse( is.na(RDOsat_percent_9) &
                                Flag_DO_9_sat==0, 7, Flag_DO_9_sat),
      Flag_DO_9_temp = ifelse( is.na(RDOTemp_C_9) &
                                Flag_DO_9_temp==0, 7, Flag_DO_9_temp),
      Flag_EXOTemp = ifelse( is.na(EXOTemp_C_1) & 
                            Flag_EXOTemp==0, 7, Flag_EXOTemp),
      Flag_Chla = ifelse( is.na(EXOChla_RFU_1) & is.na(EXOChla_ugL_1) &
                              Flag_Chla==0, 7, Flag_Chla),
      Flag_Phyco = ifelse( is.na(EXOBGAPC_RFU_1) & is.na(EXOBGAPC_ugL_1) &
                               Flag_Phyco==0, 7, Flag_Phyco),
      Flag_fDOM = ifelse( is.na(EXOfDOM_RFU_1) & is.na(EXOfDOM_QSU_1) &
                              Flag_fDOM==0, 7, Flag_fDOM),
      Flag_TDS = ifelse( is.na(EXOTDS_mgL_1) & 
                           Flag_TDS==0, 7, Flag_TDS),
      Flag_Cond = ifelse( is.na(EXOCond_uScm_1) & 
                            Flag_Cond==0, 7, Flag_Cond),
      Flag_SpCond = ifelse( is.na(EXOSpCond_uScm_1) & 
                              Flag_SpCond==0, 7, Flag_SpCond)
    )
  
 
  # modify catdata based on the information in the log
  for(i in 1:nrow(log))
  {
    # get start and end time of one maintenance event
    start <- log$TIMESTAMP_start[i]
    end <- log$TIMESTAMP_end[i]
    
    
    # get indices of columns affected by maintenance
    if(grepl("^\\d+$", log$colnumber[i])) # single num
    {
      maintenance_cols <- intersect(c(2:41), as.integer(log$colnumber[i]))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", log$colnumber[i])) # c(x;y;...)
    {
      maintenance_cols <- intersect(c(2:41), as.integer(unlist(regmatches(log$colnumber[i],
                                                                          gregexpr("\\d+", log$colnumber[i])))))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", log$colnumber[i])) # c(x:y)
    {
      bounds <- as.integer(unlist(regmatches(log$colnumber[i], gregexpr("\\d+", log$colnumber[i]))))
      maintenance_cols <- intersect(c(2:41), c(bounds[1]:bounds[2]))
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
                    "indices 2 through 39, excluding 21 and 22, which are deleted by this script. Skipping maintenance for that row."))
      next
    }
  

    
    # replace relevant data with NAs and set "all" flag while maintenance was in effect
    catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] <- NA
    catdata[catdata$DateTime >= start & catdata$DateTime <= end, "Flag_All"] <- 1
  }
##################################################################################################################
#2 hour adjustment period for DO sensors- Remove values up to 2 hours after temp string has been pulled up
  
#All based on times in the maintenance log
  
    maint = read.csv(maintenance_file)
    maint_all = maint[grepl("EXO|All_Cat",maint$parameter),] #creating file "maint" with all sensor string maintenance
    maint_all = maint_all%>%
    filter(flag==1)%>%
    filter(parameter!="fdom")
    clean_start<-as.POSIXct(maint$TIMESTAMP_start, tz = "UTC")#changed the time tz to make sure there is no conflict
    clean_end <- as.POSIXct(maint$TIMESTAMP_end, tz = "UTC")

    ADJ_PERIOD = 2*60*60 #amount of time to stabilization after cleaning in seconds

    for (i in 1:length(clean_start)){ #Set all data during cleaning and for ADJ_PERIOD after to NA
      if (maint$colnumber[i]==" c(1:41)"){
      catdata$EXODO_mgL_1[catdata$DateTime>clean_start[i]&catdata$DateTime<(clean_end[i]+ADJ_PERIOD)] <- NA
      catdata$RDO_mgL_5[catdata$DateTime>clean_start[i]&catdata$DateTime<(clean_end[i]+ADJ_PERIOD)] <- NA
      catdata$RDO_mgL_9[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
      catdata$EXODOsat_percent_1[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
      catdata$RDOsat_percent_5[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
      catdata$RDOsat_percent_9[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
      catdata$Flag_DO_1_obs[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
      catdata$Flag_DO_1_sat[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
      catdata$Flag_DO_5_obs[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
      catdata$Flag_DO_5_sat[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
      catdata$Flag_DO_9_obs[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
      catdata$Flag_DO_9_sat[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
      }
      else if(maint$colnumber[i] %in% c(" 15"," 16")){
        catdata$RDO_mgL_5[catdata$DateTime>clean_start[i]&catdata$DateTime<(clean_end[i]+ADJ_PERIOD)] <- NA
        catdata$RDOsat_percent_5[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
        catdata$Flag_DO_5_obs[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
        catdata$Flag_DO_5_sat[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
      }
      else if(maint$colnumber[i] %in% c(" 18"," 19")){
        catdata$RDO_mgL_9[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
        catdata$RDOsat_percent_9[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
        catdata$Flag_DO_9_obs[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
        catdata$Flag_DO_9_sat[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
      }
      else if (maint$colnumber[i] %in% c(" c(21:39"," 27"," 28")){
        catdata$EXODO_mgL_1[catdata$DateTime>clean_start[i]&catdata$DateTime<(clean_end[i]+ADJ_PERIOD)] <- NA
        catdata$EXODOsat_percent_1[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- NA
        catdata$Flag_DO_1_obs[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
        catdata$Flag_DO_1_sat[catdata$DateTime>clean_start[i]&catdata$DateTime<clean_end[i]+ADJ_PERIOD] <- 1
        next
        }
      }
    


##################################################################################################################  
  #Set negative DO values to 0 and Flag_DO for NA values
  catdata <- catdata %>%  #RDO at 5m
    mutate(Flag_DO_5_obs = ifelse(RDO_mgL_5 < 0 & !is.na(RDO_mgL_5) , 3, Flag_DO_5_obs),#Add a flag for DO<0
           Flag_DO_5_sat = ifelse(RDOsat_percent_5 < 0 & !is.na(RDOsat_percent_5) , 3, Flag_DO_5_sat),
           RDO_mgL_5 = ifelse(RDO_mgL_5 < 0, 0, RDO_mgL_5), #Change negative to 0
           RDOsat_percent_5 = ifelse(RDOsat_percent_5 < 0, 0, RDOsat_percent_5), #Change negative %sat to 0
          
           Flag_DO_9_obs = ifelse(RDO_mgL_9 < 0 & !is.na(RDO_mgL_9), 3, Flag_DO_9_obs), #repeat for 9m
           Flag_DO_9_sat = ifelse(RDOsat_percent_9 < 0 & !is.na(RDOsat_percent_9) , 3, Flag_DO_9_sat),
           RDO_mgL_9 = ifelse(RDO_mgL_9 < 0, 0, RDO_mgL_9),
           RDOsat_percent_9 = ifelse(RDOsat_percent_9 < 0, 0, RDOsat_percent_9),
           
           Flag_DO_1_obs = ifelse(EXODO_mgL_1 < 0 & !is.na(EXODO_mgL_1), 3, Flag_DO_1_obs), #and for 1m
           Flag_DO_1_sat = ifelse(EXODOsat_percent_1 < 0 & !is.na(EXODOsat_percent_1) , 3, Flag_DO_1_sat),
           EXODO_mgL_1 = ifelse(EXODO_mgL_1 < 0, 0, EXODO_mgL_1),
           EXODOsat_percent_1 = ifelse(EXODOsat_percent_1 <0, 0, EXODOsat_percent_1)
    )
    
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
    mutate(Chla = lag(EXOChla_ugL_1, 0),
           Chla_lag1 = lag(EXOChla_ugL_1, 1),
           Chla_lead1 = lead(EXOChla_ugL_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_Chla = ifelse(Chla < 0 & !is.na(Chla), 3, Flag_Chla)) %>% 
    mutate(Flag_Chla = ifelse(Chla < 0 & !is.na(Chla), 3, Flag_Chla)) %>% 
    mutate(EXOChla_ugL_1 = ifelse(Chla < 0 & !is.na(Chla), 0, EXOChla_ugL_1)) %>% 
    mutate(EXOChla_RFU_1 = ifelse(Chla < 0 & !is.na(Chla), 0, EXOChla_RFU_1)) %>% 
    mutate(EXOChla_ugL_1 = ifelse((abs(Chla_lag1 - Chla) > (Chla_ugL_1_threshold))  & (abs(Chla_lead1 - Chla) > (Chla_ugL_1_threshold) & !is.na(Chla)), 
                                  NA, EXOChla_ugL_1)) %>%   
    mutate(EXOChla_RFU_1 = ifelse((abs(Chla_lag1 - Chla) > (Chla_ugL_1_threshold))  & (abs(Chla_lead1 - Chla) > (Chla_ugL_1_threshold) & !is.na(Chla)), 
                                  NA, EXOChla_RFU_1)) %>% 
    mutate(Flag_Chla = ifelse((abs(Chla_lag1 - Chla) > (Chla_ugL_1_threshold))  & (abs(Chla_lead1 - Chla) > (Chla_ugL_1_threshold)) & !is.na(Chla), 
                              2, Flag_Chla)) %>% 
    select(-Chla, -Chla_lag1, -Chla_lead1)
  
  # QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
  catdata <- catdata %>% 
    mutate(phyco = lag(EXOBGAPC_ugL_1, 0),
           phyco_lag1 = lag(EXOBGAPC_ugL_1, 1),
           phyco_lead1 = lead(EXOBGAPC_ugL_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_Phyco = ifelse(phyco < 0 & !is.na(phyco), 3, Flag_Phyco)) %>% 
    mutate(Flag_Phyco = ifelse(phyco < 0 & !is.na(phyco), 3, Flag_Phyco)) %>% 
    mutate(EXOBGAPC_RFU_1 = ifelse(phyco < 0 & !is.na(phyco), 0, EXOBGAPC_RFU_1)) %>% 
    mutate(EXOBGAPC_ugL_1 = ifelse(phyco < 0 & !is.na(phyco), 0, EXOBGAPC_ugL_1)) %>% 
    mutate(EXOBGAPC_ugL_1 = ifelse((abs(phyco_lag1 - phyco) > (BGAPC_ugL_1_threshold))  & (abs(phyco_lead1 - phyco) > (BGAPC_ugL_1_threshold) & !is.na(phyco)), 
                                   NA, EXOBGAPC_ugL_1)) %>%   
    mutate(EXOBGAPC_RFU_1 = ifelse((abs(phyco_lag1 - phyco) > (BGAPC_ugL_1_threshold))  & (abs(phyco_lead1 - phyco) > (BGAPC_ugL_1_threshold) & !is.na(phyco)), 
                                   NA, EXOBGAPC_RFU_1)) %>% 
    mutate(Flag_Phyco = ifelse((abs(phyco_lag1 - phyco) > (BGAPC_ugL_1_threshold))  & (abs(phyco_lead1 - phyco) > (BGAPC_ugL_1_threshold) & !is.na(phyco)), 
                               2, Flag_Phyco)) %>%
    select(-phyco, -phyco_lag1, -phyco_lead1)
  
  #QAQC major outliers during the winter of 2018 going into 2019 due to fouling that did not get caught above. These points are removed
  catdata <- catdata %>%
    mutate(Flag_Chla = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                (! is.na(EXOChla_RFU_1) & abs(EXOChla_RFU_1 - Chla_RFU_1_mean) > Chla_RFU_1_threshold |
                                   ! is.na(EXOChla_ugL_1) & abs(EXOChla_ugL_1 - Chla_ugL_1_mean) > Chla_ugL_1_threshold),
                              4, Flag_Chla)) %>%
    mutate(Flag_Phyco = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                 (! is.na(EXOBGAPC_RFU_1) & abs(EXOBGAPC_RFU_1 - BGAPC_RFU_1_mean) > BGAPC_RFU_1_threshold |
                                    ! is.na(EXOBGAPC_ugL_1) & abs(EXOBGAPC_ugL_1 - BGAPC_ugL_1_mean) > BGAPC_ugL_1_threshold),
                               4, Flag_Phyco)) %>%
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
    mutate(Flag_Phyco = ifelse(! is.na(EXOBGAPC_RFU_1) & abs(EXOBGAPC_RFU_1 - BGAPC_RFU_1_mean) > BGAPC_RFU_1_threshold |
                                 ! is.na(EXOBGAPC_ugL_1) & abs(EXOBGAPC_ugL_1 - BGAPC_ugL_1_mean) > BGAPC_ugL_1_threshold,
                               5, Flag_Phyco)) %>%
  mutate(Flag_Chla = ifelse(! is.na(EXOChla_ugL_1) & abs(EXOChla_ugL_1 - Chla_ugL_1_mean) > Chla_ugL_1_threshold |
                               ! is.na(EXOChla_RFU_1) & abs(EXOChla_RFU_1 - Chla_RFU_1_mean) > Chla_RFU_1_threshold,
                             5, Flag_Chla))

  
  
####################################################################################################################################  
  # fdom qaqc----
  # QAQC from DWH to remove major outliers from fDOM data that are 2 sd's greater than the previous and following datapoint
  sd_fDOM <- sd(catdata$EXOfDOM_QSU_1, na.rm = TRUE) #deteriming the standard deviation of fDOM data 
  
  #
  catdata <- catdata%>% 
    #mutate(Flag_fDOM = ifelse(is.na(EXOfDOM_QSU_1), 1, 0)) #This creates Flag column for fDOM data, setting all NA's going into QAQC as 1 for missing data, and to 0 for the rest
    mutate(Flag_fDOM = ifelse(DateTime >= "2021-04-16 11:50" & DateTime < "2021-04-26 13:10",2, Flag_fDOM))%>%#bad calibration during this time
  mutate(fDOM = lag(EXOfDOM_QSU_1, 0),
         fDOM_lag1 = lag(EXOfDOM_QSU_1, 1),
         fDOM_lead1 = lead(EXOfDOM_QSU_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_fDOM = ifelse(fDOM < 0 & !is.na(fDOM), 2, Flag_fDOM),
           EXOfDOM_QSU_1 = ifelse(fDOM < 0 & !is.na(fDOM), NA, EXOfDOM_QSU_1),
           EXOfDOM_RFU_1 = ifelse(fDOM < 0 & !is.na(fDOM), NA, EXOfDOM_RFU_1))%>%
           #Flag_fDOM = ifelse(fDOM < 0, 2, Flag_fDOM)) %>% #These mutates are QAQCing for negative fDOM QSU values and setting these to NA and making a flag for these. This was done outside of the 2 sd deviation rule because there were two negative points in a row and one was not removed with the follwoing if else statements. 
    mutate(EXOfDOM_QSU_1 = ifelse(
      ( abs(fDOM_lag1 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1 - fDOM) > (2*sd_fDOM)  & !is.na(fDOM) ), NA, EXOfDOM_QSU_1
    )) %>%  #QAQC to remove outliers for QSU fDOM data 
    mutate(EXOfDOM_RFU_1 = ifelse(
      ( abs(fDOM_lag1 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1 - fDOM) > (2*sd_fDOM)  & !is.na(fDOM)  ), NA, EXOfDOM_RFU_1
    )) %>% #QAQC to remove outliers for RFU fDOM data
    mutate(Flag_fDOM = ifelse(
      ( abs(fDOM_lag1 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1 - fDOM) > (2*sd_fDOM)  & !is.na(fDOM)  ), 2, Flag_fDOM
    ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
    select(-fDOM, -fDOM_lag1, -fDOM_lead1)#This removes the columns used to run ifelse statements since they are no longer needed.
    
#####################################################################################################################################  
#QAQC from DWH to remove major outliers from condnuctity, specific conductivity and TDS data that are 2 sd's greater than the previous and following datapoint
  

  sd_4_cond <- 2*sd(catdata$EXOCond_uScm_1, na.rm = TRUE)
  threshold <- sd_4_cond
  sd_4_spcond <- 2*sd(catdata$EXOSpCond_uScm_1, na.rm = TRUE)
  threshold_spcond <- sd_4_spcond 
  sd_4_TDS <- 2*sd(catdata$EXOTDS_mgL_1, na.rm = TRUE)
  threshold_TDS <- sd_4_TDS 
  
  # QAQC on major conductivity outliers using DWH's method: datapoint set to NA if data is greater than 2*sd different from both previous and following datapoint
  #Remove any data below 1 because it is an outlier and give it a Flag Code of 2
  catdata <- catdata %>% 
    mutate(Cond = lag(EXOCond_uScm_1, 0),
           Cond_lag1 = lag(EXOCond_uScm_1, 1),
           Cond_lead1 = lead(EXOCond_uScm_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_Cond = ifelse(Cond < 0 & !is.na(Cond), 3, Flag_Cond)) %>%
    mutate(Flag_Cond = ifelse(Cond < 1 & !is.na(Cond), 2, Flag_Cond)) %>%#Remove any points less than 1
    mutate(EXOCond_uScm_1 = ifelse(Cond < 0 & !is.na(Cond), 0, EXOCond_uScm_1)) %>%
    mutate(EXOCond_uScm_1 = ifelse(Cond < 1 & !is.na(Cond), NA, EXOCond_uScm_1)) %>%
    mutate(EXOCond_uScm_1 = ifelse((abs(Cond_lag1 - Cond) > (threshold))  & (abs(Cond_lead1 - Cond) > (threshold) & !is.na(Cond)), 
                                   NA, EXOCond_uScm_1)) %>%   
    mutate(Flag_Cond = ifelse((abs(Cond_lag1 - Cond) > (threshold))  & (abs(Cond_lead1 - Cond) > (threshold) & !is.na(Cond)), 
                              2, Flag_Cond)) %>% 
    select(-Cond, -Cond_lag1, -Cond_lead1)
  
  # QAQC on major Specific conductivity outliers using DWH's method: datapoint set to NA if data is greater than 2*sd different from both previous and following datapoint
  #Remove any data below 1 because it is an outlier and give it a Flag Code of 2
  catdata <- catdata %>% 
    mutate(SpCond = lag(EXOSpCond_uScm_1, 0),
           SpCond_lag1 = lag(EXOSpCond_uScm_1, 1),
           SpCond_lead1 = lead(EXOSpCond_uScm_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_SpCond = ifelse(SpCond < 0 & !is.na(SpCond), 3, Flag_SpCond)) %>% 
    mutate(Flag_SpCond = ifelse(SpCond < 1 & !is.na(SpCond), 2, Flag_SpCond)) %>%
    mutate(EXOSpCond_uScm_1 = ifelse(SpCond < 0 & !is.na(SpCond), 0, EXOSpCond_uScm_1)) %>% 
    mutate(EXOSpCond_uScm_1 = ifelse(SpCond < 1 & !is.na(SpCond), NA, EXOSpCond_uScm_1)) %>%
    mutate(EXOSpCond_uScm_1 = ifelse((abs(SpCond_lag1 - SpCond) > (threshold_spcond))  & (abs(SpCond_lead1 - SpCond) > (threshold_spcond) & !is.na(SpCond)), 
                                     NA, EXOSpCond_uScm_1)) %>%   
    mutate(Flag_SpCond = ifelse((abs(SpCond_lag1 - SpCond) > (threshold_spcond))  & (abs(SpCond_lead1 - SpCond) > (threshold_spcond)) & !is.na(SpCond), 
                                2, Flag_SpCond)) %>% 
    select(-SpCond, -SpCond_lag1, -SpCond_lead1)
  
  # QAQC on major TDS outliers using DWH's method: datapoint set to NA if data is greater than 2*sd different from both previous and following datapoint
  #Remove any data below 1 because it is an outlier and give it a Flag Code of 2
  catdata <- catdata %>% 
    mutate(TDS = lag(EXOTDS_mgL_1, 0),
           TDS_lag1 = lag(EXOTDS_mgL_1, 1),
           TDS_lead1 = lead(EXOTDS_mgL_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_TDS = ifelse(TDS < 0 & !is.na(TDS), 3, Flag_TDS)) %>% 
    mutate(Flag_TDS = ifelse(TDS < 1 & !is.na(TDS), 2, Flag_TDS)) %>%
    mutate(EXOTDS_mgL_1 = ifelse(TDS < 0 & !is.na(TDS), 0, EXOTDS_mgL_1)) %>%
    mutate(EXOTDS_mgL_1 = ifelse(TDS < 1 & !is.na(TDS), NA, EXOTDS_mgL_1)) %>% 
    mutate(EXOTDS_mgL_1 = ifelse((abs(TDS_lag1 - TDS) > (threshold_TDS))  & (abs(TDS_lead1 - TDS) > (threshold_TDS) & !is.na(TDS)), 
                                 NA, EXOTDS_mgL_1)) %>%   
    mutate(Flag_TDS = ifelse((abs(TDS_lag1 - TDS) > (threshold_TDS))  & (abs(TDS_lead1 - TDS) > (threshold_TDS)) & !is.na(TDS), 
                             2, Flag_TDS)) %>% 
    select(-TDS, -TDS_lag1, -TDS_lead1)

#####################################################################################################################################   
  #change EXO values to NA if EXO depth is less than 0.5m and Flag as 2
  
  #index only the colummns with EXO at the beginning
  exo_idx <-grep("^EXO",colnames(catdata))
  
  #create list of the Flag columns that need to be changed to 2
  exo_flag <- c("Flag_EXOTemp", "Flag_Cond","Flag_SpCond","Flag_TDS",'Flag_DO_1_obs',
                "Flag_DO_1_sat","Flag_Chla","Flag_Phyco",'Flag_fDOM')
  
  
  #Change the EXO data to NAs when the EXO is above 0.5m and not due to maintenance
  catdata[which(catdata$EXO_depth < 0.5), exo_idx] <- NA
  #Flag the data that was removed with 2 for outliers
  catdata[which(catdata$EXO_depth<0.5),exo_flag]<- 2
  
#############################################################################################################################  
   # delete EXO_Date and EXO_Time columns
  catdata <- catdata %>% select(-EXO_Date, -EXO_Time)
  
  # add Reservoir and Site columns
  catdata$Reservoir <- "FCR"
  catdata$Site <- "50"
  
  # reorder columns
  catdata <- catdata %>% select(Reservoir, Site, -RECORD, -CR6_Batt_V, -CR6Panel_Temp_C, -Flag_All, -Flag_DO_1_obs, -Flag_DO_1_sat, 
                                -Flag_DO_1_sat,-Flag_DO_5_obs,-Flag_DO_5_sat,-Flag_Do_5_temp, -Flag_DO_9_obs,-Flag_DO_9_sat,-Flag_DO_9_temp,
                                -Flag_Chla, -Flag_Phyco, -Flag_TDS, everything())
  
  # replace NaNs with NAs
  catdata[is.na(catdata)] <- NA
  
  # change the individual flags to match the the Maintenance log
  catdata <- catdata%>%
    mutate(
      Flag_Temp_Surf = ifelse(is.na(ThermistorTemp_C_surface) & Flag_All==1 &
                                Flag_Temp_Surf==0,1, Flag_Temp_Surf),
      Flag_Temp_1 = ifelse(is.na(ThermistorTemp_C_1) & Flag_All==1 &
                             Flag_Temp_1==0,1, Flag_Temp_1),
      Flag_Temp_2 = ifelse(is.na(ThermistorTemp_C_2) & Flag_All==1 &
                             Flag_Temp_2==0,1, Flag_Temp_2),
      Flag_Temp_3 = ifelse(is.na(ThermistorTemp_C_3) & Flag_All==1 &
                             Flag_Temp_3==0,1, Flag_Temp_3),
      Flag_Temp_4 = ifelse(is.na(ThermistorTemp_C_4) & Flag_All==1 &
                             Flag_Temp_4==0,1, Flag_Temp_4),
      Flag_Temp_5 = ifelse(is.na(ThermistorTemp_C_5) & Flag_All==1 &
                             Flag_Temp_5==0,1, Flag_Temp_5),
      Flag_Temp_6 = ifelse(is.na(ThermistorTemp_C_6) & Flag_All==1 &
                             Flag_Temp_6==0,1, Flag_Temp_6),
      Flag_Temp_7 = ifelse(is.na(ThermistorTemp_C_7) & Flag_All==1 &
                             Flag_Temp_7==0,1, Flag_Temp_7),
      Flag_Temp_8 = ifelse(is.na(ThermistorTemp_C_8) & Flag_All==1 &
                             Flag_Temp_8==0,1, Flag_Temp_8),
      Flag_Temp_9 = ifelse(is.na(ThermistorTemp_C_9) & Flag_All==1 &
                             Flag_Temp_9==0,1, Flag_Temp_9),
      Flag_Pres = ifelse(is.na(Lvl_psi_9) &  Flag_All==1 &
                           Flag_Pres==0, 1, Flag_Pres),
      Flag_DO_1_obs = ifelse(is.na(EXODO_mgL_1)  &
                           Flag_All==1 & Flag_DO_1_obs==0, 1, Flag_DO_1_obs),
      Flag_DO_1_sat = ifelse( is.na(EXODOsat_percent_1) &
                               Flag_All==1 & Flag_DO_1_sat==0, 1, Flag_DO_1_sat),
      Flag_DO_5_obs = ifelse( is.na(RDO_mgL_5) &
                            Flag_All==1 & Flag_DO_5_obs==0, 1, Flag_DO_5_obs),
      Flag_DO_5_sat = ifelse(  is.na(RDOsat_percent_5) &
                                Flag_All==1 & Flag_DO_5_sat==0, 1, Flag_DO_5_sat),
      Flag_DO_5_temp = ifelse( is.na(RDOTemp_C_5) & Flag_All ==1 &
                                 Flag_DO_5_temp==0, 1, Flag_DO_5_temp),
      Flag_DO_9_obs = ifelse( is.na(RDO_mgL_9) &
                            Flag_All==1 & Flag_DO_9_obs==0, 1, Flag_DO_9_obs),
      Flag_DO_9_sat = ifelse( is.na(RDOsat_percent_9) &
                                Flag_All==1 & Flag_DO_9_sat==0, 1, Flag_DO_9_sat),
      Flag_DO_9_temp = ifelse( is.na(RDOTemp_C_9) & Flag_All ==1 &
                                 Flag_DO_9_temp==0, 1, Flag_DO_9_temp),
      Flag_EXOTemp = ifelse( is.na(EXOTemp_C_1) & Flag_All==1 &
                               Flag_EXOTemp==0, 1, Flag_EXOTemp),
      Flag_Chla = ifelse( is.na(EXOChla_RFU_1) & is.na(EXOChla_ugL_1) &
                            Flag_All==1 &  Flag_Chla==0, 1, Flag_Chla),
      Flag_Phyco = ifelse( is.na(EXOBGAPC_RFU_1) & is.na(EXOBGAPC_ugL_1) &
                             Flag_All==1 &  Flag_Phyco==0, 1, Flag_Phyco),
      Flag_fDOM = ifelse( is.na(EXOfDOM_RFU_1) & is.na(EXOfDOM_QSU_1) &
                            Flag_All==1 &  Flag_fDOM==0, 1, Flag_fDOM),
      Flag_TDS = ifelse( is.na(EXOTDS_mgL_1) & Flag_All==1 &
                           Flag_TDS==0, 1, Flag_TDS),
      Flag_Cond = ifelse( is.na(EXOCond_uScm_1) & Flag_All==1 &
                            Flag_Cond==0, 1, Flag_Cond),
      Flag_SpCond = ifelse( is.na(EXOSpCond_uScm_1) & Flag_All==1 &
                              Flag_SpCond==0, 1, Flag_SpCond)
    )
  
  
  # convert datetimes to characters so that they are properly formatted in the output file
  catdata$DateTime <- as.character(catdata$DateTime)
  
  # write to output file
  write_csv(catdata, output_file)
}

# example usage
#temp_oxy_chla_qaqc("https://raw.githubusercontent.com/CareyLabVT/SCCData/mia-data/Catwalk.csv",
#     'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/FCRWaterLevel.csv',
#     "https://raw.githubusercontent.com/CareyLabVT/SCCData/mia-data/CAT_MaintenanceLog.txt",
#    "Catwalk.csv")


