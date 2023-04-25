

qaqc_fcr <- function(data_file, maintenance_file, output_file, start_date, end_date)
{
 
  CATPRES_COL_NAMES = c("DateTime", "RECORD", "CR6Battery_V", "CR6Panel_Temp_C", "ThermistorTemp_C_surface",
                        "ThermistorTemp_C_1", "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4",
                        "ThermistorTemp_C_5", "ThermistorTemp_C_6", "ThermistorTemp_C_7", "ThermistorTemp_C_8",
                        "ThermistorTemp_C_9", "RDO_mgL_5", "RDOsat_percent_5", "RDOTemp_C_5", "RDO_mgL_9",
                        "RDOsat_percent_9", "RDOTemp_C_9", "EXO_Date", "EXO_Time", "EXOTemp_C_1", "EXOCond_uScm_1",
                        "EXOSpCond_uScm_1", "EXOTDS_mgL_1", "EXODOsat_percent_1", "EXODO_mgL_1", "EXOChla_RFU_1",
                        "EXOChla_ugL_1", "EXOBGAPC_RFU_1", "EXOBGAPC_ugL_1", "EXOfDOM_RFU_1", "EXOfDOM_QSU_1","EXOTurbidity_FNU_1",
                        "EXOPressure_psi", "EXODepth_m", "EXOBattery_V", "EXOCablepower_V", "EXOWiper_V","LvlPressure_psi_9", "LvlTemp_C_9")
  
  #Adjustment period of time to stabilization after cleaning in seconds
  ADJ_PERIOD = 2*60*60 
  
  # This section either uses the compiled data from FCR_MET_QAQC_Plots_2015_2022.Rmd which is already labeled Met
  # or it reads in and formats the current file off the data logger
  
  catdata=data_file
  
  if (is.character(catdata)==T) {
    catdata <- read_csv(data_file, skip = 1, col_names = CATPRES_COL_NAMES,
                        col_types = cols(.default = col_double(), DateTime = col_datetime()))
  }else {
    
  }
  
  
  ## read in maintenance file 
  log_read <- read_csv(maintenance_file, col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  ))
  
  log <- log_read
  
  ### identify the date subsetting for the data
  if (!is.null(start_date)){
    catdata <- catdata %>% 
      filter(DateTime >= start_date)
    log <- log %>% 
      filter(TIMESTAMP_start >= start_date)
  }
  
  if(!is.null(end_date)){
    catdata <- catdata %>% 
      filter(DateTime <= end_date)
    log <- log %>% 
      filter(TIMESTAMP_end <= end_date)
  }
  
  if (nrow(log) == 0){
    log <- log_read
  }
  
########Create Flag columns####################################################################################################################### 

  # remove NaN data at beginning when data when no sensors were connected to the data logger
  catdata <- catdata %>% filter(DateTime >= ymd_hms("2018-07-05 13:50:00"))
  
  
  # for loop to create flag columns
  for(j in c(5:20,23:42)) { #for loop to create new columns in data frame
    catdata[,paste0("Flag_",colnames(catdata[j]))] <- 0 #creates flag column + name of variable
    catdata[c(which(is.na(catdata[,j]))),paste0("Flag_",colnames(catdata[j]))] <-7 #puts in flag 7 if value not collected
  }
  
  # Change negative values to 0
  for(k in c(15:16,18:19,24:35)) { #for loop to create new columns in data frame
    catdata[c(which((catdata[,k]<0))),paste0("Flag_",colnames(catdata[k]))] <- 3
    catdata[c(which((catdata[,k]<0))),k] <- 0 #replaces value with 0
  }
  
  #add in the adjusted DO columns for used below as dates are in the maintenance log
  catdata$RDO_mgL_5_adjusted <-0
  catdata$RDOsat_percent_5_adjusted <-0
  catdata$RDO_mgL_9_adjusted <-0
  catdata$RDOsat_percent_9_adjusted <-0
  
##########Maintenance Log QAQC####################################################################################################  

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
      warning(paste("Could not parse column flagcol in row", i, "of the maintenance log. 
                    Most likely there is an NA because it is a note in the maintenance log and 
                    no data is altered. Good to double check."))
      next
    }
    
    #Get the Maintenance Flag 
    
    flag <- log$flag[i]
    
    # replace relevant data with NAs and set flags while maintenance was in effect
    if(flag==5)
      {
      catdata[catdata$DateTime >= start & catdata$DateTime <= end, flag_cols] <- flag
    }
    else if (flag==7){
      
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
        #print(paste("Row",i,"is in the maintenance log for 2020-08-19 20:00:00 or 2020-08-11 07:00:00"))
      }
      else if (start=="2020-08-26 08:00:00")
      {
        catdata[catdata$DateTime >= start & catdata$DateTime <= end, 81] = catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] + 
        sqrt(as.numeric(difftime(dt$DateTime, start, units = "mins")))/10000
        #print(paste("Row",i,"is in the maintenance log for 2020-08-26 12:00:00"))
      }
      else if (start=="2020-09-05 02:00:00")
      {
        catdata[catdata$DateTime >= start & catdata$DateTime <= end, 81] = catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] + 
          sqrt(as.numeric(difftime(dt$DateTime, start, units = "mins")))/3000
        #print(paste("Row",i,"is in the maintenance log for 2020-09-05 06:00:00"))
      }
      else{
        if(is.null(start_date)){ 
        warning(paste("Make sure row",i,"is in the maintenance log"))
        }
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
        #print(paste("Row",i,"is in the maintenance log for 2020-08-19 20:00:00 or 2020-08-11 07:00:00"))
        }
        else if (start=="2020-08-26 08:00:00")
        {
        catdata[catdata$DateTime >= start & catdata$DateTime <= end, 82] = catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] + 
        sqrt(as.numeric(difftime(dt$DateTime, start, units = "mins")))/10000/11.3*100
        #print(paste("Row",i,"is in the maintenance log for 2020-08-26 12:00:00"))
        }
        else if (start=="2020-09-05 02:00:00") 
        {
        catdata[catdata$DateTime >= start & catdata$DateTime <= end, 82] = catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] + 
        sqrt(as.numeric(difftime(dt$DateTime, start, units = "mins")))/3000/11.3*100
        #print(paste("Row",i,"is in the maintenance log for 2020-09-05 06:00:00"))
        }
      else{
        if(is.null(start_date)){ 
      warning(paste("Make sure row",i,"is in the maintenance log"))
        }
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
      # commented this out because it was a check when I was making the script and now it gives too 
      # many warnings
      #warning(paste("No DO time to adjust in row",i,"."))
    }
  }

##########Fill in non adjusted DO values##############################################################################################################
# Fill in adjusted DO values that didn't get changed. If the value didn't get changed then it is the same as values from the 
# non adjusted columns
  
  catdata=catdata%>%
    mutate(
      RDO_mgL_5_adjusted=ifelse(RDO_mgL_5_adjusted==0, RDO_mgL_5, RDO_mgL_5_adjusted),
      RDOsat_percent_5_adjusted=ifelse(RDOsat_percent_5_adjusted==0, RDOsat_percent_5, RDOsat_percent_5_adjusted),
      RDO_mgL_9_adjusted=ifelse(RDO_mgL_9_adjusted==0, RDO_mgL_9, RDO_mgL_9_adjusted),
      RDOsat_percent_9_adjusted=ifelse(RDOsat_percent_9_adjusted==0, RDOsat_percent_9, RDOsat_percent_9_adjusted)
    )

  # This is a check to make sure it worked a all columns looked like they should
#DO=catdata%>%
  #select(DateTime, RDO_mgL_5,RDO_mgL_5_adjusted, RDO_mgL_9, RDO_mgL_9_adjusted, Flag_RDO_mgL_5, Flag_RDO_mgL_9)
 
############## Remove and Flag when sensors are out of position ####################
 
#change EXO values to NA if EXO depth is less than 0.5m and Flag as 2

#index only the colummns with EXO at the beginning
exo_idx <-grep("^EXO",colnames(catdata))


#create list of the Flag columns that need to be changed to 2
exo_flag <- grep("^Flag_EXO.*_1$",colnames(catdata))

#Change the EXO data to NAs when the EXO is above 0.5m and not due to maintenance
#Flag the data that was removed with 2 for outliers
catdata[which(catdata$EXODepth_m<0.55),exo_flag]<- 2
catdata[which(catdata$EXODepth_m < 0.55), exo_idx] <- NA

#change the temp string and pressure sensor to NA if the psi is less than XXXXX and Flag as 2

#index only the colummns with EXO at the beginning
temp_idx <-grep("^Ther*|^RDO*|^Lvl*",colnames(catdata))

#create list of the Flag columns that need to be changed to 2
temp_flag <- grep("^Flag_Ther*|^Flag_RDO*|^Flag_Lvl*",colnames(catdata))

#Change the EXO data to NAs when the pressure sensor is less than 9.94 psi which is roughly 7m and not due to maintenance
#Flag the data that was removed with 2 for outliers
catdata[which(catdata$LvlPressure_psi_9< 9.94),temp_flag]<- 2
catdata[which(catdata$LvlPressure_psi_9 < 9.94), temp_idx] <- NA


############## Leading and Lagging QAQC ##########################
# This finds the point that is way out of range from the leading and lagging point 

# loops through all of the columns to catch values that are above 2 or 4 sd above or below
# the leading or lagging point 

# need to make it a data frame before

catdata=data.frame(catdata)


for (a in c(5:20,23:42)){
  Var_mean <- mean(catdata[,a], na.rm = TRUE)
  
  # For Algae sensors we use 4 sd as a threshold but for the others we use 2
  if (colnames(catdata[a]) %in% c("EXOChla_RFU_1","EXOChla_ugL_1","EXOBGAPC_RFU_1","EXOBGAPC_ugL_1")){
    Var_threshold <- 4 * sd(catdata[,a], na.rm = TRUE)
  }else{ # all other variables we use 2 sd as a threshold
    Var_threshold <- 2 * sd(catdata[,a], na.rm = TRUE)
  }
  # Create the observation column, the lagging column and the leading column
  catdata$Var <- lag(catdata[,a], 0)
  catdata$Var_lag = lag(catdata[,a], 1)
  catdata$Var_lead = lead(catdata[,a], 1)
  
  # Replace the observations that are above the threshold with NA and then put a flag in the flag column
  
  catdata[c(which((abs(catdata$Var_lag - catdata$Var) > Var_threshold) &
                     (abs(catdata$Var_lead - catdata$Var) > Var_threshold)&!is.na(catdata$Var))) ,a] <-NA
  
  catdata[c(which((abs(catdata$Var_lag - catdata$Var) > Var_threshold) &
                     (abs(catdata$Var_lead - catdata$Var) > Var_threshold)&!is.na(catdata$Var))) ,paste0("Flag_",colnames(catdata[a]))]<-2
}


# Remove the leading and lagging columns

catdata<-catdata%>%select(-c(Var, Var_lag, Var_lead))

############# Additional EXO algae sensor QAQC and Conductivity in 2020 ##################
# goal for later 2023 is to get this into the maintenance log but don't have time now

Chla_RFU_1_mean <- mean(catdata$EXOChla_RFU_1, na.rm = TRUE)
Chla_ugL_1_mean <- mean(catdata$EXOChla_ugL_1, na.rm = TRUE)
BGAPC_RFU_1_mean <- mean(catdata$EXOBGAPC_RFU_1, na.rm = TRUE)
BGAPC_ugL_1_mean <- mean(catdata$EXOBGAPC_ugL_1, na.rm = TRUE)
Chla_RFU_1_threshold <- 4 * sd(catdata$EXOChla_RFU_1, na.rm = TRUE)
Chla_ugL_1_threshold <- 4 * sd(catdata$EXOChla_ugL_1, na.rm = TRUE)
BGAPC_RFU_1_threshold <- 4 * sd(catdata$EXOBGAPC_RFU_1, na.rm = TRUE)
BGAPC_ugL_1_threshold <- 4 * sd(catdata$EXOBGAPC_ugL_1, na.rm = TRUE)

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

  
  #Flag high conductivity values in 2020 but don't remove them. If want to remove later I will but I am
  # not convinced it is a sensor malfunction. Did everything based off of conductivity which is temperature
  # so catches anomalies as opposed to turnover which would happed if we used specific conductivity.  
  
  catdata <- catdata%>%
    mutate(
      #DateTime=as.character(DateTime),#change DateTime to as.character so they line up when making changes
      Flag_EXOCond_uScm_1 = ifelse(DateTime >ymd("2020-05-01") & DateTime < ymd("2020-09-01") &
                           EXOCond_uScm_1>37 & Flag_EXOCond_uScm_1==0 ,5, Flag_EXOCond_uScm_1),
      Flag_EXOSpCond_uScm_1 = ifelse(DateTime >ymd("2020-05-01") & DateTime < ymd("2020-09-01") &
                                       EXOCond_uScm_1>37 & Flag_EXOSpCond_uScm_1==0 ,5, Flag_EXOSpCond_uScm_1),
      Flag_EXOTDS_mgL_1 = ifelse(DateTime >ymd("2020-05-01") & DateTime < ymd("2020-09-01") &
                                   EXOCond_uScm_1>37 & Flag_EXOTDS_mgL_1==0 ,5, Flag_EXOTDS_mgL_1))
  
############ Convert psi to depth for pressure sensor ###################
  
  #create depth column
  catdata=catdata%>%mutate(LvlDepth_m_9=LvlPressure_psi_9*0.70455)#1psi=2.31ft, 1ft=0.305m
  

##########Put everything in the right place##################################################################################  
   # delete EXO_Date and EXO_Time columns
  catdata <- catdata %>% select(-EXO_Date, -EXO_Time)
  
  # add Reservoir and Site columns
  catdata$Reservoir <- "FCR"
  catdata$Site <- 50
  
  
  # reorder columns
  catdata <- catdata %>% select(Reservoir, Site, DateTime, ThermistorTemp_C_surface:ThermistorTemp_C_9,
                                RDO_mgL_5, RDOsat_percent_5,RDO_mgL_5_adjusted, RDOsat_percent_5_adjusted, 
                                RDOTemp_C_5, RDO_mgL_9, RDOsat_percent_9, 
                                RDO_mgL_9_adjusted, RDOsat_percent_9_adjusted, RDOTemp_C_9,
                                EXOTemp_C_1, EXOCond_uScm_1, EXOSpCond_uScm_1, EXOTDS_mgL_1, EXODOsat_percent_1,
                                EXODO_mgL_1, EXOChla_RFU_1, EXOChla_ugL_1, EXOBGAPC_RFU_1, EXOBGAPC_ugL_1,
                                EXOfDOM_RFU_1, EXOfDOM_QSU_1,EXOTurbidity_FNU_1, EXOPressure_psi, EXODepth_m, EXOBattery_V, EXOCablepower_V,
                                EXOWiper_V, LvlPressure_psi_9, LvlTemp_C_9, LvlDepth_m_9, RECORD, CR6Battery_V, CR6Panel_Temp_C,
                                Flag_ThermistorTemp_C_surface:Flag_ThermistorTemp_C_9,Flag_RDO_mgL_5, Flag_RDOsat_percent_5, Flag_RDOTemp_C_5,
                                Flag_RDO_mgL_9, Flag_RDOsat_percent_9, Flag_RDOTemp_C_9,Flag_EXOTemp_C_1, Flag_EXOCond_uScm_1, Flag_EXOSpCond_uScm_1,Flag_EXOTDS_mgL_1,
                                Flag_EXODOsat_percent_1, Flag_EXODO_mgL_1, Flag_EXOChla_RFU_1,Flag_EXOChla_ugL_1, Flag_EXOBGAPC_RFU_1,Flag_EXOBGAPC_ugL_1,
                                Flag_EXOfDOM_RFU_1,Flag_EXOfDOM_QSU_1,Flag_EXOTurbidity_FNU_1, Flag_EXOPressure_psi, Flag_EXODepth_m, Flag_EXOBattery_V, Flag_EXOCablepower_V,
                                Flag_EXOWiper_V, Flag_LvlPressure_psi_9, Flag_LvlTemp_C_9)
  
  #order by date and time
  catdata <- catdata[order(catdata$DateTime),]
  
  # convert datetimes to characters so that they are properly formatted in the output file
  catdata$DateTime <- as.character(catdata$DateTime)
  
  # subset to only the current year when using for EDI publishing
  # current_time_end is set in Chunk 1 Set Up in Inflow_QAQC_Plots_2013_2022.Rmd
  if(is.null(start_date)){
    catdata <- catdata[catdata$DateTime<ymd_hms(current_time_end),]
  }
  
  # write to output file
  write_csv(catdata, output_file)
}

# example usage
#qaqc("https://raw.githubusercontent.com/CareyLabVT/SCCData/mia-data/Catwalk.csv",
#     'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/FCRWaterLevel.csv',
#     "https://raw.githubusercontent.com/CareyLabVT/SCCData/mia-data/CAT_MaintenanceLog.txt",
#    "Catwalk.csv")


