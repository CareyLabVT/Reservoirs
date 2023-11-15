qaqc_fcrmet <- function(data_file, maintenance_file, met_infrad, output_file, start_date, end_date)
{
  
  #### Name the data file #### 
  # This section either uses the compiled data from FCR_MET_QAQC_Plots_2015_2022.Rmd which is already labeled Met
  # or it reads in and formats the current file off the data logger
  
  Met=data_file
  
  if (is.character(Met)==T) {
    Met<-#read_csv(file.path("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data/FCRmet.csv"),skip = 4, col_names=F, show_col_types = F)
      read_csv(Met, skip = 4, col_names=F, show_col_types = F)
    Met[,17]<-NULL #remove column
    names(Met) = c("DateTime","Record", "CR3000Battery_V", "CR3000Panel_Temp_C", 
                   "PAR_umolm2s_Average", "PAR_Total_mmol_m2", "BP_Average_kPa", "AirTemp_C_Average", 
                   "RH_percent", "Rain_Total_mm", "WindSpeed_Average_m_s", "WindDir_degrees", "ShortwaveRadiationUp_Average_W_m2",
                   "ShortwaveRadiationDown_Average_W_m2", "InfraredRadiationUp_Average_W_m2",
                   "InfraredRadiationDown_Average_W_m2", "Albedo_Average_W_m2")
    
    Met$DateTime<-as.POSIXct(strptime(Met$DateTime, "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+5")
  }else {
    Met=data_file
  }
  
  ## read in maintenance file 
  log_read <- read_csv(maintenance_file, col_types = cols(
    #read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_FCRMetData/2022/misc_data_files/FCR_Met_Maintenance_2015_2022.txt", col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer(),
    colnumber = col_integer()
  )) 
  
  log <- log_read
  
  ### identify the date subsetting for the data
  if (!is.null(start_date)){
    Met <- Met %>% 
      filter(DateTime >= start_date)
    log <- log %>% 
      filter(TIMESTAMP_start >= start_date)
  }
  
  if(!is.null(end_date)){
    Met <- Met %>% 
      filter(DateTime <= end_date)
    log <- log %>% 
      filter(TIMESTAMP_end <= end_date)
  }
  
  if (nrow(log) == 0){
    log <- log_read
  }

  ####Create data flags for publishing ####
  #get rid of NaNs
  #create flag + notes columns for data columns c(5:17)
  #set flag 2
  for(i in 5:17) { #for loop to create new columns in data frame
    Met[,paste0("Flag_",colnames(Met[i]))] <- 0 #creates flag column + name of variable
    Met[,paste0("Note_",colnames(Met[i]))] <- NA #creates note column + names of variable
    Met[which(is.na(Met[,i])),i] <- NA
    Met[c(which(is.na(Met[,i]))),paste0("Flag_",colnames(Met[i]))] <-2 #puts in flag 2
    Met[c(which(is.na(Met[,i]))),paste0("Note_",colnames(Met[i]))] <- "Sample not collected" #note for flag 2
  }

  #### Load in maintenance txt file #### 
  # the maintenance file tracks when sensors were repaired or offline due to maintenance
  
  #create loop putting in maintenance flags (these are flags for values removed due
  # to maintenance and also flags potentially questionable values)
  
  #Read in the maintenance log 
  
  # log <- read_csv(maintenance_file, col_types = cols(
  #   #read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_FCRMetData/2022/misc_data_files/FCR_Met_Maintenance_2015_2022.txt", col_types = cols(
  #   .default = col_character(),
  #   TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  #   TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  #   flag = col_integer(),
  #   colnumber = col_integer()
  # )) 
  
  # Get correct timestamp with Met file
  log$TIMESTAMP_start=ymd_hms(log$TIMESTAMP_start, tz="Etc/GMT+5")#setting time zone
  log$TIMESTAMP_end=ymd_hms(log$TIMESTAMP_end, tz="Etc/GMT+5") #setting time zone
  
  # modify fcr met data based on the information in the log
  for(i in 1:nrow(log))
  {
    # get start and end time of one maintenance event
    start <- log$TIMESTAMP_start[i]
    end <- log$TIMESTAMP_end[i]
    
    # Get Maintenance column
    maintenance_cols <- log$colnumber[i]  
    
    #Get the Maintenance Flag and notes
    
    flag <- log$flag[i]
    note <- log$notes[i]
    
    # Now take out the maintenance values
    
    if(flag==1 && maintenance_cols%in%c(1:4)){
      #takes out values and replaces them with NA
      Met[c(which(Met$DateTime>=start & Met$DateTime<=end)), maintenance_cols] = NA
    }else if (flag==1 && maintenance_cols%in%c(5:17)){
      #print(j)
      Met[c(which(Met$DateTime>=start & Met$DateTime<=end)), paste0("Flag_",colnames(Met[,maintenance_cols]))] = flag #when met timestamp is between remove timestamp
      #and met column derived from remove column
      #matching time frame, inserting flag
      Met[c(which(Met$DateTime>=start & Met$DateTime<=end)), paste0("Note_",colnames(Met[,maintenance_cols]))]= note#same as above, but for notes
      
      Met[c(which(Met$DateTime>=start & Met$DateTime<=end)), maintenance_cols] = NA
    }
    ## #if statement to only write in flag 4 if there are no other flags
    if(as.character(start)=="2022-07-15 13:29:00" && flag==4 && maintenance_cols==6){
      Met[c(which(Met$DateTime>=start & Met$DateTime<=end & (Met[,paste0("Flag_",colnames(Met[,maintenance_cols]))]==0))), paste0("Note_",colnames(Met[,maintenance_cols]))] = note #same as above, but for notes
      Met[c(which(Met$DateTime>=start & Met$DateTime<=end & (Met[,paste0("Flag_",colnames(Met[,maintenance_cols]))]==0))), paste0("Flag_",colnames(Met[,maintenance_cols]))] = flag #when met timestamp is between remove timestamp
      # Fixes the PAR_Total values that were too high
      Met[c(which(Met$DateTime>=start & Met$DateTime<=end)), maintenance_cols] = (Met[c(which(Met$DateTime>=start & Met$DateTime<=end)), maintenance_cols]/2.551047)*1.275523
    }else if (as.character(start)=="2015-07-07 15:45:00" && flag==4){
      Met[c(which(Met$DateTime>=start & Met$DateTime<=end & (Met[,paste0("Flag_",colnames(Met[,maintenance_cols]))]==0))), paste0("Note_",colnames(Met[,maintenance_cols]))] = note #same as above, but for notes
      Met[c(which(Met$DateTime>=start & Met$DateTime<=end & (Met[,paste0("Flag_",colnames(Met[,maintenance_cols]))]==0))), paste0("Flag_",colnames(Met[,maintenance_cols]))] = flag #when met timestamp is between remove timestamp
      # Fixes rain gauge when met station was recording in 5 min intervals instead of 1 minute
      Met[c(which(Met$DateTime>=start & Met$DateTime<=end & Met[,"Rain_Total_mm"]>0)), maintenance_cols] = Met[c(which(Met$DateTime>=start & Met$DateTime<=end & Met[,"Rain_Total_mm"]>0)), maintenance_cols]/5
    } else if(flag==4 && as.character(start)!="2022-07-15 13:29:00" && as.character(start)!="2015-07-07 15:45:00"){
      #print(j) # #if statement to only write in flag 4 if there are no other flags
      Met[c(which(Met$DateTime>=start & Met$DateTime<=end & (Met[,paste0("Flag_",colnames(Met[,maintenance_cols]))]==0))), paste0("Note_",colnames(Met[,maintenance_cols]))] = note #same as above, but for notes
      Met[c(which(Met$DateTime>=start & Met$DateTime<=end & (Met[,paste0("Flag_",colnames(Met[,maintenance_cols]))]==0))), paste0("Flag_",colnames(Met[,maintenance_cols]))] = flag#when met timestamp is between remove timestamp
      #print(j)#and met column derived from remove column
      #matching time frame, inserting flag
      Met[c(which(Met$DateTime>=start & Met$DateTime<=end)), maintenance_cols] = NA
    }
    if (flag==5){
      
      Met[c(which(Met$DateTime>=start & Met$DateTime<=end)),paste0("Flag_",colnames(Met[,maintenance_cols]))] = flag #when met timestamp is between remove timestamp
      #and met column derived from remove column
      # adding note from maintenance log to Met file
      Met[c(which(Met$DateTime>=start & Met$DateTime<=end)), paste0("Note_",colnames(Met[,maintenance_cols]))] = note #same as above, but for notes
    }
    next
  }
  
  #### Rain totals QAQC######
  
  # Take out Rain Totals above 5mm in 1 minute
  Rain<-c(which(!is.na(Met$Rain_Total_mm) & Met$Rain_Total_mm>5))
  
  Met[Rain, "Note_Rain_Total_mm"]<-"Rain total over 5mm in one minute so removed as outlier"
  Met[Rain, "Flag_Rain_Total_mm"]<-4
  Met[Rain, "Rain_Total_mm"] <- NA
  
  
  ####Air temperature data cleaning ####
  # This is how to find the linear regression but don't need it now so can comment it out. 
  #No pre and post filter but run all through the QAQC 
  #MetAir_2015=Met[Met$DateTime<ymd_hms("2016-01-01 00:00:00"),c(1,4,8)]
  #lm_Panel2015=lm(MetAir_2015$AirTemp_C_Average ~ MetAir_2015$CR3000Panel_Temp_C)
  #summary(lm_Panel2015)#gives data on linear model parameters
  
  
  # substitute the calculated panel temp if air temp is missing
  Air<-which(Met$Flag_AirTemp_C_Average==2)
  
  Met[Air, "AirTemp_C_Average"]<- 1.6278 +(0.9008*(Met[Air, "CR3000Panel_Temp_C"]))
  Met[Air, "Note_AirTemp_C_Average"]<- "Substituted value calculated from Panel Temp and linear model"
  Met[Air, "Flag_AirTemp_C_Average"]<-4
  
  # Now substitute calculated panel temp if air temp greater than 3 sd
  # 3*sd(lm_Panel2015$residuals)= 0.9641772
  
  Air<-c(which((Met$AirTemp_C_Average - (1.6278+(0.9008*Met$CR3000Panel_Temp_C)))>(3*0.9641772) & !is.na(Met$AirTemp_C_Average)))
  
  Met[Air, "Note_AirTemp_C_Average"]<-"Substituted value calculated from Panel Temp and linear model"
  Met[Air, "Flag_AirTemp_C_Average"]<-4
  Met[Air, "AirTemp_C_Average"]<-1.6278+(0.9008*(Met[Air, "CR3000Panel_Temp_C"]))
  
  #Air temp maximum set
  
  b=c(which(!is.na(Met$AirTemp_C_Average) & Met$AirTemp_C_Average>40.6))
  
  Met[b,"Note_AirTemp_C_Average"]<-"Outlier set to NA. Value over 40.6"
  Met[b, "Flag_AirTemp_C_Average"]<-4
  Met[b, "AirTemp_C_Average"]<-NA
  
  ###Infared radiation cleaning####
  #fix infrared radiation voltage reading after airtemp correction
  #only need to do this for data from 2015 to  2016-07-25 10:12:00
  
  # name of which argument that is repeated below 
  Dn<-c(which(Met$DateTime<ymd_hms("2016-07-25 10:12:00") & Met$InfraredRadiationDown_Average_W_m2<250 & 
                !is.na(Met$InfraredRadiationDown_Average_W_m2)))
  
  Met[Dn, "Note_InfraredRadiationDown_Average_W_m2"] <- "Value corrected with InfRadDn equation as described in metadata"
  Met[Dn, "Flag_InfraredRadiationDown_Average_W_m2"] <- 4
  Met[Dn, "InfraredRadiationDown_Average_W_m2"] <- 5.67*10^-8*(Met[Dn, "AirTemp_C_Average"]+273.15)^4
  
  # name of which argument that is repeated below
  Up<-c(which(Met$DateTime<ymd_hms("2016-07-25 10:12:00") & Met$InfraredRadiationUp_Average_W_m2<100 & 
                !is.na(Met$InfraredRadiationUp_Average_W_m2)))
  
  Met[Up, "Note_InfraredRadiationUp_Average_W_m2"] <- "Value corrected with InfRadUp equation as described in metadata"
  Met[Up, "Flag_InfraredRadiationUp_Average_W_m2"] <- 4
  Met[Up, "InfraredRadiationDown_Average_W_m2"] <- 5.67*10^-8*(Met[Up, "AirTemp_C_Average"]+273.15)^4
  
  #Mean correction for InfRadDown (needs to be after voltage correction)
  #Using 2018 data, taking the mean and sd of values on DOY to correct to
  Met$DOY=yday(Met$DateTime)
  
  # Read in the infrared file from Github for QAQC. This is the DOY averages of infrared and sd for 2018 
  infrad <- read_csv(met_infrad)
  #read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_FCRMetData/2022/misc_data_files/FCR_Met_Infrad_DOY_Avg_2018.csv")
  
  #putting in columns for infrared mean and sd by DOY into main data set
  Met=merge(Met, infrad, by = "DOY") 
  Met=Met[order(Met$DateTime),] #ordering table after merging and removing unnecessary columns
  
  #If the IR Down is greater than 3SD by DOY and replace with the equation from the manual(5.67*10-8(AirTemp_C_Average+273.15)^4)
  
  # name of which argument used below
  IR_Dn=c(which(abs(Met$InfraredRadiationDown_Average_W_m2-Met$infradavg)>(2*Met$infradsd) & !is.na(Met$InfraredRadiationDown_Average_W_m2)))
  
  Met[IR_Dn, "Flag_InfraredRadiationDown_Average_W_m2"] <- 4
  Met[IR_Dn, "Note_InfraredRadiationDown_Average_W_m2"] <- "Greater than 2SD Value corrected with InfRadDn equation as described in metadata"
  Met[IR_Dn, "InfraredRadiationDown_Average_W_m2"] <- 5.67*10^-8*(Met[IR_Dn,"AirTemp_C_Average"]+273.15)^4
  
  # Get rid of columns we don't need
  Met<-Met%>%
    select(-c("DOY","infradavg","infradsd"))
  
  #Inf outliers, must go after corrections
  
  # name of which argument used below
  IR_Up=c(which(Met$InfraredRadiationUp_Average_W_m2<150 & !is.na(Met$InfraredRadiationUp_Average_W_m2)))
  
  Met[IR_Up,"Flag_InfraredRadiationUp_Average_W_m2"]<-4
  Met[IR_Up,"Note_InfraredRadiationUp_Average_W_m2"]<-"Outlier set to NA. Value below 150 W/m2"
  Met[IR_Up,"InfraredRadiationUp_Average_W_m2"]<-NA
  
  
  IR_Dn<-c(which(Met$InfraredRadiationDown_Average_W_m2>540 & !is.na(Met$InfraredRadiationDown_Average_W_m2)|Met$InfraredRadiationDown_Average_W_m2<200 & !is.na(Met$InfraredRadiationDown_Average_W_m2)))
  
  Met[IR_Dn,"Flag_InfraredRadiationDown_Average_W_m2"]<-4
  Met[IR_Dn,"Note_InfraredRadiationDown_Average_W_m2"]<-"Outlier set to NA due to probable sensor failure. Value above 540 W/m2"
  Met[IR_Dn,"InfraredRadiationDown_Average_W_m2"]<-NA
  
  #Replace missing values with estimations from the equation
  
  # Name of which argument to use below
  IR_Dn<-which(Met$Flag_InfraredRadiationDown_Average_W_m2==2)    
  
  Met[IR_Dn,"InfraredRadiationDown_Average_W_m2"]<- 5.67*10^-8*(Met[IR_Dn,"AirTemp_C_Average"]+273.15)^4 
  Met[IR_Dn,"Note_InfraredRadiationDown_Average_W_m2"]<-"Value corrected with InfRadDn equation as described in metadata" 
  Met[IR_Dn,"Flag_InfraredRadiationDown_Average_W_m2"]<-4
  
  # Name of which argument to use below
  IR_Up<-which(Met$Flag_InfraredRadiationUp_Average_W_m2==2)
  
  Met[IR_Up,"InfraredRadiationUp_Average_W_m2"]<- 5.67*10^-8*(Met[IR_Up,"AirTemp_C_Average"]+273.15)^4 
  Met[IR_Up,"Note_InfraredRadiationUp_Average_W_m2"]<-"Value corrected with InfRadUp equation as described in metadata" 
  Met[IR_Up,"Flag_InfraredRadiationUp_Average_W_m2"]<-4
  
  
  ###Impossible Outliers####
  #take out impossible outliers and Infinite values before the other outliers are removed
  #set flag 3 (see metadata: this corrects for impossible outliers)
  for(i in 5:17) { #for loop to create new columns in data frame
    Met[c(which(is.infinite(Met[,i]))),paste0("Flag_",colnames(Met[i]))] <-3 #puts in flag 3
    Met[c(which(is.infinite(Met[,i]))),paste0("Note_",colnames(Met[i]))] <- "Infinite value set to NA" #note for flag 3
    Met[c(which(is.infinite(Met[,i]))),i] <- NA #set infinite vals to NA
    
    if(i!=8) { #flag 3 for negative values for everything except air temp
      Met[c(which((Met[,i]<0))),paste0("Flag_",colnames(Met[i]))] <- 3
      Met[c(which((Met[,i]<0))),paste0("Note_",colnames(Met[i]))] <- "Negative value set to 0"
      Met[c(which((Met[,i]<0))),i] <- 0 #replaces value with 0
    }
    if(i==9) { #flag for RH over 100
      Met[c(which((Met[,i]>100))),paste0("Flag_",colnames(Met[i]))] <- 3
      Met[c(which((Met[,i]>100))),paste0("Note_",colnames(Met[i]))] <- "Value set to 100"
      Met[c(which((Met[,i]>100))),i] <- 100 #replaces value with 100
    }
  }
  
  ###Wind Outliers####
  # Take out if wind direction is not between 0 and 360 and if wind speed is above 50 m/s
  
  # Name of which argument to use below
  Wi<-c(which(Met$WindDir_degrees<0 & !is.na(Met$WindDir_degrees) | Met$WindDir_degrees>360 & !is.na(Met$WindDir_degrees)))
  
  Met[Wi,"Note_WindDir_degrees"]<-"Wind direction is below 0 or above 360 degrees"
  Met[Wi,"Flag_WindDir_degrees"]<-4
  Met[Wi,"WindDir_degrees"]<-NA
  
  # Set wind speed to NA if above 50 m/s. Update as needed. Haven't seen any real windspreed that high
  # Name of which argument used below
  Ws<- c(which(Met$WindSpeed_Average_m_s>50 & !is.na(Met$WindSpeed_Average_m_s)))
  
  Met[Ws, "Note_WindSpeed_Average_m_s"]<-"Wind Speed is above 50 m/s"
  Met[Ws, "Flag_WindSpeed_Average_m_s"]<-4
  Met[Ws, "WindSpeed_Average_m_s"]<-NA
  
  ####Remove barometric pressure outliers####
  # Name of which argument used below and then flag and replace when BP is less than 98.5
  BP<-c(which(Met$BP_Average_kPa<98.5 & !is.na(Met$BP_Average_kPa)))
  
  Met[BP,"Note_BP_Average_kPa"]<-"Outlier set to NA. Value below 98.5"
  Met[BP,"Flag_BP_Average_kPa"]<-4
  Met[BP,"BP_Average_kPa"]<-NA
  
  
  #####remove high PAR values at night ######
  #get sunrise and sunset times
  suntimes=getSunlightTimes(date = seq.Date(as.Date("2015-07-02"), Sys.Date(), by = 1),
                            keep = c("sunrise",  "sunset"),
                            lat = 37.30, lon = -79.83, tz = "Etc/GMT+5")
  
  #create date column
  Met$date <- as.Date(Met$DateTime)
  
  #create subset to join
  
  
  #now merge the datasets to get daylight time
  Met <- left_join(Met, suntimes, by = "date") %>%
    mutate(daylight_intvl = interval(sunrise, sunset)) %>%
    mutate(during_day = DateTime %within% daylight_intvl)
  
  #Remove PAR Tot
  # Take out the PAR values when it is above 1 mmol/m2 during the night
  # Name the which argument used below
  Par_Tot<-c(which(Met$during_day==FALSE & Met$PAR_Total_mmol_m2>1 & !is.na(Met$PAR_Total_mmol_m2)))
  
  Met[Par_Tot, "Flag_PAR_Total_mmol_m2"]<-4
  Met[Par_Tot, "Note_PAR_Total_mmol_m2"]<-"Outlier set to NA. Above 1 mmol_m2 during the night"
  Met[Par_Tot, "PAR_Total_mmol_m2"]<-NA
  
  # Name of which argument used below
  Par_Avg<-c(which(Met$during_day==FALSE & Met$PAR_umolm2s_Average> 5 & !is.na(Met$PAR_umolm2s_Average)))
  
  Met[Par_Avg,"Flag_PAR_umolm2s_Average"]<-4
  Met[Par_Avg,"Note_PAR_umolm2s_Average"]<-"Outlier set to NA. Above 5 umolm2s during the night"
  Met[Par_Avg,"PAR_umolm2s_Average"]<-NA
  
  ####Remove total PAR (PAR_Tot) outliers ####
  # Remove and set to NA for values over 200
  # Name of which argument to use below
  Par_Tot<-c(which(Met$PAR_Total_mmol_m2>200& !is.na(Met$PAR_Total_mmol_m2)))
  
  Met[Par_Tot, "Flag_PAR_Total_mmol_m2"]<-4
  Met[Par_Tot, "Note_PAR_Total_mmol_m2"]<-"Outlier set to NA. Above 200 mmol_m2"
  Met[Par_Tot, "PAR_Total_mmol_m2"]<-NA
  
  # Remove and set to NA for values over 3000
  # Name of which argument to use below
  Par_Avg<-c(which(Met$PAR_umolm2s_Average>3000 & !is.na(Met$PAR_umolm2s_Average)))
  
  Met[Par_Avg,"Flag_PAR_umolm2s_Average"]<-4
  Met[Par_Avg,"Note_PAR_umolm2s_Average"]<-"Outlier set to NA. Above 3000 umolm2s"
  Met[Par_Avg,"PAR_umolm2s_Average"]<-NA
  
  #Flag high average values for PAR Avg over 2500 during May 1- July 1, 2021
  # Name of which argument used below
  Par_Avg<-c(which(Met$DateTime>ymd_hms("2021-05-01 00:00:00") & Met$DateTime<ymd_hms("2021-07-01 23:59:00") & Met$PAR_umolm2s_Average>2500 & !is.na(Met$PAR_umolm2s_Average)))
  
  Met[Par_Avg,"Flag_PAR_umolm2s_Average"]<-4
  Met[Par_Avg,"Note_PAR_umolm2s_Average"]<-"Outlier set to NA. Above 2500 umolm2s during May 1 - July 1 2021"
  Met[Par_Avg,"PAR_umolm2s_Average"]<-NA
  
  # Flag high total values for PAR Tot over 150 during May 1- July 1, 2021 
  Par_Tot<-c(which(Met$DateTime>ymd_hms("2021-05-01 00:00:00") & Met$DateTime<ymd_hms("2021-07-01 23:59:00") & Met$PAR_Total_mmol_m2>150 & !is.na(Met$PAR_Total_mmol_m2)))
  
  Met[Par_Tot, "Flag_PAR_Total_mmol_m2"]<-4
  Met[Par_Tot, "Note_PAR_Total_mmol_m2"]<-"Outlier set to NA. Above 150 mmol_m2 during May 1 - July 1 2021"
  Met[Par_Tot, "PAR_Total_mmol_m2"]<-NA
  
  
  
  #Remove shortwave radiation outliers
  #first shortwave upwelling
  # Name of which argument used below
  Sr_Avg<-c(which(Met$ShortwaveRadiationUp_Average_W_m2>1500 & !is.na(Met$ShortwaveRadiationUp_Average_W_m2)))
  
  Met[Sr_Avg,"Flag_ShortwaveRadiationUp_Average_W_m2"]<-4
  Met[Sr_Avg,"Note_ShortwaveRadiationUp_Average_W_m2"]<-"Outlier set to NA. Value above 1500 W/m2"
  Met[Sr_Avg,"ShortwaveRadiationUp_Average_W_m2"]<-NA
  
  #add a flag for suspect values from 1499 to 1300
  #above 1500 already set to NA
  # Name of which argument used below
  Sr_Avg<-c(which(Met$ShortwaveRadiationUp_Average_W_m2>1300 & !is.na(Met$ShortwaveRadiationUp_Average_W_m2)))
  
  Met[Sr_Avg,"Flag_ShortwaveRadiationUp_Average_W_m2"]<-5
  Met[Sr_Avg,"Note_ShortwaveRadiationUp_Average_W_m2"]<-"Questionable value left in. Value between 1500-1300 W/m2"
  
  
  #and then shortwave downwelling (what goes up must come down)
  
  # Take out if value above 300 and set to NA
  # Name of which argument used below
  Sr_Dn<-c(which(Met$ShortwaveRadiationDown_Average_W_m2>300 & !is.na(Met$ShortwaveRadiationDown_Average_W_m2)))
  # Take out if value above 300 and set to NA
  Met[Sr_Dn,"Flag_ShortwaveRadiationDown_Average_W_m2"]<-4
  Met[Sr_Dn,"Note_ShortwaveRadiationDown_Average_W_m2"]<-"Outlier set to NA. Value above 300 W/m2"
  Met[Sr_Dn,"ShortwaveRadiationDown_Average_W_m2"]<-NA
  
  #Remove albedo outliers
  # Remove and set to NA if over 1000
  Alb<-c(which(Met$Albedo_Average_W_m2>1000 & !is.na(Met$Albedo_Average_W_m2)))
  
  Met[Alb, "Flag_Albedo_Average_W_m2"]<-4
  Met[Alb, "Note_Albedo_Average_W_m2"]<-"Outlier_set_to_NA. Value above 1000 W/m2"
  Met[Alb, "Albedo_Average_W_m2"]<-NA
  
  #set to NA when shortwave radiation up is equal to NA
  
  Alb<-c(which(is.na(Met$ShortwaveRadiationUp_Average_W_m2)|is.na(Met$ShortwaveRadiationDown_Average_W_m2)))
  
  Met[Alb, "Flag_Albedo_Average_W_m2"]<-4
  Met[Alb, "Note_Albedo_Average_W_m2"]<-"Set to NA because Shortwave equals NA"
  Met[Alb, "Albedo_Average_W_m2"]<-NA
  # Take out the columns we don't need any more and add Reservoir and Site columns
  Met=Met%>%
    select(-c(date, lat, lon, sunrise, sunset,daylight_intvl,during_day))%>%
    mutate(Reservoir="FCR",
           Site=50)
  
  
  ###7) Write file with final cleaned dataset! ###
  Met_final=Met%>%
    select(c("Reservoir","Site","DateTime","Record","CR3000Battery_V","CR3000Panel_Temp_C","PAR_umolm2s_Average","PAR_Total_mmol_m2","BP_Average_kPa",                          
             "AirTemp_C_Average","RH_percent","Rain_Total_mm","WindSpeed_Average_m_s","WindDir_degrees","ShortwaveRadiationUp_Average_W_m2",       
             "ShortwaveRadiationDown_Average_W_m2","InfraredRadiationUp_Average_W_m2","InfraredRadiationDown_Average_W_m2","Albedo_Average_W_m2",
             "Flag_PAR_umolm2s_Average","Note_PAR_umolm2s_Average","Flag_PAR_Total_mmol_m2","Note_PAR_Total_mmol_m2","Flag_BP_Average_kPa",
             "Note_BP_Average_kPa","Flag_AirTemp_C_Average","Note_AirTemp_C_Average","Flag_RH_percent","Note_RH_percent","Flag_Rain_Total_mm",
             "Note_Rain_Total_mm","Flag_WindSpeed_Average_m_s","Note_WindSpeed_Average_m_s",
             "Flag_WindDir_degrees","Note_WindDir_degrees","Flag_ShortwaveRadiationUp_Average_W_m2","Note_ShortwaveRadiationUp_Average_W_m2",
             "Flag_ShortwaveRadiationDown_Average_W_m2","Note_ShortwaveRadiationDown_Average_W_m2",
             "Flag_InfraredRadiationUp_Average_W_m2","Note_InfraredRadiationUp_Average_W_m2",
             "Flag_InfraredRadiationDown_Average_W_m2","Note_InfraredRadiationDown_Average_W_m2","Flag_Albedo_Average_W_m2","Note_Albedo_Average_W_m2"))
  
  
  
  #### Write to CSV ####
  # write_csv was giving the wrong times. Let's see if this is better. 
  write.csv(Met_final, output_file, row.names = FALSE)
  
  
}

