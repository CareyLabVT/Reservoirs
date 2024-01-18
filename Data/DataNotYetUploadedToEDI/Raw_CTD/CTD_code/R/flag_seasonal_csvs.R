#' 
#' @author Abigail Lewis
#' @title flag_seasonal_csvs
#' @description This function loads the saved CTD csv from this year (or multiple years) and adds data flags
#' 
#' @param ctd_season_csvs directory of CTD seasonal csvs
#' @param intermediate_file_name file name of un-flagged dataset
#' @param output_file_name file name of flagged dataset
#' @param CTD_FOLDER high level CTD folder where L1 output will be stored
#'
#' @return no output
#'

flag_seasonal_csvs <- function(ctd_season_csvs = "../CTD_season_csvs",
                               intermediate_file_name = "ctd_L0.csv",
                               output_file_name = "ctd_L1.csv",
                               CTD_FOLDER = "../",
                               maintenance_file = paste0(CTD_FOLDER, "CTD_Maintenance_Log.csv")) {
  
  ctd1 <- read.csv(paste0(ctd_season_csvs, "/", intermediate_file_name)) #Load saved data
  ctd = ctd1 %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%dT%H:%M:%SZ"),
           Reservoir = ifelse(Reservoir == "BRV", "BVR", Reservoir), #Fix typo
           Reservoir = sub("_", "", Reservoir), #Remove underscore
           Reservoir = as.factor(Reservoir))
  
  #Flag codes
  #0=Not suspect, 
  #1=Sample not taken, 
  #2=Instrument malfunction, 
  #3=Sample below detection,
  #4=Negative value set to 0 or NA
  #5=No sensor on CTD,
  #6=Measurement above water (removed for most vars)
  #7=Datetime missing time (date is meaningful but not time)
  #8=Measurement outside of expected range but retained in dataset
  
  ctd_flagged = ctd %>% #Add flags
    select(-Flag)%>%
    mutate(Flag_DateTime = 0,
           Flag_Temp_C = 0,
           Flag_DO_mgL = 0,
           Flag_DOsat_percent = 0,
           Flag_Cond_uScm = 0,
           Flag_SpCond_uScm = 0,
           Flag_Chla_ugL = 0,
           Flag_Turbidity_NTU = 0,
           Flag_pH = 0,
           Flag_ORP_mV = 0,
           Flag_PAR_umolm2s = 0,
           Flag_DescRate_ms = 0) %>%
    mutate(
      #TEMP
      Flag_Temp_C = ifelse(is.na(Temp_C),2,Flag_Temp_C), #Flag NA temperatures
      
      #DO
      Flag_DO_mgL = ifelse(is.na(DO_mgL),2,Flag_DO_mgL), #Flag NA
      Flag_DO_mgL = ifelse(DO_mgL < 0,4,Flag_DO_mgL), #Flag DO < 0, set to 0
      DO_mgL = ifelse(DO_mgL < 0, 0, DO_mgL), 
      
      #DO pSat
      Flag_DOsat_percent = ifelse(is.na(DOsat_percent),2,Flag_DOsat_percent), #Flag NA
      Flag_DOsat_percent = ifelse(DOsat_percent < 0,4,Flag_DOsat_percent), #Flag pSat < 0, set to 0
      DOsat_percent = ifelse(DOsat_percent < 0, 0, DOsat_percent), 
      
      #COND
      Flag_Cond_uScm = ifelse(is.na(Cond_uScm),2,Flag_Cond_uScm), #Flag NA
      Flag_Cond_uScm = ifelse(Cond_uScm < 0,4,Flag_Cond_uScm), #Flag Cond < 0, set to NA
      Cond_uScm = ifelse(Cond_uScm < 0, NA, Cond_uScm), 
      
      #SPECCOND
      Flag_SpCond_uScm = ifelse(is.na(SpCond_uScm),2,Flag_SpCond_uScm), #Flag NA
      Flag_SpCond_uScm = ifelse(SpCond_uScm < 0,4,Flag_SpCond_uScm), #Flag SpCond < 0, set to NA
      SpCond_uScm = ifelse(SpCond_uScm < 0, NA, SpCond_uScm), 
      
      #CHLA
      Flag_Chla_ugL = ifelse(is.na(Chla_ugL),2,Flag_Chla_ugL), #Flag NA
      Flag_Chla_ugL = ifelse(Chla_ugL < 0,4,Flag_Chla_ugL), #Flag Chla < 0, set to 0
      Chla_ugL = ifelse(Chla_ugL < 0, 0, Chla_ugL), 
      
      #TURB
      Flag_Turbidity_NTU = ifelse(is.na(Turbidity_NTU),2,Flag_Turbidity_NTU), #Flag NA
      Flag_Turbidity_NTU = ifelse(Turbidity_NTU < 0,4,Flag_Turbidity_NTU), #Flag turbidity < 0, set to 0
      Turbidity_NTU = ifelse(Turbidity_NTU < 0, 0, Turbidity_NTU), 
      
      #pH
      Flag_pH = ifelse(is.na(pH),2,Flag_pH), #Flag NA
      Flag_pH = ifelse(pH < 0,4,Flag_pH), #Flag pH < 0, set to NA
      pH = ifelse(pH < 0, NA, pH), 
      
      #ORP
      Flag_ORP_mV = ifelse(is.na(ORP_mV),2,Flag_ORP_mV), #Flag NA
      
      #PAR
      Flag_PAR_umolm2s = ifelse(is.na(PAR_umolm2s),2,Flag_PAR_umolm2s), #Flag NA
      Flag_PAR_umolm2s = ifelse(!is.na(PAR_umolm2s)&PAR_umolm2s < 0,4,Flag_PAR_umolm2s), #Flag PAR < 0, set to 0
      PAR_umolm2s = ifelse(!is.na(PAR_umolm2s)&PAR_umolm2s < 0, 0, PAR_umolm2s), 
      
      #DESC RATE
      Flag_DescRate_ms = ifelse(is.na(DescRate_ms),2,Flag_DescRate_ms), #Flag NA
      
      #DateTime 
      Flag_DateTime = ifelse(lubridate::hour(DateTime)==12&
                               lubridate::minute(DateTime)==0&
                               lubridate::seconds(DateTime)==0,
                             7,0) #Flag times that are missing time (date is meaningful but not time)
      )
  
  
  #Not all variables are meaningful out of the water
  Above_surface_flag = 6
  water_vars <- c("Chla_ugL","Turbidity_NTU","Cond_uScm","SpCond_uScm","DO_mgL","DOsat_percent","pH","ORP_mV")
  ctd_flagged[ctd_flagged$Depth_m<0, water_vars]<-NA
  ctd_flagged[ctd_flagged$Depth_m<0, paste0("Flag_", water_vars)] <- Above_surface_flag
  
  # Fix times
  # CTD times in 2022 are incorrect by ~2 hr
  ctd_flagged$DateTime[ctd_flagged$DateTime>as.Date("2021-12-01") &
                         ctd_flagged$DateTime<as.Date("2023-01-01")] = 
    ctd_flagged$DateTime[ctd_flagged$DateTime>as.Date("2021-12-01") &
                       ctd_flagged$DateTime<as.Date("2023-01-01")] + lubridate::hours(2) #to align with published data
  
  # CTD times in 2020 and 2021 are incorrect by ~13 hr
  ctd_flagged$DateTime[ctd_flagged$DateTime > as.Date("2020-01-01") & 
                     ctd_flagged$DateTime < as.Date("2021-12-01")] = 
    ctd_flagged$DateTime[ctd_flagged$DateTime > as.Date("2020-01-01") & 
                       ctd_flagged$DateTime < as.Date("2021-12-01")] + lubridate::hours(13) #to align with published data
  
  # CTD times in 2018 are incorrect by ~4 hr
  ctd_flagged$DateTime[lubridate::year(ctd_flagged$DateTime) == 2018] = 
    ctd_flagged$DateTime[lubridate::year(ctd_flagged$DateTime) == 2018] - lubridate::hours(4) #to align with published data
  
  final = ctd_flagged%>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
    filter(!(Reservoir == "BVR" & DateTime == "2022-04-20 09:05:48")) #This cast was mislabeled as BVR (both are present, and I can't find the csv to delete)
    
  #Fix for CTD when conductivity and specific conductivity columns were switched
  #spec_Cond_uScm=Cond_uScm/(1+(0.02*(Temp_C-25)))) so if temp is less than 25 conductivity is
  # less than specific conductivity and if temp is greater than 25 then conductivity is greater than 
  # specific conductivity. Based on this I created the a CTD_check column if the columns were good or bad. 
  # If they were bad then the conductivity and the spec. conductivity column need to be flipped. 
  
  #ABP 10 DEC 21
  
  CTD_fix=final%>%
    add_column(CTD_check = NA)%>% #create the CTD_check column
    #sets up criteria for the CTD_check column either "good","bad" or "NA"(if no data)
    mutate(
      CTD_check=ifelse(Temp_C<25& Cond_uScm<SpCond_uScm & !is.na(SpCond_uScm), "good",CTD_check),
      CTD_check=ifelse(Temp_C<25& Cond_uScm>SpCond_uScm & !is.na(SpCond_uScm), "bad",CTD_check),
      CTD_check=ifelse(Temp_C>25& Cond_uScm>SpCond_uScm & !is.na(SpCond_uScm), "good",CTD_check),
      CTD_check=ifelse(Temp_C>25& Cond_uScm<SpCond_uScm & !is.na(SpCond_uScm), "bad",CTD_check),
      CTD_check=ifelse(is.na(SpCond_uScm), "good",CTD_check),
      CTD_check=ifelse(Cond_uScm==0, "bad", CTD_check))%>%
    #the next part switches the column if labeled "bad" in CTD_check 
    transform(., SpCond_uScm = ifelse(CTD_check == 'bad' & !is.na(SpCond_uScm), Cond_uScm, SpCond_uScm), 
              Cond_uScm = ifelse(CTD_check == 'bad' & !is.na(SpCond_uScm), SpCond_uScm, Cond_uScm))%>%
    select(-CTD_check)%>%
    mutate(Site=ifelse(Reservoir=="BVR"&Site==1,40,Site),
           Site=ifelse(Site==49,50,Site))
  
  #Order columns
  CTD_fix_renamed = CTD_fix%>% 
    select(Reservoir, Site, SN, DateTime, Depth_m, Temp_C, DO_mgL, DOsat_percent, 
           Cond_uScm, SpCond_uScm, Chla_ugL, Turbidity_NTU, pH, ORP_mV, PAR_umolm2s, 
           DescRate_ms, Flag_DateTime, Flag_Temp_C, Flag_DO_mgL, Flag_DOsat_percent, 
           Flag_Cond_uScm, Flag_SpCond_uScm, Flag_Chla_ugL, Flag_Turbidity_NTU, 
           Flag_pH, Flag_ORP_mV, Flag_PAR_umolm2s, Flag_DescRate_ms)
  
  
  # ## ADD MAINTENANCE LOG FLAGS 
  # The maintenance log includes manual edits to the data for suspect samples or human error
  log_read <- read_csv(maintenance_file, col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  ))
  
  log <- log_read
  
  for(i in 1:nrow(log)){
    if (!is.na(log$Depth[i])) {
      warning("Maintenance log specifies a depth, but code is not set up to deal with specified depths")
      }
    
    ### Assign variables based on lines in the maintenance log.
    
    ### get start and end time of one maintenance event
    start <- force_tz(as.POSIXct(log$TIMESTAMP_start[i]), tzone = "America/New_York")
    end <- force_tz(as.POSIXct(log$TIMESTAMP_end[i]), tzone = "America/New_York")
    
    ### Get the Reservoir Name
    Reservoir <- log$Reservoir[i]
    # NAs mean fix everything
    if(is.na(Reservoir)){Reservoir <- unique(CTD_fix_renamed$Reservoir)}
    
    ### Get the Site Number
    Site <- as.numeric(log$Site[i])
    # NAs mean fix everything
    if(is.na(Site)){Site <- unique(CTD_fix_renamed$Site)}
    
    ### Get the SN
    SN <- as.numeric(log$SN[i])
    # NAs mean fix everything
    if(is.na(SN)){SN <- unique(CTD_fix_renamed$SN)}
    
    ### Get the Maintenance Flag
    flag <- log$flag[i]
    
    ### Get the correct columns
    colname_start <- log$start_parameter[i]
    colname_end <- log$end_parameter[i]
    ### if it is only one parameter then only one column will be selected
    if(is.na(colname_start)){
      maintenance_cols <- colnames(CTD_fix_renamed%>%select(colname_end))
    }else if(is.na(colname_end)){
      maintenance_cols <- colnames(CTD_fix_renamed%>%select(colname_start))
    }else{
      maintenance_cols <- colnames(CTD_fix_renamed%>%select(colname_start:colname_end))
    }
    
    ### Get the DateTimes
    if(is.na(end)){
      # If there the maintenance is on going then the columns will be removed until
      # an end date is added
      Time <- CTD_fix_renamed |> filter(DateTime >= start) |> select(DateTime)
    }else if (is.na(start)){
      # If there is only an end date change columns from beginning of data frame until end date
      Time <- CTD_fix_renamed |> filter(DateTime <= end) |> select(DateTime)
    }else {
      Time <- CTD_fix_renamed |> filter(DateTime >= start & DateTime <= end) |> select(DateTime)
    }
    times <- unique(Time$DateTime)
    
    ### This is where information in the maintenance log gets updated
    
    #Flag codes
    #0=Not suspect, 
    #1=Sample not taken, 
    #2=Instrument malfunction, 
    #3=Sample below detection,
    #4=Negative value set to 0 or NA
    #5=No sensor on CTD,
    #6=Measurement above water (removed for most vars)
    #7=Datetime missing time (date is meaningful but not time)
    #8=Measurement outside of expected range but retained in dataset
    
    if(flag %in% c(2, 8)){ ## UPDATE THIS WITH ANY NEW FLAGS
      # UPDATE THE MANUAL ISSUE FLAGS (BAD SAMPLE / USER ERROR) AND SET TO NEW VALUE
      if(is.na(log$update_value[i]) || !log$update_value[i] == "NO CHANGE"){
        CTD_fix_renamed[CTD_fix_renamed$DateTime %in% times &
                          CTD_fix_renamed$Reservoir %in% Reservoir &
                          CTD_fix_renamed$Site %in% Site &
                          CTD_fix_renamed$SN %in% SN, 
                        maintenance_cols] <- log$update_value[i]
      }
      CTD_fix_renamed[CTD_fix_renamed$DateTime %in% times &
                        CTD_fix_renamed$Reservoir %in% Reservoir &
                        CTD_fix_renamed$Site %in% Site &
                        CTD_fix_renamed$SN %in% SN, 
                      paste0("Flag_",maintenance_cols)] <- flag
      
    } else if (flag %in% c(8)){
      # value is suspect
      CTD_fix_renamed[CTD_fix_renamed$DateTime %in% times, paste0("Flag_",maintenance_cols)] <- flag
      
    }else{
      warning("Flag not coded in the L1 script. See Austin or Adrienne")
    }
  } # end for loop
  
  write.csv(CTD_fix_renamed, paste0(CTD_FOLDER, output_file_name), row.names = FALSE)
  message(paste0("Successfully updated ", output_file_name))
  
  return(CTD_fix_renamed)
}
