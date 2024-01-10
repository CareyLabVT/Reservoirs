pacman::p_load(oce, ocedata, tidyverse, lubridate)

setwd("./Data/DataNotYetUploadedToEDI/Raw_CTD/CTD_code/CTD_automation/")
source("ctd_functions_automated.R") #Load Carey Lab ctd functions
source("flag_seasonal_csvs.R")
source("process_CTD_file.R")
source("identify_new_files.R")
source("update_seasonal_csvs.R")

#' 
#' @author Abigail Lewis
#' @title ctd_QAQC
#' @description This function runs QAQC for any new CTD files and re-processes seasonal CSVs
#' 
#' @param raw_downloads directory where raw cvs are stored
#' @param ctd_cast_csvs directory where output csvs for individual casts are stored
#' @param ctd_season_csvs directory where seasonal output csv should be stored
#' @param CTD_FOLDER high level CTD folder (used to reference metadata, pdf outputs, etc). 
#' This is also where the ctd_L1.csv file will be stored
#'
#' @return no output
#'

ctd_QAQC <- function(raw_downloads = "../../RawDownloads",
                     ctd_cast_csvs = "../../csv_outputs",
                     ctd_season_csvs = "../../CTD_season_csvs",
                     CTD_FOLDER = "../../",
                     maintenance_file = '../../CTD_Maintenance_Log.csv', 
                     start_date = NULL, 
                     end_date = NULL){
  
  ## Identify files new files
  file_names <- identify_new_files(raw_downloads = raw_downloads,
                                   ctd_cast_csvs = ctd_cast_csvs)
  #If no new files, end QAQC
  if(length(file_names)==0){
    message("No new files")
    return()
  }
  
  ## Generate csv versions of these files (stored in csv_outputs folder)
  for(file in file_names) {
    process_CTD_file(file, 
                     raw_downloads = raw_downloads,
                     CTD_FOLDER = CTD_FOLDER) 
  }
  
  ## Generate updated seasonal csv
  update_seasonal_csvs(ctd_cast_csvs = ctd_cast_csvs,
                       ctd_season_csvs = ctd_season_csvs,
                       output_file_name = "CTD_Meta_2023.csv")
  
  ## Add data flags to seasonal csv
  ctd_df_flagged <- flag_seasonal_csvs(ctd_season_csvs = ctd_season_csvs,
                     input_file_name = "CTD_Meta_2023.csv",
                     output_file_name = "ctd_seasonal_flags.csv")
  
  Met <- ctd_df_flagged
  
  ## Add maintenance log flags
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
  
  ## ADD MAINTENANCE LOG FLAGS (manual edits to the data for suspect samples or human error)
  #maintenance_file <- 'Data/DataNotYetUploadedToEDI/YSI_PAR_Secchi/maintenance_log.txt'
  log_read <- read_csv2(maintenance_file, col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  ))
  
  log <- log_read
  
  
  ### identify the date subsetting for the data
  if (!is.null(start_date)){
    Met <- Met %>% 
      filter(DateTime >= start_date)
    log <- log %>%
      filter(TIMESTAMP_start <= end_date)
  }
  
  if(!is.null(end_date)){
    Met <- Met %>% 
      filter(DateTime <= end_date)
    log <- log %>%
      filter(TIMESTAMP_end >= start_date)
  }
  
  if (nrow(log) == 0){
    log <- log_read
  }
  
  # for(i in 6:17) { #for loop to create new columns in data frame
  #   Met[,paste0("Flag_",colnames(Met[i]))] <- 0 #creates flag column + name of variable
  #   Met[,paste0("Note_",colnames(Met[i]))] <- NA #creates note column + names of variable
  # }
  
  #Met$Reservoir <- "FCR"
  #Met$Site <- 50
  
  for(i in 1:nrow(log)){
    ### Assign variables based on lines in the maintenance log. 
    
    ### get start and end time of one maintenance event
    start <- force_tz(as.POSIXct(log$TIMESTAMP_start[i]), tzone = "America/New_York")
    end <- force_tz(as.POSIXct(log$TIMESTAMP_end[i]), tzone = "America/New_York")
    
    ### Get the Reservoir Name
    Reservoir <- log$Reservoir[i]
    
    ### Get the Site Number
    Site <- as.numeric(log$Site[i])
    
    ### Get the depth value
    Depth <- as.numeric(log$Depth[i])
    
    ### Get the Maintenance Flag 
    flag <- log$flag[i]
    
    ### Get the new value for a column
    update_value <- as.numeric(log$update_value[i])
    
    ## Get the adjustment code value for a column if needed 
    maint_adjustment_code <- log$adjustment_code[i]
    
    ### Get the names of the columns affected by maintenance
    colname_start <- log$start_parameter[i]
    colname_end <- log$end_parameter[i]
    
    ### if it is only one parameter parameter then only one column will be selected
    
    if(is.na(colname_start)){
      
      maintenance_cols <- colnames(Met%>%select(colname_end)) 
      
    }else if(is.na(colname_end)){
      
      maintenance_cols <- colnames(Met%>%select(colname_start))
      
    }else{
      maintenance_cols <- colnames(Met%>%select(colname_start:colname_end))
    }
    
    # remove supplement cols we don't need just in case 
    #avoid_cols <- c('Record','CR3000Battery_V','CR3000Panel_Temp_C')
    #maintenance_cols <- maintenance_cols[!maintenance_cols %in% avoid_cols]
    
    # remove flag and notes cols in the maint_col vector
    maintenance_cols <- maintenance_cols[!grepl('Flag', maintenance_cols)]
    maintenance_cols <- maintenance_cols[!grepl('Note', maintenance_cols)]
    
    
    if(is.na(end)){
      # If there the maintenance is on going then the columns will be removed until
      # and end date is added
      Time <- Met |> filter(DateTime >= start) |> select(DateTime)
      
    }else if (is.na(start)){
      # If there is only an end date change columns from beginning of data frame until end date
      Time <- Met |> filter(DateTime <= end) |> select(DateTime)
      
    }else {
      Time <- Met |> filter(DateTime >= start & DateTime <= end) |> select(DateTime)
    }
    
    ### This is where information in the maintenance log gets updated 
    
    if(flag %in% c(1,2)){
      # The observations are changed to NA for maintenance or other issues found in the maintenance log
      Met[Met$DateTime %in% Time$DateTime, maintenance_cols] <- NA
      Met[Met$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag
      
    }else if (flag %in% c(3)){ 
      ## BDL
      
      Met[c(which(Met[,'Site'] == Site & Met$DateTime %in% Time$DateTime)),maintenance_cols] <- NA
      Met[Met$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag
      
    }else if (flag %in% c(4)){
      ## change negative values are changed to 0
      
      Met[c(which(Met[,'Site'] == Site & Met$DateTime %in% Time$DateTime)),maintenance_cols] <- 0
      Met[Met$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag
      
    } else if(flag %in% c(5)){ 
      # NO sensor on CTD -- just flag
      Met[Met$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag
      
    }else if(flag %in% c(6)){ 
      # Mark data for sensors that are out of the water -- keep temperature -- just flag for now because I think Abby flags this somewhere else
      Met[Met$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag
      
    }else if(flag %in% c(7)){ 
      # Datetime is missing (date is accurate but time is not)
      Met[Met$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag
      
    }else if(flag %in% c(8)){ 
      # Measurement out of range -- either adjust the value or assign new value
      Met[Met$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag
      Met[c(which(Met[,'Site'] == Site & Met$DateTime %in% Time$DateTime)),maintenance_cols] <- update_value
      
      ## If the adjustment code is provided, this is where we would implement it in it's own conditional statement
      
    }else{
      warning("Flag not coded in the L1 script. See Austin or Adrienne")
    }
  }
  
  ### END MAINTENANCE LOG CODE ###
  
  write.csv(Met, '../../ctd_L1.csv', row.names = FALSE)
  
  # ## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
  # package_ID <- 'edi.200.13'
  # eml <- read_metadata(package_ID)
  # date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
  # last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)
  # 
  # 
  # updated_data <- raw_df |> filter(DateTime > last_edi_date)
  
  }

ctd_QAQC(maintenance_file = '../../CTD_Maintenance_Log.csv',
         start_date = '2023-01-01', 
         end_date = '2023-12-31')
