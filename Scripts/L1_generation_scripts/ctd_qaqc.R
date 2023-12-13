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
                     CTD_FOLDER = "../../"){
  
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
                     output_file_name = "ctd_L1.csv")
  
  ## Add maintenance log
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
  
  
  ### 4. Take out values based on the Maintenance Log (THIS IS A TEST FOR NOW) -- ALSO MIGHT MOVE SOMEWHERE ELSE IN SCRIPT
  ## TRY ADDING FLEXIBILITY TO IGNORE ANY COMMENTS IN MAINT LOG
  
  # modify raw_df based on the information in the log  
  
  # raw_df <- ctd_df_flagged
  
  # log_read <- read_csv(maintenance_file, skip=43, col_types = cols(
  #   .default = col_character(),
  #   TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  #   TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  #   flag = col_integer()
  # ))
  
  ### MAINTENANCE LOG CODE -- UNCOMMENT WHEN FILE IS READY
  # maintenance_file <- 'Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/maintenance_log.txt'
  # log_read <- read_csv(maintenance_file, col_types = cols(
  #   .default = col_character(),
  #   TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  #   TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  #   flag = col_integer()
  # ))
  # 
  # log <- log_read
  # 
  # for(i in 1:nrow(log))
  # {
  #   ### Assign variables based on lines in the maintenance log. 
  #   
  #   ### get start and end time of one maintenance event
  #   start <- log$TIMESTAMP_start[i]
  #   end <- log$TIMESTAMP_end[i]
  #   
  #   ### Get the Reservoir Name
  #   
  #   Reservoir <- log$Reservoir[i]
  #   
  #   ### Get the Site Number
  #   
  #   Site <- as.numeric(log$Site)
  #   
  #   ### Get the depth if it is not NA
  #   
  #   if(!is.na(log$Depth[i])){
  #     Depth <- as.numeric(log$new_value[i]) ## IS THERE SUPPOSED TO BE A COLUMN ADDED TO MAINT LOG CALLED NEW_VALUE?
  #   }
  #   
  #   ### Get the Maintenance Flag 
  #   
  #   flag <- log$flag[i]
  #   
  #   ### Get the new value for a column or an offset. If it is not an NA
  #   
  #   if(!is.na(log$update_value[i])){
  #     
  #     update_value <- as.numeric(log$update_value[i])
  #   }
  #   
  #   
  #   ### Get the names of the columns affected by maintenance
  #   
  #   colname_start <- log$start_parameter[i]
  #   colname_end <- log$end_parameter[i]
  #   
  #   ### if it is only one parameter parameter then only one column will be selected
  #   
  #   if(is.na(colname_start)){
  #     
  #     maintenance_cols <- colnames(raw_df%>%select(colname_end)) 
  #     
  #   }else if(is.na(colname_end)){
  #     
  #     maintenance_cols <- colnames(raw_df%>%select(colname_start))
  #     
  #   }else{
  #     maintenance_cols <- colnames(raw_df%>%select(colname_start:colname_end))
  #   }
  #   
  #   ### Get the name of the flag column -- mostly used for EXO -- remove here
  #   
  #   #flag_col <- paste0("Flag_", maintenance_cols)
  #   
  #   ### remove any Flag columns that don't exsist because we don't have a flag column for them
  #   # and they get removed before publishing
  #   
  #   #flag_col = flag_col[!flag_col %in% c(COLUMN NAMES HERE)]
  #   
  #   ### Getting the start and end time vector to fix. If the end time is NA then it will put NAs 
  #   # until the maintenance log is updated
  #   
  #   if(is.na(end)){
  #     # If there the maintenance is on going then the columns will be removed until
  #     # and end date is added
  #     Time <- raw_df$DateTime >= start
  #     
  #   }else if (is.na(start)){
  #     # If there is only an end date change columns from beginning of data frame until end date
  #     Time <- raw_df$DateTime <= end
  #     
  #   }else {
  #     
  #     Time <- raw_df$DateTime >= start & raw_df$DateTime <= end
  #   }
  #   
  #   ### This is where information in the maintenance log gets removed. 
  #   # UPDATE THE IF STATEMENTS BASED ON THE NECESSARY CRITERIA FROM THE MAINTENANCE LOG
  #   
  #   # replace relevant data with NAs and set flags while maintenance was in effect
  #   if(flag==2)
  #   { # THIS IS FOR VALUES THAT ARE FLAGGED BUT LEFT IN THE DATA SET
  #     raw_df[Time, flag_cols] <- flag
  #   }
  #   else if (flag==8){
  #     
  #     # The observations are changed to NA for maintenance or other issues found in the maintenance log
  #     raw_df[Time, maintenance_cols] <- NA
  #     raw_df[Time, flag_cols] <- flag 
  #   }
  #   else 
  #   {
  #     warning("Flag not coded in the L1 script. See Austin or Adrienne")
  #   }
  # }
  ### END MAINTENANCE LOG CODE ###
  
  # ## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
  # package_ID <- 'edi.200.13'
  # eml <- read_metadata(package_ID)
  # date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
  # last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)
  # 
  # 
  # updated_data <- raw_df |> filter(DateTime > last_edi_date)
  
  }

ctd_QAQC()
