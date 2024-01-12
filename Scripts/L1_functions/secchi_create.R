# Secchi_qaqc_2023.R
# QAQC of Secchi data from 2023
# Created by ADD, modified by HLW
# First developed: 2023-12-04
# Last edited: 2024-01-11

#install.packages('pacman')
pacman::p_load(tidyverse, lubridate,
               dplyr, EDIutils, xml2, gsheet) ## Use pacman package to install/load other packages


secchi_qaqc <- function(data_file, gsheet_data, maintenance_file = NULL, outfile){

  #gsheet_url <- 'https://docs.google.com/spreadsheets/d/1fvM0fDRliuthicQWZT7c9RYErikI5DwrzbOC7TCoMGI/edit#gid=1172894977'

  if(is.character(data_file) & gsheet_data == FALSE){
    # read catwalk data and maintenance log
    # NOTE: date-times throughout this script are processed as UTC
    secchi_df <- read_csv(data_file, skip = 1, col_names = CATPRES_COL_NAMES,
                        col_types = cols(.default = col_double(), DateTime = col_datetime()))

  } else if (is.character(data_file) & gsheet_data == TRUE){
    secchi_df <- gsheet::gsheet2tbl(data_file)

  }else {
    secchi_df <- data_file
  }

  secchi_df$Reservoir <- as.character(secchi_df$Reservoir)
  secchi_df$Site <- as.numeric(secchi_df$Site)
  secchi_df$Secchi_m <- as.numeric(secchi_df$Secchi_m)
  secchi_df$Flag_Secchi_m <- as.numeric(secchi_df$Flag_Secchi_m)
  #secchi_df$Notes <- as.character(secchi_df$Notes)


  # fix dates
  secchi_df$DateTime = lubridate::parse_date_time(secchi_df$DateTime, orders = c('ymd HMS','ymd HM','ymd','mdy'))
  #secchi_df$DateTime <- as.POSIXct(strptime(secchi_df$DateTime, "%Y-%m-%d %H:%M"), tz = 'America/New_York') ## need to fix dates that don't have timestamp
  secchi_df$Flag_DateTime <- NA

  ## fill in any missing datetimes with noon
  secchi_df <- secchi_df %>%
    mutate(Time = format(DateTime,"%H:%M:%S"),
           Time = ifelse(Time == "00:00:00", "12:00:00",Time),
           Flag_DateTime = ifelse(Time == "12:00:00", 1, 0), # Flag if set time to noon
           Date = as.Date(DateTime),
           DateTime = ymd_hms(paste0(Date, "", Time), tz = "America/New_York"),
           Hours = hour(DateTime),
           DateTime = ifelse(Hours<5, DateTime + (12*60*60), DateTime), # convert time to 24 hour time
           DateTime = as_datetime(DateTime, tz = "America/New_York"))%>% # time is in seconds put it in ymd_hms
    select(-c(Time, Date, Hours))

  # secchi_reformat <- secchi_df |>
  #   rename(Flag_Secchi_m = Flag_Secchi)

  secchi_reformat <- secchi_df |>
    #filter(!is.na(Secchi_m) ) |>   # Omit rows where all Secchi values NA (e.g., rows from files with trailing ,'s) ## DO WE WANT TO COMPLETELY REMOVE NAS? IF SO WE NEED TO RETHINK HOW FLAGS ARE ASSIGNED IN NEXT LINE
    mutate(Flag_Secchi_m = ifelse(is.na(Secchi_m), 1, 0)) |>
           #Flag_DateTime = ifelse(Notes=="No time was recorded",1,0))  |> # Add 'flag' columns for each variable; 1 = flag (Flag for night sampling)
    select(Reservoir, Site, DateTime, Secchi_m, Flag_DateTime, Flag_Secchi_m) |>    # Arrange order of columns for final data table
    arrange(Reservoir, DateTime, .by_group = TRUE )

  secchi_reformat[is.na(secchi_reformat)] <- 0

  secchi_reformat <- as.data.frame(secchi_reformat)


  # ## CHECK FOR DUPLICATES
  # secchi_dup <- secchi_reformat |>
  #   group_by(Reservoir, Site, DateTime) |>
  #   mutate(n = n()) |>
  #   filter(n > 1)
  #
  # if (nrow(secchi_dup) > 0){
  #   print('DUPLICATE DATA FOUND')
  # }

  if(!is.null(maintenance_file)){ # only run this file if the maint file arugment is non-null

  # ## ADD MAINTENANCE LOG FLAGS (manual edits to the data for suspect samples or human error)
  log_read <- read_csv(maintenance_file, col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  ))

  log <- log_read

    for(i in 1:nrow(log)){
      ### Assign variables based on lines in the maintenance log.

      ### get start and end time of one maintenance event
      start <- force_tz(as.POSIXct(log$TIMESTAMP_start[i]), tzone = "America/New_York")
      end <- force_tz(as.POSIXct(log$TIMESTAMP_end[i]), tzone = "America/New_York")

      ### Get the Reservoir Name
      Reservoir <- log$Reservoir[i]

      ### Get the Site Number
      Site <- as.numeric(log$Site[i])

      ### Get the Maintenance Flag
      flag <- log$flag[i]

      ### Get the new value for a column or an offset
      update_value <- as.numeric(log$update_value[i])

      ### Get the names of the columns affected by maintenance
      colname_start <- log$start_parameter[i]
      colname_end <- log$end_parameter[i]

      ### if it is only one parameter parameter then only one column will be selected

      if(is.na(colname_start)){

        maintenance_cols <- colnames(secchi_reformat%>%select(colname_end))

      }else if(is.na(colname_end)){

        maintenance_cols <- colnames(secchi_reformat%>%select(colname_start))

      }else{
        maintenance_cols <- colnames(secchi_reformat%>%select(colname_start:colname_end))
      }

      if(is.na(end)){
        # If there the maintenance is on going then the columns will be removed until
        # and end date is added
        Time <- secchi_reformat |> filter(DateTime >= start) |> select(DateTime)

      }else if (is.na(start)){
        # If there is only an end date change columns from beginning of data frame until end date
        Time <- secchi_reformat |> filter(DateTime <= end) |> select(DateTime)

      }else {
        Time <- secchi_reformat |> filter(DateTime >= start & DateTime <= end) |> select(DateTime)
      }

      ### This is where information in the maintenance log gets updated

      if(flag %in% c(1) & colname_start != 'Site'){ ## UPDATE THIS WITH ANY NEW FLAGS
        # UPDATE THE MANUAL ISSUE FLAGS (BAD SAMPLE / USER ERROR) AND SET TO NEW VALUE
        secchi_reformat[secchi_reformat$DateTime %in% Time$DateTime, maintenance_cols] <- NA
        secchi_reformat[secchi_reformat$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag

      } else if (flag %in% c(2) & colname_start == 'Site'){
        ## this fixes site issues for now -- no flag shown in data
        secchi_reformat[secchi_reformat$DateTime %in% Time$DateTime, maintenance_cols] <- update_value

      } else if (flag %in% c(2) & colname_start != 'Site'){
        ## this fixes site issues for now -- no flag shown in data
        secchi_reformat[secchi_reformat$DateTime %in% Time$DateTime, maintenance_cols] <- update_value
        secchi_reformat[secchi_reformat$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag

      }else if (flag %in% c(3)){
        # value is suspect
        secchi_reformat[secchi_reformat$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag

      }else if (flag %in% c(4)){
        # to be deleted
        secchi_reformat <- secchi_reformat[!(secchi_reformat$DateTime %in% Time$DateTime & (secchi_reformat$Reservoir == Reservoir)),]
      }else{
        warning("Flag not coded in the L1 script. See Austin or Adrienne")
      }
    } # end for loop
  } #end conditional statement

  # #### END MAINTENANCE LOG CODE #####

  ## CHECK FOR DUPLICATES
  secchi_dup <- secchi_reformat |>
    group_by(Reservoir, Site, DateTime) |>
    mutate(n = n()) |>
    filter(n > 1)

  if (nrow(secchi_dup) > 0){
    warning('DUPLICATE DATA FOUND - removing duplicates')
    secchi_duplicates <- secchi_reformat

    secchi_deduped <- secchi_duplicates |>
      distinct()

    secchi_reformat <- secchi_deduped
  }

  # # ## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
  # package_ID <- 'edi.198.11'
  # eml <- read_metadata(package_ID)
  # date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
  # last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)
  #
  # secchi_reformat <- secchi_reformat |> filter(DateTime > last_edi_date)

  if (!is.null(outfile)){
    # Write to CSV -- save as L1 file
    write.csv(secchi_reformat, outfile, row.names = FALSE)
  }

  return(secchi_reformat)
}
#
data_file = 'https://docs.google.com/spreadsheets/d/1fvM0fDRliuthicQWZT7c9RYErikI5DwrzbOC7TCoMGI/edit#gid=1172894977'
#data_file = 'https://pasta.lternet.edu/package/data/eml/edi/198/11/81f396b3e910d3359907b7264e689052'
maintenance_file <- 'Data/DataNotYetUploadedToEDI/Secchi/maintenance_log.csv'
outfile <- './Data/DataNotYetUploadedToEDI/Secchi/secchi_L1.csv'
#
# secchi_qaqc(data_file = data_file,
#             gsheet_data = TRUE,
#             maintenance_file = maintenance_file,
#             outfile = outfile)
