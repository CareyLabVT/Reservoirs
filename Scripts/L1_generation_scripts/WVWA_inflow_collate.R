# Combine old WVWA sensor data into one file so can read it into the weir QAQC function. 
# By: Adrienne Breef-Pilz
# Created:30 Jan 2024
# Edited: 17 April 2024 - added start and end dates

#' @author Adrienne BP
#' @title WVWA_inflow_collate
#' @description This function reads in all of the raw files for the current year and binds them together 
#' so they can be used for filling missing chunks of streaming data for the WVWA pressure transducer at the 
#' weir and the barometric pressure sensor at the Catwalk at FCR. 
#' 
#' @param raw_inflow_files directory where raw inflow files from the instrument are stored
#' @param raw_bar_files directory where raw barometric pressure files from the instrument are stored
#' @param year year of the files you want to collate. The default is the current year
#' @param outfile The name of the L1 compiled file with all observations for the year
#'
#' @return no output
#'

#### Load Packages ####
pacman::p_load("tidyverse","lubridate", "EDIutils", "xml2")

### Read in current WVWA sensor files ####

# If you want to to compile all files set year= NULL for your outfile "./Data/DataNotYetUploadedToEDI/Raw_inflow/WVWA_weirInflow_2013_ENDYEAR"

WVWA_inflow_collate <- function(raw_inflow_files = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Inflow_CSV",
                                raw_baro_files = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Barometric_CSV",
                                start_date = NULL,
                                end_date = NULL,
                                outfile = "./Data/DataNotYetUploadedToEDI/Raw_inflow/WVWA_weirInflow_L1"){
  
  
  # Function used to read in the files from the WVWA sensors and convert all times to EST
  EST_convert<-function(data) {
    files<-read.csv(data,skip= 28, header=T) #get header minus wonky Campbell rows
    
    # convert date time so we can determine if it is in EST or EDT
    Date = parse_date_time(files[1,"Date.Time"], 'dmy HMS', tz="America/New_York")
    
    # Give datetime the EST timestamp before converting
    files$Date.Time= parse_date_time(files$Date.Time, 'dmy HMS', tz="Etc/GMT+5")
    
    # Check if the first observation was in EST or EDT. 
    # If FALSE then it is in EST, if TRUE then EDT
    if(dst(Date)==T){
      
      # Data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set
      
      files<-files%>%
        mutate(Date.Time= with_tz(force_tz(Date.Time,"Etc/GMT+4"), "Etc/GMT+5"))
   
      # now has a file with all times in EST
      return(files)
    }else{# This is already in EST. Yea!!
      
      # Don't have to do anything except return the file.  
      return(files)
    }
  }
  
  # List files based on current year
  
  #myfiles = list.files(path=raw_inflow_files, pattern=".csv", full.names=TRUE)
  
  # Compile all the inflow pressure data from the weir into one file and clean it up 
  inflow_pressure<- list.files(path=raw_inflow_files, pattern=".csv", full.names=TRUE)%>%
    map_df(EST_convert)%>%
    select(-Rec..) %>%
    dplyr::rename(WVWA_Pressure_psi = Pressure.psi.,
                  WVWA_Temp_C = Temperature.degC.,
                  DateTime = Date.Time)%>%
    mutate(DateTime = round_date(DateTime,"15 minutes"))%>% # Round to the nearest 15 minutes if not 
    arrange(DateTime)%>%
    distinct(DateTime, .keep_all = TRUE)

  print(nrow(inflow_pressure))
  print('inflow done')
  
  # Compile all the bp pressure data from the catwalk into one file and clean it up
  pressure_a4d<-list.files(path=raw_baro_files, pattern=".csv", full.names=TRUE)%>%
    map_df(EST_convert)%>%
    select( -Rec..,-Temperature.degC.) %>%
    dplyr::rename(WVWA_Baro_pressure_psi = Pressure.psi.,
                  DateTime = Date.Time)%>%
    mutate(DateTime = round_date(DateTime,"15 minutes"))%>%
    arrange(DateTime)%>%
    distinct(DateTime, .keep_all = TRUE)

  print(nrow(pressure_a4d))
  print('pressure_a4d done')
  
  # merge inflow and barometric pressures to find the true pressure of the sensor
  diff <- left_join(inflow_pressure, pressure_a4d, by = "DateTime")%>%
    mutate(WVWA_Pressure_psia = WVWA_Pressure_psi-WVWA_Baro_pressure_psi)%>%
    drop_na(WVWA_Pressure_psia)%>% # Take out NAs when there is only one observation
    select(DateTime, WVWA_Temp_C, WVWA_Pressure_psi, WVWA_Baro_pressure_psi, WVWA_Pressure_psia)
  
  diff$DateTime <- force_tz(diff$DateTime, tzone="EST")
    
  
  # Filter by Year and remove if not NULL
  # Now subset the data files to the only ones that are new
  if (!is.null(start_date)){
    diff <- diff %>%
      filter(DateTime >= start_date)
  }
  
  if(!is.null(end_date)){
    diff <- diff %>%
      filter(DateTime <= end_date)
  }
  
  # if start_date is Null then read in large collated sheet because some baro values missing
  
  if(is.null(start_date)){
    
    comp <- read_csv("./Data/DataNotYetUploadedToEDI/Raw_inflow/WVWA_pressure_readings_2013_current.csv")
    
    comp$DateTime <- force_tz(comp$DateTime, tzone="EST")
    
    diff2 <- diff%>%
      bind_rows(., comp)%>%
      distinct()
    
    # take out the duplicated rows
    dups<-  diff2[!duplicated(diff2$DateTime), ]
    
    #reorder. Just to be certain everything is in order
       dups<-dups[order(dups$DateTime),]
      
  }
  
  
  # Write current file to an L1 file
  
  if(is.null(start_date)){
    
    dups$DateTime <- as.character(format(dups$DateTime))
    
    write_csv(dups, paste0(outfile, ".csv"))
    
  }else{
  
  # Change datetime to character
  diff$DateTime <- as.character(format(diff$DateTime))
  
  write_csv(diff, paste0(outfile, ".csv"))
  }
}

# How to use the function with the files even though they are the default

## Set the start_date from EDI package. This needs to be updated when new data packages are updated. 
## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
package_ID <- 'edi.202.12'
eml <- read_metadata(package_ID)
date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)

day_of_run <- Sys.Date() + lubridate::days(1)

WVWA_inflow_collate(
  raw_inflow_files = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Inflow_CSV",
  raw_baro_files = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Barometric_CSV",
  start_date = last_edi_date,
  end_date = day_of_run,
  outfile = "./Data/DataNotYetUploadedToEDI/Raw_inflow/WVWA_weirInflow_L1"
)
