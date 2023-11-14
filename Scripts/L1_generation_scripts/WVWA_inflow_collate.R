# Combine old WVWA sensor data into one file so can read it into the weir QAQC function. 
# By: Adrienne Breef-Pilz

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
pacman::p_load("tidyverse","lubridate")

### Read in current WVWA sensor files ####

WVWA_inflow_collate <- function(raw_inflow_files = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Inflow_CSV",
                                raw_baro_files = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Barometric_CSV",
                                year = format(Sys.Date(), "%Y"),
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
  
  myfiles = list.files(path=raw_inflow_files, pattern=paste0("_",year), full.names=TRUE)
  
  # Compile all the inflow pressure data from the weir into one file and clean it up 
  inflow_pressure<- list.files(path=raw_inflow_files, pattern=paste0("_",year), full.names=TRUE)%>%
    map_df(EST_convert)%>%
    select(-Rec..) %>%
    dplyr::rename(WVWA_Pressure_psi = Pressure.psi.,
                  WVWA_Temp_C = Temperature.degC.,
                  DateTime = Date.Time)%>%
    mutate(DateTime = round_date(DateTime,"15 minutes"))%>% # Round to the nearest 15 minutes if not 
    arrange(DateTime)%>%
    distinct()
    
  
  # Compile all the bp pressure data from the catwalk into one file and clean it up
  pressure_a4d<-list.files(path=raw_baro_files, pattern=paste0("_",year), full.names=TRUE)%>%
    map_df(EST_convert)%>%
    select( -Rec..,-Temperature.degC.) %>%
    dplyr::rename(WVWA_Baro_pressure_psi = Pressure.psi.,
                  DateTime = Date.Time)%>%
    mutate(DateTime = round_date(DateTime,"15 minutes"))%>%
    arrange(DateTime)%>%
    distinct()
  
  
  # merge inflow and barometric pressures to find the true pressure of the sensor
  diff <- left_join(pressure_a4d, inflow_pressure, by = "DateTime")%>% 
    mutate(WVWA_Pressure_psia = WVWA_Pressure_psi-WVWA_Baro_pressure_psi)%>%
    drop_na(WVWA_Pressure_psia)%>% # Take out NAs when there is only one observation
    select(DateTime, WVWA_Temp_C, WVWA_Pressure_psi, WVWA_Baro_pressure_psi, WVWA_Pressure_psia)%>%
    mutate(Year=year(DateTime))%>%
    filter(Year==year)%>%
    select(-Year)
  
  # Write current file to an L1 file
  write.csv(diff, paste0(outfile, ".csv"), row.names = F)
  
}

# How to use the function with the files even though they are the default
WVWA_inflow_collate(
  raw_inflow_files = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Inflow_CSV",
  raw_baro_files = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Barometric_CSV",
  year = format(Sys.Date(), "%Y"),
  outfile = "./Data/DataNotYetUploadedToEDI/Raw_inflow/WVWA_weirInflow_L1"
)
