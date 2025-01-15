# This script runs the function which is sourced from the L1_functions folder for metals samples
# Author: Adrienne Breef-Pilz
# Created: 19 May 2024
# Edited: 23 Oct 2024 - changed the arguments 

# This script uses the metals_create function
#####################################################


# Download/load libraries
pacman::p_load(tidyverse, lubridate, gsheet, rqdatatable, hms, EDIutils, xml2)


## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
package_ID <- 'edi.455.8'
eml <- read_metadata(package_ID)
date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)



# Function run below to QC the data and make an L1 
# 1. Read in Maintenance Log and Sample ID Key
# 2. Compile the files from Jeff and add Site information
# 3. Read in the Time of sampling sheet and add to data frame
# 4. Read in MRL and add flags
# 5. Use Maintenance Log to flag or change observations
# 6. Switch observations if total and soluble samples were mixed up
# 7. Save files

# source code for qaqc
source('https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Scripts/L1_functions/metals_create.R')
#source('./Scripts/L1_functions/metals_create.R')

## Run Function 
metals_qaqc(directory = "./Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/",
   	    sample_ID_key = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Scripts/Metals_Sample_Depth.csv",
   	    maintenance_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Metals_Maintenance_Log.csv",
  	    sample_time = "https://docs.google.com/spreadsheets/d/1MbSN2G_NyKyXQUEzfMHmxEgZYI_s-VDVizOZM8qPpdg/edit#gid=0",
   	    MRL_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/MRL_metals.txt",
        metals_save = T, 
  	    metals_outfile = "./Data/DataNotYetUploadedToEDI/Metals_Data/metals_L1.csv",
  	    ISCO_save = T,
        ISCO_outfile = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/ISCO_metals_L1.csv",
	      start_date =last_edi_date, # change when we update to read date from EDI
       	end_date = Sys.Date() + lubridate::days(1))

