## AUTOMATED L1 QAQC SCRIPT FOR UGGA 
## Author: Austin Delany and Adrienne Breef-Pilz
## Last edited: 09-06-2024
# This script runs the function which is sourced from the L1_functions folder 


# This script uses the UGGA_qaqc function saved as the UGGA_create.R
#####################################################

# Things the script does: 
# 1. Reads in the season_Flux_output and combines them
# 2. Read in the maintenance log and filter everything based on start and end time
# 3. Reorder and rename columns
# 4. Maintenance log to flag or remove issues
# 5. Flag for slope and no sample time
# 6. Save file

# Download/load libraries
pacman::p_load(tidyverse, lubridate, EDIutils, xml2)


## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
package_ID <- 'edi.1082.3'
eml <- read_metadata(package_ID)
date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)


source('https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Scripts/L1_functions/UGGA_create.R')

## Run Function 

UGGA_qaqc(
  files = "./Data/DataNotYetUploadedToEDI/UGGA/UGGA_Raw/",
  maintenance_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/UGGA/UGGA_Maintenance_Log.csv"
  outfile = "./Data/DataNotYetUploadedToEDI/UGGA/UGGA_L1.csv",
  start_date =last_edi_date, # change when we update to read date from EDI
  end_date = Sys.Date() + lubridate::days(1))
