# This script runs the function which is sourced from the L1_functions folder for metals samples
# Author: Adrienne Breef-Pilz
# Created: 19 May 2024
# Edited: 

# This script uses the filt_chla_qaqc function saved as the filt_chla_create.R
#####################################################

# Things the script does: 
# 1. Read in Maintenance log and read in raw chla file from the spec
#   Put in the right format for processing
# 2. Read in the filtering log and rack map
# 3. Merge everything together
# 4. Maintenance log to flag or remove issues
# 5. Process with a script based on BNN Excel script
# 6. Further QAQC processing
# 7. Save files

# Download/load libraries
pacman::p_load(tidyverse, lubridate, gsheet, EDIutils, xml2)


## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
package_ID <- 'edi.555.4'
eml <- read_metadata(package_ID)
date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)


source('https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Scripts/L1_functions/filt_chla_create.R')

## Run Function 

filt_chla_qaqc(
  directory = "./Data/DataNotYetUploadedToEDI/Raw_chla/chla_extraction/raw data from spec/",
  rack_map = "https://docs.google.com/spreadsheets/d/1N7he-0Z1gmSA5KjO96QA5tOeNXFcAKoVfAD1zix4qNk",
  filtering_log = "https://docs.google.com/spreadsheets/d/1xeF312vgwJn7d2UwN4qOD8F32ZGHE3Vv",
  final_vol_extract = 6,
  blank_vol_filt = 500,
  maintenance_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Raw_chla/Filt_Chla_Maintenance_Log.csv",
  historic_file = "./Data/DataNotYetUploadedToEDI/Raw_chla/historic_filt_chla_2014_2022.csv",
  sample_times =  "https://docs.google.com/spreadsheets/d/1NKnIM_tjMxMO0gVxzZK_zVlUSQrdi3O7KqnyRiXo4ps", 
  outfile = "./Data/DataNotYetUploadedToEDI/Raw_chla/Filt_chla_L1.csv",
  start_date =last_edi_date, # change when we update to read date from EDI
  end_date = Sys.Date() + lubridate::days(1))