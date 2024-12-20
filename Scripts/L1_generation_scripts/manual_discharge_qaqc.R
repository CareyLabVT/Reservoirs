## QAQC manual discharge use function
## This section actually uses the function 
# Created by ABP
# First developed: 2023-12-04
# Last edited: 2024-12-19

#install.packages('pacman')
pacman::p_load(tidyverse, lubridate,
               dplyr, EDIutils, xml2, gsheet) ## Use pacman package to install/load other packages

## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
package_ID <- 'edi.454.7'
eml <- read_metadata(package_ID)
date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)


source('https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Scripts/L1_functions/manual_discharge_create.R')

# source the function

# name the variables

gsheet_url <- "https://docs.google.com/spreadsheets/d/1fgYcGsZeALuwdAX3H0UeA3u57zKsuqTkouh2bxuBTKw/"
maintenance_file <- "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/refs/heads/master/Data/DataNotYetUploadedToEDI/Raw_Discharge/ManualDischarge_Maintenance_Log.csv"
output <- './Data/DataNotYetUploadedToEDI/Raw_Discharge/ManualDischarge_L1.csv'


## Run function

ManualDischarge_qaqc(gsheet_url = gsheet_url, 
                     historical_file = NULL,
                     maintenance_file = maintenance_file, 
                     output_file=output,
                     start_date = date_attribute,
                     end_date = last_edi_date)
