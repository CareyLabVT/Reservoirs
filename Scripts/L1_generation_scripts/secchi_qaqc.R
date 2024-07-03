# Secchi_qaqc_2023.R
# QAQC of Secchi data from 2023
# Created by ADD, modified by HLW
# First developed: 2023-12-04
# Last edited: 2024-07-03

#install.packages('pacman')
pacman::p_load(tidyverse, lubridate,
               dplyr, EDIutils, xml2, gsheet) ## Use pacman package to install/load other packages

## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
package_ID <- 'edi.198.11'
eml <- read_metadata(package_ID)
date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)


source('https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Scripts/L1_functions/secchi_create.R')

data_file = 'https://docs.google.com/spreadsheets/d/1fvM0fDRliuthicQWZT7c9RYErikI5DwrzbOC7TCoMGI/edit#gid=1172894977'
maintenance_file <- 'Data/DataNotYetUploadedToEDI/Secchi/maintenance_log.csv'
outfile <- './Data/DataNotYetUploadedToEDI/Secchi/secchi_L1.csv'

secchi_qaqc(data_file = data_file,
            gsheet_data = TRUE,
            maintenance_file = maintenance_file,
            outfile = outfile, 
            start_date = last_edi_date, 
            end_date = Sys.Date() + lubridate::days(1))
