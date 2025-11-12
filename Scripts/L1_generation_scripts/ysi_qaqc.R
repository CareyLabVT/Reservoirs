# ysi_qaqc_2023.R
# QAQC of YSI and PAR data from 2023
# Created by ADD, modified by HLW
# First developed: 2023-12-04
# Last edited: 2025-11-12

# 2025-11-12 updated the file path for the raw data

#install.packages('pacman') ## Run this line if you don't have "pacman" package installed
pacman::p_load(tidyverse, lubridate, dplyr,
               EDIutils, xml2, gsheet) ## Use pacman package to install/load other packages

## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
package_ID <- 'edi.198.13'
eml <- read_metadata(package_ID)
date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)


source('https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Scripts/L1_functions/ysi_create.R')

maintenance_file <- 'https://raw.githubusercontent.com/CareyLabVT/Reservoirs/refs/heads/master/Data/DataNotYetUploadedToEDI/YSI_PAR/maintenance_log.csv'
data_file <- 'https://docs.google.com/spreadsheets/d/1MX__IelyQBHO1bNxAltfYT_r_pJisMuiMtDG4oxkOok/'
outfile <- 'Data/DataNotYetUploadedToEDI/YSI_PAR/ysi_L1.csv'

ysi_qaqc(data_file = data_file,
         maintenance_file = maintenance_file,
         gsheet_data = TRUE,
         outfile = outfile,
         start_date = last_edi_date,
         end_date = Sys.Date() + lubridate::days(1))


