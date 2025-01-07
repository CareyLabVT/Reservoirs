# This script runs the function which is sourced from the L1_functions folder for fluoroprobe
# Author: Adrienne Breef-Pilz
# Created: 16 June 2024
# Edited: 

# This script uses the fluoroprobe_qaqc function saved as the fluoroprobe_create.R

rm(list=ls())

# Download/load libraries
pacman::p_load(tidyverse, lubridate, EDIutils, xml2)


## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
package_ID <- 'edi.272.8'
eml <- read_metadata(package_ID)
date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)


source('https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Scripts/L1_functions/fluoroprobe_create.R')
#source('./Scripts/L1_functions/fluoroprobe_create.R')

## Run Function 
repo_link <- "https://api.github.com/repos/melofton/Reservoirs/git/trees/master?recursive=1"
example_file_for_colnames <- "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/refs/heads/master/Data/DataAlreadyUploadedToEDI/CollatedDataForEDI/FluoroProbeData/20140404_CCR_50.txt"
current_year_data_folder <- "Data/DataNotYetUploadedToEDI/FluoroProbe"
historic_data_folder <- "Data/DataAlreadyUploadedToEDI/CollatedDataForEDI/FluoroProbeData"
historic_data_2017 <- "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/refs/heads/master/Data/DataAlreadyUploadedToEDI/CollatedDataForEDI/FluoroProbeData/FP_2017_data/FP_recal_2017.txt"
maintenance_file <- 'https://raw.githubusercontent.com/CareyLabVT/Reservoirs/refs/heads/master/Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/Maintenance_Log_FluoroProbe.csv'
#out_file <- "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/FluoroProbe_2014_2023.csv"
out_file <- "fluoroprobe_L1.csv"
start_date <- last_edi_date
end_date <- Sys.Date() + lubridate::days(1)
# run the function
fluoroprobe_qaqc(example_file_for_colnames = example_file_for_colnames,
                 current_year_data_folder = current_year_data_folder,
                 historic_data_folder = historic_data_folder,
                 historic_data_2017 = historic_data_2017,
                 maintenance_file = maintenance_file,
                 out_file = out_file,
                 start_date = start_date,
                 end_date = end_date) 

## Call healthcheck
#RCurl::url.exists("https://hc-ping.com/0b350365-b5cc-43ae-bbac-1ad5f2086354", timeout = 5)

