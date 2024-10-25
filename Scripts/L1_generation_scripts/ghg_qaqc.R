# This script runs the function which is sourced from the L1_functions folder for discreet GHG samples
# Author: Adrienne Breef-Pilz
# Created: 13 May 2024
# Edited: 

# This script uses the ghg
#####################################################


# Download/load libraries
pacman::p_load(lubridate,tidyverse, googledrive, readxl, gsheet, EDIutils, xml2)


## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
package_ID <- 'edi.551.8'
eml <- read_metadata(package_ID)
date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)


# The place of the functions used 
# function to read in the ghg files
source("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Raw_GHG/ghg_functions_for_L1.R")

# Function run below to QC the data and make an L1 
# This function:
# 1. Read in the Maintenance Log and then Raw files 
# 2. Process the files if necessary
# 3. Make Flag Columns and add flags for missing values and negative values 
# 4. Take out values based on the Maintenance Log
# 5. Additional Maintenance
# 6. Save files
source('https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Scripts/L1_functions/ghg_create.R')

## Function 
ghg_qaqc(directory = "./Data/DataNotYetUploadedToEDI/Raw_GHG/data/",
         maintenance_file = "./Data/DataNotYetUploadedToEDI/Raw_GHG/GHG_Maintenance_Log.csv",
         gdrive = F, # Are the files on Google Drive. True or False
         gshared_drive = as_id("1OMx7Bq9_8d6J-7enC9ruPYuvE43q9uKn"),
         Air_Pressure = c("https://docs.google.com/spreadsheets/d/1YH9MrOVROyOgm0N55WiMxq2vDexdGRgG", 
             "https://docs.google.com/spreadsheets/d/1ON3ZxDqfkFm65Xf5bbeyNFQGBjqYoFQg"),
         vial_digitized_sheet = "https://docs.google.com/spreadsheets/d/1HoBeXWUm0_hjz2bmd-ZmS0yhgF1WvLenpvwEa8dL008",
         Rolling_MDL = "https://docs.google.com/spreadsheets/d/1AcqbdwbogWtO8QnLH1DmtZd47o323hG9",
         output_file = "./Data/DataNotYetUploadedToEDI/Raw_GHG/L1_manual_GHG.csv",
         MDL_file = "./Data/DataNotYetUploadedToEDI/Raw_GHG/MDL_GHG_file.csv",
         Vial_Number_Check = "./Data/DataNotYetUploadedToEDI/Raw_GHG/Vial_Number_Check.csv",
        start_date =last_edi_date, # change when we update to read date from EDI
        end_date = Sys.Date() + lubridate::days(1))

