### Function to create a QAQC plot and
## clean up files produced by LiCor Eddy Pro

## Originally from Alex Hounshell's EddyPro_CleanUp script from 8 October 2021, A. Hounshell

## A. Breef-Pilz updated on 26 Oct. 2022 to included a for loop to
## read in the EddyPro output, create QAQC plots and bind all the files together into on large data frame.
## A. Breef-Pilz modified the script to create a function that reads in the current EddyFlux files
## Makes QAQC plots if they haven't been created,
## then binds current files from the year,
## does a quick QAQC check and then creates an L1 file with all of the current fluxes

# This script runs the function which is sourced from the L1_functions folder
#####################################################


# Download/load libraries
pacman::p_load(lubridate,tidyverse,hms,gridExtra,openair, googledrive)

# library(EDIutils)
# library(xml2)

source('https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Scripts/L1_functions/eddy_flux_create.R')

## Function 
eddypro_cleaning_function(
  directory = "./Data/DataNotYetUploadedToEDI/EddyFlux_Processing/",
  gdrive = F, # Are the files on Google Drive. True or False
  gshared_drive = as_id("0ACybYKbCwLRPUk9PVA"),
  #current_year = 2024,
  output_file = "/EddyPro_Cleaned_L1.csv",
  start_date = as.Date("2023-01-01") + lubridate::days(1), # change when we update to read date from EDI
  end_date = Sys.Date() + lubridate::days(1))


## Call healthcheck
#RCurl::url.exists("https://hc-ping.com/f0ba1278-7b06-4b3b-b8aa-5486e778abc3", timeout = 5)

