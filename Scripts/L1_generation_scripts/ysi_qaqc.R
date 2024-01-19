# ysi_qaqc_2023.R
# QAQC of YSI and PAR data from 2023
# Created by ADD, modified by HLW
# First developed: 2023-12-04
# Last edited: 2024-01-11

#install.packages('pacman') ## Run this line if you don't have "pacman" package installed
pacman::p_load(tidyverse, lubridate, dplyr,
               EDIutils, xml2, gsheet) ## Use pacman package to install/load other packages

source('https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Scripts/L1_functions/ysi_create.R')

maintenance_file <- 'Data/DataNotYetUploadedToEDI/YSI_PAR/maintenance_log.csv'
data_file <- 'https://docs.google.com/spreadsheets/d/1HbSBEFjMuK4Lxit5MRbATeiyljVAB-cpUNxO3dKd8V8/edit#gid=1787819257'
outfile <- 'Data/DataNotYetUploadedToEDI/YSI_PAR/ysi_L1.csv'

ysi_qaqc(data_file = data_file,
         maintenance_file = maintenance_file,
         gsheet_data = TRUE,
         outfile = outfile)
