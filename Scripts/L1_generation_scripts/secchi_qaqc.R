# Secchi_qaqc_2023.R
# QAQC of Secchi data from 2023
# Created by ADD, modified by HLW
# First developed: 2023-12-04
# Last edited: 2024-01-11

#install.packages('pacman')
pacman::p_load(tidyverse, lubridate,
               dplyr, EDIutils, xml2, gsheet) ## Use pacman package to install/load other packages

source('https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Scripts/L1_functions/secchi_create.R')

data_file = 'https://docs.google.com/spreadsheets/d/1fvM0fDRliuthicQWZT7c9RYErikI5DwrzbOC7TCoMGI/edit#gid=1172894977'
maintenance_file <- 'Data/DataNotYetUploadedToEDI/Secchi/maintenance_log.csv'
outfile <- './Data/DataNotYetUploadedToEDI/Secchi/secchi_L1.csv'

secchi_qaqc(data_file = data_file,
            gsheet_data = TRUE,
            maintenance_file = maintenance_file,
            outfile = outfile)

