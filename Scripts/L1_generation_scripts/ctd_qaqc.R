pacman::p_load(oce, ocedata, tidyverse, lubridate)

setwd("./Data/DataNotYetUploadedToEDI/Raw_CTD/CTD_code/CTD_automation/")
source("ctd_qaqc.R") #Load Carey Lab ctd functions

ctd_QAQC(start_date = "2024-01-01") #run function for this year using default values