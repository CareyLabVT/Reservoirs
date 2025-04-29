pacman::p_load(oce, ocedata, tidyverse, lubridate, EDIutils, xml2)

setwd("./Data/DataNotYetUploadedToEDI/Raw_CTD/CTD_code/")
source("ctd_QAQC.R") #Load Carey Lab ctd functions

# Get the start date from the end of the CTD file on EDI

## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
package_ID <- 'edi.200.15'
eml <- read_metadata(package_ID)
date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)


ctd_QAQC(start_date = last_edi_date + lubridate::days(1)) #run function with start date 1 day after the end date published on EDI

## Call healthcheck
#RCurl::url.exists("https://hc-ping.com/8b6cfbb6-d9f0-4ab6-a22b-c1ee1e0ac184", timeout = 5)
