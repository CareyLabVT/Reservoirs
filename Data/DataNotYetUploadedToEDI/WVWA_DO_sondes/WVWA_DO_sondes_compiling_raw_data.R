##Compiling data from WVWA DO sondes at 1m and 8m
##Dexter Howard 
#Initial compilation occured in 2017-2018 and resulted in file: FCR_DOsonde_2012to2017.csv
#Additional compilation occured in 2021-2022 as part of DWHs metabolism chapter and brought all data through summer 2022 together


#load libraries needed
library(stats)
library(rLakeAnalyzer)
library(raster)
library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)


#### appending data since jan 1 2018 #### 

#reading in already compiled data for 1 and 8m data: 2012 - 2017
dosonde_2012_17 <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DOsonde_2012to2017.csv", col_types = cols(.default = "d", Date = "T")) 

#renmaing variables to remove spaces and special characters
dosonde_2012_17 <- dosonde_2012_17 %>% 
 rename("Barometric_Pressure_Temperature_C" = 2,
        "Barometric_Pressure_Pressure_PSI" = 3,
         "DO_Temperature_C" = 4,
         "DO_Pressure_PSI" = 5,
         "DO_Power_V" = 6,
         "DO_Dissolved_Oxygen_ppm" = 7,
         "DO_Temperature2_C" = 8,
         "DO1m_Temperature_C" = 9,
         "DO1m_Pressure_PSI" = 10,
         "DO1m_Power_V" = 11,
         "DO1m_Dissolved_Oxygen_ppm" = 12,
         "DO1m_Temperature2_C" = 13,
         "Temperature_Modem_C" = 14,
         "Vbattery_Modem_Volts" = 15,
         "Vcommon_Modem_Volts" = 16)



####  reading in 1m non compiled dates from Github and formating columns and rows to join ----
do_20180806_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m 20180806.csv", skip = 32)
do_20180806_1m <- do_20180806_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))
  

do_20180319_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20180319.csv", skip = 32)
do_20180319_1m <- do_20180319_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20180820_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20180820.csv", skip = 32)
do_20180820_1m <- do_20180820_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))

do_20180912_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20180912.csv", skip = 32)
do_20180912_1m <- do_20180912_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))

do_20181008_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20181008.csv", skip = 32)
do_20181008_1m <- do_20181008_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))

do_20181029_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20181029.csv", skip = 32)
do_20181029_1m <- do_20181029_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20181119_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20181119.csv", skip = 32)
do_20181119_1m <- do_20181119_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))

do_20190208_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20190208.csv", skip = 32)
do_20190208_1m <- do_20190208_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))

do_20190318_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20190318.csv", skip = 32)
do_20190318_1m <- do_20190318_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))

do_20190415_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20190415.csv", skip = 32)
do_20190415_1m <- do_20190415_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))

do_20190603_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20190603.csv", skip = 32)
do_20190603_1m <- do_20190603_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20190920_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20190920.csv", skip = 32)
do_20190920_1m <- do_20190920_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20200824_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20200824.csv", skip = 32)
do_20200824_1m <- do_20200824_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))

do_20200902_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20200902.csv", skip = 32)
do_20200902_1m <- do_20200902_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20201102_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20201102.csv", skip = 32)
do_20201102_1m <- do_20201102_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20201214_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20201214.csv", skip = 32)
do_20201214_1m <- do_20201214_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20210107_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20210107.csv", skip = 32)
do_20210107_1m <- do_20210107_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20220512_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20220512.csv", skip = 32)
do_20220512_1m <- do_20220512_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20220627_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20220627.csv", skip = 32)
do_20220627_1m <- do_20220627_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))

do_20220126_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20220126.csv", skip = 32)
do_20220126_1m <- do_20220126_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20220209_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20220209.csv", skip = 32)
do_20220209_1m <- do_20220209_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20220317_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20220317.csv", skip = 32)
do_20220317_1m <- do_20220317_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20220412_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20220412.csv", skip = 32)
do_20220412_1m <- do_20220412_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20211109_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20111109.csv", skip = 32)
do_20211109_1m <- do_20211109_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20211108_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20211108.csv", skip = 32)
do_20211108_1m <- do_20211108_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20210906_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20210906.csv", skip = 39)
do_20210906_1m <- do_20210906_1m %>%
  select(-1) %>%
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>%
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20200921_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20200921.csv", skip = 39)
do_20200921_1m <- do_20200921_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20210416_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20210416.csv", skip = 39)
do_20210416_1m <- do_20210416_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20210416_1m_2 <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20210416_2.csv", skip = 39)
do_20210416_1m_2 <- do_20210416_1m_2 %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20201214_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20201214.csv", skip = 32)
do_20201214_1m <- do_20201214_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20210107_1m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_1m_20210107.csv", skip = 32)
do_20210107_1m <- do_20210107_1m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO1m_Temperature_C" = 2,
         "DO1m_Pressure_PSI" = 3,
         "DO1m_Power_V" = 4,
         "DO1m_Dissolved_Oxygen_ppm" = 5,
         "DO1m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


## Bring all 1m since 2018 together 

DO_1m_since2018 <- rbind(
  do_20180806_1m, do_20180319_1m, do_20180820_1m, do_20180912_1m, do_20181008_1m, do_20181029_1m,
  do_20181119_1m, do_20190208_1m, do_20190318_1m, do_20190415_1m, do_20190603_1m, do_20190920_1m,
  do_20200824_1m, do_20200902_1m, do_20201102_1m, do_20201214_1m, do_20210107_1m, 
  do_20220512_1m, do_20220627_1m, do_20220126_1m, do_20220209_1m, do_20220317_1m, do_20220412_1m,
  do_20211109_1m, do_20211108_1m, do_20210906_1m, do_20200921_1m, do_20210416_1m, do_20210416_1m_2,
  do_20201214_1m, do_20210107_1m 
)






### files for 8m that need to be added since initial 2018 compilation ----

do_20190920_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20190920.csv", skip = 32)
do_20190920_8m <- do_20190920_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20190603_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20190603.csv", skip = 32)
do_20190603_8m <- do_20190603_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20190415_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20190415.csv", skip = 32)
do_20190415_8m <- do_20190415_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20190318_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20190318.csv", skip = 32)
do_20190318_8m <- do_20190318_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20190208_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20190208.csv", skip = 32)
do_20190208_8m <- do_20190208_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20181119_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20181119.csv", skip = 32)
do_20181119_8m <- do_20181119_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20181029_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20181029.csv", skip = 32)
do_20181029_8m <- do_20181029_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20181008_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20181008.csv", skip = 32)
do_20181008_8m <- do_20181008_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20180912_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20180912.csv", skip = 32)
do_20180912_8m <- do_20180912_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20180820_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20180820.csv", skip = 32)
do_20180820_8m <- do_20180820_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20180806_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20180806.csv", skip = 32)
do_20180806_8m <- do_20180806_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20180319_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20180319.csv", skip = 32)
do_20180319_8m <- do_20180319_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20200802_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_2020082.csv", skip = 32)
do_20200802_8m <- do_20200802_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20201102_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20201102.csv", skip = 32)
do_20201102_8m <- do_20201102_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20200902_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20200902.csv", skip = 32)
do_20200902_8m <- do_20200902_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20210107_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20210107.csv", skip = 32)
do_20210107_8m <- do_20210107_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20201214_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20201214.csv", skip = 32)
do_20201214_8m <- do_20201214_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20210416_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20210416.csv", skip = 39)
do_20210416_8m <- do_20210416_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))

do_20210416_8m_2 <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20210416_2.csv", skip = 39)
do_20210416_8m_2 <- do_20210416_8m_2 %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20210416_8m_3 <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20210416_3.csv", skip = 39)
do_20210416_8m_3 <- do_20210416_8m_3 %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20200921_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20200921.csv", skip = 39)
do_20200921_8m <- do_20200921_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20210906_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_8m_DO_20210906.csv", skip = 39)
do_20210906_8m <- do_20210906_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20210921_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_DO_8m_20210921.csv", skip = 39)
do_20210921_8m <- do_20210921_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20211109_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_8m_DO_20211109.csv", skip = 32)
do_20211109_8m <- do_20211109_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20211108_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_8m_DO_20211108.csv", skip = 32)
do_20211108_8m <- do_20211108_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20220412_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_8m_DO_20220412.csv", skip = 32)
do_20220412_8m <- do_20220412_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20220317_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_8m_DO_20220317.csv", skip = 32)
do_20220317_8m <- do_20220317_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))

do_20220209_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_8m_DO_20220209.csv", skip = 32)
do_20220209_8m <- do_20220209_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20220126_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_8m_DO_20220126.csv", skip = 32)
do_20220126_8m <- do_20220126_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20220718_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_8m_DO_20220718.csv", skip = 32)
do_20220718_8m <- do_20220718_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20220620_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_8m_DO_20220620.csv", skip = 32)
do_20220620_8m <- do_20220620_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))


do_20220512_8m <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_8m_DO_20220512.csv", skip = 32)
do_20220512_8m <- do_20220512_8m %>% 
  select(-1) %>% 
  rename("Date" = 1,
         "DO8m_Temperature_C" = 2,
         "DO8m_Pressure_PSI" = 3,
         "DO8m_Power_V" = 4,
         "DO8m_Dissolved_Oxygen_ppm" = 5,
         "DO8m_Temperature2_C" = 6) %>% 
  mutate(Date = parse_date_time(Date, orders = c("dmy HMS")))



## bring all 8m since 2018 together 
DO_8m_since2018 <- rbind(
  do_20190920_8m, do_20190603_8m, do_20190415_8m, do_20190318_8m, do_20190208_8m, 
  do_20181119_8m, do_20181029_8m, do_20181008_8m, do_20180912_8m, do_20180820_8m, 
  do_20180806_8m, do_20180319_8m, do_20200802_8m, do_20201102_8m, do_20200902_8m, 
  do_20210107_8m, do_20201214_8m, do_20210416_8m, do_20210416_8m_2, do_20210416_8m_3, 
  do_20200921_8m, do_20210906_8m, do_20210921_8m, do_20211109_8m, do_20211108_8m, 
  do_20220412_8m, do_20220317_8m, do_20220209_8m, do_20220126_8m, do_20220718_8m,
  do_20220620_8m, do_20220512_8m 
 )



### reading in early 2018 missing data that should have been part of 2017 compilation ----

do_20180103 <- read_csv("./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/WVWA_DO_data_before2018/FCR_010318-020218_DO_senvu_fixeddatetime.csv")
names(do_20180103) <- c("date", "BPTemp", "BPPSI", 
                           "DOTemp", "DOPSI", "DOpower", "DOppm", "DOtemp2", 
                           "DO1Temp", "DO1PSI", "DO1power", "DO1ppm", "DO1temp2",
                           "TempModem","VbatteryModem", "VcommonModem" )


do_20180103 <- aggregate(x=do_20180103[c( "BPTemp", "BPPSI", 
                                  "DOTemp", "DOPSI", "DOpower", "DOppm", "DOtemp2", 
                                  "DO1Temp", "DO1PSI", "DO1power", "DO1ppm", "DO1temp2",
                                  "TempModem","VbatteryModem", "VcommonModem" )],
                by=list(date=do_20180103$date), mean, na.rm = TRUE) 


do_20180103 <- do_20180103 %>% 
  rename("Date" = 1,
         "Barometric_Pressure_Temperature_C" = 2,
         "Barometric_Pressure_Pressure_PSI" = 3,
         "DO_Temperature_C" = 4,
         "DO_Pressure_PSI" = 5,
         "DO_Power_V" = 6,
         "DO_Dissolved_Oxygen_ppm" = 7,
         "DO_Temperature2_C" = 8,
         "DO1m_Temperature_C" = 9,
         "DO1m_Pressure_PSI" = 10,
         "DO1m_Power_V" = 11,
         "DO1m_Dissolved_Oxygen_ppm" = 12,
         "DO1m_Temperature2_C" = 13,
         "Temperature_Modem_C" = 14,
         "Vbattery_Modem_Volts" = 15,
         "Vcommon_Modem_Volts" = 16)





#### Compiling all data to add to one csv for metabolsim ----

#join 8m and 1m since 2018 into one data frame
since2018_1m_and_8m <- full_join(DO_8m_since2018, DO_1m_since2018, by = "Date")
since2018_1m_and_8m <- since2018_1m_and_8m %>% 
  mutate(Barometric_Pressure_Temperature_C = NA,
         Barometric_Pressure_Pressure_PSI = NA,
         Temperature_Modem_C = NA,
         Vbattery_Modem_Volts = NA,
         Vcommon_Modem_Volts = NA)

#bind early 2018 to 2012 - 2017, and change names to match with above 
binded1 <- rbind(dosonde_2012_17, do_20180103)
binded1 <- binded1 %>% 
  rename(DO8m_Temperature_C = "DO_Temperature_C",
         DO8m_Pressure_PSI = "DO_Pressure_PSI",
         DO8m_Power_V = "DO_Power_V",
         DO8m_Dissolved_Oxygen_ppm = "DO_Dissolved_Oxygen_ppm",
         DO8m_Temperature2_C = "DO_Temperature2_C")



#bind binded1 to the since 2018 data 
all_data_binded <- rbind(binded1, since2018_1m_and_8m)

#aggregate any uneven rows by data 
all_data_binded_aggregated <- aggregate(x=all_data_binded[c("Barometric_Pressure_Temperature_C",
                                                 "Barometric_Pressure_Pressure_PSI",
                                                 "DO8m_Temperature_C",
                                                 "DO8m_Pressure_PSI",
                                                 "DO8m_Power_V",
                                                 "DO8m_Dissolved_Oxygen_ppm",
                                                 "DO8m_Temperature2_C",
                                                 "DO1m_Temperature_C",
                                                 "DO1m_Pressure_PSI",
                                                 "DO1m_Power_V",
                                                 "DO1m_Dissolved_Oxygen_ppm",
                                                 "DO1m_Temperature2_C",
                                                 "Temperature_Modem_C",
                                                 "Vbattery_Modem_Volts",
                                                 "Vcommon_Modem_Volts")],
                         by=list(Date=all_data_binded$Date), mean, na.rm = TRUE) 


#remove duplicated rows 
all_data_binded_aggregated_dupRemoved <- all_data_binded_aggregated[!duplicated(all_data_binded_aggregated$Date), ]



#write csv
write.csv(all_data_binded_aggregated_dupRemoved, "./Data/DataNotYetUploadedtoEDI/WVWA_DO_sondes/FCR_WVWA_DOsonde_2012_summer2022_rawData.csv")



#### FROM compilation in 2018 ----
### This compilation is what made file: FCR_DOsonde_2012_2017.csv

###organize data from senvu###

#nov17 to feb18 IGNORE FOR NOW
#jantofeb18 <- read.csv("FCR_010318-020218_DO_senvu.csv", header=T)
#dec17tojan18 <- read.csv("FCR_120217-010118_DO_senvu.csv", header = T)
#novtodec17 <- read.csv("FCR_111917-121817_DO_senvu.csv", header = T)
#comp <- rbind(dec17tojan18, novtodec17) # need to delete overlapping jan dates
#com<- comp[!duplicated(comp$Reading.Date), ]
#names(com) <- c("date", "BPTemp", "BPPSI", "DOTemp", "DOPSI", "DOpower", "DOppm", "DOtemp2", 
#               "DO1Temp", "DO1PSI", "DO1power", "DO1ppm", "DO1temp2","TempModem", "VbatteryModem", "VcommonModem")

# sep16 to 31DEc17
dec17 <- read.csv("FCR_120217-010118_DO_senvu.csv")
novtodec17 <- read.csv("FCR_111917-121817_DO_senvu.csv")
octtonov17 <- read.csv("FCR_102117-112017_DO_senvu.csv")
septooct17 <- read.csv("FCR_092117-102117_DO_senvu.csv")
augtosep17 <- read.csv("FCR_082217-092117_DO_senvu.csv")
jultoaug17 <- read.csv("FCR_072317-082217_DO_senvu.csv")
jul2217 <- read.csv("FCR_072217_DO_senvu.csv")
juntojul17 <- read.csv("FCR_062217-072217_DO_senvu.csv")
maytojun17 <- read.csv("FCR_052317-062217_DO_senvu.csv")
may2217 <- read.csv("FCR_052217_DO_senvu.csv")
aprtomay17 <- read.csv("FCR_042217-052217_DO_senvu.csv")
martoapr17 <- read.csv("FCR_032317-042217_DO_senvu.csv")
mar2217 <- read.csv("FCR_032217_DO_senvu.csv")
febtomar17 <- read.csv("FCR_022017-032217_DO_senvu.csv")
jantofeb17 <- read.csv("FCR_012117-022017_DO_senvu.csv")
dec16tojan17 <- read.csv("FCR_122316-012117_DO_senvu.csv")
novtodec16 <- read.csv("FCR_112616-122316_DO_senvu.csv")
nov2516 <- read.csv("FCR_112516_DO_senvu.csv")
octtonov16 <- read.csv("FCR_102716-112516_DO_senvu.csv")
oct2616 <- read.csv("FCR_102616_DO_senvu.csv")
septooct16 <- read.csv("FCR_092616-102616_DO_senvu.csv")

            ### gives warning messages. looses DO data.
#comp16t17 <- rbind(dec17, novtodec17, octtonov17, septooct17, augtosep17, jultoaug17, jul2217, juntojul17, maytojun17,
 #                  may2217, aprtomay17, martoapr17, mar2217, febtomar17, jantofeb17, dec16tojan17, novtodec16,
  #                 nov2516, octtonov16, oct2616, septooct16)
#compp16t17 <- comp16t17[!duplicated(comp16t17$Reading.Date), ]
#names(compp16t17) <- c("date", "BPTemp", "BPPSI", "DOTemp", "DOPSI", "DOpower", "DOppm", "DOtemp2", 
 #                     "DO1Temp", "DO1PSI", "DO1power", "DO1ppm", "DO1temp2","TempModem", "VbatteryModem", "VcommonModem")

          ### we're breaking up the large rbind to try and find/eliminate the warning issues. 
          ### this one worked and jan to jun, and oct to dec17. 
sep16todec16 <- rbind(dec16tojan17, novtodec16, nov2516, octtonov16, oct2616, septooct16)
names(sep16todec16) <- c("date", "BPTemp", "BPPSI", "DOTemp", "DOPSI", "DOpower", "DOppm", "DOtemp2", 
                       "DO1Temp", "DO1PSI", "DO1power", "DO1ppm", "DO1temp2","TempModem", "VbatteryModem", "VcommonModem")
sep16todec16A <- sep16todec16[!duplicated(sep16todec16$date),]


jan17tojun17 <- rbind(maytojun17, may2217, aprtomay17, martoapr17, mar2217, febtomar17, jantofeb17)
names(jan17tojun17) <- c("date", "BPTemp", "BPPSI", "DOTemp", "DOPSI", "DOpower", "DOppm", "DOtemp2", 
                         "DO1Temp", "DO1PSI", "DO1power", "DO1ppm", "DO1temp2","TempModem", "VbatteryModem", "VcommonModem")
jan17tojun17A <- jan17tojun17[!duplicated(jan17tojun17$date),]

oct17todec17 <- rbind(dec17, novtodec17, octtonov17)
names(oct17todec17) <- c("date", "BPTemp", "BPPSI", "DOTemp", "DOPSI", "DOpower", "DOppm", "DOtemp2", 
                         "DO1Temp", "DO1PSI", "DO1power", "DO1ppm", "DO1temp2","TempModem", "VbatteryModem", "VcommonModem")
oct17todec17A <- oct17todec17[!duplicated(oct17todec17$date),]

junttooct17 <- rbind(septooct17, augtosep17, jultoaug17, jul2217, juntojul17)
names(junttooct17) <- c("date", "BPTemp", "BPPSI", "DOTemp", "DOPSI", "DOpower", "DOppm", "DOtemp2", 
                         "DO1Temp", "DO1PSI", "DO1power", "DO1ppm", "DO1temp2","TempModem", "VbatteryModem", "VcommonModem")
junttooct17A <- junttooct17[!duplicated(junttooct17$date),]


together <- rbind(junttooct17A, jan17tojun17A, sep16todec16A)

sep2616to31dec17 <- rbind(oct17todec17A, together)
#write.csv(sep2616to31dec17, "FCR26Sep16to31Dec17_DOsonde_NLU_A.csv")

#Senvu data 28OCT15 to 23Mar16
octtonov15 <- read.csv("FCR_102815-110415_DO_senvu.csv")
nov4to1115 <- read.csv("FCR_110415-111115_DO_senvu.csv")
nov11to18 <- read.csv("FCR_111115-111815_DO_senvu.csv")
nov18to25 <- read.csv("FCR_111815-112515_DO_senvu.csv")
nov25todec2515 <- read.csv("FCR_112515-122515_DO_senvu.csv")
dec15tojan16 <- read.csv("FCR_122515-012416_DO_senvu.csv")
jantofeb16 <- read.csv("FCR_012416-022316_DO_senvu.csv")
febtomar16 <- read.csv("FCR_022316-032316_DO_senvu.csv")

## how to join data w/ differnet collumn names. 
##Rbind will join by column names, just need right number of columns and matching names 

octtonov15[c("0", "1", "2", "3", "4", "5", "6")] <- NA
names(octtonov15) <- c("date", "DOTemp", "DOPSI", "DOpower", "DOppm", "DOtemp2", "TempModem", "VbatteryModem", "VcommonModem",
                       "BPTemp", "BPPSI", "DO1Temp", "DO1PSI", "DO1power", "DO1ppm", "DO1temp2")

comp15t16 <- rbind(febtomar16, jantofeb16, dec15tojan16, nov25todec2515, nov18to25,nov11to18,nov4to1115)
comp15t16<- comp15t16[!duplicated(comp15t16$Reading.Date), ]
names(comp15t16) <- c("date", "BPTemp", "BPPSI", "DOTemp", "DOPSI", "DOpower", "DOppm", "DOtemp2",
                      "DO1Temp", "DO1PSI", "DO1power", "DO1ppm", "DO1temp2","TempModem", "VbatteryModem", "VcommonModem")

comp15t16 <- rbind(comp15t16, octtonov15)
comp15t16<- comp15t16[!duplicated(comp15t16$date),]

#03/26/16 to 09/26/16
augtosep16 <- read.csv("FCR_082716-092616_DO_senvu.csv")
jultoaug16 <- read.csv("FCR_072816-082816_DO_senvu.csv")
juntojul16 <- read.csv("FCR_062816-072816_DO_senvu.csv")
maytojun16 <- read.csv("FCR_052916-062816_DO_senvu.csv")
aprtomay16 <- read.csv("FCR_042916-052916_DO_senvu.csv")
martoapr16 <- read.csv("FCR_033016-042916_DO_senvu.csv")
endmar16 <- read.csv("FCR_032316-033016_DO_senvu.csv")

comp16 <- rbind(augtosep16, jultoaug16, juntojul16, maytojun16, aprtomay16, martoapr16, endmar16)
comp16<- comp16[!duplicated(comp16$Reading.Date), ]
names(comp16) <- c("date", "BPTemp", "BPPSI", "DOTemp", "DOPSI", "DOpower", "DOppm", "DOtemp2",
                      "DO1Temp", "DO1PSI", "DO1power", "DO1ppm", "DO1temp2","TempModem", "VbatteryModem", "VcommonModem")


               ####28Oct15 to 31Dec17, all senvu data together 
oct15todec17 <- rbind(sep2616to31dec17, comp16, comp15t16)
#write.csv(oct15todec17, "checkthisA.csv")

asdfff <- rbind(comp16, comp15t16)

#gives no errors when oct17 isnt there. 
qwerty <- rbind(oct17todec17A, together, comp16, comp15t16)
qw <- rbind(together, comp16, comp15t16)
qwe <- rbind(oct17todec17A, qw)
qwe<- qwe[!duplicated(qwe$date), ]
#write.csv(qwe, "checkqwe.csv")
#qwe worked!!!
#write.csv(qwe, "FCR_Oct15toDec17_DOsondes_qweNLU.csv")



                                     ######data from aqua4plus
##for bringing in aqua4plus data
read_csv("FCR_DO_1m_121117.csv", skip = 31)
?read_csv

#29Apr14 to 31Oct15 stretch of Aqua4plus
#missing gaps from this list are in data gathered below for CCC
oct31_15<- read_csv("FCRDO_103115.csv", skip = 31) #3aug15 to 31Oct15
colnames(oct31_15)[2] <- "Date"
oct31_15 <- oct31_15[, -c(1,8:12)]
aug03_15 <- read_csv("FCRDO_080315.csv", skip = 31) #8Jun15 to 3aug15
aug03_15 <- aug03_15[, -c(1,8:12)]
colnames(aug03_15)[1] <- "Date"

dec10_14 <- read_csv("FCRDO2014_121014.csv", skip = 31) #4Oct14 to 10dec14
colnames(dec10_14)[2] <- "Date"
dec10_14 <- dec10_14[, -c(1,8:12)]
oct10_14 <- read_csv("FCRDO2014_100414.csv", skip = 31) #4aug14 to 4oct14
colnames(oct10_14)[2] <- "Date"
oct10_14 <- oct10_14[, -c(1,8:12)]
dec14tomar15 <- read_csv("FCRDO2014_033115.csv" ,skip = 31)#10dec14 to 31Mar15
colnames(dec14tomar15)[2] <- "Date"
dec14tomar15 <- dec14tomar15[, -c(1,8:12)]

aq4p_29apr14to31oct15 <- rbind(oct31_15, aug03_15, ccc2015, dec14tomar15, dec10_14, oct10_14, ccc2014) 
write.csv(aq4p_29apr14to31oct15, "checkaq4pB.csv") #from this csv, sorted dates saved data as "FCR_29Apr14-31Oct15_DOsonde_aqua4plus.csv"


##data Cayelan needs: June 15 to July 10 2014, and April 15 to May 20 2015 
###2014
ccc2014 <- read_csv("FCRDO2014_080414.csv", skip = 31) #29Apr14 to 4Aug14
ccc2014 <- ccc2014[, -c(1,8:12)]
ccc2014a <- subset(ccc2014, ccc2014$Date >= "2014-06-14 20:00:00")
ccc2014b <- subset(ccc2014a, ccc2014a$Date <= "2014-07-10 20:00:00")
write.csv(ccc2014b, "FCRDOsonde8m_15Jun14-10Jul14.csv")
###2015
ccc2015 <- read_csv("FCRDO_060815.csv", skip = 31) #31Mar15 to 8jun15
ccc2015 <- ccc2015[, -c(1,8:12)]
ccc2015a <- subset(ccc2015, ccc2015$Date >= "2015-04-14 20:00:00")
ccc2015b <- subset(ccc2015a, ccc2015a$Date <= "2015-05-20 20:00:00")
write.csv(ccc2015b, "FCRDOsonde8m_15Apr15-20May15.csv")


#Aqua4plus data before 29Apr14
a<- read_csv("FCR_DO15min_020514.csv", skip = 32) #18Oct13 to 05feb14
colnames(a)[2] <- "Date"
a <- a[, -c(1)]
b<- read_csv("FCR15minDO_100913.csv", skip = 32) #4 readings on 8/12/13
colnames(b)[2] <- "Date"
b <- b[, -c(1)]
c<- read_csv("FCR15minDO_101813.csv", skip = 32) #9Oct13 to 17Oct 13
colnames(c)[2] <- "Date"
c <- c[, -c(1)]
d<- read_csv("FCRDO15min_070313.csv", skip = 32) #20Jun13 to 3jul13
colnames(d)[2] <- "Date"
d <- d[, -c(1)]
e<- read_csv("FCRDO15min_062713.csv", skip = 32) #20 to 27 Jun13
colnames(e)[2] <- "Date"
e <- e[, -c(1)]
f<- read_csv("FCRDO15min_062013.csv", skip = 32) #18to20 Jun 13
colnames(f)[2] <- "Date"
f <- f[, -c(1)]
g<- read_csv("FCRDO15min_061113.csv", skip = 32) #15May13 to 11jun13
colnames(g)[2] <- "Date"
g <- g[, -c(1)]
h<- read_csv("FCRDO2013_050913.csv", skip = 32) #4Apr13 to 9May13, data on the hour 
colnames(h)[2] <- "Date"
h <- h[, -c(1)]
i<- read_csv("FCRDO2013_042513.csv", skip = 32) # 4 to 25 apr13, on hours 
colnames(i)[2] <- "Date"
i <- i[, -c(1)]
j<- read_csv("FCRDO_040413.csv", skip = 32) # 28Sep12 to 4apr13, on hours
colnames(j)[2] <- "Date"
j <- j[, -c(1)]

abc <- rbind(a,b,c,d,e,f,g,h,i,j)
write.csv(abc, "FCR_28Sep12-05Feb14_DOsonde8m_aqua4plus.csv") #sorted by date in csv

#Combine two aqua4plus files
aa<- read.csv("FCR_29Apr14-31Oct15_DOsonde8m_aqua4plus.csv")
bb<- read.csv("FCR_28Sep12-05Feb14_DOsonde8m_aqua4plus.csv")
bb<- bb[,-c(1)]
aabb<- rbind(bb,aa)
#write.csv(aabb, "FCR_28Sep12-31Oct15_DOsonde8m_aqua4plus.csv") #written already 


                                    ###Line up senvu data and combine w/ aqua4plus###
y<- read.csv("FCR_28Sep12-31Oct15_DOsonde8m_aqua4plus.csv")
y[c("0", "1", "2", "3", "4", "5", "6", "7")] <- NA
names(y) <- c("date", "BPTemp", "BPPSI", "DOTemp", "DOPSI", "DOpower", "DOppm", "DOtemp2", 
                        "DO1Temp", "DO1PSI", "DO1power", "DO1ppm", "DO1temp2", "TempModem", "VbatteryModem", "VcommonModem")
#ya <- subset(y, y$date < "2015-10-28 00:15:00")

#lineup zz, DONT need now/ already done 
zz <- read.csv("FCR_Oct15toDec17_DOsondes_qweNLU_A.csv")
zzz<- aggregate(x=zz[c("BPTemp", "BPPSI", "DOTemp", "DOPSI", "DOpower", "DOppm", "DOtemp2",
                 "DO1Temp", "DO1PSI", "DO1power", "DO1ppm", "DO1temp2","TempModem", "VbatteryModem", "VcommonModem")],
          by=list(name=zz$date), mean, na.rm = TRUE) 
#write.csv(zzz, "FCR_DOsonde_October2015toDecember2017_senvu.csv")
x <- read.csv("FCR_DOsonde_October2015toDecember2017_senvu.csv")
x<- x[,-c(1)]
colnames(x)[1] <- "date"

xy <- rbind(x,y)
xyza <- xy[!duplicated(xy$date), ]

write.csv(xyza, "FCR_DOsonde_2012to2017_aq4p_senvu_combined.csv")










