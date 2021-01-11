
#Add an offset and a flag for the broken thermoister data
#THis is so I can make sure it worked
#by ABP

library(tidyverse)
library(lubridate)

download.file('https://github.com/FLARE-forecast/FCRE-data/raw/fcre-catwalk-data/Catwalk.csv','Catwalk.csv')

CATDATA_COL_NAMES = c("DateTime", "RECORD", "CR6_Batt_V", "CR6Panel_Temp_C", "ThermistorTemp_C_surface",
                      "ThermistorTemp_C_1", "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4",
                      "ThermistorTemp_C_5", "ThermistorTemp_C_6", "ThermistorTemp_C_7", "ThermistorTemp_C_8",
                      "ThermistorTemp_C_9", "RDO_mgL_5", "RDOsat_percent_5", "RDOTemp_C_5", "RDO_mgL_9",
                      "RDOsat_percent_9", "RDOTemp_C_9", "EXO_Date", "EXO_Time", "EXOTemp_C_1", "EXOCond_uScm_1",
                      "EXOSpCond_uScm_1", "EXOTDS_mgL_1", "EXODOsat_percent_1", "EXODO_mgL_1", "EXOChla_RFU_1",
                      "EXOChla_ugL_1", "EXOBGAPC_RFU_1", "EXOBGAPC_ugL_1", "EXOfDOM_RFU_1", "EXOfDOM_QSU_1",
                      "EXO_pressure", "EXO_depth", "EXO_battery", "EXO_cablepower", "EXO_wiper")


catdata <- read_csv('Catwalk.csv', skip = 4, col_names = CATDATA_COL_NAMES,
                    col_types = cols(.default = col_double(), DateTime = col_datetime()))

#create script to offset the 1m and 4m temp sensor
catdata$Flag_Temp_Surf <- 0 
catdata$Flag_Temp_1 <- 0 
catdata$Flag_Temp_2 <- 0 
catdata$Flag_Temp_3 <- 0 
catdata$Flag_Temp_4 <- 0 
catdata$Flag_Temp_5 <- 0 
catdata$Flag_Temp_6 <- 0 
catdata$Flag_Temp_7 <- 0 
catdata$Flag_Temp_8 <- 0 
catdata$Flag_Temp_9 <- 0



#Whitney This might be all you need to add in.

#Two of the thermistors started to read higher than the one above them. Fixed this using a constant offset. 
#methods described in metadat

#start time for 1m is 30 Oct 2020 13:00EST
#start time for 4m is 31 Oct 2020 5:00EST
catdata <- catdata %>%
  mutate(Flag_Temp_1 = ifelse(DateTime >= "2020-10-30 13:00" & DateTime < "2020-12-31 23:50" & (! is.na(ThermistorTemp_C_1)) ,8, Flag_Temp_1))%>%
  mutate(Flag_Temp_4 = ifelse(DateTime >= "2020-10-31 5:00" & DateTime < "2020-12-31 23:50" &
                                         (! is.na(ThermistorTemp_C_4)),8, Flag_Temp_1))%>%
 mutate(ThermistorTemp_C_11 = ifelse(DateTime >= "2020-10-30 13:00" & DateTime < "2020-12-31 23:50" &
                                      (! is.na(ThermistorTemp_C_1)), (ThermistorTemp_C_1-0.22617), ThermistorTemp_C_1 )) %>%
          mutate(ThermistorTemp_C_41 = ifelse(DateTime >= "2020-10-30 13:00" & DateTime < "2020-12-31 23:50" &
                                               (! is.na(ThermistorTemp_C_4)), (ThermistorTemp_C_4-0.18122), ThermistorTemp_C_4 )) 
          
           

