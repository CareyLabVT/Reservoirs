
#load packages 
pacman::p_load(tidyverse, lubridate, xts, cowplot, plotly)

#read in csv's downloaded from EDI 
getwd()
# exo <- read_csv("./Data/Data_EDI/catdata_edited_withDO.csv", col_types = cols(.default = "d", 
#                                                                               Reservoir = "c",
#                                                                               DateTime = "T"))  #defining column type to prevent logical errors


CATDATA_COL_NAMES = c("DateTime", "RECORD", "CR6_Batt_V", "CR6Panel_Temp_C", "ThermistorTemp_C_surface",
                      "ThermistorTemp_C_1", "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4",
                      "ThermistorTemp_C_5", "ThermistorTemp_C_6", "ThermistorTemp_C_7", "ThermistorTemp_C_8",
                      "ThermistorTemp_C_9", "RDO_mgL_5", "RDOsat_percent_5", "RDOTemp_C_5", "RDO_mgL_9",
                      "RDOsat_percent_9", "RDOTemp_C_9", "EXO_Date", "EXO_Time", "EXOTemp_C_1", "EXOCond_uScm_1",
                      "EXOSpCond_uScm_1", "EXOTDS_mgL_1", "EXODOsat_percent_1", "EXODO_mgL_1", "EXOChla_RFU_1",
                      "EXOChla_ugL_1", "EXOBGAPC_RFU_1", "EXOBGAPC_ugL_1", "EXOfDOM_RFU_1", "EXOfDOM_QSU_1",
                      "EXO_pressure", "EXO_depth", "EXO_battery", "EXO_cablepower", "EXO_wiper")

catdata <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLCatwalk/Catwalk_2020.csv", skip = 4, col_names = CATDATA_COL_NAMES,
                    col_types = cols(.default = col_double(), DateTime = col_datetime()))

### Finding duplicated rows. They aren't showing in this catwalk file
duplicated <- catdata %>% 
  filter(DateTime >= as.POSIXct("2019-04-15 13:00:00", tz = "UTC"),
         DateTime <= as.POSIXct("2019-04-15 17:00:00", tz = "UTC"))



#### Selecting EXO data for AR and formating for wavelet ####
head(catdata)


#exo_filt <- slice(exo, -c(40656:40660)) #removing duplicated hour of data from 15 April 2019


# exo_filt <- exo_filt %>% 
#   select(DateTime, EXOTemp_C_1, EXODOsat_percent_1, EXODO_mgL_1, EXOChla_ugL_1, EXOfDOM_QSU_1,
#          EXO_depth) %>% # could have RFU for chla and fdom 
#   filter(DateTime >= as.POSIXct("2018-08-06 17:30:00", tz = "UTC")) #first data with exo is 2018-08-06 17:30:00, need to define tz as UTC otherwise will select time four hours eariler becasue of EST timezone difference 
# 
# head(exo_filt)


#plot(exo_filt$DateTime, exo_filt$EXOfDOM_QSU_1)
hist(catdata$EXOfDOM_QSU_1)
mean(catdata$EXOfDOM_QSU_1, na.rm = TRUE)
median(catdata$EXOfDOM_QSU_1, na.rm = TRUE)


##writing QAQC code for EDI script 
sd_fDOM <- sd(catdata$EXOfDOM_QSU_1, na.rm = TRUE) #deteriming the standard deviation of fDOM data 

fDOM_pre_QAQC <- ggplot(data = catdata, aes(x = DateTime, y = EXOfDOM_QSU_1)) +
  geom_point()+
  ggtitle("fDOM (QSU) pre QAQC")
ggplotly(fDOM_pre_QAQC)

QAQC_EDI <- catdata %>% 
  mutate(Flag_fDOM = ifelse(is.na(EXOfDOM_QSU_1), 1, 0)) %>%  #This creates Flag column for fDOM data, setting all NA's going into QAQC as 1 for missing data, and to 0 for the rest
  mutate(fDOM = lag(EXOfDOM_QSU_1, 0),
         fDOM_lag1 = lag(EXOfDOM_QSU_1, 1),
         fDOM_lead1 = lead(EXOfDOM_QSU_1, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOfDOM_QSU_1 = ifelse(fDOM < 0, NA, EXOfDOM_QSU_1),
         EXOfDOM_RFU_1 = ifelse(fDOM < 0, NA, EXOfDOM_RFU_1),
         Flag_fDOM = ifelse(fDOM < 0, 2, Flag_fDOM)   ) %>% #These mutates are QAQCing for negative fDOM QSU values and setting these to NA and making a flag for these. This was done outside of the 2 sd deviation rule because there were two negative points in a row and one was not removed with the follwoing if else statements. 
  mutate(EXOfDOM_QSU_1 = ifelse(
    ( abs(fDOM_lag1 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1 - fDOM) > (2*sd_fDOM)   ), NA, EXOfDOM_QSU_1
  )) %>%  #QAQC to remove outliers for QSU fDOM data 
  mutate(EXOfDOM_RFU_1 = ifelse(
    ( abs(fDOM_lag1 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1 - fDOM) > (2*sd_fDOM)   ), NA, EXOfDOM_RFU_1
  )) %>% #QAQC to remove outliers for RFU fDOM data
  mutate(Flag_fDOM = ifelse(
    ( abs(fDOM_lag1 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1 - fDOM) > (2*sd_fDOM)   ), 2, Flag_fDOM
  ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
   select(-fDOM, -fDOM_lag1, -fDOM_lead1)  #This removes the columns used to run ifelse statements since they are no longer needed. 

head(QAQC_EDI)


check <- QAQC_EDI %>% 
  filter(Flag_fDOM == 2) #check that values flagged as 2 became NAs 

fDOM_post_QAQC <- ggplot(data = QAQC_EDI, aes(x = DateTime, y = EXOfDOM_QSU_1)) +
  geom_point()+
  ggtitle("fDOM (QSU) post QAQC")
ggplotly(fDOM_post_QAQC)

plot(exo$DateTime, exo$EXOfDOM_QSU_1)
plot(exo$DateTime, exo$EXOfDOM_RFU_1)


plot(QAQC_EDI$DateTime, QAQC_EDI$EXOfDOM_QSU_1)
plot(QAQC_EDI$DateTime, QAQC_EDI$EXOfDOM_RFU_1)

min(QAQC_EDI$EXOfDOM_QSU_1, na.rm = TRUE)

check_min <- QAQC_EDI %>% 
  filter(EXOfDOM_QSU_1 < 5)

sd(QAQC_EDI$EXOfDOM_QSU_1, na.rm = TRUE) 

#fDOM data undwent a QAQC protocol that set negative QSU values to NA and values that were greater or less that 2 standard deviations of the previous and following 10 minute concentration to NA.


##end edi code 



