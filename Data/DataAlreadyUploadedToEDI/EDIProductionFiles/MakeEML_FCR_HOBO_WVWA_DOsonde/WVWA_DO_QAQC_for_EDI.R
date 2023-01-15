### QAQC WVWA DO data 
### Dexter Howard 
### 29Jan2021
### updated again by DWH in September 2022 


pacman::p_load(tidyverse, lubridate, xts, cowplot, plotly, scales, patchwork)
library(zoo) #for na.approx

#### Check out HOBOs ####
hobos <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_FCR_HOBO_WVWA_DOsonde/misc_data_files/FCR_HOBO_15_16_17_18_DWH_7jan23_forEDI.csv")
hobos_long <- pivot_longer(hobos, cols = c(4:12))

hobosplot <- hobos_long %>% 
  ggplot(aes(x= DateTime, y = value, col = name))+
  geom_line()
hobosplot

#### Looking at raw WVWA DO sonde on catwalk ####

#read in data updated on 8sep22
dosonde <- read_csv("./Data/DataNotYetUploadedToEDI/WVWA_DO_sondes/FCR_WVWA_DOsonde_2012_summer2022_rawData.csv")

#Plotting 1m and 8m raw data 
ggplot(data = dosonde, aes(x = Date, y = DO8m_Dissolved_Oxygen_ppm))+
  geom_line()

ggplot(data = dosonde, aes(x = Date, y = DO8m_Temperature2_C))+
  geom_line()

ggplot(data = dosonde, aes(x = Date, y = DO8m_Pressure_PSI))+
  geom_line()
 
ggplot(data = dosonde, aes(x = Date, y = DO1m_Dissolved_Oxygen_ppm))+
  geom_line()

ggplot(data = dosonde, aes(x = Date, y = DO1m_Temperature2_C))+
  geom_line()

ggplot(data = dosonde, aes(x = Date, y = DO1m_Pressure_PSI))+
  geom_line()



#look at summary stats because of extreme high outliers 
hist(dosonde$DO1m_Dissolved_Oxygen_ppm)
summary(dosonde$DO1m_Dissolved_Oxygen_ppm)

hist(dosonde$DO1m_Temperature2_C)
summary(dosonde$DO1m_Temperature2_C)

hist(dosonde$DO8m_Dissolved_Oxygen_ppm)
summary(dosonde$DO8m_Dissolved_Oxygen_ppm)

hist(dosonde$DO8m_Temperature2_C)
summary(dosonde$DO8m_Temperature2_C)

summary(dosonde$Date)


#### QAQC DO and Temp data ####

# Flag values
# 0: no flag
# 1: value removed due to maintenance and set to NA
# 2: negative or outlier value removed and set to NA, see Methods section for more detail on QAQC process
# 3: negative values set to 0
# 4: value removed due to fouling and set to NA
# 5: questionable value due to potential fouling
# 6: very questionable value due to potential fouling. Values adjusted using a linear or square root function     to match high-resolution CTD profiles are given in RDO_mgL_5 and RDO_sat_percent_5
# 7: missing data
# 8: Value corrected using a constant offset due to two thermistor malfunctions in Fall 2020

#remove extreme outliers
dosonde_qaqc1 <- dosonde %>% 
  select(Date, DO1m_Dissolved_Oxygen_ppm, DO1m_Temperature2_C, DO8m_Dissolved_Oxygen_ppm, DO8m_Temperature2_C) %>% 
  mutate(Flag_1m = ifelse(DO1m_Dissolved_Oxygen_ppm <= 0, 2, 0),
         Flag_1m = ifelse(DO1m_Dissolved_Oxygen_ppm > 50, 2, Flag_1m),
         Flag_1m = ifelse(is.na(DO1m_Dissolved_Oxygen_ppm), 7, Flag_1m),
         Flag_8m = ifelse(DO8m_Dissolved_Oxygen_ppm <= 0, 2, 0),
         Flag_8m = ifelse(DO8m_Dissolved_Oxygen_ppm > 50, 2, Flag_8m),
         Flag_8m = ifelse(is.na(DO8m_Dissolved_Oxygen_ppm), 7, Flag_8m)
         ) %>%  #set flags for extreme outliers and missing values 
  mutate(DO1m_Dissolved_Oxygen_ppm = ifelse(Flag_1m == 2, NA, DO1m_Dissolved_Oxygen_ppm),
         DO1m_Temperature2_C = ifelse(Flag_1m == 2, NA, DO1m_Temperature2_C),
         DO8m_Dissolved_Oxygen_ppm = ifelse(Flag_8m == 2, NA, DO8m_Dissolved_Oxygen_ppm),
         DO8m_Temperature2_C = ifelse(Flag_8m == 2, NA, DO8m_Temperature2_C)
         )

#check QAQC1
ggplot(data = dosonde_qaqc1, aes(x = Date, y = DO1m_Dissolved_Oxygen_ppm))+
  geom_line()

ggplot(data = dosonde_qaqc1, aes(x = Date, y = DO1m_Temperature2_C))+
  geom_line()

ggplot(data = dosonde_qaqc1, aes(x = Date, y = DO8m_Dissolved_Oxygen_ppm))+
  geom_point()

ggplot(data = dosonde_qaqc1, aes(x = Date, y = DO8m_Temperature2_C))+
  geom_point()

#QAQC2; remove values after 1 jan 2019 where 1m DO goes awry
head(dosonde_qaqc1)

dosonde_qaqc2 <- dosonde_qaqc1 %>% 
  filter(Date < ymd_hms("2019-01-01 00:00:00"))
  

  
#QAQC3; apply 2sd qaqc 
head(dosonde_qaqc2)

sd_1m <- sd(dosonde_qaqc2$DO1m_Dissolved_Oxygen_ppm, na.rm = TRUE)
sd_8m <- sd(dosonde_qaqc2$DO8m_Dissolved_Oxygen_ppm, na.rm = TRUE)
sd_1m_temp <- sd(dosonde_qaqc2$DO1m_Temperature2_C, na.rm = TRUE)
sd_8m_temp <- sd(dosonde_qaqc2$DO8m_Temperature2_C, na.rm = TRUE)


#run QAQC for DO 
dosonde_qaqc3 <- dosonde_qaqc2 %>% 
  mutate(DO1m = lag(DO1m_Dissolved_Oxygen_ppm, 0),
         DO1m_lag1 = lag(DO1m_Dissolved_Oxygen_ppm, 1),
         DO1m_lead1 = lead(DO1m_Dissolved_Oxygen_ppm, 1),
         DO8m = lag(DO8m_Dissolved_Oxygen_ppm, 0),
         DO8m_lag1 = lag(DO8m_Dissolved_Oxygen_ppm, 1),
         DO8m_lead1 = lead(DO8m_Dissolved_Oxygen_ppm, 1) ) %>% 
  mutate(DO1m_QAQC = ifelse(
    ( abs(DO1m_lag1 - DO1m) > (2*sd_1m)   )  & ( abs(DO1m_lead1 - DO1m) > (2*sd_1m)   ), NA, DO1m_Dissolved_Oxygen_ppm
  )) %>%  #QAQC to remove outliers for 1m DO data 
  mutate(Flag_1m_2SD = ifelse(
    ( abs(DO1m_lag1 - DO1m) > (2*sd_1m)   )  & ( abs(DO1m_lead1 - DO1m) > (2*sd_1m)   ), 2, NA
  )) %>% 
  mutate(DO8m_QAQC = ifelse(
    ( abs(DO8m_lag1 - DO8m) > (2*sd_8m)   )  & ( abs(DO8m_lead1 - DO8m) > (2*sd_8m)   ), NA, DO8m_Dissolved_Oxygen_ppm
  )) %>%  #QAQC to remove outliers for 8m DO data 
  mutate(Flag_8m_2SD = ifelse(
    ( abs(DO8m_lag1 - DO8m) > (2*sd_8m)   )  & ( abs(DO8m_lead1 - DO8m) > (2*sd_8m)   ), 2, NA
  )) %>% 
  select(-DO1m, -DO1m_lag1, -DO1m_lead1, -DO8m, -DO8m_lag1, -DO8m_lead1 ) 

#run QAQC for TEMP
dosonde_qaqc4 <- dosonde_qaqc3 %>% 
  mutate(DO1m = lag(DO1m_Temperature2_C, 0),
         DO1m_lag1 = lag(DO1m_Temperature2_C, 1),
         DO1m_lead1 = lead(DO1m_Temperature2_C, 1),
         DO8m = lag(DO8m_Temperature2_C, 0),
         DO8m_lag1 = lag(DO8m_Temperature2_C, 1),
         DO8m_lead1 = lead(DO8m_Temperature2_C, 1) ) %>% 
  mutate(DO1m_Temp_QAQC = ifelse(
    ( abs(DO1m_lag1 - DO1m) > (2*sd_1m_temp)   )  & ( abs(DO1m_lead1 - DO1m) > (2*sd_1m_temp)   ), NA, DO1m_Temperature2_C
  )) %>%  #QAQC to remove outliers for 1m DO data 
  mutate(Flag_1m_Temp_2SD = ifelse(
    ( abs(DO1m_lag1 - DO1m) > (2*sd_1m_temp)   )  & ( abs(DO1m_lead1 - DO1m) > (2*sd_1m_temp)   ), 2, NA
  )) %>% 
  mutate(DO8m_Temp_QAQC = ifelse(
    ( abs(DO8m_lag1 - DO8m) > (2*sd_8m_temp)   )  & ( abs(DO8m_lead1 - DO8m) > (2*sd_8m_temp)   ), NA, DO8m_Temperature2_C
  )) %>%  #QAQC to remove outliers for 8m DO data 
  mutate(Flag_8m_Temp_2SD = ifelse(
    ( abs(DO8m_lag1 - DO8m) > (2*sd_8m_temp)   )  & ( abs(DO8m_lead1 - DO8m) > (2*sd_8m_temp)   ), 2, NA
  )) %>% 
  select(-DO1m, -DO1m_lag1, -DO1m_lead1, -DO8m, -DO8m_lag1, -DO8m_lead1 ) 
  
  
#check QAQC3
ggplot(data = dosonde_qaqc4, aes(x = Date, y = DO1m_QAQC))+
  geom_point()

ggplot(data = dosonde_qaqc4, aes(x = Date, y = DO8m_QAQC))+
  geom_point()

ggplot(data = dosonde_qaqc4, aes(x = Date, y = DO1m_Temp_QAQC))+
  geom_point()

ggplot(data = dosonde_qaqc4, aes(x = Date, y = DO8m_Temp_QAQC))+
  geom_point()

#Clean up qaqc 3 and make final csv 
head(dosonde_qaqc3)

dosonde_qaqc3 <- dosonde_qaqc3[order(dosonde_qaqc3$Date),]


dosonde_edi <- dosonde_qaqc3 %>% 
  mutate(Flag1m_Fin = ifelse(!is.na(Flag_1m_2SD), Flag_1m_2SD, Flag_1m),
         Flag8m_Fin = ifelse(!is.na(Flag_8m_2SD), Flag_8m_2SD, Flag_8m)
         ) %>% 
  mutate(Reservoir = "FCR",
         Site = 50,
         Date = as.character(Date)) %>% 
  select(Reservoir, Site, Date, DO1m_Temperature2_C, DO1m_QAQC, DO8m_Temperature2_C, DO8m_QAQC, Flag1m_Fin, Flag8m_Fin)

dosonde_edi <- dosonde_edi %>% 
  rename(DateTime = Date,
         Temp_C_1m = DO1m_Temperature2_C,
         DO_mgL_1m = DO1m_QAQC,
         Temp_C_8m = DO8m_Temperature2_C,
         DO_mgL_8m = DO8m_QAQC,
         Flag_1m = Flag1m_Fin,
         Flag_8m = Flag8m_Fin
  )












