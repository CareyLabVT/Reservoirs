library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)

#All files can be found here https://github.com/CareyLabVT/ManualDownloadsSCCData/tree/master/BVR_SensorString

 setwd("~/VT_Carey_Lab/ManualDownloadsSCCData/BVR_SensorString")
 

hobo1 <- list.files(pattern = "*.csv")
hobo <- hobo1[ !grepl("BVR_1.5m*", hobo1) ]#take out the EXO data

read.all <- sapply(hobo, read_csv, skip = 1)



changenames = function(Data)
{
  colnames(Data) <- c("remove", "DateTime", "Temp_C", "remove", "remove", "remove", "remove", "remove")
  return(Data)
}

lapply(read.all, changenames)

renamed <- lapply(read.all, changenames)

renamed$BVR50_10m_hobo65.csv[,c(2,3)]

select_cols = function(Data)
{
  Data[,c(2,3)]
}

cols_selected <- lapply(renamed, select_cols)

cols_selected$BVR_surface_20191115.csv$Depth_m <- 0.1
cols_selected$BVR50_10m_hobo65.csv$Depth_m <- 10
cols_selected$BVR50_1m_hobo66.csv$Depth_m <- 1
cols_selected$BVR50_2m_hobo4.csv$Depth_m <- 2
cols_selected$BVR50_3m_hobo60.csv$Depth_m <- 3
cols_selected$BVR50_4m_hobo63.csv$Depth_m <- 4
cols_selected$BVR50_5m_hobo55.csv$Depth_m <- 5
cols_selected$BVR50_6m_hobo25.csv$Depth_m <- 6
cols_selected$BVR50_7m_hobo23.csv$Depth_m <- 7
cols_selected$BVR50_8m_hobo18.csv$Depth_m <- 8
cols_selected$BVR50_9m_hobo12.csv$Depth_m <- 9
cols_selected$Hobo_BVR_10m_2019.csv$Depth_m <- 9
cols_selected$Hobo_BVR_11m_2019.csv$Depth_m <- 10
cols_selected$Hobo_BVR_1m_2019.csv$Depth_m <- 0.1
cols_selected$hobo_BVR_1m_20191115.csv$Depth_m <- 1
cols_selected$Hobo_BVR_2m_2019.csv$Depth_m <- 1
cols_selected$hobo_BVR_2m_20191115.csv$Depth_m <- 2
cols_selected$Hobo_BVR_3m_2019.csv$Depth_m <- 2
cols_selected$Hobo_BVR_4m_2019.csv$Depth_m <- 3
cols_selected$Hobo_BVR_5m_2019.csv$Depth_m <- 4
cols_selected$Hobo_BVR_6m_2019.csv$Depth_m <- 5
cols_selected$Hobo_BVR_7m_2019.csv$Depth_m <- 6
cols_selected$Hobo_BVR_8m_2019.csv$Depth_m <- 7
cols_selected$Hobo_BVR_9m_2019.csv$Depth_m <- 8
cols_selected$hobo18_BVR_8m_20191115.csv$Depth_m <- 8
cols_selected$hobo23_BVR_7m_20191115.csv$Depth_m <- 7
cols_selected$hobo3_BVR_10m_20191115.csv$Depth_m <- 10
cols_selected$hobo33_BVR_5m_20191115.csv$Depth_m <- 5
cols_selected$hobo36_BVR_9m_20191115.csv$Depth_m <- 9
cols_selected$hobo60_BVR_3m_20191115.csv$Depth_m <- 3
cols_selected$hobo61_BVR_4m_20191115.csv$Depth_m <- 4
cols_selected$hobo62_BVR_6m_20191115.csv$Depth_m <- 6
cols_selected$bvr_1.0_hob20170716o.csv$Depth_m <- 1
cols_selected$bvr_10_hobo20170717.csv$Depth_m <- 10
cols_selected$bvr_1m_hobo46_20200709.csv$Depth_m <- 1
cols_selected$bvr_2m_hobo4possible_20200709.csv$Depth_m <- 2
cols_selected$bvr_3m_hobo60_20200709.csv$Depth_m <- 3
cols_selected$bvr_4_hobo20170716.csv$Depth_m <- 4
cols_selected$bvr_7m_hobo23_20200709.csv$Depth_m <-7


hobo_bound <- bind_rows(cols_selected)

#convert DateTime to YYYY-MM-DD HH:MM:SS
hobo_bound$DateTime <- mdy_hms(hobo_bound$DateTime)

#All hobos started on 2018-12-06 17:00:00
# Hobo string pulled up on 2019-05-16 11:10:00 but not downloaded until 2019-06-18 15:17 
# Redeployed on 2019-06-27 15:10:00
# Hobos pulled up and downloaded on 2019-10-18 13:30:00 and 2019-10-18 14:20:00
# Hobo pulled out of the water on 2019-11-15 15:40:00

hobo_bound=hobo_bound%>%
  drop_na(Temp_C)%>%
  unique()

# Take out different sections and bind back together
hobo1=hobo_bound%>%
  filter(DateTime>"2018-12-06 11:59" & DateTime<"2019-05-16 07:15")

#Round to the nearest 10 minute mark because Hobo 7 was at the 5 minutes and not the 10 minutes
hobo1$DateTime=round_date(hobo1$DateTime, "10 mins")

hobo2=hobo_bound%>%
  filter(DateTime>"2019-06-27 11:09"& DateTime<"2019-10-18 10:10")

hobo3=hobo_bound%>%
  filter(DateTime>"2019-10-18 10:30" & DateTime<"2019-11-15 10:40")

hobo4=hobo_bound%>%
  filter(DateTime>"2016-07-21 7:59" & DateTime<"2017-05-05 8:01")

hobo5=hobo_bound%>%
  filter(DateTime>"2020-05-14 9:25" & DateTime<"2020-07-09 8:51")


hobo_GMT4=rbind(hobo4, hobo2, hobo3, hobo5)

#change to -5 GMT 
hobo_GMT4$DateTime<-with_tz(force_tz(hobo_GMT4$DateTime,"Etc/GMT+4"), "Etc/GMT+5") #pre time chan
hobo_GMT4$DateTime<-as.POSIXct(strptime(hobo_GMT4$DateTime, "%Y-%m-%d %H:%M:%S"), tz = "UTC") #change to UTC to make merging easier

# rebind with all time in EST
hobo_clean=rbind(hobo1, hobo_GMT4)
  
# HOBO Notes
# Hobo 61 and 62 were redeployed at BVR wetland and weren't read out before being deployed so data lost
# And Hobo 9, 33, and 36 were deployed in CCR 


############################################ 2017 DATA #################################################################

setwd("~/VT_Carey_Lab/ManualDownloadsSCCData/BVR_SensorString/BVR_temp_17")

hobo17 <- list.files(pattern = "*.csv")
read.all17 <- sapply(hobo17, read_csv, skip = 1)



changenames = function(Data)
{
  colnames(Data) <- c("remove", "DateTime", "Temp_C", "remove", "remove", "remove", "remove", "remove")
  return(Data)
}


lapply(read.all17, changenames)

renamed17 <- lapply(read.all17, changenames)


select_cols = function(Data)
{
  Data[,c(2,3)]
}

cols_selected17 <- lapply(renamed17, select_cols)



cols_selected17$bvr_0.1.csv$Depth_m <- 0.1
cols_selected17$bvr_1.csv$Depth_m <- 1
cols_selected17$bvr_10.5.csv$Depth_m <- 10.5
cols_selected17$bvr_10.csv$Depth_m <- 10
cols_selected17$bvr_2.csv$Depth_m <- 2
cols_selected17$bvr_3.csv$Depth_m <- 3
cols_selected17$bvr_4.csv$Depth_m <- 4
cols_selected17$bvr_5.csv$Depth_m <- 5
cols_selected17$bvr_6.csv$Depth_m <- 6
cols_selected17$bvr_7.csv$Depth_m <- 7
cols_selected17$bvr_8.csv$Depth_m <- 8
cols_selected17$bvr_9.csv$Depth_m <- 9

hobo_bound_17 <- bind_rows(cols_selected17)

#convert DateTime to YYYY-MM-DD HH:MM:SS
hobo_bound_17$DateTime <- mdy_hms(hobo_bound_17$DateTime)

#Round to the nearest 15 minute mark to average the 5m hobo over those 15 minutes
hobo_bound_17$DateTime=round_date(hobo_bound_17$DateTime, "15 mins")

#change to -5 GMT 
hobo_bound_17$DateTime<-with_tz(force_tz(hobo_bound_17$DateTime,"Etc/GMT+4"), "Etc/GMT+5") #pre time chan
hobo_bound_17$DateTime<-as.POSIXct(strptime(hobo_bound_17$DateTime, "%Y-%m-%d %H:%M:%S"), tz = "UTC") #change to UTC to make merging easier


# 5m set to measure every 10 sec. so averaged over the 10 minutes to get in line with other readings
# because it was recording every 10 sec. it ran out of room 07/07/2017 12:00:00-07/13/2017 12:49:40
# sensors out of the water on 2017-11-07 15:45:00

hobo_na=hobo_bound_17%>%
  drop_na(Depth_m)%>%
  group_by(DateTime, Depth_m)%>%
  summarize(Temp_C=mean(Temp_C))%>%
  filter(DateTime<"2017-11-07 10:45:00")

hobo_na$Temp_C=round(hobo_na$Temp_C, digits = 3)

hobo_df <- rbind(hobo_na,hobo_clean)

hobo_df=hobo_df[order(hobo_df$DateTime),]

str(hobo_df)

# Pivot wider 
hobo_wide=pivot_wider(hobo_df, id_cols = DateTime, names_from=Depth_m, values_from=Temp_C)

#Rename 
names(hobo_wide) <- c("DateTime","HoboTemp_C_1","HoboTemp_C_10","HoboTemp_C_4","HoboTemp_C_0.1","HoboTemp_C_2",
                      "HoboTemp_C_3","HoboTemp_C_5","HoboTemp_C_6","HoboTemp_C_7",
                      "HoboTemp_C_8","HoboTemp_C_9","HoboTemp_C_10.5")

hobo_wide=hobo_wide%>%
  select(c(DateTime, HoboTemp_C_0.1, HoboTemp_C_1,HoboTemp_C_2,HoboTemp_C_3,HoboTemp_C_4,
           HoboTemp_C_5,HoboTemp_C_6,HoboTemp_C_7,HoboTemp_C_8, HoboTemp_C_9,
           HoboTemp_C_10, HoboTemp_C_10.5))



#############################################################################################################
# Add in the Mini Dot

setwd("~/VT_Carey_Lab/ManualDownloadsSCCData/BVR_SensorString")

do1 <- read_csv(file = "BVR_50_DO10m_20181206_20190516.TXT", skip = 6)
do2 <- read_csv(file = "BVR_DO_5m_20181206_20191206.TXT", skip = 6)
do3 <- read_csv(file = "BVR_DO_5m_20191206_20200601.TXT", skip = 6)

do1$Depth_m <- 10
do2$Depth_m <- 5
do3$Depth_m <- 5

disox <- bind_rows(do1, do2, do3)

disox$DateTime_UTC = ymd_hms(disox$`UTC_Date_&_Time`)
disox$DateTime = ymd_hms(disox$`Eastern Standard Time`)

disox=disox%>%
  drop_na(DateTime_UTC)%>%
  distinct(DateTime, .keep_all= TRUE) #taking out the duplicate values 

# MiniDot start on 2018-12-06 13:31:00 and pulled up on 2019-05-16 12:11:00 EST

do101 <- disox%>%
  #filter(disox, Depth_m == 10)%>%
  select(DateTime, Temperature, "Dissolved Oxygen", "Dissolved Oxygen Saturation")%>%
  filter(DateTime>"2018-12-06 8:31" & DateTime<"2019-05-16 8:11")
 

do102 <- disox%>%
  #filter(disox, Depth_m == 10)%>%
  select(DateTime, Temperature, "Dissolved Oxygen", "Dissolved Oxygen Saturation")%>%
  filter(DateTime>"2020-03-16 9:31" & DateTime<"2020-05-14 4:44")
  
do10=rbind(do101, do102)

names(do10) <- c("DateTime", "MiniDotTemp_C_10","MiniDotDO_mgL_10", "MiniDotDOsat_percent_10")

# Redeployed on 2019-06-27 17:51:00 at 5m 
# Moved to 10m on 2020-03-16 13:34
# Ends at 10m on 2020_05-14 11:44
# Back at 5m on 2020-05-14 13:24

do51 <- disox%>%
  #filter(Depth_m == 5)%>%
  filter(DateTime_UTC > "2019-06-27 17:51:00" & DateTime_UTC<"2020-03-16 13:34")%>%
  select(DateTime, Temperature, "Dissolved Oxygen", "Dissolved Oxygen Saturation")
  
do52 <-disox%>%
  filter(DateTime_UTC > "2020-05-14 13:24:00")%>%
  select(DateTime, Temperature, "Dissolved Oxygen", "Dissolved Oxygen Saturation")

do5=rbind(do51, do52)

names(do5) <- c("DateTime", "MiniDotTemp_C_5","MiniDotDO_mgL_5", "MiniDotDOsat_percent_5")

do=merge(do5,do10, by="DateTime", all=T)

do$DateTime=round_date(do$DateTime, "10 mins")

do=do%>% mutate_if(is.character, as.numeric)

#check out the plots
plot(do$DateTime,do$MiniDotDO_mgL_10, type="l")
points(do$DateTime, do$MiniDotDO_mgL_5, type='l', col="red")


#####################################################################################################################
# Read in the EXO

setwd("~/VT_Carey_Lab/ManualDownloadsSCCData/BVR_SensorString")

myfiles <- list.files(pattern = "BVR_1.5m*")

#combine the files
#create an out.file for the combined data
out.file<-""

#for loop to combine the files
for(i in 1:length(myfiles)){
  file <- read.csv(myfiles[i], header=T, skip=17, skipNul = TRUE)
  out.file <- rbind(out.file,file)
}

#clean up the created file
BVR_EXO=out.file%>%
  filter(Site.Name!="")%>%
  select(-c(Site.Name,Time..Fract..Sec.,ODO...local,Sal.psu,Vertical.Position.m,
            TSS.mg.L,Cable.Pwr.V,nLF.Cond.µS.cm))%>%
  unite(., col="DateTime", c(Date..MM.DD.YYYY.,Time..HH.MM.SS.), sep=" ")


#Naming the header to match what is on the data logger
colnames(BVR_EXO)=c("DateTime","EXOChla_RFU_1.5","EXOChla_ugL_1.5","EXOCond_uScm_1.5","EXO_depth_m",
                        "EXOfDOM_RFU_1.5", "EXOfDOM_QSU_1.5",
                        "EXODOsat_percent_1.5","EXODO_mgL_1.5","EXO_pressure_psi",
                        "EXOSpCond_uScm_1.5","EXOBGAPC_RFU_1.5", "EXOBGAPC_ugL_1.5",
                        "EXOTDS_mgL_1.5","EXOTurbidity_FNU_1.5","EXO_wiper_V",
                        "EXOTemp_C_1.5","EXO_battery_V")

BVR_EXO$DateTime <- mdy_hms(BVR_EXO$DateTime)

# not automatically updated to PC time so it seems to be the time zone of the deployment
# I can't tell if the EXO deployment on 2019-12-12 is on EST or EDT- just calling them all in EDT


#change to -5 GMT 
BVR_EXO$DateTime<-with_tz(force_tz(BVR_EXO$DateTime,"Etc/GMT+4"), "Etc/GMT+5") #pre time chan
BVR_EXO$DateTime<-as.POSIXct(strptime(BVR_EXO$DateTime, "%Y-%m-%d %H:%M:%S"), tz = "UTC") #change to UTC to make merging easier


# Take out times when EXO is out of place
BVR_clean=BVR_EXO%>%
  filter(EXO_depth_m>1)

####################################################################################################################

#QAQC using BVR platform script function

# merge the three data sets together

HOBO_DO=merge(hobo_wide, do, by="DateTime", all=T)

BVR_sensor_string=merge(HOBO_DO, BVR_clean, by="DateTime", all=T)

# Make columns numeric
BVR_sensor_string[, c(2:36)] <- sapply(BVR_sensor_string[, c(2:36)], as.numeric)

# Create Flag Columns 

for(j in c(2:36)) { #for loop to create new columns in data frame
  BVR_sensor_string[,paste0("Flag_",colnames(BVR_sensor_string[j]))] <- 0 #creates flag column + name of variable
  BVR_sensor_string[c(which(is.na(BVR_sensor_string[,j]))),paste0("Flag_",colnames(BVR_sensor_string[j]))] <-1 #puts in flag 7 if value not collected
}

# Flag names
# 0=NO flag
# 1=Missing Data
# 2=Changed to NA because it is an outlier. See methods
# 4=Averaged over time step. See methods
# 5=Questionable but kept in the data set


# Add a Flag for the 5m HOBO that was set to every 10 seconds

BVR_sensor_string=BVR_sensor_string%>%
  mutate(Flag_HoboTemp_C_5=ifelse(DateTime>"2017-07-07 6:59:00" & DateTime<"2017-07-13 8:00:00",4,Flag_HoboTemp_C_5))

# Change surface Temp to NA when Hobo out of the water 2019-08-30 10:00:00

BVR_sensor_string=BVR_sensor_string%>%
  mutate(Flag_HoboTemp_C_0.1=ifelse(DateTime>"2019-08-30 6:00:00",2,Flag_HoboTemp_C_0.1),
  HoboTemp_C_0.1=ifelse(DateTime>"2019-08-30 6:00:00",NA,HoboTemp_C_0.1))


#EXO QAQC based on QAQC from FCR and BVR

EXO_FOULING_FACTOR=4

# find chla and phyo on the EXO sonde sensor data that differs from the mean by more than the standard deviation times a given factor, and

Chla_RFU_1.5_mean <- mean(BVR_sensor_string$EXOChla_RFU_1.5, na.rm = TRUE)
Chla_ugL_1.5_mean <- mean(BVR_sensor_string$EXOChla_ugL_1.5, na.rm = TRUE)
BGAPC_RFU_1.5_mean <- mean(BVR_sensor_string$EXOBGAPC_RFU_1.5, na.rm = TRUE)
BGAPC_ugL_1.5_mean <- mean(BVR_sensor_string$EXOBGAPC_ugL_1.5, na.rm = TRUE)
Chla_RFU_1.5_threshold <- EXO_FOULING_FACTOR * sd(BVR_sensor_string$EXOChla_RFU_1.5, na.rm = TRUE)
Chla_ugL_1.5_threshold <- EXO_FOULING_FACTOR * sd(BVR_sensor_string$EXOChla_ugL_1.5, na.rm = TRUE)
BGAPC_RFU_1.5_threshold <- EXO_FOULING_FACTOR * sd(BVR_sensor_string$EXOBGAPC_RFU_1.5, na.rm = TRUE)
BGAPC_ugL_1.5_threshold <- EXO_FOULING_FACTOR * sd(BVR_sensor_string$EXOBGAPC_ugL_1.5, na.rm = TRUE)


# QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
BVR_sensor_string <- BVR_sensor_string %>% 
  mutate(Chla_ugL = lag(EXOChla_ugL_1.5, 0),
         Chla_ugL_lag1 = lag(EXOChla_ugL_1.5, 1),
         Chla_ugL_lead1 = lead(EXOChla_ugL_1.5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOChla_ugL_1.5 = ifelse((abs(Chla_ugL_lag1 - Chla_ugL) > (Chla_ugL_1.5_threshold))  & (abs(Chla_ugL_lead1 - Chla_ugL) > (Chla_ugL_1.5_threshold) & !is.na(Chla_ugL)), 
                                  NA, EXOChla_ugL_1.5)) %>%   
  mutate(Flag_EXOChla_ugL_1.5 = ifelse((abs(Chla_ugL_lag1 - Chla_ugL) > (Chla_ugL_1.5_threshold))  & (abs(Chla_ugL_lead1 - Chla_ugL) > (Chla_ugL_1.5_threshold)) & !is.na(Chla_ugL), 
                                       2, Flag_EXOChla_ugL_1.5)) %>%
  mutate(Flag_EXOChla_ugL_1.5 = ifelse(is.na(Flag_EXOChla_ugL_1.5), 2, Flag_EXOChla_ugL_1.5))%>%
  select(-Chla_ugL, -Chla_ugL_lag1, -Chla_ugL_lead1)

#Chla_RFU QAQC
BVR_sensor_string <- BVR_sensor_string %>% 
  mutate(Chla_RFU = lag(EXOChla_RFU_1.5, 0),
         Chla_RFU_lag1 = lag(EXOChla_RFU_1.5, 1),
         Chla_RFU_lead1 = lead(EXOChla_RFU_1.5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOChla_RFU_1.5 = ifelse((abs(Chla_RFU_lag1 - Chla_RFU) > (Chla_RFU_1.5_threshold))  & (abs(Chla_RFU_lead1 - Chla_RFU) > (Chla_RFU_1.5_threshold) & !is.na(Chla_RFU)), 
                                  NA, EXOChla_RFU_1.5)) %>%   
  mutate(Flag_EXOChla_RFU_1.5 = ifelse((abs(Chla_RFU_lag1 - Chla_RFU) > (Chla_RFU_1.5_threshold))  & (abs(Chla_RFU_lead1 - Chla_RFU) > (Chla_RFU_1.5_threshold)) & !is.na(Chla_RFU), 
                                       2, Flag_EXOChla_RFU_1.5)) %>%
  mutate(Flag_EXOChla_RFU_1.5 = ifelse(is.na(Flag_EXOChla_RFU_1.5), 2, Flag_EXOChla_RFU_1.5))%>%
  select(-Chla_RFU, -Chla_RFU_lag1, -Chla_RFU_lead1)
# QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
BVR_sensor_string <- BVR_sensor_string %>% 
  mutate(phyco_ugL = lag(EXOBGAPC_ugL_1.5, 0),
         phyco_ugL_lag1 = lag(EXOBGAPC_ugL_1.5, 1),
         phyco_ugL_lead1 = lead(EXOBGAPC_ugL_1.5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOBGAPC_ugL_1.5 = ifelse((abs(phyco_ugL_lag1 - phyco_ugL) > (BGAPC_ugL_1.5_threshold))  & (abs(phyco_ugL_lead1 - phyco_ugL) > (BGAPC_ugL_1.5_threshold) & !is.na(phyco_ugL)), 
                                   NA, EXOBGAPC_ugL_1.5)) %>%   
  mutate(Flag_EXOBGAPC_ugL_1.5 = ifelse((abs(phyco_ugL_lag1 - phyco_ugL) > (BGAPC_ugL_1.5_threshold))  & (abs(phyco_ugL_lead1 - phyco_ugL) > (BGAPC_ugL_1.5_threshold) & !is.na(phyco_ugL)), 
                                        2, Flag_EXOBGAPC_ugL_1.5)) %>%
  mutate(Flag_EXOBGAPC_ugL_1.5 = ifelse(is.na(Flag_EXOBGAPC_ugL_1.5),2,Flag_EXOBGAPC_ugL_1.5))%>%
  select(-phyco_ugL, -phyco_ugL_lag1, -phyco_ugL_lead1)

#Phyco QAQC for RFU
BVR_sensor_string <- BVR_sensor_string %>% 
  mutate(phyco_RFU = lag(EXOBGAPC_RFU_1.5, 0),
         phyco_RFU_lag1 = lag(EXOBGAPC_RFU_1.5, 1),
         phyco_RFU_lead1 = lead(EXOBGAPC_RFU_1.5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOBGAPC_RFU_1.5 = ifelse((abs(phyco_RFU_lag1 - phyco_RFU) > (BGAPC_RFU_1.5_threshold))  & (abs(phyco_RFU_lead1 - phyco_RFU) > (BGAPC_RFU_1.5_threshold) & !is.na(phyco_RFU)), 
                                   NA, EXOBGAPC_RFU_1.5)) %>%   
  mutate(Flag_EXOBGAPC_RFU_1.5 = ifelse((abs(phyco_RFU_lag1 - phyco_RFU) > (BGAPC_RFU_1.5_threshold))  & (abs(phyco_RFU_lead1 - phyco_RFU) > (BGAPC_RFU_1.5_threshold) & !is.na(phyco_RFU)), 
                                        2, Flag_EXOBGAPC_RFU_1.5)) %>%
  mutate(Flag_EXOBGAPC_RFU_1.5 = ifelse(is.na(Flag_EXOBGAPC_RFU_1.5),2,Flag_EXOBGAPC_RFU_1.5))%>%
  select(-phyco_RFU, -phyco_RFU_lag1, -phyco_RFU_lead1)



# flag EXO sonde sensor data of value above 4 * standard deviation at other times but leave them in the dataset
BVR_sensor_string <- BVR_sensor_string %>%
  mutate(Flag_EXOBGAPC_RFU_1.5 = ifelse(! is.na(EXOBGAPC_RFU_1.5) & abs(EXOBGAPC_RFU_1.5 - BGAPC_RFU_1.5_mean) > BGAPC_RFU_1.5_threshold,
                                        5, Flag_EXOBGAPC_RFU_1.5)) %>%
  mutate(Flag_EXOBGAPC_ugL_1.5 = ifelse( ! is.na(EXOBGAPC_ugL_1.5) & abs(EXOBGAPC_ugL_1.5 - BGAPC_ugL_1.5_mean) > BGAPC_ugL_1.5_threshold,
                                         5, Flag_EXOBGAPC_ugL_1.5)) %>%
  mutate(Flag_EXOChla_RFU_1.5 = ifelse(! is.na(EXOChla_RFU_1.5) & abs(EXOChla_RFU_1.5 - Chla_RFU_1.5_mean) > Chla_RFU_1.5_threshold,
                                       5, Flag_EXOChla_RFU_1.5)) %>%
  mutate(Flag_EXOChla_ugL_1.5 = ifelse(! is.na(EXOChla_ugL_1.5) & abs(EXOChla_ugL_1.5 - Chla_ugL_1.5_mean) > Chla_ugL_1.5_threshold,
                                       5, Flag_EXOChla_ugL_1.5)) 



####################################################################################################################################  
# fdom qaqc----
# QAQC from DWH to remove major outliers from fDOM data that are 2 sd's greater than the previous and following datapoint
sd_fDOM_QSU <- sd(BVR_sensor_string$EXOfDOM_QSU_1.5, na.rm = TRUE) #deteriming the standard deviation of fDOM data 
sd_fDOM_RFU <- sd(BVR_sensor_string$EXOfDOM_RFU_1.5, na.rm = TRUE)
mean_fDOM_QSU <- mean(BVR_sensor_string$EXOfDOM_QSU_1.5, na.rm = TRUE) #deteriming the standard deviation of fDOM data 
mean_fDOM_RFU <- mean(BVR_sensor_string$EXOfDOM_RFU_1.5, na.rm = TRUE)

#fDOM QSU QAQC
BVR_sensor_string <- BVR_sensor_string%>% 
  mutate(fDOM_QSU = lag(EXOfDOM_QSU_1.5, 0),
         fDOM_QSU_lag1 = lag(EXOfDOM_QSU_1.5, 1),
         fDOM_QSU_lead1 = lead(EXOfDOM_QSU_1.5, 1)) %>%  #These mutates create columns for current fDOM_QSU, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOfDOM_QSU_1.5 = ifelse(
    ( abs(fDOM_QSU_lag1 - fDOM_QSU) > (2*sd_fDOM_QSU)   )  & ( abs(fDOM_QSU_lead1 - fDOM_QSU) > (2*sd_fDOM_QSU)  & !is.na(fDOM_QSU) ), NA, EXOfDOM_QSU_1.5
  )) %>%  #QAQC to remove outliers for QSU fDOM data 
  mutate(Flag_EXOfDOM_QSU_1.5 = ifelse(
    ( abs(fDOM_QSU_lag1 - fDOM_QSU) > (2*sd_fDOM_QSU)   )  & ( abs(fDOM_QSU_lead1 - fDOM_QSU) > (2*sd_fDOM_QSU)  & !is.na(fDOM_QSU)  ), 2, Flag_EXOfDOM_QSU_1.5
  ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
  mutate(Flag_EXOfDOM_QSU_1.5 = ifelse(is.na(Flag_EXOfDOM_QSU_1.5),2,Flag_EXOfDOM_QSU_1.5))%>%
  select(-fDOM_QSU, -fDOM_QSU_lag1, -fDOM_QSU_lead1)#This removes the columns used to run ifelse statements since they are no longer needed.


##################fDOM QSU QAQC
BVR_sensor_string <- BVR_sensor_string%>% 
  mutate(fDOM_RFU = lag(EXOfDOM_RFU_1.5, 0),
         fDOM_RFU_lag1 = lag(EXOfDOM_RFU_1.5, 1),
         fDOM_RFU_lead1 = lead(EXOfDOM_RFU_1.5, 1)) %>%  #These mutates create columns for current fDOM_RFU, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOfDOM_RFU_1.5 = ifelse(
    ( abs(fDOM_RFU_lag1 - fDOM_RFU) > (2*sd_fDOM_RFU)   )  & ( abs(fDOM_RFU_lead1 - fDOM_RFU) > (2*sd_fDOM_RFU)  & !is.na(fDOM_RFU) ), NA, EXOfDOM_RFU_1.5
  )) %>%  #QAQC to remove outliers for RFU fDOM data 
  mutate(Flag_EXOfDOM_RFU_1.5 = ifelse(
    ( abs(fDOM_RFU_lag1 - fDOM_RFU) > (2*sd_fDOM_RFU)   )  & ( abs(fDOM_RFU_lead1 - fDOM_RFU) > (2*sd_fDOM_RFU)  & !is.na(fDOM_RFU)  ), 2, Flag_EXOfDOM_RFU_1.5
  ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
  mutate(Flag_EXOfDOM_RFU_1.5 = ifelse(is.na(Flag_EXOfDOM_RFU_1.5),2,Flag_EXOfDOM_RFU_1.5))%>%
  select(-fDOM_RFU, -fDOM_RFU_lag1, -fDOM_RFU_lead1)#This removes the columns used to run ifelse statements since they are no longer needed.

# flag EXO sonde sensor data of value above 4 * standard deviation at other times but leave them in the dataset. Probably won't flag anything but for consistenacy. 
BVR_sensor_string <- BVR_sensor_string %>%
  mutate(Flag_EXOfDOM_RFU_1.5 = ifelse(! is.na(EXOfDOM_RFU_1.5) & abs(EXOfDOM_RFU_1.5 - mean_fDOM_RFU) > (4*sd_fDOM_RFU),
                                       5, Flag_EXOfDOM_RFU_1.5)) %>%
  mutate(Flag_EXOfDOM_QSU_1.5 = ifelse( ! is.na(EXOfDOM_QSU_1.5) & abs(EXOfDOM_QSU_1.5 - mean_fDOM_QSU) > (4*sd_fDOM_QSU),
                                        5, Flag_EXOfDOM_QSU_1.5)) 


###QAQC from DWH to remove major outliers from conductity, specific conductivity and TDS data that are 2 sd's greater than the previous and following datapoint


sd_cond <- sd(BVR_sensor_string$EXOCond_uScm_1.5, na.rm = TRUE)
sd_spcond <-sd(BVR_sensor_string$EXOSpCond_uScm_1.5, na.rm = TRUE)
sd_TDS <- sd(BVR_sensor_string$EXOTDS_mgL_1.5, na.rm = TRUE)
mean_cond <- mean(BVR_sensor_string$EXOCond_uScm_1.5, na.rm = TRUE)
mean_spcond <-mean(BVR_sensor_string$EXOSpCond_uScm_1.5, na.rm = TRUE)
mean_TDS <- mean(BVR_sensor_string$EXOTDS_mgL_1.5, na.rm = TRUE)


######### QAQC on major conductivity outliers using DWH's method: datapoint set to NA if data is greater than 2*sd different from both previous and following datapoint

BVR_sensor_string <- BVR_sensor_string %>% 
  mutate(Cond = lag(EXOCond_uScm_1.5, 0),
         Cond_lag1 = lag(EXOCond_uScm_1.5, 1),
         Cond_lead1 = lead(EXOCond_uScm_1.5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOCond_uScm_1.5 = ifelse((abs(Cond_lag1 - Cond) > (2*sd_cond))  & (abs(Cond_lead1 - Cond) > (2*sd_cond) & !is.na(Cond)), 
                                   NA, EXOCond_uScm_1.5)) %>%   
  mutate(Flag_EXOCond_uScm_1.5 = ifelse((abs(Cond_lag1 - Cond) > (2*sd_cond))  & (abs(Cond_lead1 - Cond) > (2*sd_cond) & !is.na(Cond)), 
                                        2, Flag_EXOCond_uScm_1.5)) %>%
  mutate(Flag_EXOCond_uScm_1.5 = ifelse(is.na(Flag_EXOCond_uScm_1.5),2,Flag_EXOCond_uScm_1.5))%>%
  select(-Cond, -Cond_lag1, -Cond_lead1)

###### QAQC on major Specific conductivity outliers using DWH's method: datapoint set to NA if data is greater than 2*sd different from both previous and following datapoint

BVR_sensor_string <- BVR_sensor_string %>% 
  mutate(SpCond = lag(EXOSpCond_uScm_1.5, 0),
         SpCond_lag1 = lag(EXOSpCond_uScm_1.5, 1),
         SpCond_lead1 = lead(EXOSpCond_uScm_1.5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOSpCond_uScm_1.5 = ifelse((abs(SpCond_lag1 - SpCond) > (2*sd_spcond))  & (abs(SpCond_lead1 - SpCond) > (2*sd_spcond) & !is.na(SpCond)), 
                                     NA, EXOSpCond_uScm_1.5)) %>%   
  mutate(Flag_EXOSpCond_uScm_1.5 = ifelse((abs(SpCond_lag1 - SpCond) > (2*sd_spcond))  & (abs(SpCond_lead1 - SpCond) > (2*sd_spcond)) & !is.na(SpCond), 
                                          2, Flag_EXOSpCond_uScm_1.5)) %>% 
  mutate(Flag_EXOSpCond_uScm_1.5 = ifelse(is.na(Flag_EXOSpCond_uScm_1.5),2,Flag_EXOSpCond_uScm_1.5))%>%
  select(-SpCond, -SpCond_lag1, -SpCond_lead1)

################### QAQC on major TDS outliers using DWH's method: datapoint set to NA if data is greater than 2*sd different from both previous and following datapoint

BVR_sensor_string <- BVR_sensor_string %>% 
  mutate(TDS = lag(EXOTDS_mgL_1.5, 0),
         TDS_lag1 = lag(EXOTDS_mgL_1.5, 1),
         TDS_lead1 = lead(EXOTDS_mgL_1.5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOTDS_mgL_1.5 = ifelse((abs(TDS_lag1 - TDS) > (2*sd_TDS))  & (abs(TDS_lead1 - TDS) > (2*sd_TDS) & !is.na(TDS)), 
                                 NA, EXOTDS_mgL_1.5)) %>%   
  mutate(Flag_EXOTDS_mgL_1.5 = ifelse((abs(TDS_lag1 - TDS) > (2*sd_TDS))  & (abs(TDS_lead1 - TDS) > (2*sd_TDS)) & !is.na(TDS), 
                                      2, Flag_EXOTDS_mgL_1.5)) %>% 
  mutate(Flag_EXOTDS_mgL_1.5 = ifelse(is.na(Flag_EXOTDS_mgL_1.5),2,Flag_EXOTDS_mgL_1.5))%>%
  select(-TDS, -TDS_lag1, -TDS_lead1)

# flag EXO sonde sensor data of value above 4 * standard deviation at other times but leave them in the dataset. Probably won't flag anything but for consistenacy. 
BVR_sensor_string <- BVR_sensor_string %>%
  mutate(Flag_EXOCond_uScm_1.5 = ifelse(! is.na(EXOCond_uScm_1.5) & abs(EXOCond_uScm_1.5 - mean_cond) > (4*sd_cond),
                                        5, Flag_EXOCond_uScm_1.5)) %>%
  mutate(Flag_EXOSpCond_uScm_1.5 = ifelse( ! is.na(EXOSpCond_uScm_1.5) & abs(EXOSpCond_uScm_1.5 - mean_spcond) > (4*sd_spcond),
                                           5, Flag_EXOSpCond_uScm_1.5)) %>%
  mutate(Flag_EXOTDS_mgL_1.5 = ifelse( ! is.na(EXOTDS_mgL_1.5) & abs(EXOTDS_mgL_1.5 - mean_TDS) > (4*sd_TDS),
                                       5, Flag_EXOTDS_mgL_1.5)) 

###################QAQC for Turbidity 

sd_Turbidity_FNU <- sd(BVR_sensor_string$EXOTurbidity_FNU_1.5, na.rm = TRUE) #deteriming the standard deviation of fDOM data 
mean_Turbidity_FNU <- mean(BVR_sensor_string$EXOTurbidity_FNU_1.5, na.rm = TRUE) #deteriming the standard deviation of fDOM data 

#Turbidity FNU QAQC
BVR_sensor_string <- BVR_sensor_string%>% 
  mutate(Turbidity_FNU = lag(EXOTurbidity_FNU_1.5, 0),
         Turbidity_FNU_lag1 = lag(EXOTurbidity_FNU_1.5, 1),
         Turbidity_FNU_lead1 = lead(EXOTurbidity_FNU_1.5, 1)) %>%  #These mutates create columns for current Turbidity_FNU, fDOM before and fDOM after. These are used to run ifelse QAQC loops
  mutate(EXOTurbidity_FNU_1.5 = ifelse(
    ( abs(Turbidity_FNU_lag1 - Turbidity_FNU) > (2*sd_Turbidity_FNU)   )  & ( abs(Turbidity_FNU_lead1 - Turbidity_FNU) > (2*sd_Turbidity_FNU)  & !is.na(Turbidity_FNU) ), NA, EXOTurbidity_FNU_1.5
  )) %>%  #QAQC to remove outliers for QSU fDOM data 
  mutate(Flag_EXOTurbidity_FNU_1.5 = ifelse(
    ( abs(Turbidity_FNU_lag1 - Turbidity_FNU) > (2*sd_Turbidity_FNU)   )  & ( abs(Turbidity_FNU_lead1 - Turbidity_FNU) > (2*sd_Turbidity_FNU)  & !is.na(Turbidity_FNU)  ), 2, Flag_EXOTurbidity_FNU_1.5
  ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
  mutate(Flag_EXOTurbidity_FNU_1.5 = ifelse(is.na(Flag_EXOTurbidity_FNU_1.5),2,Flag_EXOTurbidity_FNU_1.5))%>%
  select(-Turbidity_FNU, -Turbidity_FNU_lag1, -Turbidity_FNU_lead1)#This removes the columns used to run ifelse statements since they are no longer needed.

# flag EXO sonde sensor data of value above 4 * standard deviation at other times but leave them in the data set. Probably won't flag anything but for consistency. 
BVR_sensor_string <- BVR_sensor_string %>%
  mutate(Flag_EXOTurbidity_FNU_1.5 = ifelse( ! is.na(EXOTurbidity_FNU_1.5) & abs(EXOTurbidity_FNU_1.5 - mean_Turbidity_FNU) > (4*sd_Turbidity_FNU),
                                             5, Flag_EXOTurbidity_FNU_1.5)) 

####################################################################################################################################################################
# QAQC Plots

# Visually inspect the plots to see if any maintenance points were missing or other points that need to be QAQCd

# check the graph of Thermistor at surface 
Temp_0.1 <- ggplot(data = BVR_sensor_string, aes(x = DateTime, y = HoboTemp_C_0.1)) +
  geom_point()
Temp_0.1
#ggplotly(Temp_0.1)


# check the graph of Thermistor at postion 1 
Temp_1 <- ggplot(data = BVR_sensor_string, aes(x = DateTime, y = HoboTemp_C_1)) +
  geom_point()
Temp_1
#ggplotly(Temp_1)

# plot surface and 1

plot(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_0.1, type='l', col="red")
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_1, type='l')

Temp1and0.1=BVR_sensor_string%>%
  filter(DateTime> "2019-06-20 10:00" & DateTime<"2020-07-15  12:00")%>%
  ggplot(.,aes(x=DateTime))+
  geom_point(aes(y=HoboTemp_C_0.1), col="red") +
  geom_point(aes(y=HoboTemp_C_1)) +
  geom_point(aes(y=EXOTemp_C_1.5), col="green")

Temp1and0.1

# check the graph of Hobo at postion 2
Temp_2 <- ggplot(data = BVR_sensor_string, aes(x = DateTime, y = HoboTemp_C_2)) +
  geom_point()
Temp_2
ggplotly(Temp_2)


# check the graph of Hobo at postion 3
# check Hobo 3 temp data
Temp_3 <- ggplot(data = BVR_sensor_string, aes(x = DateTime, y = HoboTemp_C_3)) +
  geom_point()
Temp_3
#ggplotly(Temp_3)


#check the EXO temp which is deployed at 1_5m
Exo_temp <- ggplot(data = BVR_sensor_string, aes(x = DateTime, y = EXOTemp_C_1.5)) +
  geom_point()
Exo_temp
#ggplotly(Exo_temp)

# check the graph of Hobo at postion 4
Temp_4 <- ggplot(data = BVR_sensor_string, aes(x = DateTime, y = HoboTemp_C_4)) +
  geom_point()
Temp_4
#ggplotly(Temp_4)


# check the graph of Hobo at postion 5
Temp_5 <- BVR_sensor_string%>%
  #filter(DateTime>"2018-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = HoboTemp_C_5)) +
  geom_point()
Temp_5
#ggplotly(Temp_5)

#Check the temperature from the MiniDot at 5m
MiniDot_5<- BVR_sensor_string%>%
  filter(DateTime>"2018-05-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = MiniDotTemp_C_5)) +
  geom_point()
MiniDot_5
ggplotly(MiniDot_5)

Temp5andMini=BVR_sensor_string%>%
  filter(DateTime>"2018-10-01 00:00")%>%
  ggplot(.,aes(x=DateTime))+
  geom_point(aes(y=HoboTemp_C_5), col="red") +
  geom_point(aes(y=MiniDotTemp_C_5)) +
  #geom_point(aes(y=MiniDotTemp_C_10), col="gray") +
  geom_point(aes(y=HoboTemp_C_4), col="blue")

Temp5andMini

# check the graph of Hobo at postion 6
Temp_6 <- ggplot(data = BVR_sensor_string, aes(x = DateTime, y = HoboTemp_C_6)) +
  geom_point()
Temp_6
ggplotly(Temp_6)

# check the graph of Hobo at postion 7
Temp_7 <- ggplot(data = BVR_sensor_string, aes(x = DateTime, y = HoboTemp_C_7)) +
  geom_point()
Temp_7
#ggplotly(Temp_7)


# check the graph of Hobo at postion 8
Temp_8 <- ggplot(data = BVR_sensor_string, aes(x = DateTime, y = HoboTemp_C_8)) +
  geom_point()
Temp_8
#ggplotly(Temp_8)

# check the graph of Hobo at postion 9
Temp_9 <- ggplot(data = BVR_sensor_string, aes(x = DateTime, y = HoboTemp_C_9)) +
  geom_point()
Temp_9
#ggplotly(Temp_9)

# check the graph of Hobo at postion 10
Temp_10 <- ggplot(data = BVR_sensor_string, aes(x = DateTime, y = HoboTemp_C_10)) +
  geom_point()
Temp_10
ggplotly(Temp_10)

# check the graph of MiniDot at 10m
MiniDot_10 <- ggplot(data = BVR_sensor_string, aes(x = DateTime, y = MiniDotTemp_C_10)) +
  geom_point()
MiniDot_10
#ggplotly(Temp_10)

Temp10andMini=BVR_sensor_string%>%
  filter(DateTime>"2018-10-01 00:00")%>%
  ggplot(.,aes(x=DateTime))+
  geom_point(aes(y=HoboTemp_C_10), col="red") +
  geom_point(aes(y=MiniDotTemp_C_10)) 
#geom_point(aes(y=HoboTemp_C_4), col="blue")

Temp10andMini

# check the graph of Hobo at 10.5m
Temp_10.5 <- ggplot(data = BVR_sensor_string, aes(x = DateTime, y = HoboTemp_C_10.5)) +
  geom_point()
Temp_10.5
#ggplotly(Temp_10.5)

###Put all the points on the same plot using base R
plot(BVR_sensor_string$DateTime,BVR_sensor_string$HoboTemp_C_0.1, main="Water Temp", xlab="Time", ylab="degrees C", type='l', col="black", lwd=1.5, ylim=c(0,40))
points(BVR_sensor_string$DateTime,BVR_sensor_string$HoboTemp_C_1, col="firebrick4", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_2, col="firebrick1", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_3, col="DarkOrange1", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_4, col="gold", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_5, col="greenyellow", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_6, col="medium sea green", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_7, col="sea green", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_8, col="DeepSkyBlue4", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_9, col="blue2", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_10, col="blue4", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_10.5, col="darkslateblue", type='l', lwd=1.5)


Twenty17=BVR_sensor_string%>%
  filter(DateTime>"2017-05-20 00:00" & DateTime<"2017-12-31 23:59")
###Put all the points on the same plot using base R
plot(Twenty17$DateTime,Twenty17$HoboTemp_C_0.1, main="Hobo Water Temp 2017", xlab="Time", ylab="degrees C", type='l', col="black", lwd=1.5, ylim=c(0,40))
points(Twenty17$DateTime,Twenty17$HoboTemp_C_1, col="firebrick4", type='l', lwd=1.5)
points(Twenty17$DateTime, Twenty17$HoboTemp_C_2, col="firebrick1", type='l', lwd=1.5)
points(Twenty17$DateTime, Twenty17$HoboTemp_C_3, col="DarkOrange1", type='l', lwd=1.5)
points(Twenty17$DateTime, Twenty17$HoboTemp_C_4, col="gold", type='l', lwd=1.5)
points(Twenty17$DateTime, Twenty17$HoboTemp_C_5, col="greenyellow", type='l', lwd=1.5)
points(Twenty17$DateTime, Twenty17$HoboTemp_C_6, col="medium sea green", type='l', lwd=1.5)
points(Twenty17$DateTime, Twenty17$HoboTemp_C_7, col="sea green", type='l', lwd=1.5)
points(Twenty17$DateTime, Twenty17$HoboTemp_C_8, col="DeepSkyBlue4", type='l', lwd=1.5)
points(Twenty17$DateTime, Twenty17$HoboTemp_C_9, col="blue2", type='l', lwd=1.5)
points(Twenty17$DateTime, Twenty17$HoboTemp_C_10, col="blue4", type='l', lwd=1.5)
points(Twenty17$DateTime, Twenty17$HoboTemp_C_10.5, col="darkslateblue", type='l', lwd=1.5)

###Put all the points on the same plot using base R
plot(BVR_sensor_string$DateTime,BVR_sensor_string$HoboTemp_C_0.1, main="Water Temp", xlab="Time", ylab="degrees C", type='l', col="black", lwd=1.5, ylim=c(0,40))
points(BVR_sensor_string$DateTime,BVR_sensor_string$HoboTemp_C_1, col="firebrick4", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_2, col="firebrick1", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_3, col="DarkOrange1", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_4, col="gold", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_5, col="greenyellow", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_6, col="medium sea green", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_7, col="sea green", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_8, col="DeepSkyBlue4", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_9, col="blue2", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_10, col="blue4", type='l', lwd=1.5)
points(BVR_sensor_string$DateTime, BVR_sensor_string$HoboTemp_C_10.5, col="darkslateblue", type='l', lwd=1.5)


Twenty19=BVR_sensor_string%>%
  filter(DateTime>"2019-01-01 00:00" & DateTime<"2019-12-31 23:59")
###Put all the points on the same plot using base R
plot(Twenty19$DateTime,Twenty19$HoboTemp_C_0.1, main="Hobo Water Temp 2019", xlab="Time", ylab="degrees C", type='l', col="black", lwd=1.5, ylim=c(0,40))
points(Twenty19$DateTime,Twenty19$HoboTemp_C_1, col="firebrick4", type='l', lwd=1.5)
points(Twenty19$DateTime, Twenty19$HoboTemp_C_2, col="firebrick1", type='l', lwd=1.5)
points(Twenty19$DateTime, Twenty19$HoboTemp_C_3, col="DarkOrange1", type='l', lwd=1.5)
points(Twenty19$DateTime, Twenty19$HoboTemp_C_4, col="gold", type='l', lwd=1.5)
points(Twenty19$DateTime, Twenty19$HoboTemp_C_5, col="greenyellow", type='l', lwd=1.5)
points(Twenty19$DateTime, Twenty19$HoboTemp_C_6, col="medium sea green", type='l', lwd=1.5)
points(Twenty19$DateTime, Twenty19$HoboTemp_C_7, col="sea green", type='l', lwd=1.5)
points(Twenty19$DateTime, Twenty19$HoboTemp_C_8, col="DeepSkyBlue4", type='l', lwd=1.5)
points(Twenty19$DateTime, Twenty19$HoboTemp_C_9, col="blue2", type='l', lwd=1.5)
points(Twenty19$DateTime, Twenty19$HoboTemp_C_10, col="blue4", type='l', lwd=1.5)
points(Twenty19$DateTime, Twenty19$HoboTemp_C_10.5, col="darkslateblue", type='l', lwd=1.5)
#points(Twenty19$DateTime, Twenty19$MiniDotTemp_C_5, col="black", type='l', lwd=1.5)
#points(Twenty19$DateTime, Twenty19$MiniDotTemp_C_10, col="black", type='l', lwd=1.5)


Twenty20=BVR_sensor_string%>%
  filter(DateTime>"2020-01-01 00:00" & DateTime<"2020-07-20 23:59")
###Put all the points on the same plot using base R
plot(Twenty20$DateTime,Twenty20$HoboTemp_C_0.1, main="Hobo Water Temp 2020", xlab="Time", ylab="degrees C", type='l', col="black", lwd=1.5, ylim=c(0,40))
points(Twenty20$DateTime,Twenty20$HoboTemp_C_1, col="firebrick4", type='l', lwd=1.5)
points(Twenty20$DateTime, Twenty20$HoboTemp_C_2, col="firebrick1", type='l', lwd=1.5)
points(Twenty20$DateTime, Twenty20$HoboTemp_C_3, col="DarkOrange1", type='l', lwd=1.5)
points(Twenty20$DateTime, Twenty20$HoboTemp_C_4, col="gold", type='l', lwd=1.5)
points(Twenty20$DateTime, Twenty20$HoboTemp_C_5, col="greenyellow", type='l', lwd=1.5)
points(Twenty20$DateTime, Twenty20$HoboTemp_C_6, col="medium sea green", type='l', lwd=1.5)
points(Twenty20$DateTime, Twenty20$HoboTemp_C_7, col="sea green", type='l', lwd=1.5)
points(Twenty20$DateTime, Twenty20$HoboTemp_C_8, col="DeepSkyBlue4", type='l', lwd=1.5)
points(Twenty20$DateTime, Twenty20$HoboTemp_C_9, col="blue2", type='l', lwd=1.5)
points(Twenty20$DateTime, Twenty20$HoboTemp_C_10, col="blue4", type='l', lwd=1.5)
points(Twenty20$DateTime, Twenty20$HoboTemp_C_10.5, col="darkslateblue", type='l', lwd=1.5)
points(Twenty20$DateTime, Twenty20$MiniDotTemp_C_5, col="black", type='l', lwd=1.5)
points(Twenty20$DateTime, Twenty20$MiniDotTemp_C_10, col="grey", type='l', lwd=1.5)

#########MiniDot DO at 5m and 10m

#Check the obs from the MiniDot at 5m
MiniDotobs_5<- BVR_sensor_string%>%
  filter(DateTime>"2019-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = MiniDotDO_mgL_5)) +
  geom_point()
MiniDotobs_5
#ggplotly(MiniDotobs_5)

#Check the sat from the MiniDot at 5m
MiniDotsat_5<- BVR_sensor_string%>%
  filter(DateTime>"2019-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = MiniDotDOsat_percent_5)) +
  geom_point()
MiniDotsat_5
#ggplotly(MiniDotsat_5)

#Check the obs from the MiniDot at 10m
MiniDotobs_10<- BVR_sensor_string%>%
  filter(DateTime>"2018-03-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = MiniDotDO_mgL_10)) +
  geom_point()
MiniDotobs_10
#ggplotly(MiniDotobs_10)

#Check the sat from the MiniDot at 5m
MiniDotsat_10<- BVR_sensor_string%>%
  filter(DateTime>"2018-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = MiniDotDOsat_percent_10)) +
  geom_point()
MiniDotsat_10
#ggplotly(MiniDotsat_10)

#Do from the EXO
EXODO_1.5 <- BVR_sensor_string%>%
  filter(DateTime>"2019-06-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXODO_mgL_1.5)) +
  geom_point()
EXODO_1.5
ggplotly(EXODO_1.5)


EXODOsat_1.5 <- BVR_sensor_string%>%
  filter(DateTime>"2019-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXODOsat_percent_1.5)) +
  geom_point()
EXODOsat_1.5
#ggplotly(EXODOsat_1.5)


# EXO plots

# chl and phyco qaqc ----

#plot the chal from the EXO
chl_rfu <- BVR_sensor_string%>%
  filter(DateTime>"2019-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXOChla_RFU_1.5)) +
  geom_point()
chl_rfu
#ggplotly(chl_rfu)


#plot the chal from the EXO
chl_ugL <- BVR_sensor_string%>%
  filter(DateTime>"2019-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXOChla_ugL_1.5)) +
  geom_point()
chl_ugL
#ggplotly(chl_ugL)


chl_mean <- BVR_sensor_string %>%
  filter(DateTime>"2019-10-01 00:00")%>%
  select(DateTime, EXOChla_ugL_1.5) %>%
  mutate(day = date(DateTime)) %>%
  group_by(day) %>%
  mutate(daily_mean = mean(EXOChla_ugL_1.5, na.rm = TRUE)) %>%
  distinct(day, .keep_all = TRUE)
# 
# # plot the daily mean
chl_mean_plot <- ggplot(data = chl_mean, aes(x = day, y = daily_mean)) +
  geom_point()
chl_mean_plot
# # Plotly so can pick out questionable values
# ggplotly(chl_mean)
# 
# # Plot the chla and the daily mean on the same graph
plot(BVR_sensor_string$DateTime, BVR_sensor_string$EXOChla_ugL_1.5)
points(chl_mean$DateTime, chl_mean$daily_mean, type="l", col="green")


#Checking out the phycos and flagging points, EXOBGAPC_RFU_1.5, EXOBGAPC_ugL_1.5  

#plot the phyco
phyco_ugl <- BVR_sensor_string%>%
  filter(DateTime>"2019-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXOBGAPC_ugL_1.5)) +
  geom_point()
phyco_ugl
#ggplotly(phyco_ugl)


phyco_RFU <- BVR_sensor_string%>%
  filter(DateTime>"2019-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXOBGAPC_RFU_1.5)) +
  geom_point()
phyco_RFU
#ggplotly(phyco_RFU)


#### fdom qaqc

fDOM_RFU <- BVR_sensor_string%>%
  filter(DateTime>"2019-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXOfDOM_RFU_1.5)) +
  geom_point()
fDOM_RFU
#ggplotly(fDOM_RFU)

fDOM_QSU <- BVR_sensor_string%>%
  filter(DateTime>"2019-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXOfDOM_QSU_1.5)) +
  geom_point()
fDOM_QSU
#ggplotly(fDOM_QSU)


###check the conductivity values and the specific conductivity

#plot the conductivity
cond <- BVR_sensor_string%>%
  filter(DateTime>"2019-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXOCond_uScm_1.5)) +
  geom_point()
cond
#ggplotly(cond)


#plot the specific conductivity 
cond_sp <- BVR_sensor_string%>%
  filter(DateTime>"2019-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXOSpCond_uScm_1.5)) +
  geom_point()
cond_sp
#ggplotly(cond_sp)

#plot the total dissolved solids
tds <- BVR_sensor_string%>%
  filter(DateTime>"2019-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXOTDS_mgL_1.5)) +
  geom_point()
tds
#ggplotly(tds)


#turbidity
turb <- BVR_sensor_string%>%
  filter(DateTime>"2019-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXOTurbidity_FNU_1.5)) +
  geom_point()
turb
#ggplotly(turb)


#Pressure graphs
#graph the EXO pressure
Exo_pressure <- BVR_sensor_string%>%
  filter(DateTime>"2019-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXO_pressure_psi)) +
  geom_point()
Exo_pressure
#ggplotly(Exo_pressure)

#graph the EXO depth which is based on pressure so should be the same as above
Exo_depth <- BVR_sensor_string%>%
  filter(DateTime>"2019-10-01 00:00")%>%
  ggplot(., aes(x = DateTime, y = EXO_depth_m)) +
  geom_point()
Exo_depth
ggplotly(Exo_depth)

####################################################################################################################################

### Take out outliers based on the QAQC plots

# Remember that all times are in UTC 

# Flag names
# 0=NO flag
# 1=Missing Data
# 2=Changed to NA because it is an outlier or during maintenance. See methods
# 3=Negative value changed to 0 excpet temperature
# 4=Averaged over time step. See methods
# 5=Questionable but kept in the data set

BVR_sensor_string=BVR_sensor_string%>%
  mutate(
    Flag_HoboTemp_C_2=ifelse(DateTime>"2017-07-18 12:10" & DateTime<"2017-07-18 12:31", 2, Flag_HoboTemp_C_2),
    HoboTemp_C_2=ifelse(DateTime>"2017-07-18 12:10" & DateTime<"2017-07-18 12:31", NA, HoboTemp_C_2),
    Flag_HoboTemp_C_2=ifelse(DateTime=="2017-07-18 13:15", 2, Flag_HoboTemp_C_2),
    HoboTemp_C_2=ifelse(DateTime=="2017-07-18 13:15", NA, HoboTemp_C_2),
    Flag_HoboTemp_C_2=ifelse(DateTime>"2017-09-19 7:44" & DateTime<"2017-09-19 8:50", 2, Flag_HoboTemp_C_2),
    HoboTemp_C_2=ifelse(DateTime>"2017-09-19 7:44" & DateTime<"2017-09-19 8:50", NA, HoboTemp_C_2),
    Flag_HoboTemp_C_2=ifelse(DateTime>"2020-06-29 9:35" & DateTime<"2020-06-29 10:22", 2, Flag_HoboTemp_C_2),
    HoboTemp_C_2=ifelse(DateTime>"2020-06-29 9:35" & DateTime<"2020-06-29 10:22", NA, HoboTemp_C_2)
  )

#For 5m HOBO
BVR_sensor_string=BVR_sensor_string%>%
  mutate(
    Flag_HoboTemp_C_5=ifelse(DateTime>"2019-05-14 2:49" & DateTime<"2019-05-14 5:31", 2, Flag_HoboTemp_C_5),
    HoboTemp_C_5=ifelse(DateTime>"2019-05-14 2:49" & DateTime<"2019-05-14 5:31", NA, HoboTemp_C_5),
    Flag_HoboTemp_C_5=ifelse(DateTime>"2019-07-31 12:19" & DateTime<"2019-07-31 12:41", 2, Flag_HoboTemp_C_5),
    HoboTemp_C_5=ifelse(DateTime>"2019-07-31 12:19" & DateTime<"2019-07-31 12:41", NA, HoboTemp_C_5),
    Flag_HoboTemp_C_5=ifelse(DateTime>"2019-07-31 13:09" & DateTime<"2019-07-31 13:41", 2, Flag_HoboTemp_C_5),
    HoboTemp_C_5=ifelse(DateTime>"2019-07-31 13:09" & DateTime<"2019-07-31 13:41", NA, HoboTemp_C_5)
  )

#For MiniDot at 5m
BVR_sensor_string=BVR_sensor_string%>%
  mutate(
    Flag_MiniDotTemp_C_5=ifelse(DateTime>"2019-10-18 10:19" & DateTime<"2019-10-18 10:31", 2, Flag_MiniDotTemp_C_5),
    MiniDotTemp_C_5=ifelse(DateTime>"2019-10-18 10:19" & DateTime<"2019-10-18 10:31", NA, MiniDotTemp_C_5),
    Flag_MiniDotTemp_C_5=ifelse(DateTime>"2019-11-15 9:49" & DateTime<"2019-11-15 10:21", 2, Flag_MiniDotTemp_C_5),
    MiniDotTemp_C_5=ifelse(DateTime>"2019-11-15 9:49" & DateTime<"2019-11-15 10:21", NA, MiniDotTemp_C_5),
    Flag_MiniDotTemp_C_5=ifelse(DateTime=="2019-12-06 10:10", 2, Flag_MiniDotTemp_C_5),
    MiniDotTemp_C_5=ifelse(DateTime=="2019-12-06 10:10", NA, MiniDotTemp_C_5),
    Flag_MiniDotTemp_C_5=ifelse(DateTime>"2019-12-06 10:39" & DateTime<"2019-12-06 11:01", 2, Flag_MiniDotTemp_C_5),
    MiniDotTemp_C_5=ifelse(DateTime>"2019-12-06 10:39" & DateTime<"2019-12-06 11:01", NA, MiniDotTemp_C_5)
  )

#For Hobo at 6m
BVR_sensor_string=BVR_sensor_string%>%
  mutate(
    Flag_HoboTemp_C_6=ifelse(DateTime=="2017-07-20 5:45", 2, Flag_HoboTemp_C_6),
    HoboTemp_C_6=ifelse(DateTime=="2017-07-20 5:45", NA, HoboTemp_C_6),
    Flag_HoboTemp_C_6=ifelse(DateTime>"2019-04-26 20:49" & DateTime<"2019-04-26 21:11", 2, Flag_HoboTemp_C_6),
    HoboTemp_C_6=ifelse(DateTime>"2019-04-26 20:49" & DateTime<"2019-04-26 21:11", NA, HoboTemp_C_6),
    Flag_HoboTemp_C_6=ifelse(DateTime>"2019-07-31 13:09" & DateTime<"2019-07-31 13:41", 2, Flag_HoboTemp_C_6),
    HoboTemp_C_6=ifelse(DateTime>"2019-07-31 13:09" & DateTime<"2019-07-31 13:41", NA, HoboTemp_C_6)
  )

# For Hobo at 7m 
BVR_sensor_string=BVR_sensor_string%>%
  mutate(
    Flag_HoboTemp_C_7=ifelse(DateTime=="2017-07-20 5:45", 2, Flag_HoboTemp_C_7),
    HoboTemp_C_7=ifelse(DateTime=="2017-07-20 5:45", NA, HoboTemp_C_7),
    Flag_HoboTemp_C_7=ifelse(DateTime=="2020-06-18 12:50", 2, Flag_HoboTemp_C_7),
    HoboTemp_C_7=ifelse(DateTime=="2020-06-18 12:50", NA, HoboTemp_C_7)
  )

# For Hobo at 8m
BVR_sensor_string=BVR_sensor_string%>%
  mutate(
    Flag_HoboTemp_C_8=ifelse(DateTime=="2019-05-16 7:10", 2, Flag_HoboTemp_C_8),
    HoboTemp_C_8=ifelse(DateTime=="2019-05-16 7:10", NA, HoboTemp_C_8)
  )

# For Hobo at 9m
BVR_sensor_string=BVR_sensor_string%>%
  mutate(
    Flag_HoboTemp_C_9=ifelse(DateTime=="2019-06-27 10:10", 2, Flag_HoboTemp_C_9),
    HoboTemp_C_9=ifelse(DateTime=="2019-06-27 10:10", NA, HoboTemp_C_9)
  )

# For Hobo at 10m 
BVR_sensor_string=BVR_sensor_string%>%
  mutate(
    Flag_HoboTemp_C_10=ifelse(DateTime=="2019-05-16 7:10", 2, Flag_HoboTemp_C_10),
    HoboTemp_C_10=ifelse(DateTime=="2019-05-16 7:10", NA, HoboTemp_C_10),
    Flag_HoboTemp_C_10=ifelse(DateTime=="2019-06-27 10:10", 2, Flag_HoboTemp_C_10),
    HoboTemp_C_10=ifelse(DateTime=="2019-06-27 10:10", NA, HoboTemp_C_10),
    Flag_HoboTemp_C_10=ifelse(DateTime>"2017-04-07 12:49" & DateTime<"2017-04-07 13:51", 2, Flag_HoboTemp_C_10),
    HoboTemp_C_10=ifelse(DateTime>"2017-04-07 12:49" & DateTime<"2017-04-07 13:51", NA, HoboTemp_C_10)
  )

# For Hobo at 10.5m
BVR_sensor_string=BVR_sensor_string%>%
  mutate(
    Flag_HoboTemp_C_10.5=ifelse(DateTime=="2017-07-20 5:45", 2, Flag_HoboTemp_C_10.5),
    HoboTemp_C_10.5=ifelse(DateTime=="2017-07-20 5:45", NA, HoboTemp_C_10.5),
    Flag_HoboTemp_C_10.5=ifelse(DateTime>"2017-07-20 3:59" & DateTime<"2017-07-20 4:17", 2, Flag_HoboTemp_C_10.5),
    HoboTemp_C_10.5=ifelse(DateTime>"2017-07-20 3:59" & DateTime<"2017-07-20 4:17", NA, HoboTemp_C_10.5)
  )

# For MiniDot when the Hobo strings were pulled up
BVR_sensor_string=BVR_sensor_string%>%
  mutate(
    Flag_MiniDotDO_mgL_5=ifelse(DateTime>"2019-10-18 10:19" & DateTime<"2019-10-18 11:01",2,Flag_MiniDotDO_mgL_5),
    MiniDotDO_mgL_5=ifelse(DateTime>"2019-10-18 10:19" & DateTime<"2019-10-18 11:01",NA,MiniDotDO_mgL_5),
    Flag_MiniDotDOsat_percent_5=ifelse(DateTime>"2019-10-18 10:19" & DateTime<"2019-10-18 11:01",2,Flag_MiniDotDOsat_percent_5),
    MiniDotDOsat_percent_5=ifelse(DateTime>"2019-10-18 10:19" & DateTime<"2019-10-18 11:01",NA,MiniDotDOsat_percent_5),
    Flag_MiniDotDO_mgL_5=ifelse(DateTime>"2019-11-15 9:49" & DateTime<"2019-11-15 10:21",2,Flag_MiniDotDO_mgL_5),
    MiniDotDO_mgL_5=ifelse(DateTime>"2019-11-15 9:49" & DateTime<"2019-11-15 10:21",NA,MiniDotDO_mgL_5),
    Flag_MiniDotDOsat_percent_5=ifelse(DateTime>"2019-11-15 9:49" & DateTime<"2019-11-15 10:21",2,Flag_MiniDotDOsat_percent_5),
    MiniDotDOsat_percent_5=ifelse(DateTime>"2019-11-15 9:49" & DateTime<"2019-11-15 10:21",NA,MiniDotDOsat_percent_5),
    Flag_MiniDotDO_mgL_5=ifelse(DateTime>"2019-12-06 10:39" & DateTime<"2019-12-06 11:01",2,Flag_MiniDotDO_mgL_5),
    MiniDotDO_mgL_5=ifelse(DateTime>"2019-12-06 10:39" & DateTime<"2019-12-06 11:01",NA,MiniDotDO_mgL_5),
    Flag_MiniDotDOsat_percent_5=ifelse(DateTime>"2019-12-06 10:39" & DateTime<"2019-12-06 11:01",2,Flag_MiniDotDOsat_percent_5),
    MiniDotDOsat_percent_5=ifelse(DateTime>"2019-12-06 10:39" & DateTime<"2019-12-06 11:01",NA,MiniDotDOsat_percent_5),
    Flag_MiniDotDO_mgL_5=ifelse(DateTime=="2019-12-06 10:10", 2, Flag_MiniDotDO_mgL_5),
    MiniDotDO_mgL_5=ifelse(DateTime=="2019-12-06 10:10", NA, MiniDotDO_mgL_5),
    Flag_MiniDotDOsat_percent_5=ifelse(DateTime=="2019-12-06 10:10", 2, Flag_MiniDotDOsat_percent_5),
    MiniDotDOsat_percent_5=ifelse(DateTime=="2019-12-06 10:10", NA, MiniDotDOsat_percent_5),
  )

####################################################################################################################################################################
# Flag tables
#check the flag column to make sure there are no NAs in the Flag column
Flags=BVR_sensor_string%>%
  select(starts_with("Flag"))

for(b in 1:nrow(Flags)){
  print(colnames(Flags[b]))
  print(table(Flags[,b], useNA = "always"))
}

# Check where data gaps are 

#check for gaps and missing data
#order data by timestamp
BVRdata2=BVR_sensor_string
BVRdata2=BVRdata2[order(BVRdata2$DateTime),]
BVRdata2$DOY=yday(BVRdata2$DateTime)


#check record for gaps
#daily record gaps by day of year
for(c in 2:nrow(BVRdata2)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(BVRdata2$DOY[c]-BVRdata2$DOY[c-1]>1){
    print(c(BVRdata2$DateTime[c-1],BVRdata2$DateTime[c]))
  }
}

# add Reservoir and Site columns
BVR_sensor_string$Reservoir="BVR"
BVR_sensor_string$Site=50


# reorder columns
BVR_sensor_string <- BVR_sensor_string %>% select(Reservoir, Site, DateTime, HoboTemp_C_0.1, HoboTemp_C_1,HoboTemp_C_2,HoboTemp_C_3,HoboTemp_C_4,
                              HoboTemp_C_5,HoboTemp_C_6,HoboTemp_C_7,HoboTemp_C_8, HoboTemp_C_9,
                              HoboTemp_C_10, HoboTemp_C_10.5,
                              MiniDotTemp_C_5,MiniDotDO_mgL_5, MiniDotDOsat_percent_5, 
                              MiniDotTemp_C_10,MiniDotDO_mgL_10, MiniDotDOsat_percent_10,
                              EXOTemp_C_1.5, EXOCond_uScm_1.5, EXOSpCond_uScm_1.5, EXOTDS_mgL_1.5, EXODOsat_percent_1.5,
                              EXODO_mgL_1.5, EXOChla_RFU_1.5, EXOChla_ugL_1.5, EXOBGAPC_RFU_1.5, EXOBGAPC_ugL_1.5,
                              EXOfDOM_RFU_1.5, EXOfDOM_QSU_1.5,EXOTurbidity_FNU_1.5, EXOPressure_psi, EXODepth_m, 
                              EXOBattery_V, EXOWiper_V, 
                              Flag_HoboTemp_C_0.1, Flag_HoboTemp_C_1,Flag_HoboTemp_C_2,Flag_HoboTemp_C_3,Flag_HoboTemp_C_4,
                              Flag_HoboTemp_C_5,Flag_HoboTemp_C_6,Flag_HoboTemp_C_7,Flag_HoboTemp_C_8, Flag_HoboTemp_C_9,
                              Flag_HoboTemp_C_10, Flag_HoboTemp_C_10.5,
                              Flag_MiniDotTemp_C_5,Flag_MiniDotDO_mgL_5, Flag_MiniDotDOsat_percent_5, 
                              Flag_MiniDotTemp_C_10,Flag_MiniDotDO_mgL_10, Flag_MiniDotDOsat_percent_10,
                              Flag_EXOTemp_C_1.5, Flag_EXOCond_uScm_1.5, Flag_EXOSpCond_uScm_1.5,Flag_EXOTDS_mgL_1.5,
                              Flag_EXODOsat_percent_1.5, Flag_EXODO_mgL_1.5, Flag_EXOChla_RFU_1.5,Flag_EXOChla_ugL_1.5, 
                              Flag_EXOBGAPC_RFU_1.5,Flag_EXOBGAPC_ugL_1.5,
                              Flag_EXOfDOM_RFU_1.5,Flag_EXOfDOM_QSU_1.5, Flag_EXOTurbidity_FNU_1.5,
                              Flag_EXOPressure_psi, Flag_EXODepth_m, Flag_EXOBattery_V, Flag_EXOWiper_V)

# replace NaNs with NAs
BVR_sensor_string[is.na(BVR_sensor_string)] <- NA


#order by date and time
BVR_sensor_string <- BVR_sensor_string[order(BVR_sensor_string$DateTime),]



# convert datetimes to characters so that they are properly formatted in the output file
BVR_sensor_string$DateTime <- as.character(BVR_sensor_string$DateTime)

setwd("~/VT_Carey_Lab/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRsensorstring")

# write to output file
write.csv(BVR_sensor_string, "BVR_sensor_string_2016_2020.csv", row.names = FALSE, quote=FALSE)



