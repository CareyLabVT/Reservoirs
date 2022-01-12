# Title: Prepare FCR inflow data for publication to EDI
# Author: Mary Lofton
# Date 18DEC18

###############################################################################

# Updated: 17Dec19 by R. Corrigan and A. Hounshell
#   Added script to incorporate Diana (VT) data for publication
#   Updated flags for v-notch weir

###############################################################################

# Updated: 07Feb20 by A. Hounshell to included WVWA data through 31 Jan 2020

###############################################################################

# Updated: 20Feb20 by A. Hounshell - not correctly flagging low-flow data for 
# the v-notch weir; as of 07 Jun 2019, pressure sensors should be at the same 
# location as the bottom of the weir

###############################################################################

# Updated: 06Mar20 by A. Hounshell - updated V-notch weir calculations with 
# rating curve calculated from correlations between gage height and pressure 
# for the WVWA and VT pressure sensor from 7 Jun 2019 to present. Updates to this
# relationship will be needed for 2020 EDI push. Relationships is documented in 
# EDI metadata and with associated data file with data for the gage height vs. 
# pressure relationships. The gage relationship updates when the weir is too 
# low/over-topped for the period AFTER 6 Jun 2019. Nothing was changed for data 
# prior to 6 Jun 2019.

###############################################################################

# Updated: 04Jan21 by A. Hounshell - updated for EDI 2021 publishing. 
# A few key notes: 
# 1. The weir was 'renovated' in August 2020 which resulted in changes to 
# sensor (WVWA and VT) location. 
# A new rating curve has been calculated and will be applied to data collected 
# after September 2020. 
# 2. Removed the down-correction originally applied for all data after 18 Apr 
# 2016 for data collected using the v-notch weir

###############################################################################

# Updated: 22Oct21 by A. Hounshell
# A few key notes:
# 1. Updated weir rating curve after 02 Sep 2020 using additional data from:
#      2020_WeirWaterLevel: https://docs.google.com/spreadsheets/d/14XlOGqaUZYJO6aOLvMPDrvNAxooOaThK17PU8w7LCIE/edit#gid=0
#      2021_WeirWaterLevel: https://docs.google.com/spreadsheets/d/1w5i30nNg4T2nuPkcKDmC5Pp2Hly8Ddy6gtS6vR1znGk/edit#gid=0
# 2. Incorporated rating curve calculations into the script - no longer need
# additional .csv's for rating curves
# 3. Additional updates will be needed for data after July 2021

###############################################################################

# Updated: 12Jan22 by W. Woelmer
# added newest VT and WVWA data, updated file naming convention to Inflow_BEGINYEAR_ENDYEAR.csv, e.g. Inflow_2013_2022.csv
# added flags to period with missing data

###############################################################################


# Install packages
pacman::p_load(tidyverse, lubridate, magrittr, ggplot2)

#plotting theme
mytheme = theme(axis.title = element_text(size = 16),
                axis.text = element_text(size = 16))

## Data from pressure transducer installed at weir (WVWA pressure data)
# Load in files with names starting with FCR_inf_15min, should only be .csv files
inflow_pressure <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Inflow_CSV", pattern = "FCR_15min_Inf*") %>% 
  map_df(~ read_csv(file.path(path = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Inflow_CSV", .), col_types = cols(.default = "c"), skip = 28))
inflow_pressure = inflow_pressure[,-1] #limits data to necessary columns

##A wee bit o' data wrangling to get column names and formats in good shape
inflow_pressure <- inflow_pressure %>%
  rename(Date = `Date/Time`) %>%
  mutate(DateTime = parse_date_time(Date, 'dmy HMS',tz = "EST")) %>%
  select(-Date) %>%
  rename(Pressure_psi = `Pressure(psi)`,
         Temp_C = `Temperature(degC)`) %>%
  mutate(Pressure_psi = as.double(Pressure_psi),
         Temp_C = as.double(Temp_C))

## Preliminary visualization of raw pressure data from inflow transducer
plot_inflow <- inflow_pressure %>%
  mutate(Date = date(DateTime))

daily_flow <- group_by(plot_inflow, Date) %>% summarize(daily_pressure_avg = mean(Pressure_psi)) %>% mutate(Year = as.factor(year(Date)))

rawplot = ggplot(data = daily_flow, aes(x = Date, y = daily_pressure_avg))+
  geom_point()+
  ylab("Daily avg. inflow pressure (psi)")+
  geom_vline(xintercept = as.Date('2016-04-18'))+ # Date when downcorrection started
  theme_bw()
rawplot
#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_inflow_pressure.png", rawplot, device = "png")

pressure_hist = ggplot(data = daily_flow, aes(x = daily_pressure_avg, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. inflow pressure (psi)")+
  theme_bw()
pressure_hist
#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_inflow_pressure_histogram.png", pressure_hist, device = "png")

pressure_boxplot = ggplot(data = daily_flow, aes(x = Year, y = daily_pressure_avg, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. inflow pressure (psi)")+
  theme_bw()
pressure_boxplot
#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_inflow_pressure_boxplot.png", pressure_boxplot, device = "png")

## Read in catwalk pressure data: from WVWA instruments
pressure <- read_csv("./Data/DataNotYetUploadedToEDI/WVWA_DO_sondes/FCR_DOsonde_2012to2017.csv",col_types=list("c","d","d","d","d","d","d","d","l","l","l","l","l","l","l","l"))
pressure_a4d <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Barometric_CSV", pattern = "FCR_BV*") %>% 
  map_df(~ read_csv(file.path(path = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Barometric_CSV", .), col_types = cols(.default = "c"), skip = 28))
pressure_a4d = pressure_a4d[,-c(1,3)]

## Data wrangling to get columns in correct format and combine data from senvu.net and Aqua4Plus
pressure = pressure %>%
  select(Date, `Barometric Pressure Pressure (PSI)`) %>%
  mutate(DateTime = parse_date_time(Date, '%Y/%m/%d H:M:S',tz = "EST")) %>%
  rename(Baro_pressure_psi = `Barometric Pressure Pressure (PSI)`) %>%
  select(-Date) %>%
  mutate(Baro_pressure_psi = as.double(Baro_pressure_psi))

pressure_a4d <- pressure_a4d %>%
  rename(Date = `Date/Time`) %>%
  mutate(DateTime = parse_date_time(Date, 'dmy HMS',tz = "EST")) %>%
  select(-Date) %>%
  rename(Baro_pressure_psi = `Pressure(psi)`) %>%
  mutate(Baro_pressure_psi = as.double(Baro_pressure_psi))

baro_pressure <- bind_rows(pressure, pressure_a4d)
baro_pressure = baro_pressure %>%
  filter(!is.na(Baro_pressure_psi)) %>%
  arrange(DateTime) %>%
  mutate(DateTime = parse_date_time(DateTime, 'ymd HMS',tz = "EST"))

baro_pressure <- distinct(baro_pressure)

##Preliminary visualization of raw pressure data from catwalk transducer
plot_catwalk <- baro_pressure %>%
  mutate(Date = date(DateTime))

daily_catwalk <- group_by(plot_catwalk, Date) %>% summarize(daily_pressure_avg = mean(Baro_pressure_psi)) %>% mutate(Year = as.factor(year(Date)))

rawplot = ggplot(data = daily_catwalk)+
  geom_point(aes(x = Date, y = daily_pressure_avg))+
  ylab("Daily avg. catwalk pressure (psi)")+
  theme_bw()
rawplot
#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_baro_pressure.png", rawplot, device = "png")

pressure_hist = ggplot(data = daily_catwalk, aes(x = daily_pressure_avg, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. catwalk pressure (psi)")+
  theme_bw()
pressure_hist
#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_catwalk_pressure_histogram.png", pressure_hist, device = "png")

pressure_boxplot = ggplot(data = daily_catwalk, aes(x = Year, y = daily_pressure_avg, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. catwalk pressure (psi)")+
  theme_bw()
pressure_boxplot
#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_catwalk_pressure_boxplot.png", pressure_boxplot, device = "png")

## Correction to inflow pressure to down-correct data after 18APR16 - likely 
# due to slight change in pressure sensor location!
## Apply down-correction from 18Apr16 to installation of v-nocth weir (07Jun19)
# for some reasons the DateTimes are one hour off? I am confused about this but whatever
# just setting the downcorrect cutoff to be an hour off to compensate (original comments from MEL :)
inflow_pressure$DateTime <- as.POSIXct(strptime(inflow_pressure$DateTime, "%Y-%m-%d %H:%M:%S", tz = 'EST'))

inflow_pressure1 <- inflow_pressure %>%
  mutate(Pressure_psi =  ifelse(DateTime >= "2016-04-18 15:15:00 EST" & DateTime <= "2019-06-07 00:00:00 EST", (Pressure_psi - 0.14), Pressure_psi)) %>%
  rename(Pressure_psi_downcorrect = Pressure_psi) %>%
  select(-Temp_C)

downcorrect <- bind_cols(inflow_pressure, inflow_pressure1) %>%
  select(-DateTime...3) %>% rename(DateTime = DateTime...5)

##ARGH!! DATETIMES ARE NOT PLAYING NICELY!! cannot figure it out so just wrote to .csv and read in again
inflow_pressure$DateTime <- as.POSIXct(inflow_pressure$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
baro_pressure$DateTime <- as.POSIXct(baro_pressure$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

downcorrect_final <- downcorrect %>%
  select(-Pressure_psi) %>%
  rename(Pressure_psi = Pressure_psi_downcorrect)
downcorrect_final$DateTime <- as.POSIXct(downcorrect_final$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

write.csv(downcorrect_final, "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow.csv")
write.csv(baro_pressure, "./Data/DataNotYetUploadedToEDI/Raw_inflow/baro.csv")

##OK - round 2. let's see how the datetimes play together
baro <- read_csv("./Data/DataNotYetUploadedToEDI/Raw_inflow/baro.csv")
inflow <- read_csv("./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow.csv")

#correct datetime wonkiness from 2013-09-04 10:30 AM to 2014-02-05 11:00 AM
inflow$DateTime[24304:39090] = inflow$DateTime[24304:39090] - (6*60+43)

# correct dateime problems from 2019-04-15 11:06:18 for barometric pressure data and 
# 2019-04-15 15:07:20 for pressure transducer which are not on an even 15 minute interval
baro$DateTime <- floor_date(baro$DateTime, "15 minutes")
inflow$DateTime <- floor_date(inflow$DateTime, "15 minutes")

baro$DateTime <- as.POSIXct(strptime(baro$DateTime, "%Y-%m-%d %H:%M:%S", tz = 'EST'))
inflow$DateTime <- as.POSIXct(strptime(inflow$DateTime, "%Y-%m-%d %H:%M:%S", tz = 'EST'))

ggplot(data = baro, aes(x = as.Date(DateTime), y = Baro_pressure_psi)) +
  geom_point() +
  xlim(c(as.Date("2021-01-01"),as.Date("2021-12-31")))

ggplot(data = inflow, aes(x = as.Date(DateTime), y = Pressure_psi)) +
  geom_point() +
  xlim(c(as.Date("2021-01-01"),as.Date("2021-12-31")))

#merge inflow and barometric pressures to do differencing
diff = left_join(baro, inflow, by = "DateTime") %>%
  select(-c(1,4))
diff <- distinct(diff)

#eliminating pressure and temperature data we know is bad
diff <- diff %>%
  mutate(Baro_pressure_psi = ifelse(DateTime <= "2014-04-28 05:45:00" & DateTime >= "2014-03-20 09:00:00",NA,Baro_pressure_psi),
         Pressure_psi = ifelse(DateTime <= "2017-11-13 10:45:00" & DateTime >= "2017-10-15 06:00:00",NA,Pressure_psi),
         Temp_C = ifelse(DateTime <= "2017-11-13 10:45:00" & DateTime >= "2017-10-15 06:00:00",NA,Temp_C)) %>%
  mutate(Pressure_psia = Pressure_psi - Baro_pressure_psi)

diff <- diff %>%
  mutate(Pressure_psia = Pressure_psi - Baro_pressure_psi)

ggplot(data = diff, aes(x = as.Date(DateTime), y = Pressure_psia)) +
  geom_point() +
  xlim(c(as.Date("2021-01-01"),as.Date("2021-12-31")))

plot_both <- diff %>%
  mutate(Date = date(DateTime)) 

daily_pressure <- group_by(plot_both, Date) %>% 
  summarize(daily_pressure_avg = mean(Pressure_psi),
            daily_baro_pressure_avg = mean(Baro_pressure_psi),
            daily_psia = mean(Pressure_psia)) %>%
  mutate(Year = as.factor(year(Date))) %>%
  gather('daily_pressure_avg','daily_baro_pressure_avg', 'daily_psia',
         key = 'pressure_type',value = 'psi') 

daily_pressure <- daily_pressure %>%
  mutate(pressure_type = ifelse(pressure_type == "daily_pressure_avg","inflow",ifelse(pressure_type == "daily_baro_pressure_avg","barometric","corrected")))

both_pressures = ggplot(data = daily_pressure, aes(x = Date, y = psi, group = pressure_type, colour = pressure_type))+
  geom_point()+
  theme_bw()
both_pressures

#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/all_pressure_types.png", both_pressures, device = "png")

# Plot by year to look at potential issues
# 2021
both_pressures +
  xlim(c(as.Date("2021-01-01"),as.Date("2021-03-31")))

both_pressures +
  xlim(c(as.Date("2021-04-01"),as.Date("2021-06-30")))

both_pressures +
  xlim(c(as.Date("2021-07-01"),as.Date("2021-09-30"))) # End of data record - continue from here!

both_pressures +
  xlim(c(as.Date("2021-10-01"),as.Date("2021-12-31")))

# Select time period when inflow pressure was collected
diff <- diff %>%
  filter(!is.na(Pressure_psi))

# Plot Diff for 2020 (or most recent year)
diff_plot <- ggplot(diff,aes(x=DateTime,y=Pressure_psia))+
  geom_line()+
  geom_point()+
  theme_bw()
diff_plot

diff_plot+
  xlim(as.POSIXct("2020-01-01"),as.POSIXct("2021-12-31"))

## Let's actually calculate discharge from pressure!

# WW 13-sep-2019 hashtag'ed out some of the script originally written by MEL to incorporate new script that includes the equation for the
# v-notch weir which was installed 06-Jun-2019
# the calculations for pre-v-notch weir remain the same, just in a different format

#flow2 <- diff$Pressure_psia 
### CALCULATE THE FLOW RATES AT INFLOW ### #(MEL 2018-07-06)
#################################################################################################
#flow3 <- flow2*0.70324961490205 - 0.1603375 + 0.03048  # Response: Height above the weir (m); Distance between pressure sensor and weir lip: 0.1298575 m = 0.18337 psi
#pressure*conversion factor for head in m - distance from tip of transducer to lip of weir + distance from tip of transducer to pressure sensor (eq converted to meters)
#flow4 <- (0.62 * (2/3) * (1.1) * 4.43 * (flow3 ^ 1.5) * 35.3147) # Flow CFS - MEL: I have not changed this; should be rating curve with area of weir
#flow_final <- flow4*0.028316847                                  # Flow CMS - just a conversion factor from cfs to cms
#################################################################################################

# BUT FIRST! We must separate out the data for different rating curves/time periods!
# separate the dataframe into pre and post v-notch weir to apply different equations
# Rectangular weir:
diff_pre <- diff[diff$DateTime< as.POSIXct('2019-06-07 01:00:00'),]

# Original installation of V-notch weir
diff_post <- diff %>% 
  filter(DateTime >= as.POSIXct('2019-06-07 01:00:00') & DateTime <= as.POSIXct('2020-08-24 00:00:00'))

# V-notch weir post blow-out but before final pressure transducer location!
diff_aug20 <- diff %>% 
  filter(DateTime > as.POSIXct('2020-08-24 00:00:00') & DateTime <= as.POSIXct('2020-09-02 15:00:00'))

# V-notch weir post blow-out with FINAL location of the pressure transducer
diff_sep20 <- diff %>% 
  filter(DateTime > as.POSIXct('2020-09-02 15:00:00'))

######################

# the old weir equations are taken directly from MEL's Inflow Aggregation script
# Use for pressure data prior to 2019-06-06: see notes above for description of equations
# NOTE: Pressure_psia < 0.185 calculates -flows (and are automatically set to NA's)
# THIS WILL NOT CHANGE EVER AGAIN! KEEP AS IS FOR RECTANGULAR WEIR
diff_pre <- diff_pre %>% mutate(flow1 = (Pressure_psia )*0.70324961490205 - 0.1603375 + 0.03048) %>% 
  mutate(flow_cfs = (0.62 * (2/3) * (1.1) * 4.43 * (flow1 ^ 1.5) * 35.3147)) %>% 
  mutate(Flow_cms = flow_cfs*0.028316847) %>% 
  select(DateTime, Temp_C, Baro_pressure_psi, Pressure_psi, Pressure_psia, Flow_cms)

# Make flow NA when psi <= 0.184 (distance between pressure sensor and bottom of weir = 0.1298575 m = 0.18337 psi)
# Technically already completed above, but double check here
diff_pre$Flow_cms = ifelse(diff_pre$Pressure_psia < 0.184, NA, diff_pre$Flow_cms)

# Will also need to flag flows when water tops the weir: for the rectangular weir, head = 0.3 m + 0.1298575 m = 0.4298575 m
# This corresponds to Pressure_psia <= 0.611244557965199
# diff_pre$Flow_cms = ifelse(diff_pre$Pressure_psia > 0.611, NA, diff_pre$Flow_cms)
# Decided to keep data from above the weir, but flag!

# q = 2.391 * H^2.5
# where H = head in meters above the notch
# the head was 14.8 cm on June 24 at ~13:30
#14.8 cm is 0.148 m 
#14.9cm on Jun 27 at 3:49PM
# WW: Used scaling relationship to determine head:pressure relationship
# diff_post <- diff_post %>%  mutate(head = (0.149*Pressure_psia)/0.293) %>% 
#   mutate(Flow_cms = 2.391* (head^2.5)) %>% 
#   select(DateTime, Temp_C, Baro_pressure_psi, Pressure_psi, Pressure_psia, Flow_cms)

######################
## Now we will calculate discharge for the v-notch weir using a rating curve
# First, we need to load in observed gage height data at the v-notch weir
# This will need to be updated with new data each year - this is concatenated from 
# the field sheets in a google spreadsheet
# Previous and current spreadsheets can be found here:
# 2021_WeirWaterLevel: https://docs.google.com/spreadsheets/d/1w5i30nNg4T2nuPkcKDmC5Pp2Hly8Ddy6gtS6vR1znGk/edit#gid=0
# 2020_WeirWaterLevel: https://docs.google.com/spreadsheets/d/14XlOGqaUZYJO6aOLvMPDrvNAxooOaThK17PU8w7LCIE/edit#gid=0

# 2019 data was obtained from pictures - we are no longer doing this :)

# Update the 'RatingCurveDates.csv' with new dates from the spreadsheets above
# NOTE: Spreadsheet is up to date as of 22 Oct 21

# Load in this spreadsheet and align with atmospheric corrected pressure measured by the WVWA sensor
rating_curve_dates <- read_csv("./Data/DataNotYetUploadedToEDI/Raw_inflow/RatingCurveDates.csv")
rating_curve_dates$DateTime <- as.POSIXct(strptime(rating_curve_dates$DateTime, "%m/%d/%Y %H:%M", tz="EST"))

# Round time to nearest 15 minutes
rating_curve_dates <- rating_curve_dates %>% 
  mutate(DateTime = round_date(DateTime,"15 minutes")) %>% 
  select(DateTime,GageHeight_cm)

rating_curve <- left_join(rating_curve_dates, diff, by="DateTime")
# This is the data you will use to update the rating curve!
# We will calculate a rating curve with data from each time period

######################
## UPDATED 06 MAR 2020: Use rating curve (pressure vs. gage) to convert pressure to gage height
## UPDATED 06 JAN 2021: Use full rating curve for 2019-2020 to convert pressure to gage height
# THIS WILL NOT CHANGE EVER AGAIN! KEEP AS IS FOR V-NOTCH RATING CURVE WEIR DURING INITIAL INSTALL

# Calculate the rating curve from observed gage height and pressure - 
# THIS WILL NOT CHANGE AND IS COMMENTED OUT but here as a record!
# wvwa_post_rating <- rating_curve %>% 
#  filter(DateTime >= as.POSIXct('2019-06-07 01:00:00') & DateTime <= as.POSIXct('2020-08-24 00:00:00'))

# fit <- lm(GageHeight_cm ~ Pressure_psia, data = wvwa_post_rating)

diff_post <- diff_post %>% mutate(head = ((65.501*Pressure_psia)-9.849)/100) %>% 
  mutate(Flow_cms = 2.391 * (head^2.5)) %>% 
  select(DateTime, Temp_C, Baro_pressure_psi, Pressure_psi, Pressure_psia, Flow_cms)

# According to gage height vs. pressure relationship, pressure < 0.010 should be flagged
diff_post$Flow_cms = ifelse(diff_post$Pressure_psia <= 0.150, NA, diff_post$Flow_cms)

# Will need to flag flows when water tops the weir: used relationship between gage height and pressure to determine 
# pressure at 27.5 cm (when water tops weir); therefore, psi > 0.570 should be flagged
# diff_post$Flow_cms = ifelse(diff_post$Pressure_psia > 0.570, NA, diff_post$Flow_cms)
# Decided to keep data from above the weir, but flag!

######################

## Weir Blow-out noted on August 10, 2020!! Weir re-constructed on August 24, 2020 and instruments replaced
# Need to come-up with rating curve from August 24 - September 2, 2020 (sensors were moved again on
# September 2, 2020 - see below)
# THIS WILL NOT CHANGE EVER AGAIN! THIS IS FOR THE PERIOD AFTER WEIR BLOW-OUT BUT BEFORE CURRENT LOCATION!
#wvwa_aug20_rating <- rating_curve %>% 
#  filter(DateTime > as.POSIXct('2020-08-24 00:00:00') & DateTime <= as.POSIXct('2020-09-02 15:00:00'))

#fit <- lm(GageHeight_cm ~ Pressure_psia, data = wvwa_aug20_rating)

diff_aug20 <- diff_aug20 %>% mutate(head = ((55.556*Pressure_psia)-1.8333)/100) %>% 
  mutate(Flow_cms = 2.391 * (head^2.5)) %>% 
  select(DateTime, Temp_C, Baro_pressure_psi, Pressure_psi, Pressure_psia, Flow_cms)

# According to rating curve, pressure < 0.033 should be removed
diff_aug20$Flow_cms = ifelse(diff_aug20$Pressure_psia <= 0.033, NA, diff_aug20$Flow_cms)

# Will need to flag flows when water tops the weir: psi > 0.528 should be flagged
# diff_aug20$Flow_cms = ifelse(diff_aug20$Pressure_psia > 0.528, NA, diff_aug20$Flow_cms)

######################

## Inflow pressure tranducers moved on 2 Sep 2020
# Used rating curve from 2 Sep 2020 to present
# THIS WILL BE UPDATED EVERY YEAR - AS LONG AS THE PRESSURE TRANSDUCER LOCATION DOES NOT CHANGE!

# Select data for the current v-notch weir location
wvwa_current_curve <- rating_curve %>% 
  filter(DateTime >= as.POSIXct('2020-09-02 14:30:00'))

# Calculate linear regression between pressure (x) and gage height (y)
fit <- lm(GageHeight_cm ~ Pressure_psia, data = wvwa_current_curve)

# Then update the rating curve following the 'fit' results
diff_sep20 <- diff_sep20 %>% mutate(head = ((69.896*Pressure_psia)-0.447)/100) %>% # Change the slope (Pressure_psia) and intercept here!
  mutate(Flow_cms = 2.391 * (head^2.5)) %>% 
  select(DateTime, Temp_C, Baro_pressure_psi, Pressure_psi, Pressure_psia, Flow_cms)

# According to rating curve, pressure < 0.006 should be removed!
# This will need to be checked/updated each year as the rating curve changes
diff_sep20$Flow_cms = ifelse(diff_sep20$Pressure_psia <= 0.006, NA, diff_sep20$Flow_cms)

# Will need to flag flows when water tops the weir:
# Note: weir over-tops at gage height = 27.5 cm
# diff_sep20$Flow_cms = ifelse(diff_sep20$Pressure_psia >= 0.4000, NA, diff_sep20$Flow_cms)

# and put it all back together
diff <- rbind(diff_pre, diff_post, diff_aug20, diff_sep20)

#creating columns for EDI
diff$Reservoir <- "FCR" #creates reservoir column to match other data sets
diff$Site <- 100  #creates site column to match other data sets

##visualization of inflow
plot_inflow <- diff %>%
  mutate(Date = date(DateTime))

daily_inflow <- group_by(plot_inflow, Date) %>% 
  summarize(daily_flow_avg = mean(Flow_cms, na.rm = TRUE)) %>% 
  mutate(Year = as.factor(year(Date)),
         Month = month(Date))

inflow_hist = ggplot(data = daily_inflow, aes(x = daily_flow_avg, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. inflow (cms)")+
  xlim(0,0.5)+
  theme_bw()
inflow_hist
#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow_histogram.png", inflow_hist, device = "png")

inflow_boxplot = ggplot(data = daily_inflow, aes(x = Year, y = daily_flow_avg, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. inflow (cms)")+
  ylim(0,0.3)+
  theme_bw()+
  mytheme
inflow_boxplot
#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow_boxplot.png", inflow_boxplot, device = "png")

# Plot all discharge
q_all <- ggplot(data = diff, aes(x=DateTime,y=Flow_cms))+
  geom_line()+
  ylab("Inflow (cms)")+
  theme_bw()+
  mytheme
q_all

q_all +
  xlim(as.POSIXct('2020-01-01'),as.POSIXct('2021-12-31'))+
  ylim(0,0.21)

##visualization of temp
plot_temp <- diff %>%
  mutate(Date = date(DateTime))

daily_temp <- group_by(plot_temp, Date) %>% 
  summarize(daily_temp_avg = mean(Temp_C, na.rm = TRUE)) %>% 
  mutate(Year = as.factor(year(Date)),
         Month = month(Date))

temp2 = ggplot(daily_temp, aes(x = Date, y = daily_temp_avg))+
  geom_line(size = 1)+
  ylab("Avg. daily temp (C)")+
  theme_bw()
temp2
#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow_temp.png", temp2, device = "png")

temp_hist = ggplot(data = daily_temp, aes(x = daily_temp_avg, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. temp (C)")+
  theme_bw()
temp_hist
#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow_temp_histogram.png", inflow_hist, device = "png")

temp_boxplot = ggplot(data = daily_temp, aes(x = Year, y = daily_temp_avg, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. temp (C)")+
  theme_bw()
temp_boxplot
#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow_temp_boxplot.png", inflow_boxplot, device = "png")

#final data wrangling 
#Inflow_Final <- diff[,c(6,7,2,4,1,5,8,3)] #orders columns
Inflow_Final <- diff %>% select(Reservoir, Site, DateTime, Pressure_psi, Baro_pressure_psi, Pressure_psia, Flow_cms, Temp_C, everything())

Inflow_Final <- Inflow_Final[order(Inflow_Final$DateTime),] #orders file by date

Inflow_Final <- Inflow_Final %>%
  mutate(Flow_cms = ifelse(Flow_cms <= 0, NA, Flow_cms))

colnames(Inflow_Final) <- c('Reservoir', 'Site', 'DateTime', 'WVWA_Pressure_psi', 'WVWA_Baro_pressure_psi',  'WVWA_Pressure_psia', 'WVWA_Flow_cms', 'WVWA_Temp_C')

######################

#add VT sensor data to the WVWA transducer data ('Inflow_Final')
# Updated 06 Jan 2021 as data has migrated to FLARE-forecast repo on GitHub
VTsens <- read_csv(file.path('https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-weir-data/FCRweir.csv'),skip=4,col_names=c("TimeStamp","Record","BattV","PTemp_C","AirTemp_C","Lvl_psi","wtr_weir"))
VTdat <- VTsens[,c(1,6,7)]
colnames(VTdat) <- c('DateTime', 'VT_Pressure_psia', 'VT_Temp_C')
VTdat$DateTime <- as.POSIXct(strptime(VTdat$DateTime, "%Y-%m-%d %H:%M:%S", tz="EST"))

## Find dates for rating curve
## After v-notch weir was installed on 07 Jun 2019
# Load in dates for rating curve
rating_curve_VT <- left_join(rating_curve_dates, VTdat, by="DateTime")

# VT data for rectangular weir
# Used same equation as above following original Inflow Preparation script
# THIS WILL NEVER CHANGE! - WITH THE RECTANGULAR WEIR
VT_pre <- VTdat[VTdat$DateTime < as.POSIXct('2019-06-07 01:00:00'),]  
VT_pre <- VT_pre[complete.cases(VT_pre),]
VT_pre <- VT_pre %>% mutate(head = (VT_Pressure_psia)*0.70324961490205 - 0.1603375 + 0.03048) %>% 
  mutate(flow_cfs = (0.62 * (2/3) * (1.1) * 4.43 * (head ^ 1.5) * 35.3147)) %>% 
  mutate(VT_Flow_cms = flow_cfs*0.028316847) %>% 
  select(DateTime, VT_Pressure_psia, VT_Flow_cms, VT_Temp_C) 

# Make flow as NA when psi <= 0.184 (distance between pressure sensor and bottom of weir = 0.1298575 m = 0.18337 psi)
# Technically already completed above, but double check here
VT_pre$VT_Flow_cms = ifelse(VT_pre$VT_Pressure_psia < 0.184, NA, VT_pre$VT_Flow_cms)

# Will also need to flag flows when water tops the weir: for the rectangular weir, head = 0.3 m + 0.1298575 m = 0.4298575 m
# This corresponds to Pressure_psia <= 0.611244557965199
# VT_pre$VT_Flow_cms = ifelse(VT_pre$VT_Pressure_psia > 0.611, NA, VT_pre$VT_Flow_cms)
# Decided to keep data from above the weir, but flag!

######################

# VT data for v-notch weir, pre blow-out (June 2019 - August 2020)
VT_post <- VTdat %>% 
  filter(DateTime >= as.POSIXct('2019-06-07 01:00:00') & DateTime <= as.POSIXct('2020-08-24 15:15:00'))

# Need to determine when Weir blow-out occurred! Noted on August 10, 2020
# Plot VT_post data for 2020
vt_2020 <- ggplot(data = VT_post, aes(x=DateTime,y=VT_Pressure_psia))+
  geom_line()+
  ylab("Pressure (psia)")+
  theme_bw()+
  mytheme
vt_2020

vt_2020 +
  xlim(as.POSIXct("2020-07-06"),as.POSIXct("2020-09-10"))

# Rapid drop in pressure starting on 2020-07-20 after 11:00:00 - will need to remove data and flag

## UPDATED 06 MAR 2020: To use rating curve developed for VT pressure sensor (see metadata for additional information)
## to convert pressure to head

# Following V-notch weir and before blow-out in Summer 2020
# Calculate linear relationship between pressure and height in excel sheet (20210108_RatingCurve_VT_workbook.xls)
# THIS WILL NEVER CHANGE - PRIOR TO WEIR BLOW OUT!

# Calculated using rating curve!
# vt_post_rating <- rating_curve_VT %>% 
#  filter(DateTime >= as.POSIXct('2019-06-07 01:00:00') & DateTime <= as.POSIXct('2020-08-24 15:15:00'))

# fit <- lm(GageHeight_cm ~ VT_Pressure_psia, data = vt_post_rating)

VT_post <- VT_post %>% mutate(head = ((70.64*VT_Pressure_psia)-5.663)/100) %>% 
  mutate(VT_Flow_cms = 2.391*(head^2.5)) %>% 
  select(DateTime,VT_Pressure_psia,VT_Flow_cms,VT_Temp_C)

# Using rating curve, pressure at gage height = 0; pressure = 0.080
VT_post$VT_Flow_cms = ifelse(VT_post$VT_Pressure_psia <= 0.080, NA, VT_post$VT_Flow_cms)

# Will need to flag flows when water tops the weir: used gage height (height = 27.5 cm) to determine pressure = 0.469
# VT_post$VT_Flow_cms = ifelse(VT_post$VT_Pressure_psia > 0.469, NA, VT_post$VT_Flow_cms)
# Decided to keep data from above the weir, but flag!

######################

# Also need to correct data for August 24, 2020 to Sep 2, 2020 period after the weir was fixed (but before
# instruments were moved to their final location)
VT_aug20 <- VTdat %>% 
  filter(DateTime >= as.POSIXct('2020-08-24 15:30:00') & DateTime <= as.POSIXct('2020-09-02 14:15:00'))

# Update rating curve for this time period to calculate flow
# Calculate linear relationship between pressure and height in excel sheet (20210108_RatingCurve_VT_workbook.xls)
# THIS WILL NEVER CHANGE! - BEFORE THE FINAL LOCATION OF THE PRESSURE TRANSDUCER

# Rating curve development:
# vt_aug20_rating <- rating_curve_VT %>% 
#  filter(DateTime >= as.POSIXct('2019-06-07 01:00:00') & DateTime <= as.POSIXct('2020-08-24 15:15:00'))

# fit <- lm(GageHeight_cm ~ VT_Pressure_psia, data = vt_aug20_rating)

VT_aug20 <- VT_aug20 %>% mutate(head = ((58.14*VT_Pressure_psia)+2.9302)/100) %>% 
  mutate(VT_Flow_cms = 2.391*(head^2.5)) %>% 
  select(DateTime,VT_Pressure_psia,VT_Flow_cms,VT_Temp_C)

# Using rating curve, pressure at gage height = 0; pressure = -0.050
VT_aug20$VT_Flow_cms = ifelse(VT_aug20$VT_Pressure_psia <= 0, NA, VT_aug20$VT_Flow_cms)

# Will need to flag flows when water tops the weir: 
# VT_aug20$VT_Flow_cms = ifelse(VT_aug20$VT_Pressure_psia >= 0.423, NA, VT_aug20$VT_Flow_cms)

## Finally calculate flow from 2 Sep 2020 to present
VT_sep20 <- VTdat %>% filter(DateTime >= as.POSIXct('2020-09-02 14:30:00'))

# Update rating curve from 2 Sep 2020 to present
# Last updated: 03 August 20201
# UPDATE HERE USING ADDITIONAL RATING CURVE DATA IF NEEDED!
vt_post_dates <- rating_curve_VT %>% 
  filter(DateTime >= "2020-09-09 00:00:00")

# Calculate linear relationship between pressure (x) and gage height (y)
fit <- lm(GageHeight_cm ~ VT_Pressure_psia, data = vt_post_dates)

# Update the head = ((slope * VT_Pressure_psia)+Intercept)/100) line!!!!
VT_sep20 <- VT_sep20 %>% mutate(head =((70.919*VT_Pressure_psia)+6.114)/100) %>% 
  mutate(VT_Flow_cms = 2.391*(head^2.5)) %>% 
  select(DateTime,VT_Pressure_psia,VT_Flow_cms,VT_Temp_C)

# Use rating curve, pressure at gage height = 0; pressure = -0.086
VT_sep20$VT_Flow_cms = ifelse(VT_sep20$VT_Pressure_psia <= 0, NA, VT_sep20$VT_Flow_cms)

# Will need to flag flows when water tops the weir:
# Note: weir over-tops at gage height = 27.5 cm
# VT_sep20$VT_Flow_cms = ifelse(VT_sep20$VT_Pressure_psia >= 0.302, NA, VT_sep20$VT_Flow_cms)

VTinflow <- rbind(VT_pre, VT_post, VT_aug20, VT_sep20)
VTinflow$Reservoir <- 'FCR'
VTinflow$Site <- 100

# Check VT inflow data
ggplot(VTinflow,mapping=aes(x=DateTime,y=VT_Flow_cms))+
  geom_line()

# Merge with WVWA data
Inflow_Final <- merge(Inflow_Final, VTinflow, by=c('DateTime', 'Reservoir', 'Site'), all=TRUE)

# Plot data
all_flow <- ggplot()+
  geom_line(data=Inflow_Final, mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA"))+
  geom_line(data=Inflow_Final,mapping = aes(x=DateTime,y=VT_Flow_cms,color="VT"))+
  ylab("Flow (cms)")+
  theme_bw()+
  mytheme
all_flow

#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/VTvWVWA_AllFlow.png", all_flow, device = "png")


all_flow_2021 <- all_flow +
  xlim(as.POSIXct('2021-01-01'),as.POSIXct('2021-12-31'))
all_flow_2021

ggplot()+
  geom_line(data=Inflow_Final, mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA"))+
  #geom_line(data=Inflow_Final,mapping = aes(x=DateTime,y=VT_Flow_cms,color="VT"))+
  xlim(as.POSIXct('2021-01-01'),as.POSIXct('2021-12-31')) +
  ylab("Flow (cms)")+
  theme_bw()+
  mytheme

#ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/VTvWVWA_AllFlow_2020.png", all_flow_2020, device = "png")

# Check relationship between WVWA and VT
flow_comp <- ggplot(Inflow_Final,aes(x=WVWA_Flow_cms,y=VT_Flow_cms))+
  geom_point()+
  xlab("WVWA Flow")+
  ylab("VT Flow")+
  theme_bw()+
  mytheme
flow_comp

# Make sure dates are in posix.ct
Inflow_Final$DateTime <- as.POSIXct(strptime(Inflow_Final$DateTime, "%Y-%m-%d %H:%M:%S"))




#add flags for WVWA Uncorrected Inflow pressure (WVWA_Flag_Pressure_psi); WVWA Barometric pressure (WVWA_Flag_Baro_pressure_psi); WVWA corr inflow pressure
# (WVWA_Flag_Pressure_psia); and WVWA Temp (WVWA_Flag_Temp)
Inflow_Final_2 <- Inflow_Final %>%
  mutate(WVWA_Flag_Pressure_psi = ifelse(DateTime > '2021-01-07 09:15:00', 0, # (ALWAYS UPDATE!) AKA: no data after 2020-11-02 14:30:00
                                         ifelse(DateTime <= "2017-11-13 10:45:00" & DateTime >= "2017-10-15 06:00:00",5, # Flag for leaking weir
                                                ifelse(DateTime >= "2020-07-20 11:00:00" & DateTime <= "2020-08-24 18:30:00", 24, # Flag for weir blow-out
                                                      ifelse(DateTime >= "2020-05-14 17:00:00" & DateTime <= "2020-08-24 18:30:00",2, # Flag for missing data due to user error (not collecting data)
                                                              ifelse(DateTime >= "2020-09-02 14:00:00" & DateTime <= "2020-09-02 14:30:00", 4, # Flag for moving sensors on 02 Sep 2020
                                                                     ifelse(DateTime >= "2016-04-18 15:15:00 EST" & DateTime < "2019-06-07 00:00:00" ,1, # Flag for down correction after 18Apr16
                                                                            0)))))),  
         WVWA_Flag_Baro_pressure_psi = ifelse(DateTime > '2021-01-07 09:15:00', 0, # (ALWAYS UPDATE!) No data after 2020-11-02 14:30:00
                                              ifelse(DateTime <= "2014-04-28 05:45:00" & DateTime >= "2014-03-20 09:00:00",2, # Sensor malfunction 
                                                     ifelse(DateTime >= "2020-05-14 17:00:00" & DateTime <= "2020-08-24 16:00:00",2, # Flag for missing data due to user error (not collecting data)
                                                     0))),   
         WVWA_Flag_Pressure_psia = ifelse(DateTime >= '2021-01-07 09:15:00', 0, # (ALWAYS UPDATE!) AKA: no data after 2020-11-02 14:30:00 for WVWA
                                          ifelse(DateTime <= "2017-11-13 10:45:00" & DateTime >= "2017-10-15 06:00:00",5, # Flag for leaking weir
                                                 ifelse(DateTime >= "2020-07-20 11:00:00" & DateTime <= "2020-08-24 18:30:00", 24, # Flag for weir blow-out
                                                        ifelse(DateTime >= "2020-05-14 17:00:00" & DateTime <= "2020-08-24 18:30:00",2, # Flag for missing data due to user error (not collecting data)
                                                               ifelse(DateTime >= "2020-09-02 14:00:00" & DateTime <= "2020-09-02 14:30:00", 4, # Flag for moving sensors on 02 Sep 2020
                                                                      ifelse(DateTime >= "2016-04-18 15:15:00 EST" & DateTime < "2019-06-07 00:00:00",1, # Flag for down correction after 18Apr16
                                                                             0)))))), 
         WVWA_Flag_Temp = ifelse(DateTime > '2021-01-07 09:15:00', 0, # (ALWAYS UPDATE!) No WVWA data after 2020-11-02 14:30:00
                                 ifelse(DateTime <= "2017-11-13 10:45:00" & DateTime >= "2017-10-15 06:00:00",5, # Leaking weir (no NA's; just flag)
                                        ifelse(DateTime >= "2020-07-20 11:00:00" & DateTime <= "2020-08-24 18:30:00", 24, # Flag for weir blow-out
                                               ifelse(DateTime >= "2020-05-14 17:00:00" & DateTime <= "2020-08-24 18:30:00",2, # Flag for missing data due to user error (not collecting data)
                                                      ifelse(DateTime >= "2020-09-02 14:00:00" & DateTime <= "2020-09-02 14:30:00", 4, # Flag for moving sensors on 02 Sep 2020
                                        0)))))) 

# Add down-correction flag for dates after 2016-04-18
Inflow_Final_3 <- Inflow_Final_2 %>% 
  mutate(WVWA_Flag_Flow = ifelse(DateTime >= "2016-04-18 15:15:00 EST" & DateTime < "2019-06-07 00:00:00",1,0))

# Add flags for flows over the weir (during various points in time)         
Inflow_Final_4 <- Inflow_Final_3 %>% 
  mutate(WVWA_Flag_Flow = ifelse(DateTime <= "2016-04-18 15:15:00" & WVWA_Pressure_psia >= 0.611,6, # no down correction, flow over rectangular weir
                                 ifelse(DateTime >= "2016-04-18 15:15:00" & DateTime <= "2019-06-03 00:00:00" & WVWA_Pressure_psia >= 0.611,16, # Down correction, flow over rectangular weir
                                        ifelse(DateTime > "2019-06-07 00:00:00" & DateTime <= "2020-08-24 14:15:00" & WVWA_Pressure_psia >= 0.570, 6, # flow over v-notch weir - pre blow out
                                               ifelse(DateTime >= "2020-08-24 14:30:00" & DateTime <= "2020-09-02 13:45:00" & WVWA_Pressure_psia >= 0.528, 6, # flow over v-notch weir - Aug 2020 (post blow-out, pre sensor move)
                                                      ifelse(DateTime >= "2020-09-02 14:30:00" & WVWA_Pressure_psia >= 0.394, 6, # (UPDATE RATING CURVE IN 2022)  flow over v-notch weir - 2 Sep 2020 to Present
                                                             Inflow_Final_3$WVWA_Flag_Flow)))))) %>% 
  mutate(VT_Flag_Flow = ifelse(DateTime <= "2019-06-03 00:00:00" & VT_Pressure_psia >= 0.611 & is.na(VT_Flow_cms),6, # Flow too high for rectangular weir
                               ifelse(DateTime >= "2019-06-07 00:00:00" & DateTime <= "2020-08-24 14:15:00" & VT_Pressure_psia >= 0.469, 6, # flow too high for v-notch weir - pre-blow out
                                      ifelse(DateTime >= "2020-08-24 14:30:00" & DateTime <= "2020-09-02 13:45:00" & VT_Pressure_psia >= 0.423, 6, # flow too high for v-notch weir - Aug 2020 (post blow-out, pre instrument move)
                                             ifelse(DateTime >= "2020-09-02 14:30:00" & VT_Pressure_psia >= 0.302,6, # (UPDATE RATING CURVE IN 2022) flow too high for v-notch weir - 2 Sep 2020 to present)
                                      0)))))
# Add flags for low flows
Inflow_Final_5 <- Inflow_Final_4 %>% 
  mutate(WVWA_Flag_Flow = ifelse(DateTime <= "2016-04-18 15:15:00" & WVWA_Pressure_psia < 0.185, 3, # no down correction, low flows on rectangular weir
                                 ifelse(DateTime >= "2016-04-18 15:15:00 EST" & DateTime <= "2019-06-03 00:00:00" & WVWA_Pressure_psia <= 0.185, 13, # down correction and low flows on rectangular weir
                                        ifelse(DateTime >= "2019-06-07 00:00:00" & DateTime <= "2020-08-24 14:15:00" & WVWA_Pressure_psia <= 0.150, 3, # low flows on v-notch weir - pre blow out
                                               ifelse(DateTime >= "2020-08-24 14:30:00" & DateTime <= "2020-09-02 13:45:00" & WVWA_Pressure_psia <= 0.033, 3, # low flows - Aug 2020 (post blow-out, pre sensor move)
                                                      ifelse(DateTime >= "2020-09-02 14:30:00" & WVWA_Pressure_psia <=0.006, 3, # (UPDATE RATING CURVE FOR 2022) low flows - 2 Sep 2020 to Present
                                                            Inflow_Final_4$WVWA_Flag_Flow)))))) %>% 
  mutate(VT_Flag_Flow = ifelse(DateTime<= "2019-06-03 00:00:00" & VT_Pressure_psia <= 0.185 & is.na(VT_Flow_cms),3, # flow too low for rectangular weir
                               ifelse(DateTime >= "2019-06-07 00:00:00" & DateTime <= "2020-08-24 14:15:00" & VT_Pressure_psia < 0.080 & is.na (VT_Flow_cms),3, #flow too low for v-notch weir - pre-blow out
                                      ifelse(DateTime >= "2020-08-24 14:30:00" & DateTime <= "2020-09-02 13:45:00" & VT_Pressure_psia <= 0, 3, # flow too low for v-notch weir - Aug 2020 (post blow-out, pre instrument move)
                                             ifelse(DateTime >= "2020-09-02 14:30:00" & VT_Pressure_psia <= 0, 3, # (UPDATE RATING CURVE IN 2022) flow too low for v-notch weir - 2 Sep 2020 to present
                                                    Inflow_Final_4$VT_Flag_Flow)))))
           
# Add flags for everything else (sensor malfunction; demonic intrusions; etc.)
Inflow_Final_6 <- Inflow_Final_5 %>% 
  mutate(WVWA_Flag_Flow = ifelse(DateTime <= "2014-04-28 05:45:00" & DateTime >= "2014-03-20 09:00:00",2, # sensor malfunction
                                 ifelse(DateTime <= "2017-11-13 10:45:00" & DateTime >= "2017-10-15 06:00:00",5, # leaking weir; NA's
                                        ifelse(DateTime >= "2019-06-03 00:00:00" & DateTime <= "2019-06-07 00:00:00",14, # down correction and demonic intrusion (weir plug removed)
                                               ifelse(DateTime >= "2020-07-20 11:00:00" & DateTime <= "2020-08-24 18:30:00", 24, # Flag for weir blow-out
                                                      ifelse(DateTime >= "2020-05-14 17:00:00" & DateTime <= "2020-08-24 18:30:00",2, # Flag for missing data due to user error (not collecting data)
                                                             ifelse(DateTime >= "2020-09-02 14:00:00" & DateTime <= "2020-09-02 14:30:00", 4, # Flag for moving sensors on 02 Sep 2020
                                                                    Inflow_Final_5$WVWA_Flag_Flow))))))) %>% 
  mutate(VT_Flag_Flow = ifelse(DateTime >= "2019-04-22 00:00:00" & DateTime <= "2019-06-07 00:00:00",4, # weir un-plugged/rating curve not correct
                               ifelse(DateTime >= "2020-07-20 11:00:00" & DateTime <= "2020-08-24 18:30:00", 4, # Flag for weir blow-out
                                      ifelse(DateTime >= "2020-09-02 14:00:00" & DateTime <= "2020-09-02 14:30:00", 4, # Flag for moving sensors on 02 Sep 2020
                                             Inflow_Final_5$VT_Flag_Flow)))) %>% 
  mutate(VT_Flag_Pressure_psia = ifelse(DateTime < '2019-04-22 12:00:00', 0, # no data before 4-22-19
                                        ifelse(DateTime >= "2020-07-20 11:00:00" & DateTime <= "2020-08-24 18:30:00", 4, # Flag for weir blow-out
                                               ifelse(DateTime >= "2020-09-02 14:00:00" & DateTime <= "2020-09-02 14:30:00", 4, # Flag for moving sensors on 02 Sep 2020
                                                      0)))) %>% 
  mutate(VT_Flag_Temp = ifelse(DateTime < '2019-04-22 12:00:00', 0, # no data before 4-22-19
                               ifelse(DateTime >= "2020-07-20 11:00:00" & DateTime <= "2020-08-24 18:30:00", 4, # Flag for weir blow-out
                                      ifelse(DateTime >= "2020-09-02 14:00:00" & DateTime <= "2020-09-02 14:30:00", 4, # Flag for moving sensors on 02 Sep 2020
                                             0))))

### Make sure there are NA's for all weir data when there is a '4' flag
Inflow_Final_7 <- Inflow_Final_6 %>% 
  mutate(WVWA_Pressure_psi = ifelse(WVWA_Flag_Pressure_psi>1,NA,Inflow_Final_6$WVWA_Pressure_psi)) %>% 
  mutate(WVWA_Baro_pressure_psi = ifelse(WVWA_Flag_Baro_pressure_psi>1,NA,Inflow_Final_6$WVWA_Baro_pressure_psi)) %>% 
  mutate(WVWA_Pressure_psia = ifelse(WVWA_Flag_Pressure_psia>1,NA,Inflow_Final_6$WVWA_Pressure_psia)) %>% 
  mutate(WVWA_Flow_cms = ifelse(WVWA_Flag_Flow %in% c(2,3,4,13,14,24),NA,Inflow_Final_6$WVWA_Flow_cms)) %>% # But not 6 or 16
  mutate(WVWA_Temp_C = ifelse(WVWA_Flag_Temp>1,NA,Inflow_Final_6$WVWA_Temp_C)) %>% 
  mutate(VT_Pressure_psia = ifelse(VT_Flag_Pressure_psia>1,NA,Inflow_Final_6$VT_Pressure_psia)) %>% 
  mutate(VT_Flow_cms = ifelse(VT_Flag_Flow==4,NA,Inflow_Final_6$VT_Flow_cms)) %>% 
  mutate(VT_Temp_C = ifelse(VT_Flag_Temp>1,NA,Inflow_Final_6$VT_Temp_C))

### Check inflow and flags
#pdf("./Data/DataNotYetUploadedToEDI/Raw_inflow/QA_Plots.pdf",width=12,height=8)

# Uncorrected WVWA inflow pressure
ggplot()+
  geom_line(Inflow_Final_7,mapping=aes(x=DateTime,y=WVWA_Pressure_psi,color="WVWA"))+
  geom_point(Inflow_Final_7,mapping=aes(x=DateTime,y=WVWA_Flag_Pressure_psi,color="WVWA"))+
  theme_bw()

# WVWA Baro pressure
ggplot()+
  geom_line(Inflow_Final_7,mapping=aes(x=DateTime,y=WVWA_Baro_pressure_psi,color="WVWA"))+
  geom_point(Inflow_Final_7,mapping=aes(x=DateTime,y=WVWA_Flag_Baro_pressure_psi,color="WVWA"))+
  theme_bw()

# Pressure psia
ggplot()+
  geom_line(Inflow_Final_7,mapping=aes(x=DateTime,y=WVWA_Pressure_psia,color="WVWA"))+
  geom_point(Inflow_Final_7,mapping=aes(x=DateTime,y=(WVWA_Flag_Pressure_psia/10),color="WVWA"))+
  geom_line(Inflow_Final_7,mapping=aes(x=DateTime,y=VT_Pressure_psia,color="VT"))+
  geom_point(Inflow_Final_7,mapping=aes(x=DateTime,y=VT_Flag_Pressure_psia/10,color="VT"))+
  theme_bw()

# Inflow
ggplot()+
  geom_line(Inflow_Final_7,mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA"))+
  geom_line(Inflow_Final_7,mapping=aes(x=DateTime,y=VT_Flow_cms,color="VT"))+
  geom_point(Inflow_Final_7,mapping=aes(x=DateTime,y=(WVWA_Flag_Flow/10),color="WVWA"))+
  geom_point(Inflow_Final_7,mapping=aes(x=DateTime,y=(VT_Flag_Flow/10),color="VT"))+
  theme_bw()

# Temp
ggplot()+
  geom_line(Inflow_Final_7,mapping=aes(x=DateTime,y=WVWA_Temp_C,color="WVWA"))+
  geom_point(Inflow_Final_7,mapping=aes(x=DateTime,y=WVWA_Flag_Temp,color="WVWA"))+
  geom_line(Inflow_Final_7,mapping=aes(x=DateTime,y=as.numeric(VT_Temp_C),color="VT"))+
  geom_point(Inflow_Final_7,mapping=aes(x=DateTime,y=VT_Flag_Temp,color="VT"))+
  theme_bw()

dev.off()

## Re-order data table
col_order <- c("Reservoir","Site","DateTime","WVWA_Pressure_psi","WVWA_Baro_pressure_psi","WVWA_Pressure_psia",
               "WVWA_Flow_cms","WVWA_Temp_C","VT_Pressure_psia","VT_Flow_cms","VT_Temp_C","WVWA_Flag_Pressure_psi",
               "WVWA_Flag_Baro_pressure_psi","WVWA_Flag_Pressure_psia","WVWA_Flag_Flow","WVWA_Flag_Temp",
               "VT_Flag_Pressure_psia","VT_Flag_Flow","VT_Flag_Temp")

Inflow_Final_8 <- Inflow_Final_7[,col_order]

Inflow_Final_8 <- Inflow_Final_8 %>% 
  rename(VT_Flag_Temp_C = VT_Flag_Temp, WVWA_Flag_Temp_C = WVWA_Flag_Temp)

str(Inflow_Final_8)

# Write to CSV
write_csv(Inflow_Final_8, './Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/2022_Jan/Inflow_2013_2022.csv') 
