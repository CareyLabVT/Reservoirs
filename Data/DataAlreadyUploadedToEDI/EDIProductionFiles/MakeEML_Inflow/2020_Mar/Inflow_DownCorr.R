## Fiddling with weir data: down-corrections? Differences between VT and WVWA pressure?
## A Hounshell, 12 Feb 2020

# Install packages
pacman::p_load(tidyverse, lubridate, magrittr, ggplot2)

# Load in EDI corrected inflow data: Inflow PRE rating curve
inflow <- read_csv("C:/Users/ahoun/OneDrive/Desktop/Reservoir/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/FEB2020/inflow_for_EDI_2013_Mar2020.csv",
                   col_types = "cdTdddddddddddddddd")
inflow$DateTime <- as.POSIXct(strptime(inflow$DateTime, "%Y-%m-%d %H:%M:%S", tz="EST"))

#--------------------------------------------------------

### Used to the following script to determine if the down-correction was still necessary
### Short answer = YES! Otherwise, there would be an obvious step in the data
### Used data from pre-rating curve period

# Plot WVWA v. VT weir pressure
ggplot()+
  geom_line(data=inflow,mapping=aes(x=DateTime,y=WVWA_Pressure_psia,color="WVWA Corr"))+
  geom_line(data=inflow,mapping=aes(x=DateTime,y=WVWA_Pressure_psia+0.14,color="WVWA UnCorr"))+
  geom_line(data=inflow,mapping=aes(x=DateTime,y=VT_Pressure_psia,color="VT"))+
  xlim(as.POSIXct("2019-01-01"),as.POSIXct("2020-02-01"))+
  theme_classic(base_size = 15)

# Calculate if there were no down-corrections post 2016-04-08
downcorr <- inflow %>% filter(DateTime >= as.POSIXct("2016-04-08")) %>% select(DateTime,WVWA_Pressure_psia)
downcorr <- downcorr %>% mutate(WVWA_Pressure_psia_down = WVWA_Pressure_psia+0.14)

# Plot un-corrected (downcorr) and corrected (inflow) data
ggplot()+
  geom_line(data=inflow,mapping=aes(x=DateTime,y=WVWA_Pressure_psia,color="WVWA Corr"))+
  geom_line(data=downcorr,mapping=aes(x=DateTime,y=WVWA_Pressure_psia_down,color="WVWA UnCorr"))+
  geom_vline(xintercept = as.POSIXct("2016-04-08"))+
  theme_classic(base_size = 15)

# Plot around June 2019 (change from rectangular to v-notch weir)
ggplot(data=inflow,mapping=aes(x=DateTime,y=WVWA_Flow_cms))+
  geom_line()+
  theme_classic(base_size = 15)

ggplot(data=inflow,mapping=aes(x=DateTime,y=WVWA_Flow_cms))+
  geom_line()+
  xlim(as.POSIXct("2018-01-01"),as.POSIXct("2020-01-01"))+
  theme_classic(base_size = 15)

ggplot(data=inflow,mapping=aes(x=DateTime,y=WVWA_Flow_cms))+
  geom_line()+
  geom_vline(xintercept = as.POSIXct("2019-06-06"),color="red")+
  theme_classic(base_size = 15)

#-------------------------------------------------------------
## Comparisons between pre-rating curve and post-rating curve for v-notch weir
## Look at both WVWA and VT pressure sensors
# Load in post-rating curve data
inflow_post <- read_csv("C:/Users/ahoun/OneDrive/Desktop/Reservoir/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/Mar2020/inflow_for_EDI_2013_06Mar2020.csv",
                   col_types = "cdTdddddddddddddddd")
inflow_post$DateTime <- as.POSIXct(strptime(inflow_post$DateTime, "%Y-%m-%d %H:%M:%S", tz="EST"))

# Compare WVWA sensor: pre and post-rating curve
ggplot()+
  geom_line(data=inflow,mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA Pre-rating"))+
  geom_point(data=inflow,mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA Pre-rating"))+
  geom_line(data=inflow_post,mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA Post-rating"))+
  geom_point(data=inflow_post,mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA Post-rating"))+
  theme_classic(base_size = 15)

ggplot()+
  geom_line(data=inflow,mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA Pre-rating"))+
  geom_point(data=inflow,mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA Pre-rating"))+
  geom_line(data=inflow_post,mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA Post-rating"))+
  geom_point(data=inflow_post,mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA Post-rating"))+
  xlim(as.POSIXct("2019-05-01"),as.POSIXct("2020-03-06"))+
  ylim(0,0.2)+
  theme_classic(base_size = 15)

# Compare VT sensor: pre and post-rating curve
ggplot()+
  geom_line(data=inflow,mapping=aes(x=DateTime,y=VT_Flow_cms,color="VT Pre-rating"))+
  geom_point(data=inflow,mapping=aes(x=DateTime,y=VT_Flow_cms,color="VT Pre-rating"))+
  geom_line(data=inflow_post,mapping=aes(x=DateTime,y=VT_Flow_cms,color="VT Post-rating"))+
  geom_point(data=inflow_post,mapping=aes(x=DateTime,y=VT_Flow_cms,color="VT Post-rating"))+
  theme_classic(base_size = 15)

ggplot()+
  geom_line(data=inflow,mapping=aes(x=DateTime,y=VT_Flow_cms,color="VT Pre-rating"))+
  geom_point(data=inflow,mapping=aes(x=DateTime,y=VT_Flow_cms,color="VT Pre-rating"))+
  geom_line(data=inflow_post,mapping=aes(x=DateTime,y=VT_Flow_cms,color="VT Post-rating"))+
  geom_point(data=inflow_post,mapping=aes(x=DateTime,y=VT_Flow_cms,color="VT Post-rating"))+
  xlim(as.POSIXct("2019-05-01"),as.POSIXct("2020-03-06"))+
  ylim(0,0.2)+
  theme_classic(base_size = 15)

# Compare WVWA and VT pre-rating curve
ggplot()+
  geom_line(data=inflow,mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA Pre-rating"))+
  geom_point(data=inflow,mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA Pre-rating"))+
  geom_line(data=inflow,mapping=aes(x=DateTime,y=VT_Flow_cms,color="VT Pre-rating"))+
  geom_point(data=inflow,mapping=aes(x=DateTime,y=VT_Flow_cms,color="VT Pre-rating"))+
  xlim(as.POSIXct("2019-05-01"),as.POSIXct("2020-03-06"))+
  ylim(0,0.2)+
  theme_classic(base_size = 15)

# Compare WVWA and VT post-rating curve
# Select data for after v-notch weir
inflow <- inflow %>% filter(DateTime > as.POSIXct("2019-06-06"))
inflow_post <- inflow_post %>% filter(DateTime > as.POSIXct("2019-06-06"))

ggplot()+
  geom_line(data=inflow_post,mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA Post-rating"))+
  geom_point(data=inflow_post,mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA Post-rating"))+
  geom_line(data=inflow_post,mapping=aes(x=DateTime,y=VT_Flow_cms,color="VT Post-rating"))+
  geom_point(data=inflow_post,mapping=aes(x=DateTime,y=VT_Flow_cms,color="VT Post-rating"))+
  ylim(0,0.2)+
  theme_classic(base_size = 15)

ggplot()+
  geom_line(data=inflow_post,mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA Post-rating"))+
  geom_point(data=inflow_post,mapping=aes(x=DateTime,y=WVWA_Flow_cms,color="WVWA Post-rating"))+
  geom_line(data=inflow_post,mapping=aes(x=DateTime,y=VT_Flow_cms,color="VT Post-rating"))+
  geom_point(data=inflow_post,mapping=aes(x=DateTime,y=VT_Flow_cms,color="VT Post-rating"))+
  xlim(as.POSIXct("2019-07-01"),as.POSIXct("2019-08-01"))+
  ylim(0,0.2)+
  theme_classic(base_size = 15)

# Scatterplot of WVWA and VT pre-rating curve
ggplot()+
  geom_point(data=inflow,mapping=aes(x=WVWA_Flow_cms,y=VT_Flow_cms,color="Pre-Rating"))+
  geom_abline(intercept=0)+
  theme_classic(base_size = 15)

ggplot()+
  geom_point(data=inflow_post,mapping=aes(x=WVWA_Flow_cms,y=VT_Flow_cms,color="Post-Rating"))+
  geom_abline(intercept=0)+
  theme_classic(base_size = 15)
