# Script to pull in YSI and PAR data from multiple reservoirs and years ####

#install.packages('pacman') ## Run this line if you don't have "pacman" package installed
pacman::p_load(tidyverse, lubridate) ## Use pacman package to install/load other packages

#set working directory
setwd("C:/Users/CareyLab/Desktop/2018 Reservoir Field Sheets")

#### YSI Profiles ####

# Read in data from past year
raw_profiles <- read_csv("./2018_YSI_PAR_profiles.csv")%>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%m/%d/%Y %H:%M")) #mutate DateTime column to target format


  # Parse columns to target format
profiles <- raw_profiles %>%
  group_by(Reservoir, DateTime, Notes) %>% # columns not to parse to numeric
  mutate_if(is.character,funs(round(as.double(.), 2))) %>%  # parse all other columns to numeric
  
  # Add 'flag' columns for each variable; 1 = flag for NA value
    mutate(Flag_PAR = ifelse(is.na(PAR_umolm2s), 1, 0),
         Flag_Temp = ifelse(is.na(Temp_C), 1, 
                            ifelse(Temp_C > 35, 2, 0)), # if Temp is over 35 degrees C, Flag 2 = inst. malfunction
         Flag_DO = ifelse(is.na(DO_mgL), 1, 0),
         Flag_DOSat = ifelse(is.na(DOSat), 1,
                             ifelse(DOSat > 200, 2, 0))) %>% # if DO saturation is over 200%, Flag 2 = inst. malfunction
         
  # Arrange order of columns for final data table
  select(Reservoir, Site, DateTime, Depth_m, Temp_C, DO_mgL, DOSat, 
         PAR_umolm2s, Flag_Temp, Flag_DO, Flag_DOSat,Flag_PAR, Notes) %>% #select the columns you want to keep
  arrange(DateTime, Reservoir, Depth_m) #order the dataframe by values of these columns

# Write to CSV 
#write.csv(profiles, './Formatted_Data/YSI_PAR_profiles.csv', row.names=F)

#### YSI diagnostic plots ####
profiles_long <- profiles %>%
  ungroup(.) %>% #undoing the grouping of character and numeric columns we did earlier
  select(-(Flag_Temp:Notes)) %>% #getting rid of flag and note columns
  gather(metric, value, Temp_C:PAR_umolm2s) #transforming data to long format for plotting

# Plot range of values per year for each reservoir; 
# annual mean value indicated with large black dot
ggplot(subset(profiles_long, Site == 50), aes(x = year, y = value, col=Reservoir)) + #selecting all observations at deepest site of FCR
  geom_point(size=1) + #plot points
  stat_summary(fun.y="mean", geom="point",pch=21,  size=3, fill='black') + #make mean of data series a black point
  facet_grid(metric ~ Reservoir, scales= 'free_y') + #make a multiplot, or a grid of plots separated by type of measurement and reservoir
  scale_x_continuous("Date", breaks=seq(2013,2018,1)) + #setting breaks and title for x axis
  scale_y_continuous("") + #ditto for y axis
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='none') #a few other formatting tidbits to make plot pretty

# Deep hole time series for each reservoir
# Falling Creek
ggplot(subset(profiles_long, Reservoir=='FCR' & Site=="50"), aes(x = DateTime, y = value, col=Depth_m)) +
  geom_point(cex=2) +
  facet_grid(metric ~ ., scales='free') +
  scale_x_datetime("Date", date_breaks="2 months", date_labels = "%b %Y") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue") +
  ggtitle("Falling Creek Reservoir")

# Beaverdam
ggplot(subset(profiles_long, Reservoir=='BVR' & Site=="50"), aes(x = DateTime, y = value, col=Depth_m)) +
  geom_point(cex=2) +
  facet_grid(metric ~ Reservoir, scales='free') +
  scale_x_datetime("Date", date_breaks="2 months", date_labels = "%b %Y") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue") +
  ggtitle("Beaverdam Reservoir")

# # Carvins - at the moment we only have one day of PAR readings for CCR, so commenting out (06SEP18 MEL)
# ggplot(subset(profiles_long, Reservoir=='CCR' & Site=="50"), 
#        aes(x = DateTime, y = value, col=Depth_m)) +
#   geom_point(cex=2) +
#   facet_grid(metric ~ Reservoir, scales='free') +
#   scale_x_datetime("Date", date_breaks="1 month", date_labels = "%b %Y") +
#   scale_y_continuous("PAR (umol/m3/s)") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1)) +
#   scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue") + 
#   ggtitle("Carvins Cove Reservoir")
