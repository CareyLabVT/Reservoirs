library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)


setwd('~/Dropbox/chlorophyll_processing/data/')
chla_master_df <- read_csv('manual_chlorophyll_2014_2021')

snp <- filter(chla_master_df, Reservoir == "SNP")


#################################################################
########################## Plotting #############################
#################################################################


# Facetted plot of the datapoints within each reservoir over the entire dataset 

ggplot(subset(chla_master_df), aes(x = DateTime, y = Chla_ugL, col = Reservoir)) +
  geom_point(size = 1) + 
  facet_grid(Reservoir ~., scales = "free_y") + 
  ggtitle("Entire Dataset Timeseries")


# Altering dataset in order to plot stats 
chla_long_year <- chla_master_df %>% 
  ungroup(.) %>% 
  select(-(Flag_Chla)) %>% 
  gather(metric, value, Chla_ugL) %>% 
  mutate(year = year(DateTime)) %>% 
  mutate(month = month(DateTime))


# Facetted plot of the range of each reservoir for each year and the mean of the range
ggplot(subset(chla_long_year), aes(x = year, y = value, col = Reservoir))+ 
  geom_point(size = 1) + 
  stat_summary( fun.y = "mean", geom = "point", pch = 21, size = 3, fill = 'black') + 
  facet_grid(metric ~ Reservoir, scales = 'free_y') + 
  scale_x_continuous("DateTime", breaks = seq(2014, 2021, 1)) + 
  scale_y_continuous("Concentration (ugL)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') + 
  ggtitle("Range and Mean of Range of Each Reservoir Over Time")


# Facetted plot of the range of each reservoir for each year and the median of the range
ggplot(subset(chla_long_year, Site == 50), aes(x = year, y = value, col = Reservoir))+ 
  geom_point(size = 1) + 
  stat_summary( fun.y = "median", geom = "point", pch = 21, size = 3, fill = 'black') + 
  facet_grid(metric ~ Reservoir, scales = 'free_y') + 
  scale_x_continuous("DateTime", breaks = seq(2014,2021, 1)) + 
  scale_y_continuous("Concentration (ugL)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') + 
  ggtitle("Range and Median of Range of Each Reservoir Over Time")


jet.colors <- c("#00007F", "#00007F", "blue", "blue", "#007FFF", "cyan", "#7FFF7F", "#7FFF7F",
                "yellow","yellow", "#FF7F00", "#FF7F00", "red", "#7F0000")




ggplot(subset(chla_long_year, Reservoir == 'FCR' & Site == 50), aes(x = month, y = value, col = as.factor(Depth_m))) + 
  geom_point(cex = 2) + 
  facet_grid(year ~., scales = 'free_y') +
  scale_x_continuous("Month") + 
  scale_y_continuous("Concentration (ugL)") + 
  ggtitle("FCR Timeseries")

# ggplot(subset(chla_long_year, Reservoir == 'FCR' & year == 2019 & Depth_m == 0.1), aes(x = DateTime, y = value, col = as.factor(Site))) + 
#   geom_point() +
#   scale_y_continuous("Concentration (ugL)") +
#   ggtitle("FCR Multisite 0.1m (2019)")


ggplot(subset(chla_long_year, Reservoir == 'BVR' & Site == 50), aes(x = month, y = value, col = as.factor(Depth_m))) + 
  geom_point(cex = 2) + 
  facet_grid(year ~., scales = 'free_y') +
  scale_x_continuous("Month") + 
  scale_y_continuous("Concentration (ugL)") + 
  ggtitle ("BVR Timeseries")  

ggplot(subset(chla_long_year, Reservoir == 'CCR'), aes(x = month, y = value, col = as.factor(Depth_m))) + 
  geom_point(cex = 2) + 
  facet_grid(year ~., scales = 'free_y') +
  scale_x_continuous("Month") + 
  scale_y_continuous("Concentration (ugL)") + 
  ggtitle ("CCR Timeseries")  

ggplot(subset(chla_long_year, Reservoir == 'SNP'), aes(x = month, y = value, col = as.factor(Depth_m))) + 
  geom_point(cex = 2) + 
  facet_grid(year ~.) +
  scale_x_continuous("Month") + 
  scale_y_continuous("Concentration (ugL)") + 
  ggtitle ("SNP Timeseries")  


# ggplot(subset(chla_long_year, Reservoir == 'BVR' & year == 2019 & Depth_m == 0.1), aes(x = DateTime, y = value, col = as.factor(Site))) + 
#   geom_point() +
#   scale_y_continuous("Concentration (ugL)") +
#   ggtitle("BVR Multisite 0.1m (2019)")


# #2014 plots
# 
# ggplot(subset(chla_long_year, Site == 50 & year == 2014), aes(x = month, y = value, col = Reservoir))+ 
#   geom_point(size = 1) + 
#   stat_summary( fun.y = "mean", geom = "point", pch = 21, size = 3, fill = 'black') + 
#   facet_grid(metric ~ Reservoir, scales = 'free_y') + 
#   scale_x_continuous("Month", breaks = seq(1, 12, 1)) + 
#   scale_y_continuous("Concentration (ugL)") + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') + 
#   ggtitle ("Range and Mean of Range of 2014 Data")
# 
# 
# # Facetted plot of the range of each reservoir for each year and the median of the range
# ggplot(subset(chla_long_year, Site == 50 & year == 2014), aes(x = month, y = value, col = Reservoir))+ 
#   geom_point(size = 1) + 
#   stat_summary( fun.y = "median", geom = "point", pch = 21, size = 3, fill = 'black') + 
#   facet_grid(metric ~ Reservoir, scales = 'free_y') + 
#   scale_x_continuous("Month", breaks = seq(1, 12, 1)) + 
#   scale_y_continuous("Concentration (ugL)") + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
#   ggtitle ("Range and Median of Range of 2014 Data")
# 
# 
# 
# 
# ggplot(subset(chla_long_year, year == 2014), aes(x = DateTime, y = value, col = as.factor(Depth_m))) + 
#   geom_point() + 
#   facet_grid(Reservoir ~., scales = "free_y") + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle("2014 Timeseries")
# 
# 
# 
# # BVR plots 
# 
# bvr_data <- filter(chla_long_year, Reservoir == 'BVR')
# 
# 
# unique(bvr_data$Depth_m)
# 
# ggplot(subset(bvr_data,  year == 2015 & Depth_m == 0.0), aes(x = DateTime, y = value)) + geom_line() +
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2015 BVR at 0.0 m')
# 
# ggplot(subset(bvr_data,  year == 2015 &  Depth_m == 3.0), aes(x = DateTime, y = value)) + geom_line() +
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2015 BVR at 3.0 m')
# 
# ggplot(subset(bvr_data,  year == 2015 &  Depth_m == 6.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2015 BVR at 6.0 m')
# 
# ggplot(subset(bvr_data,  year == 2015 &  Depth_m == 9.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2015 BVR at 9.0 m ')
# 
# ggplot(subset(bvr_data,  year == 2015 &  Depth_m == 12.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2015 BVR at 12.0 m')
# 
# 
# 
# 
# 
# ggplot(subset(bvr_data,  year == 2016 &  Depth_m == 3.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2016 BVR at 3.0 m')
# 
# ggplot(subset(bvr_data,  year == 2016 &  Depth_m == 4.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2016 BVR at 4.0 m')
# 
# ggplot(subset(bvr_data,  year == 2016 &  Depth_m == 5.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") +
#   ggtitle('2016 BVR at 5.0 m')
# 
# ggplot(subset(bvr_data,  year == 2016 &  Depth_m == 6.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2016 BVR at 6.0 m')
# 
# ggplot(subset(bvr_data,  year == 2016 &  Depth_m == 7.0), aes(x = DateTime, y = value)) + geom_line() +
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2016 BVR at 7.0 m')
# 
# ggplot(subset(bvr_data,  year == 2016 &  Depth_m == 7.5), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2016 BVR at 7.5 m')
# 
# ggplot(subset(bvr_data,  year == 2016 &  Depth_m == 9.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2016 BVR at 9.0 m')
# 
# 
# 
# 
# 
# 
# ggplot(subset(bvr_data,  year == 2018 &  Depth_m == 0.1), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2018 BVR at 0.1 m')
# 
# ggplot(subset(bvr_data,  year == 2018 &  Depth_m == 3.0), aes(x = DateTime, y = value)) + geom_line() +
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2018 BVR at 3.0 m')
# 
# ggplot(subset(bvr_data,  year == 2018 &  Depth_m == 6.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2018 BVR at 6.0 m')
# 
# 
# 
# 
# 
# 
# 
# ggplot(subset(bvr_data,  year == 2019 &  Depth_m == 0.1 & Site == 50), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2019 BVR at 0.1 m')
# 
# ggplot(subset(bvr_data,  year == 2019 &  Depth_m == 6.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2019 BVR at 6.0 m')
# 
# ggplot(subset(bvr_data,  year == 2019 &  Depth_m == 9.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2019 BVR at 9.0 m')
# 
# 
# 
# 
# ccr_data <- filter(chla_long_year, Reservoir == 'CCR')
# 
# ggplot(subset(ccr_data, year == 2014 & Depth_m == 0.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2014 CCR at 0.0 m')
# 
# ggplot(subset(ccr_data, year == 2014 & Depth_m == 5.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2014 CCR at 5.0 m')
# 
# ggplot(subset(ccr_data, year == 2014 & Depth_m == 6.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2014 CCR at 6.0 m')
# 
# ggplot(subset(ccr_data, year == 2014 & Depth_m == 14.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2014 CCR at 14.0 m')
# 
# ggplot(subset(ccr_data, year == 2014 & Depth_m == 19.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2014 CCR at 18.0 m')
# 
# ggplot(subset(ccr_data, year == 2014 & Depth_m == 20.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2014 CCR at 20.0 m')
# 
# 
# 
# 
# 
# ggplot(subset(ccr_data, year == 2015 & Depth_m == 0.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2015 CCR at 0.0 m')
# 
# ggplot(subset(ccr_data, year == 2015 & Depth_m == 6.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2015 CCR at 6.0 m')
# 
# ggplot(subset(ccr_data, year == 2015 & Depth_m == 19.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2015 CCR at 19.0 m')
# 
# ggplot(subset(ccr_data, year == 2015 & Depth_m == 20.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2015 CCR at 20.0 m')
# 
# ggplot(subset(ccr_data, year == 2015 & Depth_m == 21.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2015 CCR at 21.0 m')
# 
# 
# 
# 
# 
# 
# ggplot(subset(ccr_data, year == 2018 & Depth_m == 0.1), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2018 CCR at 0.1 m')
# 
# ggplot(subset(ccr_data, year == 2018 & Depth_m == 6.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2018 CCR at 6.0 m')
# 
# ggplot(subset(ccr_data, year == 2018 & Depth_m == 12.0), aes(x = DateTime, y = value)) + geom_line() + 
#   scale_y_continuous("Concentration (ugL)") + 
#   ggtitle('2018 CCR at 12.0 m')
m