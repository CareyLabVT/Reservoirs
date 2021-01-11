# Script to pull in YSI and PAR data from multiple reservoirs and years ####
# Updated 11Jan2021 HLW - deleting all -0.1m values 

#install.packages('pacman') ## Run this line if you don't have "pacman" package installed
pacman::p_load(tidyverse, lubridate) ## Use pacman package to install/load other packages

#### YSI Profiles ####

# Load all data files ending in "_Profiles.csv" and merge into a dataframe
raw_profiles <- dir(path = "/Users/heatherwander/Documents/VirginiaTech/research/Field data", pattern = "*_profiles.csv") %>% 
  map_df(~ read_csv(file.path(path = "/Users/heatherwander/Documents/VirginiaTech/research/Field data", .), col_types = cols(.default = "c")))

#date format
raw_profiles$DateTime <- as.POSIXct(strptime(raw_profiles$DateTime, "%m/%d/%y %H:%M"))

#make depth numeric
raw_profiles$Depth_m <- as.numeric(raw_profiles$Depth_m)

profiles <- raw_profiles %>%
  # Parse columns to target format
    # mutate(DateTime = ymd_hms(DateTime), ## Force DateTime to be a yyyy-mm-dd hh:mm format
  #       Hour = hour(DateTime)) %>% 
  group_by(Reservoir, DateTime, Notes) %>% # columns not to parse to numeric
  mutate_if(is.character,funs(round(as.double(.), 2))) %>%  # parse all other columns to numeric
  
  # Fix conductivity values >700 to be NA; instrument error that recorded pressure as cond
   mutate(Cond_uScm = ifelse(Cond_uScm > 700, NA, Cond_uScm)) %>%
  
  # Add 'flag' columns for each variable; 1 = flag for NA value
   mutate(Flag_pH = ifelse(is.na(pH), 1, 0),
         Flag_ORP = ifelse(is.na(ORP_mV), 1, 
                           ifelse(ORP_mV > 750, 2, 0)), # Flag 2 = inst. malfunction
         Flag_PAR = ifelse(is.na(PAR_umolm2s), 1, 0),
         Flag_Temp = ifelse(is.na(Temp_C), 1, 
                            ifelse(Temp_C > 35, 2, 0)), # Flag 2 = inst. malfunction
         Flag_DO = ifelse(is.na(DO_mgL), 1, 0),
         Flag_DOSat = ifelse(is.na(DOSat), 1,
                           ifelse(DOSat > 200, 2, 0)), # Flag 2 = inst. malfunction
         Flag_Cond = ifelse(is.na(Cond_uScm), 1,
                                  ifelse((Cond_uScm < 10 | Cond_uScm > 250), 2, 0))) %>% # Flag 2 = inst. malfunction
  
  # Arrange order of columns for final data table
  select(Reservoir, Site, DateTime, Depth_m, Temp_C, DO_mgL, DOSat, 
         Cond_uScm, PAR_umolm2s, ORP_mV, pH, Flag_Temp, Flag_DO, Flag_DOSat,
         Flag_Cond, Flag_PAR, Flag_ORP, Flag_pH, Notes) %>%
  arrange(Reservoir, DateTime, Depth_m) 

### delete -0.1 m data from dataframe
profiles <- profiles[profiles$Depth_m!=-0.1,]

# Write to CSV (using write.csv for now; want ISO format embedded?)
write.csv(profiles, 'YSI_PAR_profiles_2013-2019.csv', row.names=F)
  
#------------------------------------------------------------------------------#
#add column for year
profiles$year <- year(profiles$DateTime)

#### YSI diagnostic plots #### - NOTE: change year to visually QAQC data before publishing to EDI
profiles_long <- profiles %>%
  ungroup(.) %>%
  select(-(Cond_uScm:Notes)) %>%
  gather(metric, value, Temp_C:DOSat) 

#value as numeric
profiles_long$value<- as.numeric(profiles_long$value)
profiles_long$Depth_m<- as.numeric(profiles_long$Depth_m)

# Plot ORP as a function of DO
ggplot(subset(profiles, Reservoir == "BVR" | Reservoir=="FCR"), aes(x = DO_mgL, y = ORP_mV, col = Reservoir)) + 
  geom_point() + 
  facet_grid(Reservoir ~., scales= 'free_y')

# Plot all values
ggplot(subset(profiles_long,year==2019), aes(x = DateTime, y = value, col=Reservoir)) +
  geom_point(size=1) +
  stat_summary(fun.y="mean", geom="point",pch=21,  size=3, fill='black') +
  facet_grid(metric ~ Reservoir, scales= 'free_y') +
  scale_x_datetime("Date", date_breaks="2 months", date_labels = "%b %Y") +
  scale_y_continuous("") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='none')

# Deep hole time series for each reservoir
#jpeg("YSI_depths_2019.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(profiles_long, Site=="50" & year==2019), aes(x = DateTime, y = value, col=Depth_m)) +
  geom_point(cex=2) +
  facet_grid(metric ~ Reservoir, scales='free') +
  scale_x_datetime("Date", date_breaks="1 month", date_labels = "%b %Y") +
  scale_y_continuous("") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue")
#dev.off()

# FCR only; all sampling sites 
#jpeg("FCR_YSIbySite_2019.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(profiles_long, Reservoir=='FCR' & year==2019), aes(x = DateTime, y = value, col=Depth_m)) +
  geom_point(cex=2) +
  facet_grid(metric ~ Site, scales='free') +
  scale_x_datetime("Date", date_breaks="2 months", date_labels = "%d %b") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue")
#dev.off()



