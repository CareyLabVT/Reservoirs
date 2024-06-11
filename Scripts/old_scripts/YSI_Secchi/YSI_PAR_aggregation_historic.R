# Script to pull in YSI and PAR data from multiple reservoirs and years ####
 
#install.packages('pacman') ## Run this line if you don't have "pacman" package installed
pacman::p_load(tidyverse, lubridate) ## Use pacman package to install/load other packages

#### YSI Profiles ####

# Load all data files ending in "_Profiles.csv" and merge into a dataframe
raw_profiles <- dir(path = "./Data/DataAlreadyUploadedToEDI/CollatedDataForEDI/ProfilesData", pattern = "*_Profiles.csv") %>% 
  map_df(~ read_csv(file.path(path = "./Data/DataAlreadyUploadedToEDI/CollatedDataForEDI/ProfilesData", .), col_types = cols(.default = "c")))

profiles <- raw_profiles %>%
  # Rename columns if needed (TargetName = OriginalName)
  rename(Depth_m = Depth, DateTime = Date, PAR_umolm2s = PAR, Temp_C = YSI_TEMP_C, 
         DO_mgL = YSI_DO_mgL, Cond_uScm = YSI_COND_uScm, DOSat = YSI_PSAT,
         ORP_mV = YSI_ORP_mV) %>%
  filter(Depth_m != "888" & Depth_m != "777") %>%
  
  # Parse columns to target format
  mutate(DateTime = ymd_hms(DateTime), ## Force DateTime to be a yyyy-mm-dd hh:mm format
         Hour = hour(DateTime)) %>% 
  group_by(Reservoir, DateTime, Notes) %>% # columns not to parse to numeric
  mutate_if(is.character,funs(round(as.double(.), 2))) %>%  # parse all other columns to numeric
  
  # Move values from duplicated column names into target column
  mutate(Cond_uScm = ifelse(is.na(Cond_uScm), YSI_COND, Cond_uScm),  
         DO_mgL = ifelse(is.na(DO_mgL), YSI_DO, DO_mgL), 
         PAR_umolm2s = ifelse(is.na(PAR_umolm2s), YSI_PAR, PAR_umolm2s), 
         pH = ifelse(is.na(pH), YSI_pH, pH), 
         DOSat = ifelse(is.na(DOSat), YSI_DOSAT, DOSat), 
         Temp_C = ifelse(is.na(Temp_C), YSI_TEMP, Temp_C),
         Site = ifelse(!is.na(Site), Site, 
                       ifelse(Depth_m == 999, 100, 50)), # Add Site ID; 50 = deep hole/dam; 100 = inflow
         Depth_m = replace(Depth_m, Depth_m == 999, 0.1)) %>% 
  
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
  arrange(DateTime, Reservoir, Depth_m) 

# Write to CSV (using write.csv for now; want ISO format embedded?)
#write.csv(profiles, './Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/YSI_PAR_profiles.csv', row.names=F)
  
#### YSI diagnostic plots ####
profiles_long <- profiles %>%
  ungroup(.) %>%
  select(-(Flag_Temp:Notes)) %>%
  gather(metric, value, Temp_C:pH) %>% 
  mutate(year = year(DateTime))

# Plot ORP as a function of DO
ggplot(subset(profiles, Reservoir == "BVR" | Reservoir=="FCR"), aes(x = DO_mgL, y = ORP_mV, col = Reservoir)) + 
  geom_point() + 
  facet_grid(Reservoir ~., scales= 'free_y')

# Plot range of values per year for each reservoir; 
# annual mean value indicated with large black dot
ggplot(subset(profiles_long, Site == 50), aes(x = year, y = value, col=Reservoir)) +
  geom_point(size=1) +
  stat_summary(fun.y="mean", geom="point",pch=21,  size=3, fill='black') +
  facet_grid(metric ~ Reservoir, scales= 'free_y') +
  scale_x_continuous("Date", breaks=seq(2013,2017,1)) +
  scale_y_continuous("") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='none')

# Deep hole time series for each reservoir
# Falling Creek
ggplot(subset(profiles_long, Reservoir=='FCR' & Site=="50"), aes(x = DateTime, y = value, col=Depth_m)) +
  geom_point(cex=2) +
  facet_grid(metric ~ ., scales='free') +
  scale_x_datetime("Date", date_breaks="2 months", date_labels = "%b %Y") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue")

# FCR 2017 only; all sampling sites 
ggplot(subset(profiles_long, Reservoir=='FCR' & year =='2017'), aes(x = DateTime, y = value, col=Depth_m)) +
  geom_point(cex=2) +
  facet_grid(metric ~ Site, scales='free') +
  scale_x_datetime("Date", date_breaks="2 weeks", date_labels = "%d %b %Y") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue")

# Beaverdam
ggplot(subset(profiles_long, Reservoir=='BVR' & Site=="50"), aes(x = DateTime, y = value, col=Depth_m)) +
  geom_point(cex=2) +
  facet_grid(metric ~ Reservoir, scales='free') +
  scale_x_datetime("Date", date_breaks="2 months", date_labels = "%b %Y") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue")

# Gatewood
ggplot(subset(profiles_long, Reservoir=='GWR' & Site=="50"), aes(x = DateTime, y = value, col=Depth_m)) +
  geom_point(cex=2) +
  facet_grid(metric ~ Reservoir, scales='free') +
  scale_x_datetime("Date", date_breaks="2 months", date_labels = "%b %Y") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue")

# Carvins & Spring Hollow (PAR only)
ggplot(subset(profiles_long, (Reservoir=='CCR' | Reservoir == 'SHR') & Site=="50" & metric == 'PAR_umolm2s'), 
       aes(x = DateTime, y = value, col=Depth_m)) +
  geom_point(cex=2) +
  facet_grid(metric ~ Reservoir, scales='free') +
  scale_x_datetime("Date", date_breaks="1 month", date_labels = "%b %Y") +
  scale_y_continuous("PAR (umol/m3/s)") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue")
