# 2022 YSI and PAR QAQC/collation
# 30 Dec 2022

#install.packages('pacman') ## Run this line if you don't have "pacman" package installed
pacman::p_load(tidyverse, lubridate,dplyr) ## Use pacman package to install/load other packages

#### YSI Profiles ####

#NOTE - delete everything above new data read in for 2022 appendage
# read in file from last year (only need to do this to add sp cond and then rewrite to folder)
ysi_old <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021/YSI_PAR_profiles_2013-2021.csv")

#change DO %sat for fcr 100 15aug 2016 to NA because same as DO mg/L (and is wrong...) 
ysi_old$DOSat[ysi_old$Reservoir=="FCR" & ysi_old$Site==100 & as.Date(ysi_old$DateTime)=="2016-08-15"] <- NA

#and change B01 to B40 (new name for upstream pipe in BVR)
ysi_old$Site[ysi_old$Reservoir=="BVR" & ysi_old$Site==1] <- 40

write.csv(ysi_old, file.path(getwd(),'Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021/YSI_PAR_profiles_2013-2021_final.csv'), row.names=F)

#------------------------------------------------------------------------------#
#read in new data
raw_profiles <- read_csv(file.path("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2022/Data/2022_YSI_PAR_profiles.csv"))

#rename sp cond column because for some reason it changed...
names(raw_profiles)[names(raw_profiles) == 'SpCond_uScm'] <- 'Sp_cond_uScm'

#date format
raw_profiles$DateTime <- as.POSIXct(strptime(raw_profiles$DateTime, "%m/%d/%y %H:%M"))

#make depth numeric
raw_profiles$Depth_m <- as.numeric(raw_profiles$Depth_m)

#QAQC data + add flags
profiles <- raw_profiles %>%
  mutate(Flag_DateTime = ifelse(Notes=="There is no time recorded",1,0)) %>%
  select(!Notes) %>%
  group_by(Reservoir, DateTime) %>% # columns not to parse to numeric
  mutate_if(is.character,funs(round(as.double(.), 2))) %>%  # parse all other columns to numeric
  
  # Fix conductivity values >700 to be NA; instrument error that recorded pressure as cond
   mutate(Cond_uScm = ifelse(Cond_uScm > 700, NA, Cond_uScm)) %>%
  
  # Add 'flag' columns for each variable; 1 = flag for NA value
   mutate(Flag_pH = ifelse(is.na(pH), 1,
                          ifelse(pH > 14, 2, # Flag 2 = inst. malfunction
                                 ifelse(pH < 0, 3, 0))), #Flag 3 = below 0
         Flag_ORP = ifelse(is.na(ORP_mV), 1, 
                           ifelse(ORP_mV > 750, 2,  # Flag 2 = inst. malfunction
                                  ifelse(ORP_mV < 0, 3, 0))), #Flag 3 = below 0
         Flag_PAR = ifelse(is.na(PAR_umolm2s), 1,
                           ifelse(PAR_umolm2s < 0, 3, 0)), #Flag 3 = below 0
         Flag_Temp = ifelse(is.na(Temp_C), 1, 
                            ifelse(Temp_C > 35, 2, 0)), # Flag 2 = inst. malfunction
         Flag_DO = ifelse(is.na(DO_mgL), 1,
                          ifelse(DO_mgL > 70, 2, # Flag 2 = inst. malfunction
                                 ifelse(DO_mgL < 0, 3, 0))),  #Flag 3 = below 0
         Flag_DOSat = ifelse(is.na(DOSat), 1,
                             ifelse(DOSat > 200, 2, # Flag 2 = inst. malfunction
                                    ifelse(DO_mgL < 0, 3, 0))),  #Flag 3 = below 0
         Flag_Cond = ifelse(is.na(Cond_uScm), 1,
                            ifelse((Cond_uScm < 10 | Cond_uScm > 250), 2, # Flag 2 = inst. malfunction
                                   ifelse(DO_mgL < 0, 3, 0))),  #Flag 3 = below 0
         Flag_Sp_Cond = ifelse(is.na(Sp_cond_uScm), 1,
                               ifelse(DO_mgL > 250, 2,  # Flag 2 = inst. malfunction
                                      ifelse(DO_mgL < 0, 3, 0)))) %>%  #Flag 3 = below 0
         
  
  #set data for any 2 flags to NA and any 3 flags to 0
  mutate(pH = ifelse(Flag_pH == 2, NA, 
              ifelse(Flag_pH == 3, 0, paste0(pH))),
         ORP_mV = ifelse(Flag_ORP == 2, NA,
                  ifelse(Flag_ORP == 3, 0, ORP_mV)),
         Temp_C = ifelse(Flag_Temp == 2, NA, paste0(Temp_C)),
         PAR_umolm2s = ifelse(Flag_PAR == 3, 0, paste0(PAR_umolm2s)),
         DO_mgL = ifelse(Flag_DO == 2, NA, 
                  ifelse(Flag_DO == 3, 0, DO_mgL)),
         DOSat = ifelse(Flag_DOSat == 2, NA,
                 ifelse(Flag_DOSat == 3, 0, DOSat)),
         Cond_uScm = ifelse(Flag_Cond == 2, NA, 
                     ifelse(Flag_Cond == 3, 0, paste0(Cond_uScm))),
         Sp_cond_uScm = ifelse(Flag_Sp_Cond == 2, NA, 
                        ifelse(Flag_Sp_Cond == 3, 0, paste0(Sp_cond_uScm)))) %>%
  
  # Arrange order of columns for final data table
  select(Reservoir, Site, DateTime, Depth_m, Temp_C, DO_mgL, DOSat, 
         Cond_uScm, Sp_cond_uScm, PAR_umolm2s, ORP_mV, pH, Flag_DateTime, Flag_Temp, Flag_DO, Flag_DOSat,
         Flag_Cond, Flag_Sp_Cond, Flag_PAR, Flag_ORP, Flag_pH) %>%
  arrange(Reservoir, DateTime, Depth_m) 

# Write to CSV (using write.csv for now; want ISO format embedded?)
write.csv(profiles, file.path('./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2022/Data/YSI_PAR_profiles_2022_final.csv'), row.names=F)
  
#------------------------------------------------------------------------------#
#add column for year
profiles$year <- year(profiles$DateTime)

#### YSI diagnostic plots #### 
profiles_long <- profiles %>%
  ungroup(.) %>%
  select(-c(PAR_umolm2s:Flag_pH, Cond_uScm)) %>%
  gather(metric, value, c(Temp_C:DOSat,Sp_cond_uScm)) 

#value as numeric
profiles_long$value<- as.numeric(profiles_long$value)
profiles_long$Depth_m<- as.numeric(profiles_long$Depth_m)

# Plot ORP as a function of DO
ggplot(subset(profiles, Reservoir == "BVR" | Reservoir=="FCR"), aes(x = DO_mgL, y = ORP_mV, col = Reservoir)) + 
  geom_point() + 
  facet_grid(Reservoir ~., scales= 'free_y')

# Plot all values
ggplot(profiles_long, aes(x = DateTime, y = value, col=Reservoir)) +
  geom_point(size=1) +
  stat_summary(fun.y="mean", geom="point",pch=21,  size=3, fill='black') +
  facet_grid(metric ~ Reservoir, scales= 'free_y') +
  scale_x_datetime("Date", date_breaks="2 months", date_labels = "%b %Y") +
  scale_y_continuous("") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='none')

# Deep hole time series for each reservoir
ggplot(subset(profiles_long, Site=="50"), aes(x = DateTime, y = value, col=Depth_m)) +
  geom_point(cex=2) +
  facet_grid(metric ~ Reservoir, scales='free') +
  scale_x_datetime("Date", date_breaks="1 month", date_labels = "%b %Y") +
  scale_y_continuous("") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue")
ggsave(file.path("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2022/Figures/YSI_depths_2022.jpg"),width=3.5, height=4)

# FCR only; all sampling sites 
ggplot(subset(profiles_long, Reservoir=='FCR'), aes(x = DateTime, y = value, col=Depth_m)) +
  geom_point(cex=2) +
  facet_grid(metric ~ Site, scales='free') +
  scale_x_datetime("Date", date_breaks="2 months", date_labels = "%d %b") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue")
ggsave(file.path("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2022/Figures/FCR_YSIbySite_2022.jpg"),width=3.5, height=4)

#combine old and new ysi and secchi files here
ysi_old <- read_csv(file.path("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021/YSI_PAR_profiles_2013-2021_final.csv")) #only final because I had to modify the published dataset
ysi_new <- read_csv(file.path("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2022/Data/YSI_PAR_profiles_2022_final.csv")) 

ysi <- rbind(ysi_old,ysi_new)

# Arrange order of columns for final data table
ysi <- ysi %>% select(Reservoir, Site, DateTime, Depth_m, Temp_C, DO_mgL, DOSat, 
       Cond_uScm, Sp_cond_uScm, PAR_umolm2s, ORP_mV, pH, Flag_DateTime, Flag_Temp, Flag_DO, Flag_DOSat,
       Flag_Cond, Flag_Sp_Cond, Flag_PAR, Flag_ORP, Flag_pH) %>%
  arrange(Reservoir, DateTime, Depth_m) 

write.csv(ysi,file.path("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021/YSI_PAR_profiles_2013-2022.csv"), row.names=FALSE)

#### YSI diagnostic plots ####
ysi_long <- ysi %>% 
  gather(metric, value, Temp_C:pH) %>% 
  mutate(month = strftime(DateTime, "%b")) %>%
  mutate(DateTime = as.Date(DateTime))

# FCR only; all sampling sites 
ggplot(subset(ysi_long, Reservoir=='FCR'), aes(x = DateTime, y = value, col=Depth_m)) +
  geom_point(cex=2) +
  facet_grid(metric ~ Site, scales='free') +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue")

