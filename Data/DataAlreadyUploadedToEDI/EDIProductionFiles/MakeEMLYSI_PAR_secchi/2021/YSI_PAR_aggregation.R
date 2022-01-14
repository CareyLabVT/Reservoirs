# Script to pull in YSI and PAR data from multiple reservoirs and years ####
# Updated 11Jan2021 HLW and JHW

#install.packages('pacman') ## Run this line if you don't have "pacman" package installed
pacman::p_load(tidyverse, lubridate,dplyr) ## Use pacman package to install/load other packages

#### YSI Profiles ####

#NOTE - delete everything above new data read in for 2022 appendage
# read in file from last year (only need to do this to add sp cond and then rewrite to folder)
ysi_old <- read_csv(file.path("/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2020/YSI_PAR_profiles_2013-2020.csv"))

#read in sp cond data (appending a few values to 2019), change date format, drop first letter in site col
sp_cond <- read_csv(file.path("/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2019/2019_spcond_EDI.csv"))
sp_cond$date <- as.Date(sp_cond$DateTime, format="%m/%d/%y")
sp_cond$Site <- as.numeric(substring(sp_cond$Site, 2))
names(sp_cond)[5] = "Sp_cond_uScm"
sp_cond <- sp_cond[,-3]
sp_cond$Sp_cond_uScm <- round(sp_cond$Sp_cond_uScm,1)
sp_cond <- sp_cond[!is.na(sp_cond$Sp_cond_uScm),]

#find and delete dups
dups <- duplicated(sp_cond[,c(1:3,5)])
table(dups)["TRUE"]
sp_cond <- sp_cond[!dups,]

#fix the weird merging issue here by summarizing across first 4 cols
ysi_old <- ysi_old  %>% group_by(Reservoir, Site, DateTime, Depth_m) %>%
  summarize(across(c(Temp_C:pH), mean, na.rm=T))
ysi_old$date <- as.Date(ysi_old$DateTime)

#replace all nans with na
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

ysi_old[is.nan(ysi_old)] <- NA

#new df with datetime from ysi_old based on reservoir, site, depth, and date matching in both dfs
matching_dates <- merge(sp_cond, ysi_old, by=c("Reservoir","Site","date","Depth_m"))

#sort matching dates and sp_cond
matching_dates <- matching_dates %>% arrange(-desc(Reservoir),-desc(Site), -desc(date), -desc(Depth_m))
sp_cond <- sp_cond %>% arrange(-desc(Reservoir),-desc(Site), -desc(date), -desc(Depth_m))

#manually add the 2 sp cond values that don't line up to a sampling day
matching_dates[c(105:106),c(1,2,4,5,3)] <- sp_cond[c(38,102),]
matching_dates$DateTime[105] <- as.POSIXct(paste(matching_dates$date[105],"12:00:00"), format="%Y-%m-%d %H:%M:%S", tz="UTC")
matching_dates$DateTime[106] <- as.POSIXct(paste(matching_dates$date[106],"12:00:00"), format="%Y-%m-%d %H:%M:%S",tz="UTC")

#find the duplicated data
dups <- duplicated(matching_dates[,c(1:3,5)])
table(dups)["TRUE"]

#delete the duplicated data
matching_dates <- matching_dates[!dups,]

#now replace sp_cond date with matching_dates datetime
sp_cond$date <- matching_dates$DateTime

#change date to datetime for merging 
names(sp_cond)[5] <- "DateTime"

#now merge again to combine sp cond w/ correct dates to ysi_old 
ysi_old <-rbind(ysi_old, sp_cond)

#fix the weird merging issue here by summarizing across first 4 cols
ysi_old <- ysi_old  %>% group_by(Reservoir, Site, DateTime, Depth_m) %>%
  summarize(across(Temp_C:Sp_cond_uScm, mean, na.rm=T))

#replace all nans with na
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

ysi_old[is.nan(ysi_old)] <- NA


#add in flags
ysi_old <-  ysi_old %>% select(-date) %>%
            mutate(Flag_pH = ifelse(is.na(pH), 1, 0),
                    Flag_ORP = ifelse(is.na(ORP_mV), 1, 
                         ifelse(ORP_mV > 750, 2, 0)), # Flag 2 = inst. malfunction
                    Flag_PAR = ifelse(is.na(PAR_umolm2s), 1, 0),
                    Flag_Temp = ifelse(is.na(Temp_C), 1, 
                                       ifelse(Temp_C > 35, 2, 0)), # Flag 2 = inst. malfunction
                    Flag_DO = ifelse(is.na(DO_mgL), 1,
                                     ifelse(DO_mgL > 70, 2, 0)),  # Flag 2 = inst. malfunction
                    Flag_DOSat = ifelse(is.na(DOSat), 1,
                                        ifelse(DOSat > 200, 2, 0)), # Flag 2 = inst. malfunction
                    Flag_Cond = ifelse(is.na(Cond_uScm), 1,
                          ifelse((Cond_uScm < 10 | Cond_uScm > 250), 2, 0)), # Flag 2 = inst. malfunction
                   Flag_Sp_Cond = ifelse(is.na(Sp_cond_uScm), 1, 0), 
                   Flag_DateTime = ifelse(hour(DateTime)==12 & minute(DateTime)==0, 1,0)) %>%

  # Arrange order of columns for final data table
  select(Reservoir, Site, DateTime, Depth_m, Temp_C, DO_mgL, DOSat, 
         Cond_uScm, Sp_cond_uScm, PAR_umolm2s, ORP_mV, pH, Flag_DateTime, Flag_Temp, Flag_DO, Flag_DOSat,
         Flag_Cond, Flag_Sp_Cond, Flag_PAR, Flag_ORP, Flag_pH) %>%
  arrange(Reservoir, DateTime, Depth_m) 

write.csv(ysi_old, file.path(getwd(),'Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2020/YSI_PAR_profiles_2013-2020_final.csv'), row.names=F)

#------------------------------------------------------------------------------#
#read in new data
raw_profiles <- read_csv(file.path("/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021/2021_YSI_PAR_profiles.csv"))

#date format
raw_profiles$DateTime <- as.POSIXct(strptime(raw_profiles$DateTime, "%m/%d/%y %H:%M"))

#make depth numeric
raw_profiles$Depth_m <- as.numeric(raw_profiles$Depth_m)

profiles <- raw_profiles %>%
  # Parse columns to target format
    # mutate(DateTime = ymd_hms(DateTime), ## Force DateTime to be a yyyy-mm-dd hh:mm format
  #       Hour = hour(DateTime)) %>% 
  select(Reservoir:Flag_pH) %>%
  group_by(Reservoir, DateTime) %>% # columns not to parse to numeric
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
         Flag_DO = ifelse(is.na(DO_mgL), 1,
                          ifelse(DO_mgL > 70, 2, 0)),  # Flag 2 = inst. malfunction
         Flag_DOSat = ifelse(is.na(DOSat), 1,
                           ifelse(DOSat > 200, 2, 0)), # Flag 2 = inst. malfunction
         Flag_Cond = ifelse(is.na(Cond_uScm), 1,
                                  ifelse((Cond_uScm < 10 | Cond_uScm > 250), 2, 0)), # Flag 2 = inst. malfunction
         Flag_Sp_Cond = ifelse(is.na(Sp_cond_uScm), 1, 0), 
         Flag_DateTime = ifelse(hour(DateTime)==12 & minute(DateTime)==0, 1,0)) %>%
          
           
  # Arrange order of columns for final data table
  select(Reservoir, Site, DateTime, Depth_m, Temp_C, DO_mgL, DOSat, 
         Cond_uScm, Sp_cond_uScm, PAR_umolm2s, ORP_mV, pH, Flag_DateTime, Flag_Temp, Flag_DO, Flag_DOSat,
         Flag_Cond, Flag_Sp_Cond, Flag_PAR, Flag_ORP, Flag_pH) %>%
  arrange(Reservoir, DateTime, Depth_m) 

#remove empty rows at bottom
profiles <- profiles[!is.na(profiles$Reservoir) & !is.na(profiles$Site) & !is.na(profiles$DateTime),]

# Write to CSV (using write.csv for now; want ISO format embedded?)
write.csv(profiles, file.path(getwd(),'Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021/YSI_PAR_profiles_2021_final.csv'), row.names=F)
  
#------------------------------------------------------------------------------#
#add column for year
profiles$year <- year(profiles$DateTime)

#### YSI diagnostic plots #### 
profiles_long <- profiles %>%
  ungroup(.) %>%
  select(-(PAR_umolm2s:Flag_pH)) %>%
  gather(metric, value, Temp_C:Sp_cond_uScm) 

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
#jpeg("YSI_depths_2021.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(profiles_long, Site=="50"), aes(x = DateTime, y = value, col=Depth_m)) +
  geom_point(cex=2) +
  facet_grid(metric ~ Reservoir, scales='free') +
  scale_x_datetime("Date", date_breaks="1 month", date_labels = "%b %Y") +
  scale_y_continuous("") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue")
#dev.off()

# FCR only; all sampling sites 
#jpeg("FCR_YSIbySite_2021.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(profiles_long, Reservoir=='FCR'), aes(x = DateTime, y = value, col=Depth_m)) +
  geom_point(cex=2) +
  facet_grid(metric ~ Site, scales='free') +
  scale_x_datetime("Date", date_breaks="2 months", date_labels = "%d %b") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue")
#dev.off()

#combine old and new ysi and secchi files here
ysi_old <- read_csv(file.path("/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2020/YSI_PAR_profiles_2013-2020_final.csv")) #only final becuase I had to modify the published dataset
ysi_new <- read_csv(file.path("/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021/YSI_PAR_profiles_2021_final.csv")) 

ysi <- rbind(ysi_old,ysi_new)

#drop ysi row where depth is NA becuase can't find this in field data sheets
ysi <- ysi[!is.na(ysi$Depth_m),]

write.csv(ysi,file.path("/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021/YSI_PAR_profiles_2013-2021.csv"), row.names=FALSE)

#### YSI diagnostic plots ####
ysi_long <- ysi %>% 
  gather(metric, value, Temp_C:pH) %>% 
  mutate(month = strftime(DateTime, "%b")) %>%
  mutate(DateTime = as.Date(DateTime))

# FCR deep hole data time series plot
ggplot(subset(ysi_long, metric="DO_mgL"), aes(x=DateTime, y=value,color=as.factor(Depth_m))) +
  facet_wrap(~Reservoir) + geom_point(cex=2) + theme_bw()
