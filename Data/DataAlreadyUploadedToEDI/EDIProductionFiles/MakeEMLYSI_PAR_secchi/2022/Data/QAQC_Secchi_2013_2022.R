# 2022 Secchi QAQC/collation 
# 30 Dec 2022
 
#install.packages('pacman') ## Run this line if you don't have "pacman" package installed
pacman::p_load(tidyverse, lubridate) ## Use pacman package to install/load other packages

#### Secchi depths ####

# read in new data file
raw_secchi <- read_csv(file.path("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2022/Data/2022_secchi_depth.csv"))

#date format
raw_secchi$DateTime <- as.POSIXct(strptime(raw_secchi$DateTime, "%m/%d/%y %H:%M"))
raw_secchi$Flag_DateTime <- NA

secchi <- raw_secchi %>%
  # Omit rows where all Secchi values NA (e.g., rows from files with trailing ,'s)
  filter(!is.na(Secchi_m) ) %>%
  
  # Add 'flag' columns for each variable; 1 = flag 
  mutate(Flag_Secchi = ifelse(is.na(Secchi_m), 1, 0), # Flag for night sampling
         Flag_DateTime = ifelse(Notes=="No time was recorded",1,0))  %>%  
  
  # Arrange order of columns for final data table
  select(Reservoir, Site, DateTime, Secchi_m, Flag_DateTime, Flag_Secchi) %>%
  arrange(Reservoir, DateTime, .by_group = TRUE ) 

#replce NA in flag col with 0
secchi[is.na(secchi)] <- 0

# Write to CSV (using write.csv for now; want ISO format embedded?)
write.csv(secchi, file.path('./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2022/Data/2022_secchi_depth_final.csv'), row.names=F)
  
#------------------------------------------------------------------------------#
#### Secchi diagnostic plots #### 
secchi_long <- secchi %>%
  mutate(year = as.factor(year(DateTime)), day = yday(DateTime))

# Plot range of values per year for each reservoir; 
# annual mean value indicated with large black dot
ggplot(secchi_long, aes(x = year, y = Secchi_m, col=Reservoir)) +
  geom_point(size=1) +
  stat_summary(fun.y="mean", geom="point",pch=21,  size=3, fill='black') +
  facet_grid(. ~ Reservoir, scales= 'free_x') +
  scale_x_discrete("Date", breaks=seq(2013,2019,1)) +
  scale_y_continuous("Secchi depth (m)", breaks=seq(0,5,1), limits=c(0,5.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='none')

# All reservoirs time series 
ggplot(secchi_long, aes(x = DateTime, y = Secchi_m, col=Reservoir)) +
  geom_point(size=1) +
  facet_grid(. ~ Reservoir, scales= 'free_x') +
  scale_x_datetime("Date", date_breaks= "6 months", date_labels = "%b %Y") +
  scale_y_continuous("Secchi depth (m)", breaks=seq(0,5,1), limits=c(0,5.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='none')
#ggsave(file.path("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2022/Figures/Secchi_months.jpg"),width=3.5, height=4)


# Time series for each reservoir by julian day (see interannual varaibility)
#jpeg("Secchi_JulianDay.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(secchi_long, aes(x = day, y = Secchi_m)) +
  geom_point(size=2) + 
  facet_grid(Reservoir ~ ., scales= 'free_y') +
  scale_x_continuous("Julian day", limits=c(10,315), breaks=seq(50,300,50))+
  scale_y_reverse("Secchi depth (m)") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='bottom')
ggsave(file.path("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2022/Figures/Secchi_JulianDay.jpg"),width=3.5, height=4)

secchi_old <- read_csv(file.path("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021/Secchi_depth_2013-2021.csv"))
secchi_new <- read_csv(file.path("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2022/Data/2022_secchi_depth_final.csv"))

#merge last year's EDI secchi data with new data from this year
secchi <- rbind(secchi_old,secchi_new)

# Arrange order of columns for final data table
secchi <- secchi %>% select(Reservoir, Site, DateTime, Secchi_m, Flag_DateTime, Flag_Secchi) %>%
  arrange(Reservoir, DateTime) 

#add units to flag column for final df
names(secchi)[6] <- "Flag_Secchi_m" 

write.csv(secchi,file.path("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2022/Data/Secchi_depth_2013-2022.csv"), row.names=FALSE)

#### secchi diagnostic plots ####
secchi_long <- secchi %>% 
  gather(metric, value, Secchi_m) %>% 
  mutate(month = strftime(DateTime, "%b")) %>%
  mutate(DateTime = as.Date(DateTime))

ggplot(secchi_long, aes(x=DateTime, y=value )) +
  facet_wrap(~Reservoir) + geom_point(cex=2) + theme_bw()
