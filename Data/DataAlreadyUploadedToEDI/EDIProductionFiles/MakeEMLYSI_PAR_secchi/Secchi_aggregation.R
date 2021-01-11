# Script to pull in Secchi data from multiple reservoirs and years ####
 
#install.packages('pacman') ## Run this line if you don't have "pacman" package installed
pacman::p_load(tidyverse, lubridate) ## Use pacman package to install/load other packages

#### Secchi depths ####

# Load all data files ending in "_Secchi.csv" and merge into a dataframe
raw_secchi <- dir(path = "/Users/heatherwander/Documents/VirginiaTech/research/Field data", pattern = "*_Secchi.csv") %>% 
  map_df(~ read_csv(file.path(path = "/Users/heatherwander/Documents/VirginiaTech/research/Field data", .)))

#date format
raw_secchi$DateTime <- as.POSIXct(strptime(raw_secchi$DateTime, "%m/%d/%y %H:%M"))

secchi <- raw_secchi %>%
  # Omit rows where all Secchi values NA (e.g., rows from files with trailing ,'s)
  filter(!is.na(Secchi_m) ) %>%
  
  # Add 'flag' columns for each variable; 1 = flag 
  mutate(Flag_Secchi = ifelse(is.na(Secchi_m), 1, 0))  %>%  # Flag for night sampling
  
  # Arrange order of columns for final data table
  select(Reservoir, Site, DateTime, Secchi_m, Flag_Secchi) %>%
  arrange(Reservoir, DateTime, .by_group = TRUE ) 

# Write to CSV (using write.csv for now; want ISO format embedded?)
write.csv(secchi, 'Secchi_depth_2013-2019.csv', row.names=F)
  
#------------------------------------------------------------------------------#
#add column for year
profiles$year <- year(profiles$DateTime)

#### Secchi diagnostic plots #### - NOTE: change year to visually QAQC data before publishing to EDI
secchi_long <- secchi %>%
  mutate(year = as.factor(year(DateTime)), day = yday(DateTime))

# Plot range of values per year for each reservoir; 
# annual mean value indicated with large black dot
ggplot(subset(secchi_long, year==2019), aes(x = year, y = Secchi_m, col=Reservoir)) +
  geom_point(size=1) +
  stat_summary(fun.y="mean", geom="point",pch=21,  size=3, fill='black') +
  facet_grid(. ~ Reservoir, scales= 'free_x') +
  scale_x_discrete("Date", breaks=seq(2013,2019,1)) +
  scale_y_continuous("Secchi depth (m)", breaks=seq(0,15,3), limits=c(0,15)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='none')

# All reservoirs time series 
#jpeg("Secchi_months.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(secchi_long, year==2019), aes(x = DateTime, y = Secchi_m, col=Reservoir)) +
  geom_point(size=1) +
  facet_grid(. ~ Reservoir, scales= 'free_x') +
  scale_x_datetime("Date", date_breaks= "6 months", date_labels = "%b %Y") +
  scale_y_continuous("Secchi depth (m)", breaks=seq(0,15,3), limits=c(0,15)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='none')
#dev.off()

# Time series for each reservoir by julian day (see interannual varaibility)
#jpeg("Secchi_JulianDay.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(secchi_long, year==2019), aes(x = day, y = Secchi_m, col=year)) +
  geom_point(size=2) + 
  facet_grid(Reservoir ~ ., scales= 'free_y') +
  scale_x_continuous("Julian day", limits=c(10,315), breaks=seq(50,300,50))+
  scale_y_continuous("Secchi depth (m)") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='bottom')
