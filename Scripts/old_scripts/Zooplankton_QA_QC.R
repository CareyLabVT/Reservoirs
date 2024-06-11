# Preliminary QA/QC for zooplankton field data

pacman::p_load(tidyverse, hms, lubridate)

# load Jon's zoop data
raw_zoop <- read_csv('./Data/EDI_Zooplankton.csv')

# Set up for QA/QC
zoop <- raw_zoop %>% 
  
  # Eliminate rows with Density = 0
  filter(Density_IndPerL > 0) %>%
  
  # Parse columns to target format and number of digits
  mutate(DateTime = ymd_hms(DateTime),
         SampleTime = hms::as.hms(DateTime, tz = "UTC"),
         Density_IndPerL = round(Density_IndPerL, 2),
         MeanLength_mm = round(as.double(MeanLength_mm),3), 
         MeanWeight_ug = round(as.double(MeanWeight_ug),3),
         Biomass_ugL = round(as.double(Biomass_ugL), 3)) %>%
  
  # Add columns for Flags
  mutate(Flag_Length = ifelse(is.na(MeanLength_mm), 1,0),
         Flag_Weight = ifelse(is.na(MeanWeight_ug), 1, 0), 
         Flag_Biomass = ifelse(is.na(Biomass_ugL), 1, 0)) %>%
  
  # Arrange column order for final sheet
  select(Reservoir:DateTime, StartDepth_m:CollectionMethod ,Taxon, Density_IndPerL:Biomass_ugL, Flag_Length:Flag_Biomass)

# Write formatted data to csv
write.csv(zoop, './Formatted_Data/zooplankton.csv', row.names=F)
