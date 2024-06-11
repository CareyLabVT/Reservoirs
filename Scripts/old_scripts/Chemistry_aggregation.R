# Script to pull in water chemistry data from multiple reservoirs and years ####

#install.packages('pacman')
pacman::p_load(tidyverse, lubridate)

#### In-lake and inflow water chemistry data ####
# Load .csv files whose file names end in _Chemistry
raw_chem <- dir(path = "./Data/DataAlreadyUploadedToEDI/CollatedDataForEDI/ChemistryData", pattern = "*_Chemistry.csv") %>% 
  map_df(~ read_csv(file.path(path = "./Data/DataAlreadyUploadedToEDI/CollatedDataForEDI/ChemistryData", .), col_types = cols(.default = "c")))

chemistry <- raw_chem %>%
  # Rename columns if needed (TargetName = OriginalName)
  rename(Depth_m = Depth, DateTime = Date) %>%
  
  # Parse columns to target format
  mutate(DateTime = ymd_hms(DateTime)) %>%
  group_by(Reservoir, DateTime, Notes) %>% # columns not to parse to numeric
  mutate_if(is.character,funs(as.double(.))) %>%  # parse all other columns to numeric
  
  # Move values from duplicated column names into target column
  mutate(NO3NO2_ugL = ifelse(is.na(NO3NO2_ugL), NO3_ugL, NO3NO2_ugL),  
         SRP_ugL = ifelse(is.na(SRP_ugL), PO4_ugL, SRP_ugL), 
         `OI DOC Conc (ppm)` = ifelse(is.na(`OI DOC Conc (ppm)`), `OI TOC Conc (ppm)`,`OI DOC Conc (ppm)`),
         DOC_OIAnalytical_mgL = ifelse(Year < 2016, DOC_mgL,
                                       ifelse(Year == 2016, `OI DOC Conc (ppm)`, NA)),
         `Vario DOC Mean of 3 reps` = ifelse(is.na(`Vario DOC Mean of 3 reps`), `Vario TOC Mean of 3 reps`,`Vario DOC Mean of 3 reps`),
         DOC_VarioDOC_mgL = ifelse(Year == 2016, `Vario DOC Mean of 3 reps`,
                                          ifelse(Year > 2016, DOC_mgL, NA)), 
         DOC_mgL = ifelse(is.na(DOC_OIAnalytical_mgL),DOC_VarioDOC_mgL,DOC_OIAnalytical_mgL),
         Site = ifelse(Depth_m != 999, Site, 100), # Add site ID; 50 = deep hole; 100 = inflow; 20 = upstream FCR
         Site = ifelse(is.na(Site),50,Site),
         Depth_m = replace(Depth_m, Depth_m == 999, 0.1), # Set depth of Inflow samples
         Depth_m = replace(Depth_m, Depth_m == 0, 0.1)) %>% # Update 0m chem depths to 0.1m

  # Round values to specified precision
  mutate(TP_ugL = round(TP_ugL, 1), 
         TN_ugL = round(TN_ugL, 1), 
         NH4_ugL = round(NH4_ugL, 0),
         NO3NO2_ugL = round(NO3NO2_ugL, 0),
         SRP_ugL = round(SRP_ugL, 0),
         DOC_mgL = round(DOC_mgL, 1)) %>%
  
  # Add 'flag' columns for each analyte; 3 = concentration below detection; 
  # 4 = negative value set to 0
  mutate(Flag_TP = ifelse(is.na(TP_ugL), 0, 
                          ifelse((TP_ugL < 1), 3, ifelse((TP_ugL < 0), 4, 0))), # Flag TP values <1 ug/L
         Flag_TN = ifelse(is.na(TN_ugL), 0,
                          ifelse(TN_ugL < 1, 3, ifelse((TN_ugL < 0), 4, 0))), 
         Flag_NH4 = ifelse(is.na(NH4_ugL), 0, 
                           ifelse(NH4_ugL < 1, 3, ifelse((NH4_ugL < 0), 4, 0))),
         Flag_NO3NO2 = ifelse(is.na(NO3NO2_ugL), 0,
                              ifelse(NO3NO2_ugL < 1, 3, ifelse((NO3NO2_ugL < 0), 4, 0))),
         Flag_SRP = ifelse(is.na(SRP_ugL), 0,
                           ifelse(SRP_ugL < 1, 3, ifelse((SRP_ugL < 0), 4, 0))),
         Flag_DOC = ifelse(is.na(DOC_mgL), 0,
                           ifelse(DOC_mgL < 1, 3, ifelse((DOC_mgL < 0), 4, 0)))) %>%
  
  # Set negative concentrations to 0
  mutate(TP_ugL = ifelse(TP_ugL < 0, 0, TP_ugL), 
         TN_ugL = ifelse(TN_ugL < 0, 0, TN_ugL), 
         NH4_ugL = ifelse(NH4_ugL < 0, 0, NH4_ugL),
         NO3NO2_ugL = ifelse(NO3NO2_ugL < 0, 0, NO3NO2_ugL),
         SRP_ugL = ifelse(SRP_ugL < 0, 0, SRP_ugL),
         DOC_mgL = ifelse(DOC_mgL < 0, 0, DOC_mgL)) %>%
  
  # Omit rows where all analytes NA (e.g., rows that only had infTPloads_g)
  filter(!is.na(TN_ugL) | !is.na(TP_ugL) | !is.na(NH4_ugL) | !is.na(NO3NO2_ugL) |
           !is.na(SRP_ugL) | !is.na(DOC_mgL)) %>%
  
  # Arrange order of columns for final data table
  select(Reservoir, Site, DateTime, Depth_m, TN_ugL, TP_ugL, # reorder columns by name
         NH4_ugL, NO3NO2_ugL, SRP_ugL, DOC_mgL, 
         Flag_TN, Flag_TP, Flag_NH4, Flag_NO3NO2, Flag_SRP, Flag_DOC, Notes) %>%
  arrange(Reservoir, DateTime, Depth_m) 
  
# Write to CSV
#write.csv(chemistry, './Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLCHemistry/chemistry.csv', row.names=F)

#### Chemistry diagnostic plots ####
chemistry_long <- chemistry %>% 
  ungroup(.) %>%
  select(-(Flag_TN:Notes)) %>%
  gather(metric, value, TN_ugL:DOC_VarioDOC_mgL) %>% 
  mutate(year = year(DateTime))

# Plot range of values per constituent per year for each reservoir; 
  # annual mean value indicated with large black dot
ggplot(subset(chemistry_long, Site == 50), aes(x = year, y = value, col=Reservoir)) +
  geom_point(size=1) +
  stat_summary(fun.y="mean", geom="point",pch=21,  size=3, fill='black') +
  facet_grid(metric ~ Reservoir, scales= 'free_y') +
  scale_x_continuous("Date", breaks=seq(2013,2017,1)) +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='none')

# FCR deep hole data time series
jet.colors <- c("#00007F", "#00007F", "blue", "blue", "#007FFF", "cyan", "#7FFF7F", "#7FFF7F",
                                 "yellow","yellow", "#FF7F00", "#FF7F00", "red", "#7F0000")

my_cols <- c('#a50026', '#d73027', '#f46d43', '#fdae61', '#fee090', '#ffffbf', '#ffffbf',
             '#e0f3f8','#e0f3f8', '#abd9e9', '#abd9e9', '#74add1','#4575b4', '#313695')

ggplot(subset(chemistry_long, Reservoir=='FCR' & Site=="50"), aes(x = DateTime, y = value, col=as.factor(Depth_m))) +
  geom_point(cex=2) +
  facet_grid(metric ~ ., scales='free') +
  scale_x_datetime("Date", date_breaks="2 months", date_labels = "%b %Y") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_manual("Depth (m)", values = jet.colors)

# Non-FCR reservoir deep hole data time series
ggplot(subset(chemistry_long, Reservoir!='FCR' & Site=="50"), aes(x = DateTime, y = value, fill=as.factor(Depth_m))) +
  geom_point(pch=21, col='black') +
  facet_grid(metric ~ Reservoir, scales='free') +
  scale_x_datetime("Date", date_breaks="4 months", date_labels = "%b %Y") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_fill_discrete(name="Depth (m)")
