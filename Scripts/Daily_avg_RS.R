# Title: Daily Avg. for sensor data for AI chla paper
# Author: Adrienne Breef-Pilz based on functions from VERA targets files
# Created: 11 Sept 2025
# Edited: 


# Load packages
# install the pacman package to load the other packages
install.packages("pacman")
pacman::p_load(tidyverse, rLakeAnalyzer) 

# Read in the data packages from EDI

# Read in data of the BVR streaming sensors from EDI
bvr <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/725/5/f649de0e8a468922b40dcfa34285055e")

# Read in data of the FCR streaming sensors from EDI
fcr <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/271/9/f23d27b67f71c25cb8e6232af739f986")

# Read in data from the FCR met station
met <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/389/9/62647ecf8525cdfc069b8aaee14c0478")


# FCR
# Select only the needed columns and get the daily average from 2019 - 2024. 
# The variables are Bottom DO % sat labeled 13, water temp from the EXO at 1.6m, Chla from the EXO at 1.6m, fDOM from the EXO at 1.6m. 

fcr_daily <- fcr|>
  filter(DateTime>= as.Date("2019-01-01"))|>
  select(DateTime, RDOsat_percent_9_adjusted, EXOChla_ugL_1, EXOfDOM_QSU_1, EXOTemp_C_1)|>
  mutate(Date = as.Date(DateTime))|>
  group_by(Date)|>
  # average for each column for each day
  summarise(across(c(RDOsat_percent_9_adjusted:EXOTemp_C_1), \(x) mean(x, na.rm = TRUE), .names = "FCR_mean_{.col}"))|>
  ungroup()


# BVR
# Select only the needed columns and get the daily average from 2020-2024
# The variables are bottom DO % sat labeled 9, water temp from the EXO at ~ 1.5m, Chla from the EXO at 1.5m, fDOM from the EXO at ~1.5m 

bvr_daily <- bvr|>
  filter(DateTime>= as.Date("2020-07-01"))|> # change where you want the data to start
  select(DateTime, RDOsat_percent_13, EXOChla_ugL_1.5, EXOfDOM_QSU_1.5, EXOTemp_C_1.5)|>
  mutate(Date = as.Date(DateTime))|>
  group_by(Date)|>
  # average for each column for each day
  summarise(across(c(RDOsat_percent_13:EXOTemp_C_1.5), \(x) mean(x, na.rm = TRUE), .names = "BVR_mean_{.col}"))|>
  ungroup()

# Weather data
# Get the daily average for air temp, shortwave radiation (is in incoming or out going?), wind speed. For rain get the daily sum

met_daily <- met|>
  filter(DateTime>= as.Date("2019-01-01"))|> # change where you want the data to start
  select(DateTime, ShortwaveRadiationUp_Average_W_m2, AirTemp_C_Average, WindSpeed_Average_m_s)|>
  mutate(Date = as.Date(DateTime))|>
  group_by(Date)|>
  # average for each column for each day
  summarise(across(c(ShortwaveRadiationUp_Average_W_m2:WindSpeed_Average_m_s), \(x) mean(x, na.rm = TRUE), .names = "mean_{.col}"))|>
  ungroup()

# Get the daily sum of the rain

met_daily_rain <- met|>
  filter(DateTime >= as.Date("2019-01-01"))|>
  select(DateTime, Rain_Total_mm)|>
  mutate(Date = as.Date(DateTime))|>
  group_by(Date)|>
  summarise(sum_Rain_Total_mm = sum(Rain_Total_mm))|>
  ungroup()%>%
  merge(met_daily,., by = "Date")


# Calculate the daily Schmidt stability for BVR and FCR

# Putting the function for calculating Schmidt Stability that Freya wrote here because I had to modify it because we don't have a current data file.Here is the original function from GitHub https://github.com/LTREB-reservoirs/vera4cast/blob/main/targets/target_functions/target_generation_SchmidtStability.R

generate_schmidt.stability <- function(historic_file) {
  
  #source('targets/target_functions/find_depths.R')
  
  source("https://raw.githubusercontent.com/LTREB-reservoirs/vera4cast/refs/heads/main/targets/target_functions/find_depths.R")

  
  # read in historical data file
  # EDI
  
 # historic_file = "https://pasta.lternet.edu/package/data/eml/edi/725/5/f649de0e8a468922b40dcfa34285055e"
 # historic_file <- "https://pasta.lternet.edu/package/data/eml/edi/271/9/f23d27b67f71c25cb8e6232af739f986"
  
  historic_df <- readr::read_csv(historic_file, show_col_types = F) |>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp'))
  
  if (historic_df$Reservoir[1] == 'BVR') {
    bvr_depths <- find_depths(data_file = historic_file,
                              depth_offset = "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv",
                              output = NULL,
                              date_offset = "2021-04-05",
                              offset_column1 = "Offset_before_05APR21",
                              offset_column2 = "Offset_after_05APR21") |>
      dplyr::filter(variable == 'ThermistorTemp') |>
      dplyr::select(Reservoir, DateTime, Depth_m, variable, depth_bin, Position) |>
      dplyr::rename(WaterLevel = Depth_m,
                    depth = depth_bin) 
      #dplyr::mutate(date = lubridate::as_date(DateTime))
    
    historic_df_1 <- historic_df  |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = c('variable','Position'),
                          names_sep = '_C_',
                          values_to = 'observation') |>
      dplyr::mutate(
        #date = lubridate::as_date(DateTime),
                    Position = as.numeric(Position)) |>
      na.omit() |>
      dplyr::left_join(bvr_depths,
                       by = c('Position', 'DateTime', 'Reservoir', 'variable'), multiple = 'all')|>
      dplyr::mutate(date = lubridate::as_date(DateTime))|>
      dplyr::group_by(date, Reservoir, depth) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       WaterLevel = mean(WaterLevel, na.rm=T),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 144/3, NA, observation)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
      na.omit()
    
  }
  
  # read in differently for FCR
  if (historic_df$Reservoir[1] == 'FCR') {
    historic_df_1 <- historic_df |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = 'depth',
                          names_prefix = 'ThermistorTemp_C_',
                          values_to = 'observation') |>
      na.omit()|>
      dplyr::mutate(date = lubridate::as_date(DateTime),
                    depth = ifelse(depth == 'surface', 0.1, depth)) |>
      na.omit() |>
      dplyr::group_by(date, Reservoir, depth) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 144/3, NA, observation)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
      na.omit()
    
  }
  message('EDI file ready')
  
  #combine the current and historic dataframes
  final_df <- dplyr::bind_rows(historic_df_1) |>
    dplyr::select(any_of(c('date', 'Reservoir', 'depth', 'observation', 'WaterLevel')))
  
  # Need bathymetry
  # infile2 <- tempfile()
  # try(download.file("https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184",
  #                   infile2, method="curl"))
  # if (is.na(file.size(infile2))) download.file("https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184",
  #                                              infile2,method="auto")
  
  bathymetry <- readr::read_csv("https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184", show_col_types = F)  |>
    dplyr::select(Reservoir, Depth_m, SA_m2) |>
    # dplyr::rename(depths = Depth_m,
    #               areas = SA_m2) |>
    dplyr::filter(Reservoir == unique(final_df$Reservoir))
  
  # BVR requires flexible bathymetry to generate schmidt stability
  if (final_df$Reservoir[1] == 'BVR') {
    
    #Create a dataframe with bathymetry at each date
    flexible_bathy <- final_df |> # takes the depth at each day
      dplyr::distinct(date, WaterLevel, Reservoir) |>
      dplyr::full_join(bathymetry, multiple = 'all', by = 'Reservoir')|> 
      dplyr::group_by(date) |>
      dplyr::mutate(Depth_m = Depth_m - (max(Depth_m) - mean(unique(WaterLevel))),
                    WaterLevel = mean(WaterLevel)) |>
      dplyr::filter(Depth_m>=0) |>
      dplyr::distinct()
    
  }
  
  if (final_df$Reservoir[1] == 'FCR') {
    flexible_bathy <- final_df|>
      dplyr::full_join(bathymetry, multiple = 'all', by = 'Reservoir')
  }
  
  
  #Calculate schmidt stability each day
  schmidts <- numeric(length(unique(final_df$date)))
  dates <- unique(final_df$date)
  
  for(i in 1:length(dates)) {
    baths <- flexible_bathy |>
      dplyr::filter(date==dates[i])
    
    temps <- final_df |>
      dplyr::filter(date == dates[i],
                    # cannot have an observation at a depth shallower than the
                    # shallowest bathymetry (returns NA below) so these are filtered out
                    depth >= min(baths$Depth_m))
    
    
    
    schmidts[i] <- rLakeAnalyzer::schmidt.stability(wtr = temps$observation,
                                                    depths = temps$depth,
                                                    bthA = baths$SA_m2,
                                                    bthD = baths$Depth_m,
                                                    sal = rep(0,length(temps$observation)))
    
    # print(i)
    
  }
  
  final_ss <- data.frame(datetime = unique(final_df$date),
                         #site_id = current_df$Reservoir[1],
                         depth_m = as.numeric(NA),
                         observation = schmidts,
                         variable = 'SchmidtStability_Jm2_mean',
                         duration = "P1D",
                         project_id = 'vera4cast') 
  # dplyr::mutate(site_id = ifelse(site_id == 'FCR',
  #                                'fcre',
  #                                ifelse(site_id == 'BVR',
  #                                       'bvre', NA)))
  ## Match data to flare targets file
  return(final_ss)
}

# Use the function from above to calculate Schmidt stability for FCR
fcr_schmidt <- generate_schmidt.stability(historic_file = "https://pasta.lternet.edu/package/data/eml/edi/271/9/f23d27b67f71c25cb8e6232af739f986")

# Clean up the data file to put it in the format we want 

fcr_ss <- fcr_schmidt|>
  select(datetime, observation)|>
  filter(datetime > as.Date("2018-12-31"))|>
  dplyr::rename(Date = datetime,
                FCR_mean_schmidt_stability = observation)

# Use the function to get schmidt stability

bvr_schmidt <- generate_schmidt.stability(historic_file = "https://pasta.lternet.edu/package/data/eml/edi/725/5/f649de0e8a468922b40dcfa34285055e")

# Clean up the data file to put it in the format we want 

bvr_ss <- bvr_schmidt|>
  select(datetime, observation)|>
  filter(datetime > as.Date("2020-06-30"))|>
  dplyr::rename(Date = datetime,
                BVR_mean_schmidt_stability = observation)

# combine all the data frames into one

final_datafile_all <- left_join(fcr_daily, fcr_ss, by = "Date")%>%
  left_join(.,bvr_daily, by = "Date")%>%
  left_join(., bvr_ss, by = "Date")%>%
  left_join(., met_daily_rain, by = "Date")

# make plots to check the data

diagnostic_plot <- final_datafile_all|>
  pivot_longer(!Date, names_to = "variable", values_to = "obs")%>%
  ggplot(., aes(x = Date, y = obs))+
  geom_point()+
  facet_wrap(vars(variable), scales = "free")+
  theme_bw()

# print the plot
diagnostic_plot

# write data file to a csv
write.csv(final_datafile_all, "FCR_BVR_Met_daily_obs_2019_2024.csv", row.names = F)
