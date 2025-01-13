### file for generating ice binary L1 file ## 
# taken from the catwalk data
#source('Data/DataNotYetUploadedToEDI/Ice_binary/ice_binary_targets_function.R')


### THIS IS ONE OF TWO FILES FOR ICE COVER ###
# THIS FILE IS MEANT TO SHOW JUST THE DAYS WHEN ICE COVER CHANGES (VISUAL OR CALCULATED)
# THE OTHER FILE WILL INCLUDE ALL DAYS

## current and historic = catwalk

daily_ice_cover_binary <- function(current_file, historic_wq_file, historic_file, maint_log = NULL, ice_site){
  
  ## read in current data file
  # Github, Googlesheet, etc.
  
  current_wq_df <- readr::read_csv(current_file, show_col_types = F)|>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp')) |>
    tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                        names_to = 'depth',
                        names_prefix = 'ThermistorTemp_C_',
                        values_to = 'observation') |>
    dplyr::mutate(datetime = lubridate::as_datetime(paste0(format(DateTime, "%Y-%m-%d %H"), ":00:00"))) |>
    dplyr::group_by(Reservoir, datetime, depth) |>
    dplyr::summarise(observation = mean(observation, na.rm = T),
                     .groups = 'drop') |>
    dplyr::mutate(site_id = ifelse(Reservoir == 'FCR',
                                   'fcre',
                                   ifelse(Reservoir == 'BVR',
                                          'bvre', NA)))
  #dplyr::rename(site_id = Reservoir)
  
  # the depths used to assess will change depending on the current depth of FCR
  depths_use_current <- current_wq_df |>
    dplyr::mutate(depth = ifelse(depth == "surface", 0.1, depth)) |>
    na.omit() |>
    dplyr::group_by(datetime) |>
    dplyr::summarise(top = min(as.numeric(depth)),
                     bottom = max(as.numeric(depth))) |>
    tidyr::pivot_longer(cols = top:bottom,
                        values_to = 'depth')
  
  
  current_ice_df <- current_wq_df |>
    dplyr::mutate(depth = as.numeric(ifelse(depth == "surface", 0.1, depth))) |>
    dplyr::right_join(depths_use_current, by = c('datetime', 'depth')) |>
    dplyr::select(-depth) |>
    tidyr::pivot_wider(names_from = name,
                       values_from = observation) |>
    # Ice defined as when the top is cooler than the bottom, and temp below 4 oC
    dplyr::mutate(temp_diff = top - bottom,
                  ice_check = ifelse(temp_diff < -0.1 & top <= 4, 1, 0), 
                  datetime = as.Date(datetime)) |>
    dplyr::group_by(datetime) |> 
    dplyr::mutate(ice_presence = ifelse(1 %in% ice_check, 1, 0),
                  ice_hour_count = sum(ice_check == 1),
                  IceOn_hour_index = list(which(ice_check == 1))) |> 
    ungroup() |> 
    distinct(site_id, datetime, ice_presence, .keep_all = TRUE) |> 
    select(Reservoir, site_id, datetime, ice_presence) |> 
    dplyr::mutate(Method = 'T')
  
  message('Current file ready')
  
  
  
  ## historic wq data 
  historic_wq_df <- read_csv(historic_wq_file) |> 
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp')) |>
    tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                        names_to = 'depth',
                        names_prefix = 'ThermistorTemp_C_',
                        values_to = 'observation') |>
    dplyr::mutate(datetime = lubridate::as_datetime(paste0(format(DateTime, "%Y-%m-%d %H"), ":00:00"))) |> 
    dplyr::filter(datetime > lubridate::as_datetime('2018-07-05')) |> ## hard cut-off from WVWA
    dplyr::mutate(site_id = ifelse(Reservoir == 'FCR',
                                   'fcre',
                                   ifelse(Reservoir == 'BVR',
                                          'bvre', NA)))
  
  depths_use_historic <- historic_wq_df |>
    dplyr::mutate(depth = ifelse(depth == "surface", 0.1, depth)) |>
    na.omit() |>
    dplyr::group_by(datetime) |>
    dplyr::summarise(top = min(as.numeric(depth)),
                     bottom = max(as.numeric(depth))) |>
    tidyr::pivot_longer(cols = top:bottom,
                        values_to = 'depth')
  
  historic_wq_ice_df <- historic_wq_df |>
    dplyr::mutate(depth = as.numeric(ifelse(depth == "surface", 0.1, depth))) |>
    dplyr::right_join(depths_use_historic, by = c('datetime', 'depth')) |>
    dplyr::select(-depth) |>
    tidyr::pivot_wider(names_from = name,
                       values_from = observation) |>
    # Ice defined as when the top is cooler than the bottom, and temp below 4 oC
    dplyr::mutate(temp_diff = top - bottom,
                  ice_check = ifelse(temp_diff < -0.1 & top <= 4, 1, 0), 
                  datetime = as.Date(datetime)) |>
    dplyr::group_by(datetime) |> 
    dplyr::mutate(ice_presence = ifelse(1 %in% ice_check, 1, 0)) |> 
    ungroup() |> 
    distinct(site_id, datetime, .keep_all = TRUE) |> 
    select(Reservoir, site_id, datetime, ice_presence) |> 
    dplyr::mutate(Method = 'T')
  
  # read in historical data file
  # EDI
  infile <- tempfile()
  try(download.file(historic_file, infile, method="curl"))
  if (is.na(file.size(infile))) download.file(historic_file,infile,method="auto")
  
  historic_ice_df <- readr::read_csv(infile, show_col_types = F) |>
    dplyr::mutate(site_id = ifelse(Reservoir == 'FCR', 'fcre',
                                   ifelse(Reservoir == 'BVR', 'bvre', NA))) |>
    dplyr::filter(site_id == current_wq_df$site_id[1], 
                  Date < '2018-07-05') |> #### hard cut-off from WVWA
    dplyr::select(Reservoir, site_id, datetime = Date, ice_presence = IceOn, Method)
  
  # combine available data and then join onto daily dataframe
  if (nrow(historic_ice_df) == 0){
    combined_df <- dplyr::bind_rows(historic_wq_ice_df, current_ice_df)
  } else{
  combined_df <- dplyr::bind_rows(historic_ice_df, historic_wq_ice_df, current_ice_df)
  }
  
  ice_df_build <- data.frame(datetime = seq.Date(min(as.Date(combined_df$datetime)), 
                                                 max(as.Date(combined_df$datetime)), 
                                                 by = 'day'))
  
  reservoir_name <- combined_df$Reservoir[1]
  site_identifier <- combined_df$site_id[1]
  
  daily_ice_df <- ice_df_build |> 
    dplyr::full_join(combined_df, by = c('datetime')) |> 
    dplyr::mutate(Reservoir = ifelse(is.na(Reservoir), reservoir_name, Reservoir), 
                  ice_presence = ifelse(is.na(ice_presence), zoo::na.locf(ice_presence), ice_presence),
                  Method = ifelse(is.na(Method), 'T', Method), 
                  Flag = 0) |> 
    dplyr::rename(Date = datetime)
  
  
  if(!is.null(maint_log)){ # only run this file if the maint file argument is non-null
    
    # ## ADD MAINTENANCE LOG FLAGS (manual edits to the data for suspect samples or human error)
    
    log_read <- gsheet::gsheet2tbl(maint_log) |> 
      filter(Reservoir == ice_site) |> 
      select(Reservoir, Date = DateTime, Maint_Ice_Presence = Ice_Presence, Flag_Ice_Presence)
    
    daily_ice_df <- daily_ice_df |> 
      full_join(log_read, by = c('Date', 'Reservoir')) |> 
      dplyr::mutate(ice_presence = ifelse((!is.na(Maint_Ice_Presence) & (ice_presence != Maint_Ice_Presence)), Maint_Ice_Presence, ice_presence),
                    Method = ifelse(!is.na(Maint_Ice_Presence), 'V', Method),
                    Flag = ifelse(is.na(Flag_Ice_Presence), 0, Flag_Ice_Presence)) |> 
      select(-Maint_Ice_Presence, -Flag_Ice_Presence, -site_id)
    
  }
  
  message('EDI file ready')
  
  
  ## return dataframe formatted to match FLARE targets
  return(daily_ice_df)
}

## USE CODE BELOW TO TEST FUNCTION ##

# current_files <- c("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv",
#                    "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")
# 
# historic_wq_files <- c('https://pasta.lternet.edu/package/data/eml/edi/725/4/9adadd2a7c2319e54227ab31a161ea12',
#                        'https://pasta.lternet.edu/package/data/eml/edi/271/8/fbb8c7a0230f4587f1c6e11417fe9dce')
# 
# historic_ice_files <- c("https://pasta.lternet.edu/package/data/eml/edi/456/5/ebfaad16975326a7b874a21beb50c151")
# 
# ice_maintenance_log <- c('https://docs.google.com/spreadsheets/d/1viYhCGs3UgstzHEWdmP2Ig6uxyNM3ZC_uisG_R0QNpI/edit?gid=0#gid=0')
# 
# 
# bvr_ice_data <- daily_ice_cover_binary(current_file = current_files[1],
#                                             historic_wq_file = historic_wq_files[1],
#                                             historic_file = historic_ice_files,
#                                        maint_log = NULL,
#                                        ice_site = 'BVR')
# 
# fcr_ice_data <- daily_ice_cover_binary(current_file = current_files[2],
#                                             historic_wq_file = historic_wq_files[2],
#                                             historic_file = historic_ice_files,
#                                        maint_log = ice_maintenance_log,
#                                        ice_site = 'FCR')
# 
# combined_ice_data <- dplyr::bind_rows(bvr_ice_data, fcr_ice_data)
