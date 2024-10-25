### file for generating ice binary L1 file ## 
# taken from the catwalk data
#source('Data/DataNotYetUploadedToEDI/Ice_binary/ice_binary_targets_function.R')


### THIS IS ONE OF TWO FILES FOR ICE COVER ###
# THIS FILE IS MEANT TO SHOW JUST THE DAYS WHEN ICE COVER CHANGES (VISUAL OR CALCULATED)
# THE OTHER FILE WILL INCLUDE ALL DAYS

## current and historic = catwalk

target_IceTransition_binary <- function(current_file, historic_wq_file, historic_file, ice_site, maint_log = NULL){
  
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
                  variable = ifelse(temp_diff < -0.1 & top <= 4, 'IceOn', 'IceOff'), 
                  IceOn_hourly = ifelse(variable == "IceOn", 1, 0), 
                  IceOff_hourly = ifelse(variable == "IceOff", 1, 0), 
                  datetime = as.Date(datetime)) |>
    dplyr::group_by(datetime) |> 
    dplyr::mutate(IceOn = ifelse(1 %in% IceOn_hourly, 1, 0),
                  IceOff = ifelse(1 %in% IceOn_hourly, 0, 1)) |> 
    ungroup() |> 
    distinct(site_id, datetime, .keep_all = TRUE) |> 
    select(Reservoir, site_id, datetime, IceOn, IceOff) |> 
    dplyr::filter(IceOn != dplyr::lag(IceOn))  |> # only select rows where ice condition changes
    dplyr::mutate(Site = 50, 
                  Year = lubridate::year(datetime), 
                  DayOfYear = lubridate::yday(datetime),
                  Method = 'T', 
                  Flag_IceValue = 0) |> 
    select(Reservoir, Site, Date = datetime, Year, DayOfYear, IceOn, IceOff, Method, Flag_IceValue, site_id)
  
  
  ### rename and add columns to match EDI file ##
    
    
  #   #surface cooler than bottom (within error (0.1) of the sensors)
  #   dplyr::select(datetime, site_id, variable) |>
  #   tidyr::pivot_longer(cols = variable,
  #                       names_to = 'variable',
  #                       values_to = 'observation')
  # 
  # #when does the value change from ice on to off? (vice versa)
  # rle_ice <- rle(current_on_off$observation)
  # 
  # current_ice_df <- data.frame(variable = rle_ice$values, # this is whether the ice is on or off
  #                              length = rle_ice$lengths) |>  # how long is the run of this condition?
  #   dplyr::mutate(end_n = cumsum(length), # cumsum is the index of the end of each run
  #                 start_n = (end_n - length+1),# this the index of the start of the run
  #                 datetime = lubridate::as_date(current_on_off$datetime[start_n])) |>  # relates this the date
  #   dplyr::select(variable, datetime) |>
  #   dplyr::mutate(site_id = current_wq_df$site_id[1],
  #                 observation = 1) |> # these are all where ice on/off is occuring
  #   dplyr::select(site_id, datetime, variable, observation)
  
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
    dplyr::filter(datetime > lubridate::as_datetime('2018-07-05')) |> 
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
                  variable = ifelse(temp_diff < -0.1 & top <= 4, 'IceOn', 'IceOff'), 
                  IceOn_hourly = ifelse(variable == "IceOn", 1, 0), 
                  IceOff_hourly = ifelse(variable == "IceOff", 1, 0), 
                  datetime = as.Date(datetime)) |>
    dplyr::group_by(datetime) |> 
    dplyr::mutate(IceOn = ifelse(1 %in% IceOn_hourly, 1, 0),
                  IceOff = ifelse(1 %in% IceOn_hourly, 0, 1)) |> 
    ungroup() |> 
    distinct(site_id, datetime, .keep_all = TRUE) |> 
    select(Reservoir, site_id, datetime, IceOn, IceOff) |> 
    dplyr::filter(IceOn != dplyr::lag(IceOn))  |> # only select rows where ice condition changes
    dplyr::mutate(Site = 50, 
                  Year = lubridate::year(datetime), 
                  DayOfYear = lubridate::yday(datetime),
                  Method = 'T', 
                  Flag_IceValue = 0) |> 
    select(Reservoir, Site, Date = datetime, Year, DayOfYear, IceOn, IceOff, Method, Flag_IceValue, site_id)
  
  
  # read in historical data file
  # EDI
  infile <- tempfile()
  try(download.file(historic_file, infile, method="curl"))
  if (is.na(file.size(infile))) download.file(historic_file,infile,method="auto")
  
  historic_ice_df <- readr::read_csv(infile, show_col_types = F) |>
    dplyr::mutate(site_id = ifelse(Reservoir == 'FCR', 'fcre',
                                   ifelse(Reservoir == 'BVR', 'bvre', NA))) |>
    dplyr::filter(site_id == current_wq_df$site_id[1], 
                  Date < '2018-07-05')
  
  # historic_ice_df <- historic_df |>
  #   dplyr::select(site_id, Date, IceOn, IceOff) |>
  #   tidyr::pivot_longer(names_to = 'variable',
  #                       values_to = 'observation',
  #                       cols = IceOn:IceOff) |>
  #   dplyr::rename(datetime = Date)
  # 
  # historic_ice_wide_df <- historic_ice_df |> 
  #   tidyr::pivot_wider(names_from = variable, values_from = observation)
  
  
  
  combined_df <- dplyr::bind_rows(historic_ice_df, historic_wq_ice_df, current_ice_df)
  
  
  if(!is.null(maint_log)){ # only run this file if the maint file argument is non-null
    
    # ## ADD MAINTENANCE LOG FLAGS (manual edits to the data for suspect samples or human error)
    
    log_read <- gsheet::gsheet2tbl(maint_log) |> 
      filter(Reservoir == ice_site) |> 
      select(Reservoir, Date = DateTime, Ice_Presence, Site, Flag_Ice_Presence)
    
    combined_df <- combined_df |> 
      full_join(log_read, by = c('Date', 'Reservoir', "Site")) |> 
      mutate(Method = ifelse(is.na(IceOn),'V', Method), 
             IceOn = ifelse(is.na(IceOn),Ice_Presence, IceOn), 
             IceOff = ifelse(is.na(IceOff) & Ice_Presence == 0, 1, IceOff),
             IceOff = ifelse(is.na(IceOff) & Ice_Presence == 1, 0, IceOff), 
             Year = lubridate::year(Date), 
             DayOfYear = lubridate::yday(Date), 
             Flag_IceValue = ifelse(is.na(Flag_Ice_Presence), 0, Flag_Ice_Presence), 
             site_id = site_id[1]) |> 
      select(-Ice_Presence, -Flag_Ice_Presence)
    
  }
  
  message('EDI file ready')
  
  
  
  
  # combined_df <- dplyr::bind_rows(historic_ice_df, current_ice_df) |> 
  #   tidyr::pivot_wider(names_from = variable, values_from = observation)
  # 
  ## manipulate the data files to match each other
  
  # dates
  # period <- dplyr::bind_rows(historic_ice_df, current_ice_df) |>
  #   dplyr::summarise(first = min(datetime),
  #                    last = max(datetime))
  
  # get all the days to fill in with 0
  # all_dates <- expand.grid(datetime = seq.Date(period$first,
  #                                              period$last, by = 'day'),
  #                          variable = c('IceOn', 'IceOff'),
  #                          site_id = current_wq_df$site_id[1])
  
  ## bind the two files using row.bind()
  # final_df <- dplyr::bind_rows(historic_ice_df, current_ice_df) |>
  #   dplyr::full_join(all_dates, by = dplyr::join_by(site_id, datetime, variable)) |>
  #   dplyr::arrange(site_id, datetime) |>
  #   dplyr::mutate(observation = ifelse(is.na(observation), 0, observation))
  ## Match data to flare targets file
  # Use pivot_longer to create a long-format table
  # for time specific - use midnight UTC values for daily
  # for hourly
  
  ## return dataframe formatted to match FLARE targets
  return(combined_df)
}

current_files <- c("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv",
                   "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")

historic_wq_files <- c('https://pasta.lternet.edu/package/data/eml/edi/725/4/9adadd2a7c2319e54227ab31a161ea12',
                       'https://pasta.lternet.edu/package/data/eml/edi/271/8/fbb8c7a0230f4587f1c6e11417fe9dce')

historic_ice_files <- c("https://pasta.lternet.edu/package/data/eml/edi/456/5/ebfaad16975326a7b874a21beb50c151")

ice_maintenance_log <- c('https://docs.google.com/spreadsheets/d/1viYhCGs3UgstzHEWdmP2Ig6uxyNM3ZC_uisG_R0QNpI/edit?gid=0#gid=0')


bvr_ice_data <- target_IceTransition_binary(current_file = current_files[1],
                                            historic_wq_file = historic_wq_files[1],
                                            historic_file = historic_ice_files,
                                            ice_site = 'BVR',
                                            maint_log = NULL)

fcr_ice_data <- target_IceTransition_binary(current_file = current_files[2],
                                            historic_wq_file = historic_wq_files[2],
                                            historic_file = historic_ice_files,
                                            ice_site = "FCR",
                                            maint_log = ice_maintenance_log)

combined_ice_data <- dplyr::bind_rows(bvr_ice_data, fcr_ice_data) |> select(-site_id)

#write.csv(combined_ice_data, "C:/Users/13188/Desktop/Data_repository/DataNotYetUploadedToEDI/Ice_binary/ice_L1.csv", row.names = FALSE)


# fcr_ice_data |> 
#   ggplot(aes(x = Date, y = IceOn)) + 
#   geom_point() + 
#   geom_point(aes(y = IceOff, color = 'red'), alpha = 0.6) + 
#   labs(title="FCR Ice Transition Binary", x ="Date", y = "IceOn/IceOff Binary")
# 
# bvr_ice_data |> 
#   ggplot(aes(x = Date, y = IceOn)) + 
#   geom_point() + 
#   geom_point(aes(y = IceOff, color = 'red'), alpha = 0.6) + 
#   labs(title="BVR Ice Transition Binary", x ="Date", y = "IceOn/IceOff Binary")
# 
# 
# historic_transition_check <- historic_ice_df |> # DEFINED IN CODE ABOVE
#   mutate(IceOn_historic = IceOn, 
#          IceOff_historic = IceOff, 
#          Method_historic = Method) |> 
#   select(Date, IceOn_historic, IceOff_historic, Method_historic) |> 
#   right_join(fcr_ice_data, by = c('Date'))
# 
# historic_transition_check |> 
#   ggplot(aes(x = Date, y = IceOn)) +
#   geom_point() +
#   geom_point(aes(y = IceOn_historic, color = 'red'), alpha = 0.6)
#   #geom_point() #+
#   #geom_point(aes(y = IceOn_historic, color = 'red'), alpha = 0.3)
#   
# historic_transition_check |> 
#   ggplot(aes(x = Date, y = IceOff)) +
#   geom_point() +
#   geom_point(aes(y = IceOff_historic, color = 'red'), alpha = 0.6)
#   #geom_point()
