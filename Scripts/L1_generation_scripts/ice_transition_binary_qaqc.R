## CREATE ICE TRANSITION FILE (DETERMINED THROUGH VISUAL OR DERIVED SOURCE)
# Created by ADD
# First developed: 2024-01-12
# Last edited: 2024-12-12

library(dplyr)
library(readr)

source('https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Scripts/L1_functions/ice_transition_binary_create.R')

current_files <- c("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv",
                   "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")

historic_wq_files <- c('https://pasta.lternet.edu/package/data/eml/edi/725/4/9adadd2a7c2319e54227ab31a161ea12',
                       'https://pasta.lternet.edu/package/data/eml/edi/271/8/fbb8c7a0230f4587f1c6e11417fe9dce')

historic_ice_files <- c("https://pasta.lternet.edu/package/data/eml/edi/456/5/ebfaad16975326a7b874a21beb50c151")

ice_maintenance_log <- c('https://docs.google.com/spreadsheets/d/1viYhCGs3UgstzHEWdmP2Ig6uxyNM3ZC_uisG_R0QNpI/edit?gid=0#gid=0')


bvr_ice_data <- ice_transition_binary_create(current_file = current_files[1],
                                            historic_wq_file = historic_wq_files[1],
                                            historic_file = historic_ice_files,
                                            ice_site = 'BVR',
                                            maint_log = NULL)

fcr_ice_data <- ice_transition_binary_create(current_file = current_files[2],
                                            historic_wq_file = historic_wq_files[2],
                                            historic_file = historic_ice_files,
                                            ice_site = "FCR",
                                            maint_log = ice_maintenance_log)

combined_ice_data <- dplyr::bind_rows(bvr_ice_data, fcr_ice_data) |> select(-site_id)

write.csv(combined_ice_data, './Data/DataNotYetUploadedToEDI/ice_binary/ice_transition_binary_L1.csv', row.names = FALSE)