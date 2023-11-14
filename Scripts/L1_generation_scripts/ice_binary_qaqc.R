### file for generating ice binary L1 file ## 
# taken from the catwalk data
source('Data/DataNotYetUploadedToEDI/Ice_binary/ice_binary_targets_function.R')

current_files <- c("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv",
               "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")

historic_files <- c("https://pasta.lternet.edu/package/data/eml/edi/725/3/a9a7ff6fe8dc20f7a8f89447d4dc2038",
                    "https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f")


bvr_ice_data <- target_IceCover_binary(current_file = current_files[1], historic_file = historic_files[1])
fcr_ice_data <- target_IceCover_binary(current_file = current_files[2], historic_file = historic_files[2])
