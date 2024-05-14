source("C:/Users/13188/Desktop/Reservoirs/Data/DataNotYetUploadedToEDI/EddyFlux_Processing/eddy_pro_cleaning_function.R")
Apr_May_2024 <- eddypro_cleaning_function(directory = "./Data/DataNotYetUploadedToEDI/EddyFlux_Processing/",
                                  gdrive = T, # Are the files on Google Drive. True or False
                                  gshared_drive = as_id("0ACybYKbCwLRPUk9PVA"),
                                  current_year = 2024)
