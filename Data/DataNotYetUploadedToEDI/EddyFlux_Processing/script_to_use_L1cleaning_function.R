source("C:/Users/13188/Desktop/Reservoirs/Data/DataNotYetUploadedToEDI/EddyFlux_Processing/eddy_flux_create.R")

update_L1_csv <- eddypro_cleaning_function(
  directory = "./Data/DataNotYetUploadedToEDI/EddyFlux_Processing/data",
  gdrive = T, # Are the files on Google Drive. True or False
  gshared_drive = as_id("0ACybYKbCwLRPUk9PVA"),
  #current_year = 2024,
  output_file = "./Data/DataNotYetUploadedToEDI/EddyFlux_Processing/EddyPro_Cleaned_L1.csv",
  start_date = as.Date("2023-12-31") + lubridate::days(1),
  end_date = Sys.Date() + lubridate::days(1))
