pacman::p_load(oce, ocedata, tidyverse, lubridate)

setwd(".Data/DataNotYetUploadedToEDI/Raw_CTD/CTD_code/CTD_automation/")
source("ctd_functions_CTD7809_automated.R") #Load Carey Lab ctd functions
source("flag_seasonal_csvs.R")
source("process_CTD_file.R")
source("identify_new_files.R")
source("update_seasonal_csvs.R")

ctd_QAQC <- function(raw_downloads = "../../RawDownloads",
                     ctd_cast_csvs = "../../csv_outputs",
                     ctd_season_csvs = "../../CTD_season_csvs",
                     CTD_FOLDER = "../../"){
  
  ## Identify files new files
  file_names <- identify_new_files(raw_downloads = raw_downloads,
                                   ctd_cast_csvs = ctd_cast_csvs)
  #If no new files, end QAQC
  if(length(file_names)==0){
    message("No new files")
    return()
  }
  
  ## Generate QAQCed versions of these files (stored in csv_outputs folder)
  for(file in file_names) {
    process_CTD_file(file, 
                     raw_downloads = raw_downloads,
                     CTD_FOLDER = CTD_FOLDER) 
  }
  
  ## Generate updated seasonal csv
  update_seasonal_csvs(ctd_cast_csvs = ctd_cast_csvs,
                       ctd_season_csvs = ctd_season_csvs,
                       output_file_name = "CTD_Meta_2023.csv")
  
  ## Add data flags
  flag_seasonal_csvs(directory = ctd_season_csvs,
                     input_file_name = "CTD_Meta_2023.csv",
                     output_file_name = "CTD_2023.csv")
}

ctd_QAQC(raw_downloads = "../../RawDownloads",
         ctd_cast_csvs = "../../csv_outputs",
         ctd_season_csvs = "../../CTD_season_csvs",
         CTD_FOLDER = "../../")