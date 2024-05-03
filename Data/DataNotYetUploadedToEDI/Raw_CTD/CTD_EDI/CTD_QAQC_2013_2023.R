#' 
#' @title QAQC script
#' @description This function runs QAQC for any new CTD files and re-processes seasonal CSVs
#' @author Abigail Lewis
#' 
#' @param raw_downloads directory where raw cnvs are stored
#' @param ctd_cast_csvs directory where output csvs for individual casts are stored
#' @param ctd_season_csvs directory where seasonal output csv should be stored
#' @param CTD_FOLDER high level CTD folder (used to reference metadata, pdf outputs, etc)
#' This is also where the ctd_L1.csv file will be stored
#' @param start_date only process files after this date
#' @param force_reprocessing if TRUE, process all files, even if they have already been processed
#' @param historical_files if TRUE, add the files from 2013-2017 to the data file
#' @param output_file_name name of output file (usually ctd_L1.csv)
#' @param intermediate_file_name name of the file saved in the CTD_season_csvs folder

#'
#' @return L1 (processed data)
#'

pacman::p_load(oce, ocedata, tidyverse, lubridate, here)
code_folder <- here("Data", "DataNotYetUploadedToEDI", "Raw_CTD", "CTD_code")
# Load Carey Lab ctd functions
source(here(code_folder,"/R/ctd_functions_automated.R") )
source(here(code_folder,"/R/flag_seasonal_csvs.R"))
source(here(code_folder,"/R/process_CTD_file.R"))
source(here(code_folder,"/R/identify_new_files.R"))
source(here(code_folder,"/R/update_seasonal_csvs.R"))

# Create function
ctd_QAQC <- function(raw_downloads = here(code_folder,"../RawDownloads"),
                     ctd_cast_csvs = here(code_folder,"../csv_outputs"),
                     ctd_season_csvs = here(code_folder,"../CTD_season_csvs"),
                     CTD_FOLDER = here(code_folder,"../"),
                     start_date = as.Date(paste0(year(Sys.Date()),"-01-01")),
                     force_reprocessing = FALSE,
                     historical_files = FALSE,
                     output_file_name = "ctd_L1.csv",
                     intermediate_file_name = "ctd_L0.csv"){
  
  ## Identify files new files
  file_names <- identify_new_files(raw_downloads = raw_downloads,
                                   ctd_cast_csvs = ctd_cast_csvs,
                                   start_date = start_date,
                                   force_reprocessing = force_reprocessing)
  
  #If no new files, end QAQC
  if(length(file_names)==0){
    message("No new files could be processed")
    #return()
  } else{
    
    ## Generate csv versions of these files (stored in csv_outputs folder)
    for(file in file_names) {
      message(file)
      process_CTD_file(file, 
                       raw_downloads = raw_downloads,
                       CTD_FOLDER = CTD_FOLDER) 
    }
  }
  
  ## Generate updated seasonal csv (L0) - this combines all ctd_cast_csvs and takes a bit to run
  # This is the function that adds SN to the file!
  update_seasonal_csvs(ctd_cast_csvs = ctd_cast_csvs,
                       ctd_season_csvs = ctd_season_csvs,
                       intermediate_file_name = intermediate_file_name,
                       start_date = start_date)
  
  ## Add data flags to seasonal csv (L1)
  l1 <- flag_seasonal_csvs(ctd_season_csvs = ctd_season_csvs,
                           intermediate_file_name = intermediate_file_name,
                           output_file_name = output_file_name,
                           historical_files = historical_files)
  
  return(l1)
}