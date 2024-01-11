#'
#' @author Abigail Lewis
#' @title identify_new_files
#' @description This function checks the raw downloads folder to see if there are new, unprocessed files
#' 
#' @param raw_downloads file name to process
#' @param ctd_cast_csvs directory where output csvs should be
#' @param start_date only process files after this date
#' @param force_reprocessing if TRUE, process all files, even if they have already been processed
#'
#' @return no output
#'

library(lubridate)
identify_new_files <- function(raw_downloads = "../RawDownloads",
                               ctd_cast_csvs = "../csv_outputs",
                               start_date = as.Date("2012-01-01"), #if no start date, process all files
                               force_reprocessing = FALSE
                               ) {
  
  #Load all files in RawDownloads and csv_output folders
  downloads <- sub(".cnv","",list.files(raw_downloads, pattern = ".cnv"))
  outputs <- sub(".csv","",list.files(ctd_cast_csvs))
  
  #Files that are in downloads but not outputs have not yet been processed
  if(force_reprocessing){missing <- downloads} else {
    missing <- downloads[!downloads%in%outputs]
  }
  
  if(length(missing)==0){return()}
  
  dates <- as.Date(paste0("20",substr(missing,5,6),"-",
                          substr(missing,1,2), "-",
                          substr(missing,3,4)))
  
  warning("Can't parse the following dates (not processing these files):\n", 
          paste0(missing[is.na(dates)], sep = "\n"))
  
  #Only need files since the start date
  missing_recent <- missing[dates>=as.Date(start_date) & !is.na(dates)]
  
  #See if we can get reservoir name and site from file name. 
  #If not, not worth processing (warning message below)
  location <- sub("^[0-9]*_", "", sub("\\.csv","", missing_recent))
  Reservoir <- toupper(sub("[0-9]+.*", "", location))
  Site <- as.numeric(sub("_.*","", 
                         sub("^[A-Z|a-z]*", "", 
                             sub("_[a-z]+", "", location))))
  
  to_process <- missing_recent[!is.na(Reservoir)&!is.na(Site)]
  #Warn about unprocessed files
  if(length(missing_recent[is.na(Reservoir)|is.na(Site)])>0){
    warning("Couldn't identify reservoir and/or site for the following files (and therefore skipped processing):\n",
            paste0(missing_recent[is.na(Reservoir)|is.na(Site)], sep = "\n"))
  }
  
  return(to_process)
}
