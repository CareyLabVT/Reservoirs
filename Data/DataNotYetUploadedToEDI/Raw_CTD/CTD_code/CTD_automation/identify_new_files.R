#'
#' @author Abigail Lewis
#' @title identify_new_files
#' @description This function checks the raw downloads folder to see if there are new, unprocessed files
#' 
#' @param raw_downloads file name to process
#' @param ctd_cast_csvs directory where output csvs should be
#'
#' @return no output
#'

identify_new_files <- function(raw_downloads = "../../RawDownloads",
                               ctd_cast_csvs = "../../csv_outputs"
                               ) {
  #Load all files in RawDownloads and csv_output folders
  downloads <- sub(".cnv","",list.files(raw_downloads, pattern = ".cnv"))
  outputs <- sub(".csv","",list.files(ctd_cast_csvs))
  
  #Files that are in downloads but not outputs have not yet been processed
  missing <- downloads[!downloads%in%outputs]
  
  #Only need 2023 files
  missing_2023 <- missing[substr(missing,5,6)=="23"]
  
  #See if we can get reservoir name and site from file name. 
  #If not, not worth processing
  location <- sub("^[0-9]*_", "", sub("\\.csv","", missing_2023))
  Reservoir <- toupper(sub("[0-9]+.*", "", location))
  Site <- as.numeric(sub("_.*","", 
                         sub("^[A-Z|a-z]*", "", 
                             sub("_[a-z]+", "", location))))
  
  to_process <- missing_2023[!is.na(Reservoir)&!is.na(Site)]
  
  # FOR NOW ONLY PROCESS OLD CTD
  to_process_S7809 <- to_process[!grepl("S8188",to_process)]
  
  return(to_process_S7809)
}
