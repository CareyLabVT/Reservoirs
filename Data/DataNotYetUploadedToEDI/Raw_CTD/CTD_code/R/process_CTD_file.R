#' 
#' @author Abigail Lewis
#' @title update_seasonal_csvs
#' @description This function loads the saved CTD csvs from this year and combines them
#' 
#' @param file file name to process
#' @param raw_downloads directory where raw file is stored
#' @param output_directory directory where output csv should be
#'
#' @return no output
#'

process_CTD_file <- function(file, 
                                 raw_downloads = "../../RawDownloads",
                                 CTD_FOLDER = "../../") {
  #Specify global variables
  DATE <- as.Date(substr(file,1,6), "%m%d%y")
  MAX_DEPTH <- 100 # was 9.3 for FCR, 11 for BVR
  AUTO_FOLDER <- TRUE 
  REP <- str_extract(file, "_S[0-9]+[a-z]+|_S[0-9]+") # get the ending of the file
  if(is.na(REP)){ # prior to 2022, naming conventions were different and REP was included
    REP <- ""     # keeping REP to maintain consistency with older files
  }
  SN <- as.numeric(str_extract(sub("^[0-9]*_","",sub("\\.cnv","",file)), "\\d{4}"))
  if(is.na(SN)|SN < 6000){
    message("SN is missing, setting to 7809")
    SN <- 7809
    
    if(year(DATE) >= 2024){
      stop("SN is missing and date is after 2024, please check the file name")
    }
  }
  
  #These are in ctd_functions_automated.R
  #trim ctd
  ctdTrimmed <- trim_ctd(file, raw_downloads) 
  #do the rest of the processing. This does NOT add SN to file
  epic_ctd_function(ctdTrimmed, file, SN, AUTO_FOLDER, 
                    CSV_FOLDER_OVERRIDE, MAX_DEPTH, CTD_FOLDER)
  
  
  
}
