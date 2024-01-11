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
  location <- sub("^[0-9]*_","",sub("\\.cnv","",file))
  SITE <- sub("_S[0-9]+[a-z]*", "",sub("test","",location))
  SAMPLER <- ""
  DATE <- substr(file,1,6)
  DATE_TEXT <- format(as.Date(DATE, "%m%d%y"), '%d-%b-%Y') #format should be "01-Aug-2019"
  MAX_DEPTH <- 100 #9.3 for FCR, 11 for BVR
  AUTO_NAME <- TRUE 
  AUTO_FOLDER <- TRUE 
  REP <- str_extract(file, "_S[0-9]+[a-z]+|_S[0-9]+") # get the ending of the file
  if(is.na(REP)){
    REP <- ""
  }
  SN <- as.numeric(str_extract(location, "\\d{4}"))
  if(is.na(SN)|SN < 6000){
    message("SN is missing, setting to 7809")
    SN <- 7809
  }
  
  #trim ctd
  ctdTrimmed <- trim_ctd(DATE_TEXT, AUTO_NAME, SITE, REP, NAME_OVERRIDE, raw_downloads)
  
  #do the rest of the processing. This does NOT add SN to file
  epic_ctd_function(ctdTrimmed, DATE_TEXT, SITE, SAMPLER, 
                    REP, SN, AUTO_NAME, NAME_OVERRIDE, AUTO_FOLDER, 
                    CSV_FOLDER_OVERRIDE, MAX_DEPTH, CTD_FOLDER)
  
  
  
}
