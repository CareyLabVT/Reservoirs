#' 
#' @author Abigail Lewis
#' @title update_seasonal_csvs
#' @description This function loads the saved CTD csvs from this year and combines them
#' 
#' @param ctd_cast_csvs directory of individual CTD cast csv files
#' @param ctd_season_csvs directory for output of un-flagged seasonal csv
#' @param output_file_name file name for output (un-flagged seasonal csv)
#'
#' @return no output
#'

update_seasonal_csvs <- function(ctd_cast_csvs = "../csv_outputs",
                                 ctd_season_csvs = "../CTD_season_csvs",
                                 output_file_name = "CTD_Meta_2023.csv") {
  
  # This reads all the files into the R environment
  files = list.files(ctd_cast_csvs, pattern = ".*\\d+.*.csv") #Get all csv files
  files <- files[!grepl("PAR",files)&!grepl("matlab",files)] #That do not include PAR or matlab
  omit <- c("") #fill in with any files you need to omit
  files <- files[!files %in% omit]
  files <- files[!grepl("test", files)] # take out files that were labeled test
  
  # list of column headers that need to be changed if they are still in the data frame
  
  lookup <- c(PAR_umolm2s  = "PAR",
              DescRate_ms  = 'Descent Rate (m/s)',
              DateTime = "Date",
              DOsat_percent = "DO_pSat",
              SpCond_uScm = "Spec_Cond_uScm",
              Turbidity_NTU = "Turb_NTU")
  
  #Function to load files
  load_file <- function(file){
    ctd = read_csv(paste0(ctd_cast_csvs, "/", file)) 
    location <- sub("^[0-9]*_","",sub("\\.csv","",file))
    sn <- str_extract(location, "\\d{4}")
    ctd = ctd%>%
      mutate(Reservoir = toupper(sub("[0-9]+.*","",location)),
             Site = as.numeric(sub("_.*","",sub("^[A-Z|a-z]*","",sub("_[a-z]+","",location)))),
             SN =  as.numeric(str_extract(location, "\\d{4}")))%>%
      dplyr::rename(any_of(lookup))%>%
      select(-Salinity)
    return(ctd)
  }
  
  ctd <- map(files, load_file) %>%
    dplyr::bind_rows()
  
  write_csv(ctd, paste0(ctd_season_csvs, "/", output_file_name))
}
