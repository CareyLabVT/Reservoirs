#' 
#' @author Abigail Lewis
#' @title update_seasonal_csvs
#' @description This function loads the saved CTD csvs from this year and combines them
#' 
#' @param ctd_cast_csvs directory of individual CTD cast csv files
#' @param ctd_season_csvs directory for output of un-flagged seasonal csv
#' @param intermediate_file_name file name for output (un-flagged seasonal csv)
#'
#' @return no output

# Updates:
# 11 Feb 25- ABP added warning to find duplicated ctd casts in the csv_outputs folder

update_seasonal_csvs <- function(ctd_cast_csvs = "../csv_outputs",
                                 ctd_season_csvs = "../CTD_season_csvs",
                                 intermediate_file_name = "ctd_L0.csv",
                                 start_date = as.Date(paste0(year(Sys.Date()),"-01-01"))) {

  # This reads all the files into the R environment
  files = list.files(ctd_cast_csvs, pattern = ".*\\d+.*.csv") #Get all csv files
  files <- files[!grepl("PAR",files)&!grepl("matlab",files)] #That do not include PAR or matlab
  omit <- c("072121_fcr50_sit_1.6m.csv") #fill in with any files you need to omit
  files <- files[!files %in% omit]
  files <- files[!grepl("test", files)] # take out files that were labeled test
  
  dates <- as.Date(paste0("20",substr(files,5,6),"-",
                          substr(files,1,2), "-",
                          substr(files,3,4)))
  
  files_to_load <- files[dates >= as.Date(start_date)]
  
  
  # added in the ctd_cast_csvs file path
  
  ctd <- map(files_to_load, load_file, ctd_cast_csvs = ctd_cast_csvs)%>%  #see function below. Using map() makes loading files faster
    dplyr::bind_rows()
  
  ## making a warning if there are dups in the csv_output files
  
  check_dup_casts <- ctd|>
    select(file_name, Res, Site_name, SN_number, ctd_cast_date)|>
    unique()
  
  
  dupser <- check_dup_casts|>
    select(ctd_cast_date)
  
  dup_dates <-dupser[duplicated(dupser),]
  
  ctd_dups <- check_dup_casts|>
    filter(ctd_cast_date %in% dup_dates$ctd_cast_date)|>
    arrange(ctd_cast_date)
  
  
  if(nrow(ctd_dups)>0){
    
    warning("There are ", nrow(dup_dates), 
            " duplicated ctd casts in csv_outputs. 
            These are the name of the files with the same data:\n",
            list(ctd_dups$file_name))
    
  }
  
  # take out the dup checks columns
  
  ctd_final <- ctd|>
    select(-c(file_name, Res, Site_name, SN_number, ctd_cast_date))
  
  
  
  write_csv(ctd_final, paste0(ctd_season_csvs, "/", intermediate_file_name))
  message(paste0("Successfully updated ", paste0(ctd_season_csvs, "/", intermediate_file_name)))
}

#Function to load files
load_file <- function(file,
                      ctd_cast_csvs="../csv_outputs"){
  
  
  # list of column headers that need to be changed if they are still in the data frame
  lookup <- c(PAR_umolm2s  = "PAR",
              DescRate_ms  = 'Descent Rate (m/s)',
              DateTime = "Date",
              DOsat_percent = "DO_pSat",
              SpCond_uScm = "Spec_Cond_uScm",
              Turbidity_NTU = "Turb_NTU")
  
  # name the file
 # print(paste0("Name of the file ",file))
  
  
  ctd = read_csv(paste0(ctd_cast_csvs, "/", file), show_col_types = F) 
  location <- sub("^[0-9]*_","",sub("\\.csv","",file))
  SN_cast <- as.numeric(str_extract(location, "\\d{4}"))
  
  # the datetime stamp of the file. Trying to find where dups get introduced
  #print(paste0("The timestamp of the cast ", ctd$DateTime[1]))
  
  if(is.na(SN_cast)|SN_cast < 6000){
    SN_cast <- 7809
  }
  
  ctd = ctd %>%
    mutate(Reservoir = toupper(sub("[0-9]+.*","",location)),
           Site = as.numeric(sub("_.*","",sub("^[A-Z|a-z]*","",sub("_[a-z]+","",location)))),
           SN =  as.numeric(SN_cast)) %>%
    dplyr::rename(any_of(lookup)) %>%
    select(-Salinity)
  
  # make a data frame of the name of the file and the startdate of the cast
  
  find_dup_cast <- data.frame(file_name = file,
                              Res = ctd$Reservoir[1],
                              Site_name = ctd$Site[1],
                              SN_number = ctd$SN[1],
             ctd_cast_date = ctd$DateTime[1]
             )
  
  return(c(find_dup_cast, ctd))
}
