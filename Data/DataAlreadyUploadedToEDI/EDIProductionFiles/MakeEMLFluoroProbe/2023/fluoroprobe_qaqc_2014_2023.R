#Title: Script for aggregating and QAQCing FP txt files for publication to EDI
#Author: Mary Lofton
#Updated version: Austin Delany
#Date created: 16DEC19
#Last updated: 2024-01-10


library(tidyverse)
library(lubridate)
#library(httr)

rm(list=ls())

# load packages
#install.packages('pacman')
#pacman::p_load(tidyverse, lubridate, googlesheets4)

##### Function to qaqc Fluoroprobe data
#' @param maintenance_file filepath to the maintenance log
#' @param start_date first day of current year (day after last day of data pushed to EDI)
#' @param end_date last day of current year (last day before we publish a revision to EDI)

fluoroprobe_qaqc <- function(maintenance_file ,
                             start_date = NULL, 
                             end_date = NULL){

# Load in column names for .txt files to get template
col_names <- names(read_tsv("./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/20230111_BVR_50.txt", n_max = 0))

# Load in all txt files
fp_casts <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe", pattern = paste0("*.txt")) %>%
  map_df(~ data_frame(x = .x), .id = "cast")

raw_fp <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe", pattern = paste0("*.txt")) %>% 
  map_df(~ read_tsv(file.path(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe", .), 
                    col_types = cols(.default = "c"), col_names = col_names, skip = 2), .id = "cast")

#split out column containing filename to get Reservoir and Site data
fp2 <- left_join(raw_fp, fp_casts, by = c("cast")) %>%
  rowwise() %>% 
  mutate(Reservoir = unlist(strsplit(x, split='_', fixed=TRUE))[2],
         Site = unlist(strsplit(x, split='_', fixed=TRUE))[3],
         Site = unlist(strsplit(Site, split='.', fixed=TRUE))[1]) %>%
  ungroup()
fp2$Site <- as.numeric(fp2$Site)

#check to make sure strsplit function worked for all casts
check <- subset(fp2, is.na(fp2$Site))
unique(fp2$Reservoir)
unique(fp2$Site)

# Rename and select useful columns; drop metrics we don't use or publish such as cell count;
# eliminate shallow depths because of quenching
fp3 <- fp2 %>%
  mutate(DateTime = `Date/Time`, GreenAlgae_ugL = as.numeric(`Green Algae...2`), Bluegreens_ugL = as.numeric(`Bluegreen...3`),
         BrownAlgae_ugL = as.numeric(`Diatoms...4`), MixedAlgae_ugL = as.numeric(`Cryptophyta...5`), YellowSubstances_ugL = as.numeric(`Yellow substances...9`),
         TotalConc_ugL = as.numeric(`Total conc.`), Transmission = as.numeric(`Transmission`), Depth_m = as.numeric(`Depth`), Temp_degC = as.numeric(`Temp. Sample`),
         RFU_525nm = as.numeric(`LED 3 [525 nm]`), RFU_570nm = as.numeric(`LED 4 [570 nm]`), RFU_610nm = as.numeric(`LED 5 [610 nm]`),
         RFU_370nm = as.numeric(`LED 6 [370 nm]`), RFU_590nm = as.numeric(`LED 7 [590 nm]`), RFU_470nm = as.numeric(`LED 8 [470 nm]`),
         Pressure_unit = as.numeric(`Pressure`)) %>%
  select(x,cast, Reservoir, Site, DateTime, GreenAlgae_ugL, Bluegreens_ugL, BrownAlgae_ugL, MixedAlgae_ugL, YellowSubstances_ugL,
         TotalConc_ugL, Transmission, Depth_m, Temp_degC, RFU_525nm, RFU_570nm, RFU_610nm,
         RFU_370nm, RFU_590nm, RFU_470nm) %>%
  mutate(DateTime = as.POSIXct(as_datetime(DateTime, tz = "", format = "%m/%d/%Y %I:%M:%S %p"))) %>%
  filter(Depth_m >= 0.2) |> 
  dplyr::mutate(DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                DateTime = lubridate::with_tz(DateTime, tzone = "UTC"))

#trim casts to eliminate poor readings due to sediment interference at bottom of reservoir
#for our purposes, we consider the "max depth" of each reservoir to be:
#FCR = 9.5 m; BVR = 10.0 m; CCR = 21 m 

fp4 = fp3[FALSE,]

for (i in 1:length(unique(fp3$cast))){
  profile = subset(fp3, cast == unique(fp3$cast)[i])
  if(profile$Reservoir[1] == "FCR"){
    profile_trim <- profile %>% filter(Depth_m <= 9.5)
  } else if (profile$Reservoir[1] == "CCR"){
    profile_trim <- profile %>% filter(Depth_m <= 21)
  } else if (profile$Reservoir[1] == "BVR"){
    profile_trim <- profile %>% filter(Depth_m <= 10)
  }
  fp4 <- bind_rows(fp4, profile_trim)
} 

# general QAQC of other variables

# transmission
fp6 <- fp4 

#get rid of columns we don't need for final publication
fp7 <- fp6 %>%
  select(-x, -cast) %>%
  mutate(Site = as.numeric(Site))

#reorder columns to match current EDI data package
fp8 <- fp7[,c(1,2,3,11,4,5,6,7,8,9,10,12,13,14,15,16,17,18)]


#ADD FLAGS

fp_final <- fp8 %>%
  mutate(Flag_GreenAlgae_ugL = ifelse(Transmission < 90, 3, 0),
         Flag_BluegreenAlgae_ugL = ifelse(Transmission < 90, 3, 0),
         Flag_BrownAlgae_ugL = ifelse(Transmission < 90, 3, 0),
         Flag_MixedAlgae_ugL = ifelse(Transmission < 90, 3, 0),
         Flag_YellowSubstances_ugL = ifelse(Transmission < 90, 3, 0),
         Flag_TotalConc_ugL = ifelse(Transmission < 90, 3, 0),
         Flag_Temp_C = 0, # example: ifelse(date(DateTime) %in% bad_temp_days,2,0),
         Flag_Transmission_perc = ifelse(Transmission < 90, 3, 0),
         Flag_RFU_525nm = ifelse(Transmission < 90, 3, 0),
         Flag_RFU_570nm = ifelse(Transmission < 90, 3, 0),
         Flag_RFU_610nm = ifelse(Transmission < 90, 3, 0),
         Flag_RFU_370nm = ifelse(Transmission < 90, 3, 0),
         Flag_RFU_590nm = ifelse(Transmission < 90, 3, 0),
         Flag_RFU_470nm = ifelse(Transmission < 90, 3, 0)) %>%
  rename(Temp_C = Temp_degC,
         Transmission_perc = Transmission) 


### 4. Take out values based on the Maintenance Log (THIS IS A TEST FOR NOW) -- ALSO MIGHT MOVE SOMEWHERE ELSE IN SCRIPT
## TRY ADDING FLEXIBILITY TO IGNORE ANY COMMENTS IN MAINT LOG

# modify raw_df based on the information in the log

#log <- utils::read.table('Data/DataNotYetUploadedToEDI/YSI_PAR_Secchi/maintenance_log.txt', sep = ',', header = TRUE)

#log <- fread("grep -v '^#' Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/maintenance_log.txt", data.table = FALSE)

# log_read <- read_csv(maintenance_file, skip=43, col_types = cols(
#   .default = col_character(),
#   TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
#   TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
#   flag = col_integer()
# ))

### MAINTENANCE LOG CODE -- UNCOMMENT WHEN FILE IS READY
#maintenance_file <- 'Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/Maintenance_Log_FluoroProbe.csv'

log_read <- read_csv(maintenance_file, col_types = cols(
  .default = col_character(),
  TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  flag = col_integer()
))

log <- log_read

### identify the date subsetting for the data
if (!is.null(start_date)){
  fp_final <- fp_final %>% 
    filter(DateTime >= start_date)
  log <- log %>%
    filter(TIMESTAMP_start <= end_date)
}

if(!is.null(end_date)){
  fp_final <- fp_final %>% 
    filter(DateTime <= end_date)
  log <- log %>%
    filter(TIMESTAMP_end >= start_date)
}

# if (nrow(log) == 0){
#   log <- log_read
# }

if (nrow(log) > 0){
  
for(i in 1:nrow(log)){
  ### Assign variables based on lines in the maintenance log.

  ### get start and end time of one maintenance event
  start <- log$TIMESTAMP_start[i]
  end <- log$TIMESTAMP_end[i]

  ### Get the Reservoir Name

  Reservoir <- log$Reservoir[i]

  ### Get the Site Number

  Site <- as.numeric(log$Site)

  ### Get the depth if it is not NA

  if(!is.na(log$Depth[i])){
    Depth <- as.numeric(log$new_value[i]) ## IS THERE SUPPOSED TO BE A COLUMN ADDED TO MAINT LOG CALLED NEW_VALUE?
  }

  ### Get the Maintenance Flag

  flag <- log$flag[i]

  ### Get the new value for a column or an offset. If it is not an NA

  if(!is.na(log$update_value[i])){

    update_value <- as.numeric(log$update_value[i])
  }


  ### Get the names of the columns affected by maintenance

  colname_start <- log$start_parameter[i]
  colname_end <- log$end_parameter[i]

  ### if it is only one parameter parameter then only one column will be selected

  if(is.na(colname_start)){

    maintenance_cols <- colnames(fp_final%>%select(colname_end))

  }else if(is.na(colname_end)){

    maintenance_cols <- colnames(fp_final%>%select(colname_start))

  }else{
    maintenance_cols <- colnames(fp_final%>%select(colname_start:colname_end))
  }
  
  # remove flag and notes cols in the maint_col vector
  maintenance_cols <- maintenance_cols[!grepl('Flag', maintenance_cols)]
  maintenance_cols <- maintenance_cols[!grepl('Note', maintenance_cols)]

  if(is.na(end)){
    # If there the maintenance is on going then the columns will be removed until
    # and end date is added
    Time <- fp_final$DateTime >= start

  }else if (is.na(start)){
    # If there is only an end date change columns from beginning of data frame until end date
    Time <- fp_final$DateTime <= end

  }else {

    Time <- fp_final$DateTime >= start & fp_final$DateTime <= end
  }

  ### This is where information in the maintenance log gets removed.
  # UPDATE THE IF STATEMENTS BASED ON THE NECESSARY CRITERIA FROM THE MAINTENANCE LOG

  # replace relevant data with NAs and set flags while maintenance was in effect
  if(flag %in% c(1)){
    # Flagged values but keep in dataset
    #Met[Met$DateTime %in% Time$DateTime, maintenance_cols] <- NA
    fp_final[fp_final$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag
    
  }else if (flag %in% c(2)){ 
    ## Instrument error
    
    fp_final[c(which(fp_final[,'Site'] == Site & fp_final$DateTime %in% Time$DateTime)),maintenance_cols] <- NA
    fp_final[fp_final$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag
    
  }
  else
  {
    warning("Flag not coded in the L1 script. See Austin or Adrienne")
  }
} # end for loop
} # end conditional statement

### END MAINTENANCE LOG CODE ###

# ## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
# package_ID <- 'edi.272.7'
# eml <- read_metadata(package_ID)
# date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
# last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)
# 
# 
# fp_final <- raw_df |> filter(DateTime > last_edi_date)

write.csv(fp_final, "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/fluoroprobe_L1.csv", row.names = FALSE)

}

#maintenance_file <- 'Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/Maintenance_Log_FluoroProbe.csv'
maintenance_file <- './Maintenance_Log_FluoroProbe.csv'
start_date <- '2023-01-01'
end_date <- '2023-12-31'

# run the function
fluoroprobe_qaqc(maintenance_file = maintenance_file, 
                 start_date = start_date,
                 end_date = end_date) 

