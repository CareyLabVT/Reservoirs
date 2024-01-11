# ysi_qaqc_2023.R
# QAQC of YSI and PAR data from 2023
# Created by ADD, modified by HLW
# First developed: 2023-12-04
# Last edited: 2024-01-11

#install.packages('pacman') ## Run this line if you don't have "pacman" package installed
pacman::p_load(tidyverse, lubridate, dplyr,
               EDIutils, xml2, gsheet) ## Use pacman package to install/load other packages


ysi_qaqc <- function(data_file, gsheet_data, maintenance_file = NULL, outfile){

  if(is.character(data_file) & gsheet_data == FALSE){
    # read catwalk data and maintenance log
    # NOTE: date-times throughout this script are processed as UTC
    raw_profiles <- read_csv(data_file, skip = 1, col_names = CATPRES_COL_NAMES,
                          col_types = cols(.default = col_double(), DateTime = col_datetime()))

  } else if (is.character(data_file) & gsheet_data == TRUE){
    raw_profiles <- gsheet::gsheet2tbl(data_file)

  }else {
    raw_profiles <- data_file
  }


# gsheet_url <- 'https://docs.google.com/spreadsheets/d/1HbSBEFjMuK4Lxit5MRbATeiyljVAB-cpUNxO3dKd8V8/edit#gid=1787819257'
# raw_profiles <- gsheet::gsheet2tbl(gsheet_url)

#date format
raw_profiles$DateTime = lubridate::parse_date_time(raw_profiles$DateTime, orders = c('ymd HMS','ymd HM','ymd','mdy'), tz = "America/New_York")

# ### Create a DateTime Flag for non-recorded times ####
# # (i.e., 12:00) and set to noon
# # Convert time that are in 12 hours to 24 hours
raw_profiles <- raw_profiles %>%
  mutate(Time = format(DateTime,"%H:%M:%S"),
         Time = ifelse(Time == "00:00:00", "12:00:00",Time),
         Flag_DateTime = ifelse(Time == "12:00:00", 1, 0), # Flag if set time to noon
         Date = as.Date(DateTime),
         DateTime = ymd_hms(paste0(Date, "", Time), tz = "America/New_York"),
         Hours = hour(DateTime),
         DateTime = ifelse(Hours<5, DateTime + (12*60*60), DateTime), # convert time to 24 hour time
         DateTime = as_datetime(DateTime, tz = "America/New_York"))%>% # time is in seconds put it in ymd_hms
  select(-c(Time, Date, Hours))

#make sure other columns are the correct type
raw_profiles$Reservoir <- as.character(raw_profiles$Reservoir)
raw_profiles$Site <- as.numeric(raw_profiles$Site)
raw_profiles$Temp_C <- as.numeric(raw_profiles$Temp_C)
raw_profiles$Depth_m <- as.numeric(raw_profiles$Depth_m)
raw_profiles$DO_mgL <- as.numeric(raw_profiles$DO_mgL)
raw_profiles$DOsat_percent <- as.numeric(raw_profiles$DOsat_percent)
raw_profiles$Cond_uScm <- as.numeric(raw_profiles$Cond_uScm)
raw_profiles$SpCond_uScm <- as.numeric(raw_profiles$SpCond_uScm)
raw_profiles$PAR_umolm2s <- as.numeric(raw_profiles$PAR_umolm2s)
raw_profiles$ORP_mV <- as.numeric(raw_profiles$ORP_mV)
raw_profiles$pH <- as.numeric(raw_profiles$pH)


## update raw data into new table to make rerunnig easier
update_profiles <- raw_profiles

## ADD FLAGS (note that only flags 5 and 6 can come from the maintenance file)
# 0 - NOT SUSPECT
# 1 - SAMPLE NOT TAKEN
# 2 - INSTRUMENT MALFUNCTION
# 3 - SAMPLE BELOW DETECTION LIMIT
# 4 - NEGATIVE VALUE SET TO ZERO
# 5 - SUSPECT SAMPLE
# 6 - HUMAN ERROR

## AUTOMATED FLAGS THAT CAN BE APPLIED TO ENTIRE TABLE BY INDEX ##
for(j in colnames(update_profiles%>%select(DateTime, Temp_C:pH))) {

  #create new flag column in data frame and set to zero
  update_profiles[,paste0("Flag_",colnames(update_profiles[j]))] <- 0 #creates flag column + name of variable

  #puts in flag 1 if value not collected
  update_profiles[c(which(is.na(update_profiles[,j]))),paste0("Flag_",colnames(update_profiles[j]))] <- 1
}

# ## check for values less than zero
for(j in colnames(update_profiles%>%select(DO_mgL:pH))){ # omit temp because it can be negative
  ## set flags for negative values
  update_profiles[c(which(update_profiles[,j] < 0)),paste0("Flag_",colnames(update_profiles[j]))] <- 4
  update_profiles[c(which(update_profiles[,j]<0)),j] <- 0 #replaces value with 0
}

## AUTOMATED FLAGS THAT ARE DONE FOR EACH VARIABLE INDIVIDUALLY (Instrument malfunction / MDL)
update_profiles <- update_profiles |>
  mutate(Flag_pH = ifelse((!is.na(pH) & (pH > 14 | pH < 4)), 2, Flag_pH),
         Flag_ORP_mV = ifelse((!is.na(ORP_mV) & (ORP_mV > 750)), 2, Flag_ORP_mV),
         #Flag_PAR_umolm2s = No bounds given,
         Flag_Temp_C = ifelse((!is.na(Temp_C) & (Temp_C > 35)), 2, Flag_Temp_C),
         Flag_DO_mgL = ifelse((!is.na(DO_mgL) & (DO_mgL > 70)), 2, Flag_DO_mgL),
         Flag_DOsat_percent = ifelse((!is.na(DOsat_percent) & (DOsat_percent > 200)), 2,Flag_DOsat_percent),
         Flag_Cond_uScm = ifelse((!is.na(Cond_uScm) & ((Cond_uScm < 10 | Cond_uScm > 250))), 2, Flag_Cond_uScm),
         Flag_SpCond_uScm = ifelse((!is.na(SpCond_uScm) & (SpCond_uScm > 250)), 2, Flag_SpCond_uScm)
  )


if (!is.null(maintenance_file)){ # check to see if maint log is non-null value

## ADD MAINTENANCE LOG FLAGS (manual edits to the data for suspect samples or human error)
#maintenance_file <- 'Data/DataNotYetUploadedToEDI/YSI_PAR/maintenance_log.csv'
log_read <- read_csv(maintenance_file, col_types = cols(
  .default = col_character(),
  TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  flag = col_integer()
))

log <- log_read

for(i in 1:nrow(log)){
  ### Assign variables based on lines in the maintenance log.

  ### get start and end time of one maintenance event
  start <- force_tz(as.POSIXct(log$TIMESTAMP_start[i]), tzone = "America/New_York")
  end <- force_tz(as.POSIXct(log$TIMESTAMP_end[i]), tzone = "America/New_York")

  ### Get the Reservoir Name
  Reservoir <- log$Reservoir[i]

  ### Get the Site Number
  Site <- as.numeric(log$Site[i])

  ### Get the depth value
  Depth <- as.numeric(log$Depth[i]) ## IS THERE SUPPOSED TO BE A COLUMN ADDED TO MAINT LOG CALLED NEW_VALUE?

  ### Get the Maintenance Flag
  flag <- log$flag[i]

  ### Get the new value for a column or an offset
  update_value <- as.numeric(log$update_value[i])

  ### Get the names of the columns affected by maintenance
  colname_start <- log$start_parameter[i]
  colname_end <- log$end_parameter[i]

  ### if it is only one parameter parameter then only one column will be selected

  if(is.na(colname_start)){

    maintenance_cols <- colnames(update_profiles%>%select(colname_end))

  }else if(is.na(colname_end)){

    maintenance_cols <- colnames(update_profiles%>%select(colname_start))

  }else{
    maintenance_cols <- colnames(update_profiles%>%select(colname_start:colname_end))
  }

  if(is.na(end)){
    # If there the maintenance is on going then the columns will be removed until
    # and end date is added
    Time <- update_profiles |> filter(DateTime >= start) |> select(DateTime)

  }else if (is.na(start)){
    # If there is only an end date change columns from beginning of data frame until end date
    Time <- update_profiles |> filter(DateTime <= end) |> select(DateTime)

  }else {
    Time <- update_profiles |> filter(DateTime >= start & DateTime <= end) |> select(DateTime)
  }

  ### This is where information in the maintenance log gets updated

  if(flag %in% c(1,2)){
    # The observations are changed to NA for maintenance or other issues found in the maintenance log
    update_profiles[update_profiles$DateTime %in% Time$DateTime, maintenance_cols] <- NA
    update_profiles[update_profiles$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag

  }else if (flag %in% c(3)){
    ## BDL
    update_profiles[update_profiles$DateTime %in% Time$DateTime, maintenance_cols] <- NA
    update_profiles[update_profiles$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag

  }else if (flag %in% c(4)){
    ## change negative values are changed to 0

    update_profiles[update_profiles$DateTime %in% Time$DateTime, maintenance_cols] <- 0
    update_profiles[update_profiles$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag

  } else if(flag %in% c(5)){
    # Suspect sample
    update_profiles[update_profiles$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag

  }else if(flag %in% c(6) & colname_start != 'Site'){
    ## human error
    update_profiles[update_profiles$DateTime %in% Time$DateTime, maintenance_cols] <- update_value
    update_profiles[update_profiles$DateTime %in% Time$DateTime, paste0("Flag_",maintenance_cols)] <- flag

  }else if(flag %in% c(6) & colname_start == 'Site'){
    ## human error for site, which we don't indicate in final dataset
    update_profiles[update_profiles$DateTime %in% Time$DateTime, maintenance_cols] <- update_value

  }else{
    warning("Flag not coded in the L1 script. See Austin or Adrienne")
  }
}#end for loop
}#end conditional statement

#### END MAINTENANCE LOG CODE


## ORGANIZE FINAL TABLE ##
ysi <- update_profiles |>
  select(Reservoir, Site, DateTime, Depth_m, Temp_C, DO_mgL, DOsat_percent,
         Cond_uScm, SpCond_uScm, PAR_umolm2s, ORP_mV, pH, Flag_DateTime, Flag_Temp_C, Flag_DO_mgL, Flag_DOsat_percent,
         Flag_Cond_uScm, Flag_SpCond_uScm, Flag_PAR_umolm2s, Flag_ORP_mV, Flag_pH) |>
  arrange(Reservoir, DateTime, Depth_m)


## FINAL GENERAL QAQC ##

#change all ccr site 100 to 101
ysi$Site[ysi$Reservoir=="CCR" & ysi$Site==100] <- 101

#add a 5 flag for all pH values between 4 and 5 from past years
ysi$Flag_pH[!is.na(ysi$pH) & ysi$pH < 5] <- 5

#manually switch the one pH value < 4 to have a 2 flag for instrument malfunction
ysi$Flag_pH[!is.na(ysi$pH) & ysi$pH < 4] <- 2

#then set that value to NA
ysi$pH[!is.na(ysi$pH) & ysi$pH < 4] <- NA

## CHECK FOR DUPLICATES
dup_check <- ysi |>
  group_by(Reservoir, Site, DateTime, Depth_m) |>
  mutate(n = n()) |>
  filter(n > 1)

if (nrow(dup_check) > 0){
  warning('DUPLICATE DATA FOUND - removing duplicates')
  duplicates_df <- ysi

  deduped_df <- duplicates_df |>
    distinct()

  ysi <- deduped_df
}


# ## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
#package_ID <- 'edi.198.11'
#eml <- read_metadata(package_ID)
#date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
#last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)

#ysi <- ysi |> filter(DateTime > last_edi_date)

if (!is.null(outfile)){
# Write to CSV -- save as L1 file
write.csv(ysi, outfile, row.names = FALSE)
}

return(ysi)
}

#maintenance_file <- 'Data/DataNotYetUploadedToEDI/YSI_PAR/maintenance_log.csv'
#data_file <- 'https://docs.google.com/spreadsheets/d/1HbSBEFjMuK4Lxit5MRbATeiyljVAB-cpUNxO3dKd8V8/edit#gid=1787819257'
#outfile <- 'Data/DataNotYetUploadedToEDI/YSI_PAR/ysi_L1.csv'

#ysi_qaqc(data_file = data_file,
#         maintenance_file = maintenance_file,
#         gsheet_data = TRUE,
#         outfile = outfile)
