## QAQC secchi data -- pull from google sheets and save as csv 

library(tidyverse)
library(gsheet)

secchi_qaqc <- function(maintenance_log = NULL){
  
gsheet_url <- 'https://docs.google.com/spreadsheets/d/1fvM0fDRliuthicQWZT7c9RYErikI5DwrzbOC7TCoMGI/edit#gid=1172894977'
secchi_df <- gsheet::gsheet2tbl(gsheet_url)

secchi_df$Reservoir <- as.character(secchi_df$Reservoir)
secchi_df$Site <- as.numeric(secchi_df$Site)
secchi_df$Secchi_m <- as.numeric(secchi_df$Secchi_m)
secchi_df$Flag_Secchi_m <- as.numeric(secchi_df$Flag_Secchi_m)
secchi_df$Notes <- as.character(secchi_df$Notes)


# fix dates
secchi_df$DateTime = lubridate::parse_date_time(secchi_df$DateTime, orders = c('ymd HMS','ymd HM','ymd','mdy'))
#secchi_df$DateTime <- as.POSIXct(strptime(secchi_df$DateTime, "%Y-%m-%d %H:%M"), tz = 'America/New_York') ## need to fix dates that don't have timestamp
secchi_df$Flag_DateTime <- NA

## fill in any missing datetimes with noon
secchi_df <- secchi_df %>%
  mutate(Time = format(DateTime,"%H:%M:%S"),
         Time = ifelse(Time == "00:00:00", "12:00:00",Time),
         Flag_DateTime = ifelse(Time == "12:00:00", 1, 0), # Flag if set time to noon
         Date = as.Date(DateTime),
         DateTime = ymd_hms(paste0(Date, "", Time), tz = "America/New_York"),
         Hours = hour(DateTime),
         DateTime = ifelse(Hours<5, DateTime + (12*60*60), DateTime), # convert time to 24 hour time
         DateTime = as_datetime(DateTime, tz = "America/New_York"))%>% # time is in seconds put it in ymd_hms
  select(-c(Time, Date, Hours))

# secchi_reformat <- secchi_df |> 
#   rename(Flag_Secchi_m = Flag_Secchi)

secchi_reformat <- secchi_reformat |> 
  #filter(!is.na(Secchi_m) ) |>   # Omit rows where all Secchi values NA (e.g., rows from files with trailing ,'s) ## DO WE WANT TO COMPLETELY REMOVE NAS? IF SO WE NEED TO RETHINK HOW FLAGS ARE ASSIGNED IN NEXT LINE
  mutate(Flag_Secchi_m = ifelse(is.na(Secchi_m), 1, 0), 
  Flag_DateTime = ifelse(Notes=="No time was recorded",1,0))  |> # Add 'flag' columns for each variable; 1 = flag (Flag for night sampling) 
  select(Reservoir, Site, DateTime, Secchi_m, Flag_DateTime, Flag_Secchi_m) |>    # Arrange order of columns for final data table
  arrange(Reservoir, DateTime, .by_group = TRUE ) 

secchi_reformat[is.na(secchi_reformat)] <- 0



# ## ADD MAINTENANCE LOG FLAGS (manual edits to the data for suspect samples or human error)
# maintenance_file <- 'Data/DataNotYetUploadedToEDI/YSI_PAR_Secchi/maintenance_log.txt'
# log_read <- read_csv(maintenance_file, col_types = cols(
#   .default = col_character(),
#   TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
#   TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
#   flag = col_integer()
# ))
# 
# log <- log_read
# 
# for(i in 1:nrow(log)){
#   ### Assign variables based on lines in the maintenance log. 
#   
#   ### get start and end time of one maintenance event
#   start <- force_tz(as.POSIXct(log$TIMESTAMP_start[i]), tzone = "America/New_York")
#   end <- force_tz(as.POSIXct(log$TIMESTAMP_end[i]), tzone = "America/New_York")
#   
#   ### Get the Reservoir Name
#   Reservoir <- log$Reservoir[i]
#   
#   ### Get the Site Number
#   Site <- as.numeric(log$Site[i])
#   
#   ### Get the Maintenance Flag 
#   flag <- log$flag[i]
#   
#   ### Get the new value for a column or an offset
#   update_value <- as.numeric(log$update_value[i])
#   
#   ### Get the names of the columns affected by maintenance
#   colname_start <- log$start_parameter[i]
#   colname_end <- log$end_parameter[i]
#   
#   ### if it is only one parameter parameter then only one column will be selected
#   
#   if(is.na(colname_start)){
#     
#     maintenance_cols <- colnames(update_profiles%>%select(colname_end)) 
#     
#   }else if(is.na(colname_end)){
#     
#     maintenance_cols <- colnames(update_profiles%>%select(colname_start))
#     
#   }else{
#     maintenance_cols <- colnames(update_profiles%>%select(colname_start:colname_end))
#   }
#   
#   if(is.na(end)){
#     # If there the maintenance is on going then the columns will be removed until
#     # and end date is added
#     Time <- update_profiles |> filter(DateTime >= start) |> select(DateTime)
#     
#   }else if (is.na(start)){
#     # If there is only an end date change columns from beginning of data frame until end date
#     Time <- update_profiles |> filter(DateTime <= end) |> select(DateTime)
#     
#   }else {
#     Time <- update_profiles |> filter(DateTime >= start & DateTime <= end) |> select(DateTime)
#   }
#   
#   ### This is where information in the maintenance log gets updated 
#   
#   if(flag %in% c(5,6)){ ## UPDATE THIS WITH ANY NEW FLAGS
#     # UPDATE THE MANUAL ISSUE FLAGS (BAD SAMPLE / USER ERROR) AND SET TO NEW VALUE
#     
#     update_profiles[c(which(update_profiles[,'Site'] == Site & update_profiles$DateTime %in% Time$DateTime)),paste0("Flag_",maintenance_cols)] <- as.numeric(flag)
#     update_profiles[c(which(update_profiles[,'Site'] == Site & update_profiles$DateTime %in% Time$DateTime)),maintenance_cols] <- as.numeric(update_value)
#     
#   }else{
#     warning("Flag not coded in the L1 script. See Austin or Adrienne")
#   }
# }
# #### END MAINTENANCE LOG CODE #####


write.csv(secchi_reformat, './Data/DataNotYetUploadedToEDI/Secchi/secchi_L1.csv')

}

secchi_qaqc()
