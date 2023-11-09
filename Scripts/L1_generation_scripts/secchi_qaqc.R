## QAQC secchi data -- pull from google sheets and save as csv 

library(tidyverse)
library(gsheet)

secchi_qaqc <- function(maintenance_log = NULL){
  
gsheet_url <- 'https://docs.google.com/spreadsheets/d/1fvM0fDRliuthicQWZT7c9RYErikI5DwrzbOC7TCoMGI/edit#gid=1172894977'
secchi_df <- gsheet::gsheet2tbl(gsheet_url)

secchi_df$DateTime =parse_date_time(secchi_df$DateTime, orders = c('ymd HMS','ymd HM','ymd','mdy'))
#secchi_df$DateTime <- as.POSIXct(strptime(secchi_df$DateTime, "%Y-%m-%d %H:%M"), tz = 'America/New_York') ## need to fix dates that don't have timestamp
secchi_df$Flag_Datetime <- NA

secchi_reformat <- secchi_df |> 
  rename(Flag_Secchi_m = Flag_Secchi)

secchi_reformat <- secchi_reformat |> 
  filter(!is.na(Secchi_m) ) |>   # Omit rows where all Secchi values NA (e.g., rows from files with trailing ,'s)
  mutate(Flag_Secchi_m = ifelse(is.na(Secchi_m), 1, 0), 
  Flag_DateTime = ifelse(Notes=="No time was recorded",1,0))  |> # Add 'flag' columns for each variable; 1 = flag (Flag for night sampling) 
  select(Reservoir, Site, DateTime, Secchi_m, Flag_DateTime, Flag_Secchi_m) |>    # Arrange order of columns for final data table
  arrange(Reservoir, DateTime, .by_group = TRUE ) 

secchi_reformat[is.na(secchi_reformat)] <- 0


# #Read in the maintneance log 
# 
# log <- read_csv(maintenance_file, col_types = cols(
#   .default = col_character(),
#   TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
#   TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
#   flag = col_integer()
# ))
# 
# # modify catdata based on the information in the log  
# 
# #Filter out the 7 flag because it is already NAs in the dataset and not maintenance
# log=log%>%filter(flag!=7)
# 
# # modify bvrdata based on the information in the log
# for(i in 1:nrow(log))
# {
#   # get start and end time of one maintenance event
#   start <- log$TIMESTAMP_start[i]
#   end <- log$TIMESTAMP_end[i]
#   
#   
#   # get indices of columns affected by maintenance
#   if(grepl("^\\d+$", log$colnumber[i])) # single num
#   {
#     maintenance_cols <- intersect(c(2:46), as.integer(log$colnumber[i]))
#   }
#   
#   else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", log$colnumber[i])) # c(x;y;...)
#   {
#     maintenance_cols <- intersect(c(2:46), as.integer(unlist(regmatches(log$colnumber[i],
#                                                                         gregexpr("\\d+", log$colnumber[i])))))
#   }
#   else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", log$colnumber[i])) # c(x:y)
#   {
#     bounds <- as.integer(unlist(regmatches(log$colnumber[i], gregexpr("\\d+", log$colnumber[i]))))
#     maintenance_cols <- intersect(c(2:46), c(bounds[1]:bounds[2]))
#   }
#   else
#   {
#     warning(paste("Could not parse column colnumber in row", i, "of the maintenance log. Skipping maintenance for",
#                   "that row. The value of colnumber should be in one of three formats: a single number (\"47\"), a",
#                   "semicolon-separated list of numbers in c() (\"c(47;48;49)\"), or a range of numbers in c() (\"c(47:74)\").",
#                   "Other values (even valid calls to c()) will not be parsed properly."))
#     next
#   }
#   
#   # remove EXO_Date and EXO_Time columns from the list of maintenance columns, because they will be deleted later
#   maintenance_cols <- setdiff(maintenance_cols, c(24, 25))
#   
#   if(length(maintenance_cols) == 0)
#   {
#     warning(paste("Did not parse any valid data columns in row", i, "of the maintenance log. Valid columns have",
#                   "indices 2 through 46, excluding 24 and 25, which are deleted by this script. Skipping maintenance for that row."))
#     next
#   }
#   
#   #index the Flag columns
#   if(grepl("^\\d+$", log$flagcol[i])) # single num
#   {
#     flag_cols <- intersect(c(47:86), as.integer(log$flagcol[i]))
#     
#   }
#   else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", log$flagcol[i])) # c(x;y;...)
#   {
#     flag_cols <- intersect(c(47:86), as.integer(unlist(regmatches(log$flagcol[i],
#                                                                   gregexpr("\\d+", log$flagcol[i])))))
#   }
#   
#   else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", log$flagcol[i])) # c(x:y)
#   {
#     bounds_flag <- as.integer(unlist(regmatches(log$flagcol[i], gregexpr("\\d+", log$flagcol[i]))))
#     flag_cols <- intersect(c(47:86), c(bounds_flag[1]:bounds_flag[2]))
#   }
#   else
#   {
#     warning(paste("Could not parse column flagcol in row", i, "of the maintenance log. Skipping maintenance for",
#                   "that row. The value of colnumber should be in one of three formats: a single number (\"47\"), a",
#                   "semicolon-separated list of numbers in c() (\"c(47;48;49)\"), or a range of numbers in c() (\"c(47:74)\").",
#                   "Other values (even valid calls to c()) will not be parsed properly."))
#     next
#   }
#}

write.csv(secchi_reformat, './Data/DataNotYetUploadedToEDI/Secchi/secchi_L1.csv')

}

secchi_qaqc()
