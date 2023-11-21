# This is a template script to use to make your L1 QAQC function. 
# This script includes:
# 1. Read in files and Maintenance Log
# 2. Process the files if necessary. Eg. GHG get concentration
# 3. Make Flag columns and add Flags for missing values or negative values if applicable
# 4. Take out observations based on the Maintenance Log
# 5. Additional maintenance
# 6. Write file to a csv

# Identify the package you need for your QAQC function
# Download/load libraries
pacman::p_load(lubridate,tidyverse)

# Start function HERE
variable_qaqc<- function(raw_file =,
                         maintenance_log =, 
                         year = , 
                         out_file =){
  
  #### 1. Read in the Raw files then collate if necessary and Maintenance Log ####
  
  # If it is just a raw file then 
  raw_df <- read_csv(raw_file)
  
  ## If the files need to be bound together
  
  # List files based on current year. We can decide if we want only keep the current year. 
  # We will have to look at how the file names are formatted
  myfiles = list.files(path=raw_files, pattern=paste0("_",year), full.names=TRUE)
  
  # Take out files if necessary. CHANGE PATTERN
  files <- myfiles[ !grepl("BVR_manual_*", myfiles) ]
  
  # Use the purr function to combine the data files. Might have to change the file names or 
  # create a vector with new file names
  
  raw_df<-files %>%
    map_df(~ read_csv(.x, skip = 1, col_types = list(
      TIMESTAMP = "T", .default = "d")))%>%
    filter(grepl("^20", TIMESTAMP))%>% #keep only the right TIMESTAMP rows 
    distinct(TIMESTAMP, .keep_all= TRUE) # eliminates duplicates. Might need to change this. 
  
  ## Read in Maintenance File
   
  log_read <- read_csv(maintenance_file, col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  ))
  
  log <- log_read
  
  
  ### 2. Process the files if necessary ####
  
  
  ### 3. Make Flag Columns and add flags for missing values and negative values ####
  
  ### List the flags and what they mean here
  
  # for loop to create flag columns
  for(j in colnames(raw%>%select(ADD COLUMN NAMES HERE))) { 
    #for loop to create flag column + name of variable
    raw_df[,paste0("Flag_",colnames(raw_df[j]))] <- 0 
    #puts in flag 7 if value not collected
    raw_df[c(which(is.na(raw_df[,j]))),paste0("Flag_",colnames(raw_df[j]))] <-7 
  }
  
  # Change negative values to 0
  for(k in colnames(raw_df%>%select(ADD COLUMN NAMES HERE))) { 
    #for loop to create new columns in data frame
    raw_df[c(which((raw_df[,k]<0))),paste0("Flag_",colnames(raw_df[k]))] <- 3
    raw_df[c(which((raw_df[,k]<0))),k] <- 0 #replaces value with 0
  }
  
  ### 4. Take out values based on the Maintenance Log ####
  
  # modify raw_df based on the information in the log   
  
  for(i in 1:nrow(log))
  {
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
      Depth <- as.numeric(log$new_value[i])
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
      
      maintenance_cols <- colnames(raw_df%>%select(colname_end)) 
      
    }else if(is.na(colname_end)){
      
      maintenance_cols <- colnames(raw_df%>%select(colname_start))
      
    }else{
      maintenance_cols <- colnames(raw_df%>%select(colname_start:colname_end))
    }
    
    ### Get the name of the flag column
    
    flag_col <- paste0("Flag_", maintenance_cols)
    
    ### remove any Flag columns that don't exsist because we don't have a flag column for them
    # and they get removed before publishing
    
    flag_col = flag_col[!flag_col %in% c(COLUMN NAMES HERE)]
    
    ### Getting the start and end time vector to fix. If the end time is NA then it will put NAs 
    # until the maintenance log is updated
    
    if(is.na(end)){
      # If there the maintenance is on going then the columns will be removed until
      # and end date is added
      Time <- raw_df$DateTime >= start
      
    }else if (is.na(start)){
      # If there is only an end date change columns from beginning of data frame until end date
      Time <- raw_df$DateTime <= end
      
    }else {
      
      Time <- raw_df$DateTime >= start & raw_df$DateTime <= end
    }
    
    ### This is where information in the maintenance log gets removed. 
    # UPDATE THE IF STATEMENTS BASED ON THE NECESSARY CRITERIA FROM THE MAINTENANCE LOG
    
    # replace relevant data with NAs and set flags while maintenance was in effect
    if(flag==5)
    { # THIS IS FOR VALUES THAT ARE FLAGGED BUT LEFT IN THE DATA SET
      raw_df[Time, flag_cols] <- flag
    }
    else if (flag==7){
      
    }
    else if(flag==8 && maintenance_cols==NAME HERE)
    { # THIS IS CHANGED BASED ON OFFSET
      raw_df[Time, maintenance_cols] <- raw_df[Time, maintenance_cols]-0.22617
      raw_df[Time, flag_cols] <- flag
    }
    else if (flag==8 && maintenance_cols==NAME HERE)
    {
      raw_df[Time, maintenance_cols] <- raw_df[Time, maintenance_cols]-0.18122 
      raw_df[Time, flag_cols] <- flag
    }
    else 
    {
      # The observations are changed to NA for maintenance or other issues found in the maintenance log
      raw_df[Time, maintenance_cols] <- NA
      raw_df[Time, flag_cols] <- flag
    }
  }
  
  ### 5. Additional Maintenance ####
  
  ### 6. Write file to a csv ###
  
  # convert datetimes to characters so that they are properly formatted in the output file
  raw_df$DateTime <- as.character(raw_df$DateTime)
  
  
  # write to output file
  write_csv(raw_df, output_file)
  
}
