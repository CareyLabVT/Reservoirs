# Title: Metals data wrangling script
# Author: Cece Wood
# Date: 18JUL23
# Edit: 05 Dec 23 A. Breef-Pilz

# Purpose: convert metals data from the ICP-MS lab format to the format needed
# for publication to EDI

# 1. Read in Maintenance Log and Sample ID Key
# 2. Compile the files from Jeff and add Site information
# 3. Read in the Time of sampling sheet and add to data frame
# 4. Read in MRL and add flags
# 5. Use Maintenance Log to flag or change observations
# 6. Switch observations if total and soluble samples were mixed up
# 7. Save files

# Read in packages
pacman::p_load("tidyverse", "lubridate", "gsheet")

metals_qaqc <- function(directory = "./Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/2023/",
                        sample_ID_key = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Scripts/Metals_Sample_Depth.csv", 
                        maintenance_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Metals_Maintenance_Log.csv",
                        sample_time = "https://docs.google.com/spreadsheets/d/1NKnIM_tjMxMO0gVxzZK_zVlUSQrdi3O7KqnyRiXo4ps/edit#gid=344320056",
                        MRL_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/MRL_metals.txt",
                        outfile = "./Data/DataNotYetUploadedToEDI/Metals_Data/metals_L1.csv",
                        ISCO_outfile = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/ISCO_metals_L1.csv"){
  
  # These are so I can run the function one step at a time and figure everything out. 
  # Leave for now while still in figuring out mode
  directory = "./Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/2023/"
  sample_ID_key = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Scripts/Metals_Sample_Depth.csv"
  maintenance_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Metals_Maintenance_Log.csv"
  sample_time = "https://docs.google.com/spreadsheets/d/1NKnIM_tjMxMO0gVxzZK_zVlUSQrdi3O7KqnyRiXo4ps/edit#gid=344320056"
  MRL_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/MRL_metals.txt"
  outfile = "./Data/DataNotYetUploadedToEDI/Metals_Data/metals_L1.csv"
  ISCO_outfile = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/ISCO_metals_L1.csv"
  
  #### 1. Read in Maintenance Log and Sample ID Key ####
  
  # Read in Maintenance Log
  
  log_read <- read_csv(maintenance_file, skip=38, col_types = cols(
    .default = col_character(),
    Sample_Date = col_date("%Y-%m-%d"),
    flag = col_integer(),
    Sample_ID = col_integer(),
    Site = col_integer(),
    Depth_m = col_integer()
  ))
  
  # Read in Sample ID Key 
  
  #read in metals ID, reservoir, site, depth, and total/soluble key
  metals_key <- read_csv(sample_ID_key)%>% 
    dplyr::rename(Depth_m =`Sample Depth (m)`,
                  Sample_ID = Sample)
  
  # Combine to add Reservoir, Site and Depth to the Log for files that are identified with sample ID
  log <- left_join(log_read, metals_key, by = c('Sample_ID'))%>%
        mutate(
          Reservoir = coalesce(Reservoir.x, Reservoir.y),
          Site = coalesce(Site.x, Site.y),
          Depth_m = coalesce(Depth_m.x, Depth_m.y)
               )%>%
    # Select the columns we want
    select(Reservoir, Site, Depth_m, Filter, DataStream, Sample_ID,
           Sample_Date, start_parameter, end_parameter, flag, notes)
    
  
  ### 2. Read in and combine all metals files ####
  
  # make a function that reads in the files and takes the columns we want
  read_metals_files <- function(FILES){
    
  al <- read_csv(FILES, skip = 3, col_names = T)%>%
    dplyr::rename(Date_ID = `...1`)%>%
    drop_na(Date_ID)%>%
    rename_with(~paste0(gsub("[[:digit:]]", "", gsub("\\s*\\([^\\)]+\\)", "", .)), "_mgL"), -1)%>%
    separate(Date_ID,c("DateTime","Sample_ID")," - ") %>%
    mutate(DateTime = as.Date(DateTime,format = "%m/%d/%Y"),
           Sample_ID = as.numeric(Sample_ID))%>%
    select(DateTime, Sample_ID, Li_mgL, Na_mgL, Mg_mgL, Al_mgL, Si_mgL, K_mgL, Ca_mgL,
           Fe_mgL, Mn_mgL,Cu_mgL, Sr_mgL, Ba_mgL)%>%
    modify_if(is.character, ~as.numeric(gsub(",","",.))/1000)
    
    return(al)
 
  }
  # use purr to read in all the files using the function above
  ICP<-list.files(path=directory, pattern="", full.names=TRUE)%>%
    map_df(~ read_metals_files(.x))
  
 
 
#set up final data frame with correct formatting!
 frame1 <- left_join(ICP, metals_key, by = c('Sample_ID'))%>% 
   select(-Sample_ID)%>%
   group_by(DateTime, Reservoir, Depth_m, Site)%>% 
   pivot_wider(names_from = 'Filter', 
                              values_from = c('Li_mgL':'Ba_mgL'),
                               names_glue = "{Filter}_{.value}")%>%  # names the columns as we want
   # sum obs is for a tabulator for duplicates and average in case there are duplicates
   summarise(across(everything(), list(z = ~mean(.x, na.rm = TRUE), n = ~ sum(!is.na(.))), .names ="{.fn}_{.col}"))%>%
   rename_all(~ str_remove(., "^z_"))%>% #named the mean column z but then remove the z
   mutate(across(everything(), ~ifelse(is.nan(.), NA, .))) %>%  #gets rid of NaNs created by taking mean during summarize step
   ungroup()
   
   
   
   # read in the timesheet with the date and time the samples were taken
   
   time_sheet <- gsheet::gsheet2tbl(sample_time)%>%
     select(Reservoir, Site,DateTime,Depth_m,VT_Metals)%>%
     filter(VT_Metals =="X")%>% #only take obs when metals samples were collected
     mutate(
       DateTime = parse_date_time(DateTime, orders = c('ymd HMS','ymd HM','ymd','mdy')),
       Date = as.Date(DateTime),
       Site = as.numeric(Site),
       Depth_m = as.numeric(Depth_m))%>%
     select(-VT_Metals)
   
   # add the time the sample was collected
   
   frame2 <- 
     merge(frame1,time_sheet, by.x=c("DateTime", "Reservoir", "Site", "Depth_m"), 
                                by.y=c("Date", "Reservoir", "Site", "Depth_m"), all.x=T)%>%
     mutate(
       DateTime.y = ifelse(is.na(DateTime.y), as_datetime(DateTime), DateTime.y),
       DateTime.y = as_datetime(DateTime.y) # time is in seconds put it in ymd_hms
     )%>%
     select(-DateTime)%>%
     dplyr::rename(DateTime=DateTime.y)%>%
     # This section flags if there was no time recorded and also changes time to 25 hour time
     mutate(Time = format(DateTime,"%H:%M:%S"),
            Time = ifelse(Time == "00:00:00", "12:00:00",Time),
            Flag_DateTime = ifelse(Time == "12:00:00", 1, 0), # Flag if set time to noon
            Date = as.Date(DateTime),
            DateTime = ymd_hms(paste0(Date, "", Time)),
            Hours = hour(DateTime),
            DateTime = ifelse(Hours<5, DateTime + (12*60*60), DateTime), # convert time to 24 hour time
            DateTime = as_datetime(DateTime))%>% # time is in seconds put it in ymd_hms
     select(-c(Time, Date, Hours))%>%
     relocate(DateTime, .before = Depth_m) 
   
   ### 4. Read in the Minimum Reporting Limits and add flags ####
   
   MRL <- read_csv(MRL_file)%>%
     pivot_wider(names_from = 'Symbol', 
                 values_from = "MRL_mgL")
   
   
   # Establish flag columns and add ones for missing values
   for(j in colnames(frame2%>%select(starts_with(c("T_","S_"))))) { 
     
     #for loop to create new columns in data frame
     #creates flag column + name of variable
     frame2[,paste0("Flag_",colnames(frame2[j]))] <- 0 
     
     # puts in flag 1 if value not collected
     frame2[c(which(is.na(frame2[,j]))),paste0("Flag_",colnames(frame2[j]))] <- 1 
     
     # puts in flag 7 for sample run twice and we report the mean
     frame2[c(which(frame2[,paste0("n_",colnames(frame2[j]))]>1)),paste0("Flag_",colnames(frame2[j]))] <- 7 
     
     # If value negative set to minimum reporting level
     frame2[c(which(frame2[,j]<0)),paste0("Flag_",colnames(frame2[j]))] <- 4 
     
     # get the minimum detection level
     MRL_value <- as.numeric(MRL[1,gsub("T_|S_","",j)]) 
     
     # If value is less than MRL then flag and will set to MRL later
     frame2[c(which(frame2[,j]<=MRL_value & frame2[,paste0("Flag_",colnames(frame2[j]))]==0)),paste0("Flag_",colnames(frame2[j]))] <- 3 
     
     # replace the negative values or below MRL with the MRL
     frame2[c(which(frame2[,j]<=MRL_value)),j] <- MRL_value 
     
     # Get the sd and the mean for flagging
     sd_value <- sd(frame2[,j], na.rm = TRUE) # get the minimum detection level
     
     mean_value <- mean(frame2[,j], na.rm = TRUE)
     
     # Flag values over 3 standard deviations above the mean for the year. 
     #This will change each time we add more observations
     
     frame2[c(which(frame2[,j]>=mean_value + (sd_value*3))),paste0("Flag_",colnames(frame2[j]))] <- 8 
     
   }
   
   # Now we can remove the number of observation columns
   raw_df <- frame2%>%
     select(-starts_with("n_"))
  
   
   ### 5. Use Maintenance Log to flag or change observations ####
   
   # Filter the Maintenance Log based on observations in the data frame
   raw_df <- raw_df%>%
      arrange(DateTime)
   
   # Get the date the data starts
    start_date <- raw_df[1,"DateTime"]
     
   # Get the date the data ends
    end_date <- tail(raw_df, n=1)$DateTime
    
    # Filter out the maintenance log
    log <- log%>%
      filter(Sample_Date>=start_date & Sample_Date<= end_date)
   
   
   ### 5.1 Get the information in each row of the Maintenance Log ####
   # modify raw_df based on the information in the log  
    
  # only run if there are observations in the maintenance log  
  if(nrow(log)>0){
   
   for(i in 1:nrow(log)){
     
     ### Get the date the samples was taken
     Sample_Date <- log$Sample_Date[i]
     
     
     
     ### Get the Reservoir
     
     Reservoir <- log$Reservoir[i]
     
     ### Get the Site
     
    Site <- log$Site[i]
     
     ### Get the Depth
     
    Depth <- log$Depth_m[i]


     ### Get the Maintenance Flag 
     
     flag <- log$flag[i]
     
     
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
     
     flag_cols <- paste0("Flag_", maintenance_cols)
  
     
     
     ### 5.2 Actually remove values in the maintenance log from the data frame 
     ## This is where information in the maintenance log gets removed. 
     # UPDATE THE IF STATEMENTS BASED ON THE NECESSARY CRITERIA FROM THE MAINTENANCE LOG
    
     # replace relevant data with NAs and set flags while maintenance was in effect
     if(flag==1){ 
       # Sample not collected. Not used in the maintenance log
       
     }
     else if (flag==2){
       # Instrument Malfunction. How is this one removed?
       raw_df[c(which(raw_df[,"Date"] == Sample_Date & raw_df[,"Reservoir"] == Reservoir 
                      & raw_df[,"Site"] == Site & raw_df[,"Depth_m"] == Depth)), 
              maintenance_cols] <- NA
       
       raw_df[c(which(raw_df[,"Date"] == Sample_Date & raw_df[,"Reservoir"] == Reservoir 
                      & raw_df[,"Site"] == Site & raw_df[,"Depth_m"] == Depth)), 
              flag_cols] <- flag
     } 
     else if (flag ==6){
       # Sample was digested because there were so need to multiply the concentration by 2.2
      
       raw_df[c(which(raw_df[,"Date"] == Sample_Date & raw_df[,"Reservoir"] == Reservoir 
                      & raw_df[,"Site"] == Site & raw_df[,"Depth_m"] == Depth)), 
              maintenance_cols] <- 
         raw_df[c(which(raw_df[,"Date"] == Sample_Date & raw_df[,"Reservoir"] == Reservoir 
                                                  & raw_df[,"Site"] == Site & raw_df[,"Depth_m"] == Depth)), 
                                          maintenance_cols] * 2.2
       # Flag the sample here
       raw_df[c(which(raw_df[,"Date"] == Sample_Date & raw_df[,"Reservoir"] == Reservoir 
                      & raw_df[,"Site"] == Site & raw_df[,"Depth_m"] == Depth)), 
              flag_cols] <- flag
     }
     
     else {
       warning("Flag used not defined in the L1 script. Talk to Austin and Adrienne if you get this message")
     }
    
     next
   }
  }
    
    
    
    
    #
    #
    #
    #
    #
    #
    #
    #
    #
   #### 6. Switch observations if total and soluble samples were mixed up ####
   
   # Determine if totals and soluble samples were switched. 
   # Totals plus the Minimum reporting level is less than the soluble sample then they need to be 
   # switched. 
   # Cece is this what you want it to be? It looks like some of the observations are very close.
    #we want to do 3 MRL for Fe, Al, and Si, give it a flag of 9, and then see what it looks like
   for(l in colnames(raw_df%>%select(starts_with(c("T_"))))) { 
     #for loop to create new columns in data frame
     raw_df[,paste0("Check_",colnames(raw_df[l]))] <- 0 #creates Check column + name of variable
     
     MRL_value <- as.numeric(MRL[1,gsub("T_|S_","",j)]) # get the minimum detection level
     
     # Puts "SWITCHED" in the Check column if the soluble concentration is greater than the totals plus the MRL
     raw_df[c(which(raw_df[,l]+MRL_value<raw_df[,gsub("T_", "S_", l)])),paste0("Check_",colnames(raw_df[l]))] <- "SWITCHED" 
     
     # Swap the observations from the totals and solubles if the Check column is labeled "SWITCHED" 
     
     raw_df[c(which(raw_df[,paste0("Check_",l)]=="SWITCHED")), c(l,gsub("T_", "S_", l)) ] <- 
       raw_df[c(which(raw_df[,paste0("Check_",l)]=="SWITCHED")), c(gsub("T_", "S_", l), l)]
   }
   
   # Change the column headers so they match what is already on EDI. Added T_ because it is easier in the 
  
   frame4 <- raw_df%>%
     rename_with(~gsub("T_", "T", gsub("S_", "S",.)), -1)
   
#let's write the final csv
#note: you must edit the script each time to save the correct file name
 frame4 <- frame4 %>% 
   select(Reservoir, Site, DateTime, Depth_m,  TLi_mgL:SBa_mgL, Flag_DateTime, 
          Flag_TLi_mgL, Flag_SLi_mgL, Flag_TNa_mgL, Flag_SNa_mgL,
          Flag_TMg_mgL, Flag_SMg_mgL, 
          Flag_TAl_mgL, Flag_SAl_mgL, Flag_TSi_mgL, Flag_SSi_mgL,
          Flag_TK_mgL, Flag_SK_mgL, Flag_TCa_mgL, Flag_SCa_mgL, 
          Flag_TFe_mgL, Flag_SFe_mgL, Flag_TMn_mgL, Flag_SMn_mgL,
          Flag_TCu_mgL, Flag_SCu_mgL, Flag_TSr_mgL, Flag_SSr_mgL,
          Flag_TBa_mgL, Flag_SBa_mgL) %>% 
   arrange(DateTime, Reservoir, Site, Depth_m)
 
 #### 7. Save Files ####
 
 # Save the metals data frame
 # Remove the ISCO samples 
 final <- frame4%>%
   filter(Site != 100.1)
 
 # Write the L1 file 
 write.csv(final, outfile, row.names = F)
 
 # Save the ISCO observations
 ISCO <- frame4%>%
   filter(Site == 100.1)
 
 write.csv(ISCO, ISCO_outfile, row.names = F)
 

}
