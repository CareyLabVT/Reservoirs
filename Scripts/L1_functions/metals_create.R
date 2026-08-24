# Title: Metals data wrangling script
# Author: Cece Wood
# Date: 18JUL23
# Edit: 07 Mar. 24 A. Breef-Pilz
# Edit: 30 May 2024 ABP. Move the save ISCO file section up. 
# 24 Sep. 24 Round numeric columns to 4 digits
# 22 Oct. 24 Changed the flipped metals to look at just Fe and Mn
# 23 Oct. 24 Added more arguments so you can save or return the ISCO and or the metals data frame
# 04 Feb. 25 Specified the columns when reading in the historical file and added a step to get times for ISCO observations. For now they are the same as the weir samples. 
# 18 Feb. 25 Added a function when there were no observations for the year
# 23 May 25 Changed the ISCO to take the higher of the duplicated values
# 24 Fed 26 Fixed the section on adding historical MDLS and added in ability to pull from GitHub instead of local directories
# 24 Aug 26 add a line when reading in the maintenance log to accept different time formats

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
pacman::p_load("tidyverse", "lubridate", "gsheet", "rqdatatable", "hms", "httr2")

metals_qaqc <- function(directory,
                        historic = NULL, 
                        sample_ID_key, 
                        maintenance_file,
                        sample_time,
                        MRL_file,
                        metals_save, 
                        metals_outfile, # put metals_save=T and Null to return the file
                        ISCO_save = F, # Do you want to save or use the ISCO file? This allows us to use the function for metals and ISCO separatly.
                        ISCO_outfile, # put ISCO_save=T and Null to return the file
                        start_date = NULL,
                        end_date = NULL)
                        
{
  
 # These are so I can run the function one step at a time and figure everything out.
 # Leave for now while still in figuring out mode
 #directory = c("https://api.github.com/repos/CareyLabVT/Reservoirs/contents/Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/2025/", "https://api.github.com/repos/CareyLabVT/Reservoirs/contents/Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/2026/" )
 #  directory = "./Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/"
 #  historic = "./Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/historic_raw_2014_2019_w_unique_samp_campaign.csv"
 #  sample_ID_key = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Scripts/Metals_Sample_Depth.csv"
 #  maintenance_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Metals_Maintenance_Log.csv"
 #  sample_time = "https://docs.google.com/spreadsheets/d/1MbSN2G_NyKyXQUEzfMHmxEgZYI_s-VDVizOZM8qPpdg/edit#gid=0"
 # MRL_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/MRL_metals.csv"
 # metals_save = T
 # metals_outfile = "./Data/DataNotYetUploadedToEDI/Metals_Data/metals_L1.csv"
 # ISCO_save = T
 # ISCO_outfile = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/ISCO_metals_L1.csv"
 # start_date = NULL
 # end_date = NULL
 #start_date = as.Date("2025-01-01") # change when we update to read date from EDI
 #end_date = Sys.Date() + lubridate::days(1)

  #### 1. Read in Maintenance Log and Sample ID Key ####
  
  # Read in Maintenance Log
  
  log <- read_csv(maintenance_file, col_types = cols(
    .default = col_character(),
    Sample_Date = col_character(),
    flag = col_integer(),
    Sample_ID = col_integer(),
    Site = col_number(),
    Depth_m = col_number()
  ))|>
   mutate(Sample_Date = parse_date_time(Sample_Date, orders = c("ymd", "mdy", "dmy")))
  
  # Read in Sample ID Key 
  
  #read in metals ID, reservoir, site, depth, and total/soluble key
  metals_key <- read_csv(sample_ID_key, show_col_types = F)|> 
    dplyr::rename(Depth_m =`Sample Depth (m)`,
                  Sample_ID = Sample)

    
  
  ### 2. Read in and combine all metals files ####
  
  # make a function that reads in the files and takes the columns we want
  read_metals_files <- function(FILES){
    
  al <- read_csv(FILES, skip = 3, col_names = T, show_col_types = F)|>
    dplyr::rename(Date_ID = `...1`)|>
    select(starts_with("Date"), contains("(STDR"))|> # only select the columns that are the date column and end with (STDR) which is how the samples are labeled 
    drop_na(Date_ID) |>
    rename_with(~paste0(gsub("[[:digit:]]", "", gsub("\\s*\\([^\\)]+\\)", "", .)), "_mgL"), -1)
  
  print(FILES)
  print(al$Date_ID[1])
  
  # warning if the Date_ID column is not acutally not a Date but a names
  if(grepl('[A-Z]', al$Date_ID[1])==T){
  
    al <- NULL
    
    warning("In ", FILES, " The Date_ID column is not in the right format.",
    "Please make sure it does not contain any letters and only has the state and the site number.",
    "File is not included in the combined data frame.")
    
  }else{
    
   al <- al|>
     filter(!grepl("[A-Z]", Date_ID)) |> #filter out 
      separate(Date_ID,c("Date","Sample_ID"),sep = "  | - |-")
   
   # Another check on the Date_ID and Sample_ID column to make sure they have the date and site ID
   if(is.na(al$Sample_ID)[1]==T){
     
     al <- NULL
     
     # Since there the Sample ID doesn't exist then we don't want to add it. 
     warning("In ", FILES, " There are no sample IDs.",
     "Check the first column in the data frame.",
     "File is not included in the combined data frame.")
   }else{
    
    # Determine the order of the Date_ID columns and make sure Date and Sample are in the correct column
    if(is.na(as.Date(al$Date[1], format = "%m/%d/%Y"))){
      
      # If you try to parse the top Date in the data frame and you get an NA,
      # that means the the DateTime and Sample_ID column were switched
      
      al <- al |>
        dplyr::rename("Sample_ID" = Date,
                      "Date" = Sample_ID)
      
    }
    
    al <- al |>
      mutate(Date =parse_date_time(Date, c("mdY", "mdy")),
             Sample_ID = as.numeric(Sample_ID))|>
      select(Date, Sample_ID, Li_mgL, Na_mgL, Mg_mgL, Al_mgL, Si_mgL, K_mgL, Ca_mgL,
             Fe_mgL, Mn_mgL,Cu_mgL, Sr_mgL, Ba_mgL)|>
      modify_if(is.character, ~as.numeric(gsub(",","",.))/1000)
    
    
    
  }
  }
  return(al)
  }
  
  # Read in the data from either your local computer or from GitHub depending on what your put in the directory argument. 
  
  
  if(grepl("https", directory)){
    
    #create list of file names
    resp <- request(directory) |>
      req_headers(Accept = "application/vnd.github+json") |>
      req_perform()
    
    dat <- resp_body_json(resp, simplifyVector = TRUE)
    
    # make a blank data frame
    files <- NULL
    
    # run through all the files
    for(i in 1:nrow(dat)){
      # get the files in the sub folder
      if(dat$size[i] == 0){
        #print(i)
        resp_sub <- request(dat$url[i])|>
          req_headers(Accept = "application/vnd.github+json") |>
          req_perform()
        
        dat2 <- resp_body_json(resp_sub, simplifyVector = TRUE)
        
        files <- append(files, dat2$download_url)
        
        # make a list of the files
      }else{
        files <- append(files, dat$download_url)|>
          unique()
      }
    }
    
    # now that we have a list of files get the ones that match the pattern
    
    files <- files[grepl("ICPMS", files) ]#make sure they follow the right name
    print("Files from GitHub")
    
  }else{
    
    # List the files in the folder on your local computer 
    files<-list.files(path= directory, pattern="ICPMS", full.names=TRUE, recursive = T)
    print("Files from local computer")
  }
  
  
  # Take out the files that are in the Files_dont_follow_key folder
  ICP2 <- files[grepl("\\d+[/ICPMS]", files)]
  
  # use map to read in all the files using the function above
  ICP <-ICP2 |>
    #list.files(path=directory, pattern="ICPMS", full.names=TRUE, recursive=TRUE)|>
    map_df(~ read_metals_files(.x))
    #drop_na(Date) # when NA in DateTime column. Maybe a warning?
  
  # Take out dup observations when ISCO samples when we were able to run samples without needing a digestion
  
   ## This is a quick fix until we figure out what to do/where the other metals dups came from 
  
  ICP_ISCO <- ICP|>
    filter(Sample_ID %in% c(29,30))|> # just ISCO samples for now
    group_by(Date, Sample_ID)|>
    dplyr::slice_max(Al_mgL, n=1)|> # take the higher of the two values
    ungroup()
  
  ICP_notISCO <- ICP|>
    filter(Sample_ID != 29)|>
    filter(Sample_ID != 30)
  
  ICP2 <- bind_rows(ICP_notISCO, ICP_ISCO)
  
  print("Read in files and combined them together")
 
#set up data frame with Reservoir, Site, Depth, and filter
  # then pivot longer so we can get the mean of any samples that had to be rerun
 frame1 <- left_join(ICP2, metals_key, by = c('Sample_ID'))|>
   select(-Sample_ID)|>
   distinct(Date, Reservoir, Depth_m, Site, Filter, .keep_all = TRUE) |>
   select(Reservoir, Site, Depth_m, Filter, Date, everything()) |>
   pivot_longer(cols=c(Li_mgL:Ba_mgL), names_to="element", values_to="obs")|>
   group_by(Reservoir, Site, Depth_m, Filter, Date, element)|>
    summarize(
     count = n(), # get the number of samples
     mean = mean(obs, na.rm = TRUE))|> # take the mean. Most if not all are one so is the same value
   ungroup()

 # now pivot wider so we can make the flag columns
 frame <- frame1|>
   pivot_wider(names_from = "element", values_from = c("mean", "count"))

 # take out mean from column header
 names(frame) = gsub(pattern = "mean_", replacement = "", x = names(frame))

 # reorder the columns
 frame2 <- frame|>
   select(Reservoir, Site, Depth_m, Filter, Date, Li_mgL, Na_mgL, Mg_mgL,
          Al_mgL, Si_mgL, K_mgL, Ca_mgL, Fe_mgL, Mn_mgL, Cu_mgL, Sr_mgL, Ba_mgL, everything())

 ## add a warning if observation does not have a Reservoir and Site

 # Add in the historic files from 2014_2019 plus some one off sampling campaigns. We only have Fe and Mn for that time.

 if (is.null(start_date) & !is.null(historic)|| start_date<as.Date("2020-01-01") & !is.null(historic)){
   
   hist <- read_csv(historic, col_types = list(Reservoir = "c",
                                               Site = "d",
                                               Date = "T",
                                               Time = "t",
                                               Filter = 'c',
                                               Fe_mgL = 'd',
                                               Mn_mgL = 'd',
                                               count_Fe_mgL = 'd',
                                               count_Mn_mgL = 'd'))
   
   print("Added historic file")
 }else{
   hist <- NULL
   
   print("Did not add historic file")
 }
 
 

 # bind the historic files and the current files
 frame22 <- bind_rows(frame2, hist)%>%
   select(Reservoir, Site, Depth_m, Filter, Date, Li_mgL, Na_mgL, Mg_mgL,
          Al_mgL, Si_mgL, K_mgL, Ca_mgL, Fe_mgL, Mn_mgL, Cu_mgL, Sr_mgL, Ba_mgL, everything())

 # Reorder the date
 frame2 <- frame22[order(frame22$Date),]
 
 # Subset the data for the start and end time 
 ### identify the date subsetting for the data
 if (!is.null(start_date)){
   #force tz check
   start_date <- force_tz(as.POSIXct(start_date), tzone = "America/New_York")
   
   frame2 <- frame2 %>%
     filter(Date >= start_date)
   
 }
 
 if(!is.null(end_date)){
   #force tz check
   end_date <- force_tz(as.POSIXct(end_date), tzone = "America/New_York")
   
   frame2 <- frame2 %>%
     filter(Date <= end_date)
   
 }
 
 # Check if there are any files for the L1. If not then end the script
 
 if(nrow(frame2)==0){
   
   print("No new files for the current year")
   
 }else{

 # Establish flag columns and add ones for missing values
 for(j in colnames(frame2|>select(Li_mgL:Ba_mgL))) {

   #for loop to create new columns in data frame
   #creates flag column + name of variable
   frame2[,paste0("Flag_",j)] <- 0

   # puts in flag 1 if value not collected
   frame2[c(which(is.na(frame2[,j]))),paste0("Flag_",j)] <- 1

   # puts in flag 5 for sample run twice and we report the mean. Use the count columns made above
   frame2[c(which(frame2[,paste0("count_",colnames(frame2[j]))]>1)),paste0("Flag_",j)] <- 5
 }

 # Now we can remove the number of observation columns
 raw_df <- frame2|>
   select(-starts_with("count_"))


   ### 5. Use Maintenance Log to flag or change observations ####

   # Filter the Maintenance Log based on observations in the data frame
   raw_df <- raw_df|>
     arrange(Date)|>
     mutate(Date = as.Date(Date))

   # Get the date the data starts
   start <- head(raw_df, n=1)$Date

   # Get the date the data ends
   end <- tail(raw_df, n=1)$Date

   # Filter out the maintenance log
   log <- log|>
     filter(Sample_Date>=start & Sample_Date<= end)



   ### 5.1 Get the information in each row of the Maintenance Log ####
   # modify raw_df based on the information in the log


   # only run if there are observations in the maintenance log
   if(nrow(log)==0){
     print('No Maintenance Events Found...')

   } else {


     for(i in 1:nrow(log)){

       ### Get the date the samples was taken
       Sample_Date <- as.Date(log$Sample_Date[i])

       ### Get the Reservoir

       Reservoir <- log$Reservoir[i]

       ### Get the Site

       Site <- log$Site[i]

       ### Get the Depth

       Depth <- log$Depth_m[i]

       ### Get the Filter status

       Filt <- log$Filter[i]


       ### Get the Maintenance Flag

       flag <- log$flag[i]


       ### Get the names of the columns affected by maintenance

       colname_start <- log$start_parameter[i]
       colname_end <- log$end_parameter[i]

       ### if it is only one parameter parameter then only one column will be selected

       if(is.na(colname_start)){

         maintenance_cols <- colnames(raw_df|>select(colname_end))

       }else if(is.na(colname_end)){

         maintenance_cols <- colnames(raw_df|>select(colname_start))

       }else{
         maintenance_cols <- colnames(raw_df|>select(c(colname_start:colname_end)))
       }

       ### Get the name of the flag column

       flag_cols <- paste0("Flag_", maintenance_cols)


       #### find the row where all of these match
       #### The first part is the list of columns in the data frame then after %in% is the value we want
       #### to find in the data frame.
       #### All give us the rows that everything is true

     All <-  which(raw_df$Date %in% Sample_Date & raw_df$Reservoir %in% Reservoir &
                     raw_df$Site %in% Site & raw_df$Depth_m %in% Depth & raw_df$Filter %in% Filt)


       ### 5.2 Actually remove values in the maintenance log from the data frame
       ## This is where information in the maintenance log gets removed.
       # UPDatetime THE IF STATEMENTS BASED ON THE NECESSARY CRITERIA FROM THE MAINTENANCE LOG

       # replace relevant data with NAs and set flags while maintenance was in effect
       if(flag==1){
         # Sample not collected. Not used in the maintenance log

       }
       else if (flag==2){
         # Instrument Malfunction. How is this one removed?
         raw_df[All, maintenance_cols] <- NA

         # Flag the sample here
         raw_df[All, flag_cols] <- flag
       }
       else if (flag ==4){
         # Sample was digested because there were particulates, so need to multiply the concentration by 2.2

         raw_df[All, maintenance_cols] <- raw_df[All, maintenance_cols] * 2.2

         # Flag the sample here
         raw_df[All, flag_cols] <- flag
       }
       else if (flag==6){
         # suspect sample, doesn't get flagged below but is manually flagged in maintenance log

         # Flag the sample here
         raw_df[All, flag_cols] <- flag
       }
     else if (flag==8){
       # improper procedure, set all data columns to NA and all flag columns to 10
       raw_df[All, maintenance_cols] <- NA
       
       # Flag the sample here
       raw_df[All, flag_cols] <- flag
     }
       else {
         warning("Flag used in row ", i ," in the maintenance log not defined in the L1 script. Talk to Austin and Adrienne if you get this message")
       }

       next
     }
   }


   print("Created flag columns and used maintenance log to qaqc the data.")
   ### 4. Read in the Minimum Reporting Limits and add flags ####

   ### ABP fixed to make it work now but please feel free to change 
   
   MRL <- read_csv(MRL_file, show_col_types = F)#|>
     #separate_wider_delim(Symbol, delim = "_", names = c("Metal", "units"))
     # pivot_wider(names_from = 'Symbol',
     #             values_from = "MRL_mgL") %>%
     # rename_with(~str_c("MRL_", .), Al_mgL:Sr_mgL)
   
   
   # pivot the data frame longer
   
   
   long_raw_df <- raw_df |>
     # normalize naming so Conc and Flag columns share a pattern: {prefix}_{Metal}_mgL
     rename_with(~paste0("Conc_", .x), .cols = ends_with("mgL") & !starts_with("Flag")) |>
     pivot_longer(
       cols = -c(Reservoir, Site, Date, Depth_m, Filter),
       names_to = c(".value", "Metal"),
       names_pattern = "^(Conc|Flag)_(.+)_mgL$") |>
     mutate(Year = year(Date), Metal_mgL = paste0(Metal, "_mgL")) |>
     left_join(MRL, by = join_by(Metal_mgL == Symbol, Year)) |>
     mutate(
       Flag = as.character(Flag),
       Flag = case_when(
         is.na(Conc) ~ "1",           # missing concentration
         Conc <= MRL_mgL ~ "3",         # below/at minimum reporting level
         TRUE ~ Flag),
       Conc = if_else(Flag == "3", MRL_mgL, Conc) # if Conc is below/at the MRL, set it to the MRL
     ) |>
     pivot_wider(
       id_cols = c(Reservoir, Site, Date, Depth_m, Filter),
       names_from = Metal,
       values_from = c(Conc, Flag),
       names_glue = "{ifelse(.value == 'Conc', paste0(Metal, '_mgL'), paste0('Flag_', Metal, '_mgL'))}"
     )
   
   

   
     
   # old code below and the code above does the same thing but in a different way  
   
   # for ease of use, add year column to raw_df
   # raw_df <-  raw_df |>
   #   mutate(Year = year(Date)) |> 
   #   left_join(MRL)  |>
   #   mutate(Flag_Li_mgL = if_else(!is.na(Li_mgL) & Li_mgL <= MRL_Li_mgL, as.numeric(paste0(Flag_Li_mgL, 3, sep = '')), Flag_Li_mgL),
   #          Flag_Na_mgL = if_else(!is.na(Na_mgL) & Na_mgL <= MRL_Na_mgL, as.numeric(paste0(Flag_Na_mgL, 3, sep = '')), Flag_Na_mgL),
   #          Flag_Mg_mgL = if_else(!is.na(Mg_mgL) & Mg_mgL <= MRL_Mg_mgL, as.numeric(paste0(Flag_Mg_mgL, 3, sep = '')), Flag_Mg_mgL),
   #          Flag_Al_mgL = if_else(!is.na(Al_mgL) & Al_mgL <= MRL_Al_mgL, as.numeric(paste0(Flag_Al_mgL, 3, sep = '')), Flag_Al_mgL),
   #          Flag_Si_mgL = if_else(!is.na(Si_mgL) & Si_mgL <= MRL_Si_mgL, as.numeric(paste0(Flag_Si_mgL, 3, sep = '')), Flag_Si_mgL),
   #          Flag_K_mgL = if_else(!is.na(K_mgL) & K_mgL <= MRL_K_mgL, as.numeric(paste0(Flag_K_mgL, 3, sep = '')), Flag_K_mgL),
   #          Flag_Ca_mgL = if_else(!is.na(Ca_mgL) & Ca_mgL <= MRL_Ca_mgL, as.numeric(paste0(Flag_Ca_mgL, 3, sep = '')), Flag_Ca_mgL),
   #          Flag_Fe_mgL = if_else(!is.na(Fe_mgL) & Fe_mgL <= MRL_Fe_mgL, as.numeric(paste0(Flag_Fe_mgL, 3, sep = '')), Flag_Fe_mgL),
   #          Flag_Mn_mgL = if_else(!is.na(Mn_mgL) & Mn_mgL <= MRL_Mn_mgL, as.numeric(paste0(Flag_Mn_mgL, 3, sep = '')), Flag_Mn_mgL),
   #          Flag_Cu_mgL = if_else(!is.na(Cu_mgL) & Cu_mgL <= MRL_Cu_mgL, as.numeric(paste0(Flag_Cu_mgL, 3, sep = '')), Flag_Cu_mgL),
   #          Flag_Sr_mgL = if_else(!is.na(Sr_mgL) & Sr_mgL <= MRL_Sr_mgL, as.numeric(paste0(Flag_Sr_mgL, 3, sep = '')), Flag_Sr_mgL),
   #          Flag_Ba_mgL = if_else(!is.na(Ba_mgL) & Ba_mgL <= MRL_Ba_mgL, as.numeric(paste0(Flag_Ba_mgL, 3, sep = '')), Flag_Ba_mgL))
   # 
   # 
   # # flag minimum reporting level
   # for(j in colnames(raw_df|>select(Li_mgL:Ba_mgL))) {
   # 
   # # If value negative set to minimum reporting level
   # 
   #   # If value negative and was digested flag with both
   #   raw_df[c(which(raw_df[,j]<0 & raw_df[,paste0("Flag_",j)]==4)),paste0("Flag_",j)] <- 34
   # 
   #   # If value negative flag
   #   raw_df[c(which(raw_df[,j]<0 & raw_df[,paste0("Flag_",j)]!=34)),paste0("Flag_",j)] <- 3
   #   
   #   ### ABP do a quick fix 
   #   
   #   # get the year
   #  # ColYear <- raw_df[,"Date"] %>% 
   #  #   mutate(Date = year(as.POSIXlt(Date, format = "%Y-%d-%m"))) %>% 
   #  #   rename('Year' = 'Date')
   # 
   # # get the minimum detection level
   # # MRL_value <- as.numeric(MRL[which(MRL[,"Year"] == ColYear)])
   #   
   #   # get the MRL value that 
   #   MRL <- raw_df[,]
   # 
   # # If value is less than MRL and has been digested then flag both  and will set to MRL later
   # raw_df[c(which(raw_df[,j]<=MRL_value & raw_df[,paste0("Flag_",j)]==4)),paste0("Flag_",j)] <- 34
   # 
   # # If value is less than MRL the flag and will set to MRL later
   # raw_df[c(which(raw_df[,j]<=MRL_value & raw_df[,paste0("Flag_",j)]!=34 & raw_df[,paste0("Flag_",j)]!=4)),paste0("Flag_",j)] <- 3

   # # replace the negative values or below MRL with the MRL
   # raw_df[c(which(raw_df[,j]<=MRL_value)),j] <- MRL_value

   # # Get the sd and the mean for flagging
   # sd_value <- sd(as.numeric(unlist(raw_df[j])), na.rm = TRUE) # get the minimum detection level
   # 
   # mean_value <- mean(as.numeric(unlist(raw_df[j])), na.rm = TRUE)

   
   # for 2025 data: not flagging samples 3sd above the mean
   
   # Some samples are 3sd above the mean and we processed with a non-standard method, aka digestion
   # raw_df[c(which(raw_df[,j]>=mean_value + (sd_value*3) & raw_df[,paste0("Flag_",j)]==4)),paste0("Flag_",j)] <- 46
   
   # Now flagging observations that were not digested and are 3sd above the mean
   # raw_df[c(which(raw_df[,j]>=mean_value + (sd_value*3) & raw_df[,paste0("Flag_",j)]!=46)),paste0("Flag_",j)] <- 6

   # print(j)
   # print("mean")
   # print(mean_value)
   # print("sd")
   # print(sd_value)
   # print("MRL value")
   # print(MRL_value)

  # }

   
   # read in the timesheet with the date and time the samples were taken.
   # For the ISCO just use the weir time. Figure out how to do this.
   
   time_sheet <- gsheet::gsheet2tbl(sample_time)|>
     select(Reservoir, Site,Date,Time,Depth_m)|>
     #filter(VT_Metals =="X")|> #only take obs when metals samples were collected
     mutate(
       Date = parse_date_time(Date, orders = c('ymd HMS','ymd HM','ymd','mdy')),
       Date = as.Date(Date),
       Site = as.numeric(Site),
       Depth_m = as.numeric(Depth_m))
   #select(-VT_Metals)
   
   print("read in time data frame")
   
   # Make a data frame with just weir samples and then change to ISCO times. This is a crude way of doing it because we don't always collect metals samples when we collect ISCO samples, but it works for now. 
   
   weir <- time_sheet|>
     filter(Site==100)|>
     mutate(Site = ifelse(Site==100, 100.1, Site))
   
   time_sheet <- bind_rows(time_sheet, weir)|>
     arrange(Date)
   
   print("made a weir data frame")
   # add the time the sample was collected. Use Natural join to override NAs
   
   raw_df2 <-
     natural_join(long_raw_df,time_sheet,
                  by = c("Reservoir", "Site","Date","Depth_m"),
                  jointype = "LEFT")|>
     #select(-Site)|>
     #dplyr::rename(Site=clean_site)|>
     #select(Reservoir, Site, Date, Time, Depth_m, Filter, starts_with("Flag"), ends_with("mgL"))|>
     mutate(
       Time = as.character(hms::as_hms(Time)), # convert time and flag if time is NA
       Flag_DateTime = ifelse(is.na(Time), 1, 0),
       Time = ifelse(Flag_DateTime==1, "12:00:00",Time), # set flagged time to noon
       DateTime = ymd_hms(paste0(Date," ",Time)))|>
     select(-c(Date, Time))|>
     mutate_if(is.numeric, round, digits = 4) # round to 4 digits
   
   print("added time to the data frame")
   # ABP come back here to figure out pivot wider
   # Pivot the data wider so that there is a T_element and and S_element

  wed <- raw_df2 |>
    # order the columns so the time column is not in the middle of the elements
    select(Reservoir, Site, DateTime, Depth_m, Filter, everything())|>
    drop_na(Filter)|> # take out if there are NAs in the filter column
   #group_by(DateTime, Reservoir, Depth_m, Site) |>
    pivot_wider(names_from = 'Filter',
                id_cols = c('Reservoir', 'Site', 'DateTime','Depth_m', 'Flag_DateTime'),
                values_from = Al_mgL:Sr_mgL,
                names_glue = '{Filter}_{.value}') |> 
    rename_with(
      ~ .x |> gsub('T_Flag_', 'Flag_T_', x = _) |> gsub('S_Flag_', 'Flag_S_', x = _),
      .cols = everything())
  
  # now that we pivoted wider again, reassign flags for NA values
  
  tsc <- wed |> select(starts_with("T_"), starts_with("S_")) |> colnames()
  
  for (j in tsc) {
    flag_col <- paste0("Flag_", j)
    
    wed <- wed |>
      mutate(
        "{flag_col}" := case_when(
          is.na(.data[[j]]) & is.na(.data[[flag_col]]) ~ "1",
          TRUE ~ .data[[flag_col]]
        )
      )
  }

  # rename the Flag column
  # Change the column headers so they match what is already on EDI. Take out the "Conc_

  # raw_df <- wed |>
  #   rename_with(~gsub("Conc_", "", colnames(wed)))
    # mutate(
    #   clean_site = Site,
    #   Site = ifelse(Site==100.1, 100, Site)
    # )

 


   #### 6. Switch observations if total and soluble samples were mixed up ####

   # Determine if totals and soluble samples were switched.

  
  # create columns for 5 percent threshold - this is the threshold for solubles being greater than totals
  wed <- wed %>% 
    mutate(
      across(
        .cols = starts_with("T_") | starts_with("S_"),
        .fns = ~ .x *0.05,
        .names = 'fivepercent_{.col}'
      )
    )

  # create columns for 10 percent threshold - this is the threshold for tubes being switched
  wed <- wed %>% 
    mutate(
      across(
        .cols = starts_with("T_") | starts_with("S_"),
        .fns = ~ .x *0.1,
        .names = 'tenpercent_{.col}'
      )
    )
  
  
  
  # flag Fe/Mn/Al as "SWITCHED" where T + 10% < S
  metals_check <- c("T_Fe_mgL", "T_Mn_mgL", "T_Al_mgL")
  
  for (l in metals_check) {
    ten_col   <- paste0("tenpercent_", l)
    s_col     <- gsub("T_", "S_", l)
    check_col <- paste0("Check_", l)
    
    wed <- wed |>
      mutate(
        "{check_col}" := if_else(.data[[l]] + .data[[ten_col]] < .data[[s_col]], "SWITCHED", "0")
      )
  }
  
  # mark rows where all three metals were switched and none is already flagged "3"
  wed <- wed |>
    mutate(
      switch_all = 0,
      switch_all = if_else(
        Check_T_Fe_mgL == "SWITCHED" & Check_T_Mn_mgL == "SWITCHED" & Check_T_Al_mgL == "SWITCHED" &
          Flag_T_Fe_mgL != "3" & Flag_T_Mn_mgL != "3" & Flag_T_Al_mgL != "3" & Flag_T_Fe_mgL != "1" & Flag_T_Mn_mgL != "1" & Flag_T_Al_mgL != "1",
        1, switch_all
      ),
      switch_all = if_else(is.na(switch_all), 0, switch_all)
    )
  
  # swap T_/S_ values for every metal where switch_all == 1
  t_cols <- wed |> select(starts_with("T_") & !starts_with(c("Check_"))) |> colnames()
  
  for (l in t_cols) {
    s_col  <- gsub("T_", "S_", l)
    orig_t <- wed[[l]]   # capture originals so the two assignments below don't clobber each other
    orig_s <- wed[[s_col]]
    
    wed <- wed |>
      mutate(
        "{l}"     := if_else(switch_all == 1, orig_s, orig_t),
        "{s_col}" := if_else(switch_all == 1, orig_t, orig_s)
      )
  }
  
  # recheck flags on T_ columns now that switching has happened
  for (l in t_cols) {
    five_col <- paste0("fivepercent_", l)
    s_col    <- gsub("T_", "S_", l)
    flag_col <- paste0("Flag_", l)
    
    wed <- wed |>
      mutate(
        "{flag_col}" := case_when(
          .data[[l]] + .data[[five_col]] < .data[[s_col]] & .data[[flag_col]] != "1" & .data[[flag_col]] == "6" ~ "69",
          .data[[l]] + .data[[five_col]] < .data[[s_col]] & .data[[flag_col]] != "1" & .data[[flag_col]] != "6" ~ "9",
          TRUE ~ .data[[flag_col]]
        )
      )
  }
  
  # propagate T_ flags to matching S_ flags
  s_cols <- wed |> select(starts_with("S_") & !starts_with(c("Check_"))) |> colnames()
  
  for (i in s_cols) {
    t_flag_col <- paste0("Flag_", gsub("S_", "T_", i))
    flag_col   <- paste0("Flag_", i)
    
    wed <- wed |>
      mutate(
        "{flag_col}" := case_when(
          .data[[t_flag_col]] %in% c("9", "69") & .data[[flag_col]] != "1" & .data[[flag_col]] == "6" ~ "69",
          .data[[t_flag_col]] %in% c("9", "69") & .data[[flag_col]] != "1" & .data[[flag_col]] != "6" ~ "9",
          TRUE ~ .data[[flag_col]]
        )
      )
  }
  
  print("fixed switched samples")
  
  # older code
#   for(l in c('T_Fe_mgL', 'T_Mn_mgL', 'T_Al_mgL')){
#     wed[,paste0("Check_",colnames(wed[l]))] <- "0"  #creates Check column + name of variable
#     #MRL_value <- as.numeric(MRL[1,gsub("T_|S_","",l)]) # get the minimum detection level
#     #switch_threshold <- MRL_value*3
# 
#     # Puts "SWITCHED" in the Check column if the soluble concentration is greater than the totals plus three times the MRLA;s
#     wed[ which( wed[,l]+wed[,paste0('tenpercent_',l)] < wed[,gsub("T_", "S_", l)]),paste0("Check_",colnames(wed[l]) ) ] <- "SWITCHED"
#   }
# 
# 
#   ## assign rows where all three variables were switched
#   wed$switch_all <- 0
#   for (i in 1:nrow(wed)){
#   if (wed[i,'Check_T_Fe_mgL'] == 'SWITCHED' &
#       wed[i,'Check_T_Mn_mgL'] == 'SWITCHED' &
#       wed[i,'Check_T_Al_mgL'] == 'SWITCHED' &
#       wed[i,'Flag_T_Fe_mgL'] != 3 & # add 34
#       wed[i,'Flag_T_Mn_mgL'] != 3 &
#       wed[i,'Flag_T_Al_mgL'] != 3){
#     
#     # add a one to the column
#     wed[i,'switch_all'] <- 1
#   }
# }
# 
#   for(l in colnames(wed|>select(starts_with(c("T_"))))) {
#     wed[which(wed[,'switch_all'] == 1), c(l,gsub("T_", "S_", l)) ] <-
#       wed[which(wed[,'switch_all'] == 1), c(gsub("T_", "S_", l), l)]
#   }
# 
#   
#   # now that all rows have been switched, check to see if solubles are greater than totals
#   for (i in colnames(wed|>select(starts_with('T_')))) {
#     wed[c(which(wed[,i]+wed[,paste0('fivepercent_',i)] < wed[,gsub("T_", "S_", i)] & wed[paste0("Flag_",i)]!=1 & wed[paste0("Flag_",i)]!=6)), paste0("Flag_", i)] <- 9
#     wed[c(which(wed[,i]+wed[,paste0('fivepercent_',i)] < wed[,gsub("T_", "S_", i)] & wed[paste0("Flag_",i)]!=1 & wed[paste0("Flag_",i)]==6)), paste0("Flag_", i)] <- 69
#   }
# 
#   for (i in colnames(wed|>select(starts_with(c('S_'))))) {
#     wed[c(which(wed[paste0("Flag_", gsub('S_', 'T_', i))] == 9 & wed[paste0("Flag_",i)]!=1 & wed[paste0("Flag_",i)]!=6)), paste0("Flag_", i)] <- 9
#     wed[c(which(wed[paste0("Flag_", gsub('S_', 'T_', i))] == 69 & wed[paste0("Flag_",i)]!=1 & wed[paste0("Flag_",i)]!=6)), paste0("Flag_", i)] <- 9
#     wed[c(which(wed[paste0("Flag_", gsub('S_', 'T_', i))] == 9 & wed[paste0("Flag_",i)]!=1 & wed[paste0("Flag_",i)]==6)), paste0("Flag_", i)] <- 69
#     wed[c(which(wed[paste0("Flag_", gsub('S_', 'T_', i))] == 69 & wed[paste0("Flag_",i)]!=1 & wed[paste0("Flag_",i)]==6)), paste0("Flag_", i)] <- 69
#   }
#   
#   print("fixed switched samples")
  
  #raw_df[c(which(is.na(raw_df[,j]) & is.na(raw_df[paste0("Flag_",j)]))),paste0("Flag_",j)] <- 1
   # for(l in colnames(raw_df|>select(starts_with(c("T_"))))) {
   #   #for loop to create new columns in data frame
   #   raw_df[,paste0("Check_",colnames(raw_df[l]))] <- 0 #creates Check column + name of variable
   #
   #   MRL_value <- as.numeric(MRL[1,gsub("T_|S_","",j)]) # get the minimum detection level
   #
   #   # Puts "SWITCHED" in the Check column if the soluble concentration is greater than the totals plus the MRL
   #   raw_df[T_Al_mgLc(which(raw_df[,l]+MRL_value<raw_df[,gsub("T_", "S_", l)])),paste0("Check_",colnames(raw_df[l]))] <- "SWITCHED"
   #
   #   # Swap the observations from the totals and solubles if the Check column is labeled "SWITCHED"
   #
   #   raw_df[c(which(raw_df[,paste0("Check_",l)]=="SWITCHED")), c(l,gsub("T_", "S_", l)) ] <-
   #     raw_df[c(which(raw_df[,paste0("Check_",l)]=="SWITCHED")), c(gsub("T_", "S_", l), l)]
   # }

  # Flag all Na in the data frame again
  tsc <- wed |> select(starts_with("T_"), starts_with("S_")) |> colnames()
  
  for (j in tsc) {
    flag_col <- paste0("Flag_", j)
    
    wed <- wed |>
      mutate(
        "{flag_col}" := case_when(
          is.na(.data[[j]]) & is.na(.data[[flag_col]]) ~ "1",
          switch_all == 1 & .data[[flag_col]] != "1" & .data[[flag_col]] != "6" &
            .data[[flag_col]] != "69" & .data[[flag_col]] != "9" ~ "7",
          TRUE ~ .data[[flag_col]]
        )
      )
  }
   # Change the column headers so they match what is already on EDI. Added T_ because it is easier in the

   frame4 <- wed |>
     rename_with(~gsub("T_", "T", gsub("S_", "S",.)), -1)

   print("flag NAs again")
#let's write the final csv
#note: you must edit the script each time to save the correct file name
 frame4 <- frame4 |>
   select(Reservoir, Site, DateTime, Depth_m,
          TLi_mgL, SLi_mgL, TNa_mgL, SNa_mgL,
          TMg_mgL, SMg_mgL, TAl_mgL, SAl_mgL,
          TSi_mgL, SSi_mgL, TK_mgL, SK_mgL,
          TCa_mgL, SCa_mgL, TFe_mgL, SFe_mgL,
          TMn_mgL, SMn_mgL, TCu_mgL, SCu_mgL,
          TSr_mgL, SSr_mgL, TBa_mgL, SBa_mgL,
          Flag_DateTime,
          Flag_TLi_mgL, Flag_SLi_mgL, Flag_TNa_mgL, Flag_SNa_mgL,
          Flag_TMg_mgL, Flag_SMg_mgL,
          Flag_TAl_mgL, Flag_SAl_mgL, Flag_TSi_mgL, Flag_SSi_mgL,
          Flag_TK_mgL, Flag_SK_mgL, Flag_TCa_mgL, Flag_SCa_mgL,
          Flag_TFe_mgL, Flag_SFe_mgL, Flag_TMn_mgL, Flag_SMn_mgL,
          Flag_TCu_mgL, Flag_SCu_mgL, Flag_TSr_mgL, Flag_SSr_mgL,
          Flag_TBa_mgL, Flag_SBa_mgL) |>
   arrange(DateTime, Reservoir, Site, Depth_m)
 
 print("make final data frame")

 #### 7. Save Files 

 # Save the metals data frame
 # Remove the ISCO samples
 final <- frame4|>
   filter(Site != 100.1)
 

 # Do we want to get ISCO oput up 
 
 if(isTRUE(ISCO_save)){
   
   # Save the ISCO observations
 ISCO <- frame4|>
   filter(Site == 100.1)
 
 if(!is.null(ISCO_outfile)){
   
   # save the ISCO file
   ISCO$DateTime <- as.character(format(ISCO$DateTime)) # convert DateTime to character
   
   # save the ISCO file
   write_csv(ISCO, ISCO_outfile)
   
   print(paste0("ISCO file will be save here: ", ISCO_outfile))
   
 }else{
   return(ISCO)
   
   print("ISCO data frame returned to the enviornment")
  }
 
  
 }else{
   warning("ISCO file is not saved and will not be returned. This is a check to make sure you know this.")
 }
 

 # add in filter later. Right now save everything.
 

  # save the metals L1 file. If the outfile=NULL then it just returns the file. 
if(metals_save==T){
  if(!is.null(metals_outfile)){
    
     # If there is an outfile argument, that is where the data are saved
    final$DateTime <- as.character(format(final$DateTime)) # convert DateTime to character

    # Write the L1 file
 write_csv(final, metals_outfile)
 
 # print where the file will be saved
 print(paste0("Metals file will be save here: ", metals_outfile))

  }else{
   return(final)
    print("Metals data frame returned to the enviornment")
 }
  }else{
   warning("The metals data frame was not save or returned. This is a check to make sure you know this.")
}
 
 # returns a list of data frames from both the ISCO and the metals. 
 if(metals_save==T && is.null(metals_outfile) && ISCO_save==T && is.null(ISCO_outfile)){
   
   # make a list of ISCO and metals data frames that gets returned
   
   all_plots <- list(final, ISCO)
   
   return(all_plots)
   
   print("Data frames are in a list with the metals data frame being first and then the ISCO data frame")
 
  }
 }
} 

# closes the function


