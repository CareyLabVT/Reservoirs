# Chla Processing L1 script
# By: Adrienne Breef-Pilz
# Written: 24 Nov. 23
# Edit: 16 Jun 24- add in historical file, add a start and end date filter, add in time of sample

# Things the script does: 
# 1. Read in Maintenance log and read in raw chla file from the spec
#   Put in the right format for processing
# 2. Read in the filtering log and rack map
# 3. Merge everything together
# 4. Maintenance log to flag or remove issues
# 5. Process with a script based on BNN Excel script
# 6. Further QAQC processing
# 7. Save files

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(lubridate,tidyverse, gsheet)


filt_chla_qaqc <- function(directory, 
                      rack_map,
                      filtering_log,
                      final_vol_extract = 6, 
                      blank_vol_filt = 500, 
                      maintenance_file,
                      historic_file,
                      sample_times,
                      outfile,
                      start_date,
                      end_date)
  {
  
  # directory = "./Data/DataNotYetUploadedToEDI/Raw_chla/chla_extraction/raw data from spec/"
  # rack_map = "https://docs.google.com/spreadsheets/d/1N7he-0Z1gmSA5KjO96QA5tOeNXFcAKoVfAD1zix4qNk"
  # filtering_log = "https://docs.google.com/spreadsheets/d/1xeF312vgwJn7d2UwN4qOD8F32ZGHE3Vv"
  # final_vol_extract = 6
  # blank_vol_filt = 500
  # maintenance_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Raw_chla/Filt_Chla_Maintenance_Log.csv"
  # historic_file = "./Data/DataNotYetUploadedToEDI/Raw_chla/historic_filt_chla_2014_2022.csv"
  # sample_times =  "https://docs.google.com/spreadsheets/d/1NKnIM_tjMxMO0gVxzZK_zVlUSQrdi3O7KqnyRiXo4ps" 
  # outfile = "./Data/DataNotYetUploadedToEDI/Raw_chla/Filt_chla_L1.csv"
  
  #### 1. Read in Maintenance file and the Raw files from the spec 
  ### 1.1 Read in Maintenance file #### 
  log_read <- read_csv(maintenance_file, col_types = cols(
    .default = col_character(),
    Date_processed = col_date("%Y-%m-%d"),
    Sample_date = col_date("%Y-%m-%d"),
    flag = col_integer()
  ))
  
    log <- log_read
  
  
  
  ### 1.2 Select files from current year 
  # Name the directory where the full output files are found. Ours are on GitHub 
  mydir <-directory
  
  # list of raw chla samples for the current year
  files <- list.files(path=mydir, pattern = ".txt", full.names=TRUE)
  

 #### 1.3 Read in files 
 
# Create a blank data frame to use in the for loop below
 out.file<-NULL
 
 
 #  Collate the files and add a processing date to the file
 
 for(j in 1:length(files)){
   
   if(!is.na(files[j])){
     # Get the date the samples were processed on the spec
     sed <- str_extract(files[j], "_\\d+")
     
     # Take out the extra underscore
     Date <- sub("_","",sed)
     
     # Put the date in the proper format
     Date_processed <- as.Date(Date, "%Y%m%d")
     
     data <- read.csv(files[j])
     # Add the date processed to the files
     data$Date_processed <- as.Date(Date, "%Y%m%d")
     
     # Bind the files
     out.file=bind_rows(data, out.file)
     
   }
   
 }
 
 
 ### 1.4  Label observations 
 
 
 
 # Label the types of samples are so they are easier to sort label
 out.file2<- out.file%>%
   mutate(
     samp_type = ifelse(grepl("et|blan", Sample.ID), "eth_blank",NA),
     samp_type = ifelse(grepl("[0-9]", Sample.ID), "res_samp", samp_type),
     samp_type = ifelse(grepl("fa", Sample.ID), "fake", samp_type),
     samp_type = ifelse(grepl("ref", Sample.ID), "ref", samp_type)
     
   )
 
 # Create an ethanol blank data frame
 # ethon_blank <- out.file2%>%
 #   filter(samp_type=="eth_blank")
 
 # Create a data frame of just the samples. 
 # Put the before and after acid labels in the timing column. 
 # Isolate Sample ID number to match to rack map
 res_samp <- out.file2%>%
   filter(samp_type=="res_samp")%>%
   mutate(
     timing = gsub("_","", gsub("[[:digit:]]", "", Sample.ID)),
     timing = ifelse(str_detect(Sample.ID,"b|B")==T,"b", 
                     ifelse(str_detect(Sample.ID, "a|A")==T, "a", timing)),
     Sample_ID = sub("_", "", gsub("[a-z]+", "", Sample.ID))
   )%>%
   separate(Sample_ID, c("Sample_ID", "Need_Rep"))%>%
   mutate(
     Sample_ID = as.numeric(Sample_ID),
     Need_Rep = as.numeric(Need_Rep)
   )

 ### 2. Get the sample ID number and match with the reservoir and site
 
 ### 2.1 Read in the rack map file ####
 
 rack_map <- gsheet::gsheet2tbl(rack_map)
 
 # Join together by Date_processed and Sample_ID
 # perform full_join based on multiple columns
 df3 <- full_join(res_samp, rack_map, by=c('Date_processed'='Date_processed', 'Sample_ID'='Sample_ID'))
 
 # label ethanol blank samples so we can find them later
 
 df4 <- df3%>%
   mutate(
     samp_type = ifelse(str_detect(Sample_date, "^et")==T, "eth_blank", samp_type)
   )
   
 
# Get sample dates in the right format
 res_samp2 <- df4%>%
   mutate(
     Sample_date2= ifelse(grepl("^20", Sample_date)==T, Sample_date, NA),
     Sample_date = as.Date(Sample_date2) # put date in format
   )
   
 ### 2.2 read in filtering log 
 
 filtering_log <- gsheet::gsheet2tbl(filtering_log)
 
 # Clean up the data frame
 filtering_log2 <- filtering_log%>%
   mutate(
     Sample_date= parse_date_time(`Sample Date`, orders = c('dBy')),
     Vol_filt_mL = as.numeric(`Volume filtered (mL)`),
     Final_vol_extract_mL = final_vol_extract
   )%>%
   select(ResSite, Depth,Sample_date, Rep, Vol_filt_mL, Final_vol_extract_mL, Comments)
 
 ### 3. Combine with the filtering log 
 comb <- left_join(res_samp2, filtering_log2, 
                   by=c("Sample_date"="Sample_date", "ResSite"="ResSite", "Rep"="Rep"))
 
 
 # add the vol filt and final vol used for the ethanol samples 
 # We use 500 mL for volume filtered for the ethanol blanks
 comb2 <- comb%>%
   mutate(
     Vol_filt_mL = ifelse(samp_type=="eth_blank", 500, Vol_filt_mL),
     Final_vol_extract_mL = ifelse(samp_type=="eth_blank",final_vol_extract, Final_vol_extract_mL)
   )
 
 ### 4. Take out values based on the Maintenance Log 
 
 ## Define Maintenance Flags HERE:
 
 # Add Flag columns
 
 raw_df<-comb2%>%
   mutate(
     Flag_Chla_ugL  = 0,
     Flag_Pheo_ugL  = 0
   )
 
 ### 4.1 Set up the Values to be used 
 # modify raw_df based on the information in the log   
 
 for(i in 1:nrow(log)){
   
   ### Get the date the samples were processed
   Date_processed <- log$Date_processed[i]
   
   ### Get the date the samples were collected
   Sample_date <- log$Sample_date[i]
   
   ### Get the Reservoir and Site combo name
   
   ResSite <- log$ResSite[i]
   
   ### Get the Sample.ID. This is how we will pick out the samples
   
   Sample.ID <- log$Sample.ID[i]
   
   ### Get the Rep 
   
   Rep <- log$Rep[i]
   
   ### Get the updated value that will be replaced
   
   Updated_value <- log$updated_value[i]
   
   ### Get the Maintenance Flag 
   
   flag <- log$flag[i]
   
   
   ### Get the names of the columns affected by maintenance
   
   colname_start <- log$start_parameter[i]
   colname_end <- log$end_parameter[i]
   
   ### if it is only one parameter parameter then only one column will be selected
   
   if(is.na(colname_start)){
     
     maintenance_cols <- colnames(raw_df%>%select(all_of(colname_end))) 
     
   }else if(is.na(colname_end)){
     
     maintenance_cols <- colnames(raw_df%>%select(all_of(colname_start)))
     
   }else{
     maintenance_cols <- colnames(raw_df%>%select(colname_start:colname_end))
   }
   
   ### Get the name of the flag column. These are the only flags we have
   
   flag_cols <- c("Flag_Chla_ugL",
                  "Flag_Pheo_ugL")
   
   
   ### 4.2 Actually remove values in the maintenance log from the data frame 
   ## This is where information in the maintenance log gets removed. 
   # UPDATE THE IF STATEMENTS BASED ON THE NECESSARY CRITERIA FROM THE MAINTENANCE LOG
   
   # replace relevant data with NAs and set flags while maintenance was in effect
   if(flag==1){ 
     # Sample below detection. Not used in the maintenance log
     
   }else if (flag==2){
     # This one is below minimum detection level and most likely won't be in the maintenance log
     raw_df[c(which(raw_df[,"Date_processed"] == Date_processed & raw_df[,"Sample.ID"] == Sample.ID)), 
            maintenance_cols] <- NA
     raw_df[c(which(raw_df[,"Date_processed"] == Date_processed & raw_df[,"Sample.ID"] == Sample.ID)), 
            flag_cols] <- flag
     
   }else if (flag==99){
     # Rename the Sample.ID if there were messed up while processing
     raw_df[c(which(raw_df[,"Date_processed"] == Date_processed & raw_df[,"Sample.ID"] == Sample.ID)), 
            maintenance_cols] <- Updated_value
     
     
   }else {
     warning("Flag used not defined in the L1 script. Talk to Austin and Adrienne if you get this message")
   }
   next
 }
 
 
 
 ### 5. Get the Chla concentration from wavelengths from Spec 
 
 # Re run this because we changed some of Sample.IDs in the maintenance log 
 # because before and after were mixed up
 raw_df2 <- raw_df%>%
   mutate(
     timing = gsub("_","", gsub("[[:digit:]]", "", Sample.ID)),
     timing = ifelse(str_detect(Sample.ID,"b|B")==T,"b", 
                     ifelse(str_detect(Sample.ID, "a|A")==T, "a", timing)),
     Sample_ID = sub("_", "", gsub("[a-z]+", "", Sample.ID))
   )%>%
   separate(Sample_ID, c("Sample_ID", "Need_Rep"))%>%
   mutate(
     Sample_ID = as.numeric(Sample_ID),
     Need_Rep = as.numeric(Need_Rep)
   )
 
 
 # The calculations are from BNN Chla processing excel sheet

 ### 5.1 Separate the wavelength by before acid and after and then merge together wider 
 
 before_comb2 <- raw_df2%>%
   filter(Flag_Chla_ugL!=2)%>%
   filter(timing=="b"|timing=="before")%>%
   dplyr::rename("before_acid_abs_750" = "WL750.0",
                 "before_acid_abs_665" = "WL665.0",
                 "before_acid_abs_664" = "WL664.0",
                 "before_acid_abs_663" = "WL663.0",
                 "before_acid_abs_647" = "WL647.0",
                 "before_acid_abs_630" = "WL630.0",
                 "before_acid_abs_490" = "WL490.0",
                 "before_acid_abs_384" = "WL384.0"
                 )%>%
   select(Sample_ID, Date_processed, samp_type, ResSite, Depth, Rep, Sample_date,Vol_filt_mL, Final_vol_extract_mL,
          before_acid_abs_750:before_acid_abs_384, Flag_Chla_ugL, Flag_Pheo_ugL, Notes)
 
 # Now do it for after acid readings
 
 after_comb2 <- raw_df2%>%
   filter(Flag_Chla_ugL!=2)%>%
   filter(timing=="a"|timing=="after")%>%
   dplyr::rename("after_acid_abs_750" = "WL750.0",
                 "after_acid_abs_665" = "WL665.0",
                 "after_acid_abs_664" = "WL664.0",
                 "after_acid_abs_663" = "WL663.0",
                 "after_acid_abs_647" = "WL647.0",
                 "after_acid_abs_630" = "WL630.0",
                 "after_acid_abs_490" = "WL490.0",
                 "after_acid_abs_384" = "WL384.0"
   )%>%
   select(Sample_ID, Date_processed, samp_type, ResSite, Depth, Rep, Sample_date,Vol_filt_mL, Final_vol_extract_mL,
          after_acid_abs_750:after_acid_abs_384,Flag_Chla_ugL, Flag_Pheo_ugL, Notes)
 
 # Join the two data frames together
 
 comb3 <- full_join(before_comb2, after_comb2, 
                      by=c("Sample_ID","Date_processed", "samp_type", "ResSite", "Depth","Rep", "Sample_date",
                            "Vol_filt_mL", "Final_vol_extract_mL", "Flag_Chla_ugL", "Flag_Pheo_ugL", "Notes"))
 
### 5.2 Claculate the concentration of Chla in ugL 
 
 comb3_calc <- comb3%>%
   mutate(
 # before acidification (turbidity corrected)664-750
 
 before_acid = before_acid_abs_664-before_acid_abs_750,
 
 #after acidification (turbidity corrected) 665-750
 
  after_acid = after_acid_abs_665-after_acid_abs_750,
 
 # difference before and after problems if negative
 
  diff_be_af = before_acid-after_acid)
 
 ### 5.21 Get the average blanks for each processing date 

 avg_e_blank <- comb3_calc%>%
   filter(samp_type=="eth_blank")%>%
   group_by(Date_processed)%>%
   summarise(across(c("before_acid", "after_acid", "diff_be_af"), ~ mean(.x, na.rm = TRUE)))%>%
   ungroup()%>%
     dplyr::rename(blank_before_acid_avg = before_acid,
                   blank_after_acid_avg = after_acid,
                   blank_diff_be_af_avg = diff_be_af)%>%
   select(Date_processed, blank_before_acid_avg, blank_after_acid_avg, blank_diff_be_af_avg)
   
   comb4_calc <- left_join(comb3_calc, avg_e_blank, by="Date_processed")
   
   ### 5.22 Now finish the calculations 
   
   comb5_calc <- comb4_calc%>%
     
     mutate(
 # Chlorophyll a in extract (ug/L from Arar) BD if <~65
 
  chla_extract = 1000*28.64*diff_be_af,
 
 # Pheopigment in extract (ug/L from Arar)BD if <~85
 
  pheo_extract = 1000*28.64*((1.72*after_acid)-before_acid),
 
 # Pheopigment in extract (ug/L) with ethanol blank correction
 
   pheo_extract_wblank_corr = 1000*28.64*(1.72*(after_acid-blank_after_acid_avg)-
                                            (before_acid-blank_before_acid_avg)),
 
 # Chlorophyll a Conc of original water sample in ug/L (or mg/m3-same thing- APHA)
 
 chla_in_water = (chla_extract*Final_vol_extract_mL/1000)/((Vol_filt_mL/1000)*1),
 
 # Pheopigment Conc of original water sample in ug/L (or mg/m3-same thing- APHA)
 
 pheo_in_water = (pheo_extract*Final_vol_extract_mL/1000)/((Vol_filt_mL/1000)*1),
 
 # Ratio before and after (1 = all pheo, 1.72 = all chla)problems if not between 1 and 1.72
 
 ratio_be_af = before_acid/after_acid,
 
 # Ratio ethanol blank corrected
 
  ratio_be_af_eth_corr=(before_acid-blank_before_acid_avg)/
   (after_acid-blank_after_acid_avg)
)
   
  ### 5.3 Select only the columns of interest and need for EDI and QAQC ###
   # Take out the ethanol blank rows 
   chla_df<- comb5_calc%>%
     filter(samp_type!="eth_blank")%>%
     select(ResSite,
            Depth,
            Sample_date,
            before_acid_abs_664,
            chla_extract,
            pheo_extract,
            chla_in_water,
            pheo_in_water,
            Flag_Chla_ugL,
            Flag_Pheo_ugL)%>%
     dplyr::rename( # rename the columns for below
       Date = Sample_date,
       Depth_m = Depth,
       Check_Absorb = before_acid_abs_664,
       Check_chla = chla_extract,
       Check_pheo = pheo_extract,
       Chla_ugL = chla_in_water,
       Pheo_ugL = pheo_in_water
     )

   ### 6. Further QAQC 
   
   chla_new<-chla_df%>%
     #filter(Sample_ID!="")%>%
     # Get Reservoir and Site
     separate(.,col = ResSite, into = c("Reservoir", "Site"), sep = 1)%>%
     mutate(Reservoir=ifelse(Reservoir=="B","BVR", Reservoir),
            Reservoir=ifelse(Reservoir=="F","FCR", Reservoir),
            Reservoir=ifelse(Reservoir=="S","SNP", Reservoir),
            Reservoir=ifelse(Reservoir=="C","CCR", Reservoir))%>%
     # Add flags for low absorbance and pigment below detection
     mutate(Flag_Chla_ugL=ifelse(Check_Absorb<0.03,1, Flag_Chla_ugL),
            Flag_Pheo_ugL=ifelse(Check_Absorb<0.03,1, Flag_Pheo_ugL),
            Flag_Chla_ugL=ifelse(Check_chla<34,4,Flag_Chla_ugL),
            Flag_Pheo_ugL=ifelse(Check_pheo<34,4,Flag_Pheo_ugL),
            Pheo_ugL=ifelse(Pheo_ugL<0, 0, Pheo_ugL))%>%
     # Average the dups
     group_by(Reservoir, Site, Date, Depth_m)%>%
     mutate(count = n())%>%
     mutate(Chla_ugL = mean(Chla_ugL)) %>%
     mutate(Pheo_ugL = mean(Pheo_ugL))%>%
     ungroup()%>%
     mutate(Flag_Chla_ugL=ifelse(Flag_Chla_ugL==0 & count==2,5,Flag_Chla_ugL),
            Flag_Chla_ugL=ifelse(Flag_Chla_ugL==4 & count==2,45,Flag_Chla_ugL), 
            Flag_Pheo_ugL=ifelse(Flag_Pheo_ugL==0 & count==2,5,Flag_Pheo_ugL),
            Flag_Pheo_ugL=ifelse(Flag_Pheo_ugL==4 & count==2,45,Flag_Pheo_ugL))%>%
     distinct(Reservoir, Site, Date, Depth_m, .keep_all = T)%>%
     mutate(Depth_m=as.numeric(Depth_m))%>%
     mutate(Site=as.numeric(Site))%>%
     dplyr::rename(DateTime=Date)%>%
     select(Reservoir,Site,DateTime, Depth_m, Chla_ugL,Pheo_ugL,Flag_Chla_ugL,Flag_Pheo_ugL)%>%
     mutate(Flag_Chla_ugL=ifelse(is.na(Chla_ugL), 2, Flag_Chla_ugL))%>% #Add a 2 flag if an observation is missing
     mutate(Flag_Pheo_ugL=ifelse(is.na(Pheo_ugL),2,Flag_Pheo_ugL))
   
   # read in the historical file if start_date is NULL
   
   if(is.null(start_date)){
     hist <- read_csv(historic_file)%>%
       mutate(DateTime = parse_date_time(DateTime, c("mdy HM")),
              DateTime = as.Date(DateTime))%>%
       drop_na(DateTime)
   }else{
     hist <- NULL
   }
   
   # combine the historical file and the current file
   chla_all <- dplyr::bind_rows(hist, chla_new)
   
   # read in the sampling times file
   
   samp_times <- gsheet::gsheet2tbl(sample_times)%>%
     mutate(DateTime_samp = parse_date_time(DateTime, c("ymd HMS","ymd HM", "ymd")),
            DateTime = as.Date(DateTime))%>%
     select(Reservoir, Site, DateTime, Depth_m, Chla, DateTime_samp)%>%
     drop_na(Chla)
     
   
   # merge the chla sampel with sample times
   
   chla_all2 <- left_join(chla_all, samp_times, by = c("Reservoir", "Site", "Depth_m", "DateTime"))
   
     # If there is no sample time then set it to noon and flag as 1 
   final <- chla_all2%>%
     mutate(Date = as.Date(DateTime),
            Flag_DateTime = ifelse(is.na(DateTime_samp), 1, 0),
            DateTime_samp = ifelse(is.na(DateTime_samp), paste0(Date," ","12:00:00"),as.character(DateTime_samp)),
            DateTime = ymd_hms(DateTime_samp))%>%
     select(Reservoir, Site, DateTime, Depth_m, Chla_ugL, Pheo_ugL,
            Flag_DateTime, Flag_Chla_ugL, Flag_Pheo_ugL)
     
     
     # put in order
     final <- chla_new[order(final$DateTime),]
     
    # subset to make the L1 file 
     
     if (!is.null(start_date)){
       #force tz check
       start_date <- force_tz(as.POSIXct(start_date), tzone = "America/New_York")
       
       final$DateTime <- as.character(format(final$DateTime)) # convert DateTime to character
       
       final <- final %>%
         filter(DateTime >= start_date)
       
     }
     
     if(!is.null(end_date)){
       #force tz check
       end_date <- force_tz(as.POSIXct(end_date), tzone = "America/New_York")
       
       final$DateTime <- as.character(format(final$DateTime)) # convert DateTime to character
       
       final <- final %>%
         filter(DateTime <= end_date)
       
     }
     
     
     
  ### 7. Save the file. If outfile is NULL then return the file
     
     if(is.null(outfile)){
       
       return(final)
     }else{
       
        write_csv(final, outfile)
     }
   
    
  
}

