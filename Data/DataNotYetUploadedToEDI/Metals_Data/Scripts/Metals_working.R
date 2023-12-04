# Title: Metals data wrangling script
# Author: Cece Wood
# Date: 18JUL23
# Edit: 29 Nov 23 A. Breef-Pilz

# Purpose: convert metals data from the ICP-MS lab format to the format needed
# for publication to EDI


pacman::p_load(tidyverse, lubridate, gsheet)

metals_qaqc <- function(directory =,
                        sample_ID_key = "https://raw.githubusercontent.com/abreefpilz/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Scripts/Metals_Sample_Depth.csv", 
                        maintenance_file =,
                        sample_time = "https://docs.google.com/spreadsheets/d/1NKnIM_tjMxMO0gVxzZK_zVlUSQrdi3O7KqnyRiXo4ps/edit#gid=344320056",
                        LDT_sheet = 
                        outfile = "./Data/DataNotYetUploadedToEDI/Metals_Data/metals_L1.csv"){
  
  directory = "./Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/2023/"
  sample_ID_key = "https://raw.githubusercontent.com/abreefpilz/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Scripts/Metals_Sample_Depth.csv"
  maintenance_file =
  sample_time = "https://docs.google.com/spreadsheets/d/1NKnIM_tjMxMO0gVxzZK_zVlUSQrdi3O7KqnyRiXo4ps/edit#gid=344320056"
  LDT_sheet = 
  outfile = "./Data/DataNotYetUploadedToEDI/Metals_Data/metals_L1.csv"
  
  # make a function that reads in the files and takes the columns we want
  read_metals_files <- function(FILES){
    
  al <- read_csv(FILES, skip = 3, col_names = T)%>%
    dplyr::rename(Date_ID = `...1`)%>%
    drop_na(Date_ID)%>%
    rename_with(~paste0(gsub("[[:digit:]]", "", gsub("\\s*\\([^\\)]+\\)", "", .)), "_mgL"), -1)%>%
    separate(Date_ID,c("DateTime","Sample")," - ") %>%
    mutate(DateTime = as.Date(DateTime,format = "%m/%d/%Y"),
           Sample = as.numeric(Sample))%>%
    select(DateTime, Sample, Li_mgL, Na_mgL, Mg_mgL, Al_mgL, Si_mgL, K_mgL, Ca_mgL,
           Fe_mgL, Mn_mgL,Cu_mgL, Sr_mgL, Ba_mgL)%>%
    modify_if(is.character, ~as.numeric(gsub(",","",.))/1000)
    
    return(al)
 
  }
  # use purr to read in all the files using the function above
  ICP<-list.files(path=directory, pattern="", full.names=TRUE)%>%
    map_df(~ read_metals_files(.x))
  
  
#joining maintenance log
  
   
#read in metals ID, reservoir, site, depth, and total/soluble key
 metals_key <- read_csv(sample_ID_key)%>% 
   rename(Depth_m =`Sample Depth (m)`)
 
 
#set up final data frame with correct formatting!
 frame1 <- left_join(ICP, metals_key, by = c('Sample'))%>% 
   select(-Sample)%>%
   group_by(DateTime, Reservoir, Depth_m, Site)%>% 
   pivot_wider(names_from = 'Filter', 
                              values_from = c('Li_mgL':'Ba_mgL'),
                               names_glue = "{Filter}{.value}")%>%  # names the columns as we want
   # sum obs is for a tabulator for duplicates and average in case there are duplicates
   summarise(across(everything(), list(z = ~mean(.x, na.rm = TRUE), n = ~ sum(!is.na(.))), .names ="{.fn}_{.col}"))%>%
   rename_all(~ str_remove(., "^z_"))%>% #named the mean column z but then remove the z
   mutate(across(everything(), ~ifelse(is.nan(.), NA, .))) %>%  #gets rid of NaNs created by taking mean during summarize step
   ungroup()%>%
   subset(Site != 100.1) #removes ISCO samples, part of different data package
   
   
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
   
   # Read in the minimum detection limits from Jeff
   
   
   # Establish flag columns and add ones for missing values
   for(j in colnames(frame2%>%select(starts_with(c("T","S")), -Site))) { 
     #for loop to create new columns in data frame
     frame2[,paste0("Flag_",colnames(frame2[j]))] <- 0 #creates flag column + name of variable
     frame2[c(which(is.na(frame2[,j]))),paste0("Flag_",colnames(frame2[j]))] <- 1 #puts in flag 1 if value not collected
     frame2[c(which(frame2[,paste0("n_",colnames(frame2[j]))]>1)),paste0("Flag_",colnames(frame2[j]))] <- 7 #puts in flag 7 for sample run twice and we report the mean
     }
   
   # Now we can remove the number of observation columns
   frame3 <- frame2%>%
     select(-starts_with("n_"))
   
   
   
   # Use the maintenance Log to take out values
 
  
 
 #let's set up flags! Some will be manually entered, but we can at least make the columns
 frame1 <- frame1 %>% 
  mutate(Flag_DateTime = 0, #needs to be manually entered
         Flag_TFe_mgL = 0,
         Flag_TFe_mgL = ifelse(is.na(TFe_mgL), 1, Flag_TFe_mgL), #missing value
         Flag_TFe_mgL = ifelse(TFe_mgL < 0.01 & !is.na(TFe_mgL), 3, Flag_TFe_mgL), #below reporting level, set to min reporting level later
         Flag_TFe_mgL = ifelse(TFe_mgL < 0 & !is.na(TFe_mgL), 4, Flag_TFe_mgL), #negative value, set to min reporting level later
         Flag_TFe_mgL = ifelse(n_TFe > 1, 7, Flag_TFe_mgL), #flag for sample run multiple times, mean
         Flag_TFe_mgL = ifelse(TFe_mgL > 18.47622 & !is.na(TFe_mgL), 8, Flag_TFe_mgL), # 18.47622 is 3 sd above mean (2014-2022), flag for abnormally high value
         Flag_TMn_mgL = 0,
         Flag_TMn_mgL = ifelse(is.na(TMn_mgL), 1, Flag_TMn_mgL), #missing value
         Flag_TMn_mgL = ifelse(TMn_mgL < 0.0001 & !is.na(TMn_mgL), 3, Flag_TMn_mgL), #below reporting level, set to min reporting level later
         Flag_TMn_mgL = ifelse(TMn_mgL < 0 & !is.na(TMn_mgL), 4, Flag_TMn_mgL), #negative value, set to min reporting level later
         Flag_TMn_mgL = ifelse(n_TMn > 1, 7, Flag_TMn_mgL), #flag for sample run multiple times, mean
         Flag_TMn_mgL = ifelse(TMn_mgL > 2.70027 & !is.na(TMn_mgL), 8, Flag_TMn_mgL), # 2.70027 is 3 sd above mean (2014-2022), flag for abnormally high value
         Flag_SFe_mgL = 0,
         Flag_SFe_mgL = ifelse(is.na(SFe_mgL), 1, Flag_SFe_mgL), #missing value
         Flag_SFe_mgL = ifelse(SFe_mgL < 0.01 & !is.na(SFe_mgL), 3, Flag_SFe_mgL), #below reporting level, set to min reporting level later
         Flag_SFe_mgL = ifelse(SFe_mgL < 0 & !is.na(SFe_mgL), 4, Flag_SFe_mgL), #negative value, set to min reporting level later
         Flag_SFe_mgL = ifelse(n_SFe > 1, 7, Flag_SFe_mgL), #flag for sample run multiple times, mean
         Flag_SFe_mgL = ifelse(SFe_mgL > 17.22513 & !is.na(SFe_mgL), 8, Flag_SFe_mgL), # 17.22513 is 3 sd above mean (2014-2022), flag for abnormally high value
         Flag_SMn_mgL = 0,
         Flag_SMn_mgL = ifelse(is.na(SMn_mgL), 1, Flag_SMn_mgL), #missing value
         Flag_SMn_mgL = ifelse(SMn_mgL < 0.0001 & !is.na(SMn_mgL), 3, Flag_SMn_mgL), #below reporting level, set to min reporting level later
         Flag_SMn_mgL = ifelse(SMn_mgL < 0 & !is.na(SMn_mgL), 4, Flag_SMn_mgL), #negative value, set to min reporting level later
         Flag_SMn_mgL = ifelse(n_SMn > 1, 7, Flag_SMn_mgL), #flag for sample run multiple times, mean
         Flag_SMn_mgL = ifelse(SMn_mgL > 2.628231 & !is.na(SMn_mgL), 8, Flag_SMn_mgL)) # 2.628231 is 3 sd above mean (2014-2022), flag for abnormally high value)
 
    #metals flags 2 (instrument malfunction) and 6 (non-standard method) will have to be entered manually
  
#make a test to see if the totals and solubles didn't get mixed up: difference column, print values that are negative?
 
#let's write the final csv
#note: you must edit the script each time to save the correct file name
 frame1 <- frame1 %>% 
   select(DateTime, Reservoir, Depth_m, Site, TFe_mgL, TMn_mgL, SFe_mgL, SMn_mgL, Flag_DateTime,
          Flag_TFe_mgL, Flag_TMn_mgL, Flag_SFe_mgL, Flag_SMn_mgL) %>% 
   arrange(DateTime, Reservoir, Site, Depth_m)
 
write.csv(frame1, file = '~/Documents/GitHub/Reservoirs/Data/DataNotYetUploadedToEDI/Metals_Data/EDI_Working/2023/Metals_230508_230529.csv')
}