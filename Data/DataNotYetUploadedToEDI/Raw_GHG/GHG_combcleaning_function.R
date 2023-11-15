# Download/load libraries
pacman::p_load(lubridate,tidyverse, googledrive, readxl, googlesheets4, gsheet)


GHG_combcleaning_function<-function(directory = "./Data/DataNotYetUploadedToEDI/Raw_GHG/",
                                    gdrive = T, # Are the files on Google Drive. True or False
                                    gshared_drive = as_id("1YD8QyV4AsaMMzn974jPY8lhb2pcr2ql2"),
                                    current_year = 2023 )# Current Year. Must be numeric
{
  
  directory = "./Data/DataNotYetUploadedToEDI/Raw_GHG/"
  gdrive = T # Are the files on Google Drive. True or False
  gshared_drive = as_id("1YD8QyV4AsaMMzn974jPY8lhb2pcr2ql2")
  current_year = 2023 
  
  
  # Name the directory where the full output files are found. Ours are on GitHub 
  mydir <-directory
  
  # list of GHG files on Github
  rfiles <- list.files(path=paste0(mydir,"data/"),pattern="", full.names=TRUE)
  
  
  # Are the files on Google Drive? If so then download missing EddyPro Full Output files
  
  if(gdrive==T){
    # Get the file info of the GHG spreadsheets from GoogleDrive
    gdrive_files<-googledrive::drive_ls(path = gshared_drive, 
                                         pattern = "GC", 
                                         type = "xlsx")
    
    
    
    # download output files and put them on GitHub
    
    for(i in 1:nrow(gdrive_files)){
      
      #extract the beginning of the file name to see if a qaqc plot has been made
      dfile<-sub("\\_full.*", "",sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(gdrive_files$name[i])))
      
      
      if(any(grepl(dfile,rfiles))==F){
        # download and put on GitHub
        
        name<-gdrive_files$name[i]
        
        googledrive::drive_download(gdrive_files$id[i], path = paste0(mydir,"data/",name))
        
      }else{
        
      }
    }
    
  }
  
  #### Calculating GHG gas concentrations #####
  # All numbers are taken from a spreadsheet Bobbie made called GHG_MEGA_GC_SHEET_EXCEL
  
  # For now let's make one big data frame and then do the calculations each time, find the sampling locations,
  # and then does a QAQC
  
  # Create a function that reads in the file from the GC, selects the columns we want, get clean vial numbers, 
  # and get notes from the vial numbers
  
  read_ghg_files<-function(FILES){
  
  df <- read_excel(FILES, skip=7, col_names=T)%>%
    select("Date Acquired...3","Sample Name...4",
            "Season specific ranged CAL Measured headspace CH4  in ppm from GC in ppm",
            "Season specific CAL Measured headspace CO2 in ppm from GC in ppm", Note)%>%
    dplyr::rename("date_acquired"="Date Acquired...3",
                  "vial_number"="Sample Name...4",
                  "CH4_GC_headspace_ppm"="Season specific ranged CAL Measured headspace CH4  in ppm from GC in ppm",
                  "CO2_GC_headspace_ppm"="Season specific CAL Measured headspace CO2 in ppm from GC in ppm")%>%
    filter(grepl("^[0-9]", vial_number))%>%
    mutate(clean_vial_number=as.numeric(gsub("\\D", "", vial_number)),
            notes_from_vial_number= gsub("\\d+", "", vial_number),
           CH4_GC_headspace_ppm = as.numeric(gsub("nd", NA, CH4_GC_headspace_ppm)), # if headspace labeled nd change it to NA
           CO2_GC_headspace_ppm = as.numeric(gsub("nd", NA, CO2_GC_headspace_ppm)),
           count = nchar(gsub("[^0-9]+", "", clean_vial_number)), # count the number of digits and if there are too many then it is caught in the if statement below
          notes = coalesce(Note, notes_from_vial_number))%>%
          select(!c(Note, notes_from_vial_number))
  
  # There are numbers over 3 digits then duplicate the row and split the numbers
  for (i in 1:nrow(df)){
  if(df$count[i]>3){
    fg <- df[i,]
    
    # now allit up the vial numbers
    # only take the first three and replace in the data frame
  df[i,"clean_vial_number"] <-as.numeric(sub("\\D*(\\d{3}).*", "\\1.", df$clean_vial_number[i]))
   
   # now we just want the last three digits because we took the first three
   fg[1,"clean_vial_number"]<-as.numeric(str_sub(fg$clean_vial_number[1], start= -3))
   
  # bind everything together so we have the duplicate  
   df<-bind_rows(df,fg)
    
  }
  }  
   # Now that we have the correct number of digits we should recount. Do we need to do this?
  #df$count<- nchar(gsub("[^0-9]+", "", df$clean_vial_number))
    
  return(df)
  }        
  
  # use purr to read in all the files using the function above
  
  all<-list.files(path=paste0(mydir,"data/"),pattern="", full.names=TRUE)%>%
    map_df(~ read_ghg_files(.x))
  
  #### Let's get the concentrations ####
  
  # read in the file Bobbie created for air temp and pressure
  # Use this section of code once Bobbie changes the permissions because we don't need to use and authentification token
  
  gsheet_url <- "https://docs.google.com/spreadsheets/d/1YH9MrOVROyOgm0N55WiMxq2vDexdGRgG/edit#gid=462758328"
  
  temp_pres <- gsheet::gsheet2tbl(gsheet_url)
  
  # Use this now to just make the script and check it works. It downloads the file locally. 
  
  # dl <- drive_download(
  #   as_id(gsheet_url),
  #   path = 'temp1.xlsx', 
  #   overwrite = TRUE, 
  #   type = "xlsx")
  
  # take out the first 9 rows because we don't need them
  temp_pres<-temp_pres[9:nrow(temp_pres),1:4]
  
  # Rename the columns
  names(temp_pres)<- c("Date","lab_temp", "weather_station_bp", "notes")
  
  
  temp_pres2 <- temp_pres%>%
      mutate(
        Date= as.Date(Date, format = "%d-%b-%y"),
        lab_temp=as.numeric(lab_temp),
        weather_station_bp=as.numeric(weather_station_bp))%>%
        select(-notes)
        # weather_station_bp_2=ifelse(weather_station_bp>100, as.numeric(paste0(
        #   substr(weather_station_bp, 1, 2),
        #   ".",
        #   substr(weather_station_bp, 3, 7))), weather_station_bp))
  
  ### Assign the lab temp and BP based on the observations ###
  
  # set date_acquired as just a date. The dates should line up but just incase they don't 
  # let's take the closes observation
  all$date_acquired_comp<-as.Date(all$date_acquired)

  # find the closest date and then join the two data frames
  by<-join_by(closest(date_acquired_comp>= Date))
  fg<-full_join(all, temp_pres2, by)
  
  fg<-fg%>%drop_na(date_acquired)
  
  
  #### Assigning all the variables to calculate the GHG concentration ####
  
  # For both CO2 and CH4
  
  # Head space volume in Liters
  fg$headspace_L <- 0.002
  
  # Volume of water in Liters
  fg$vol_water_L <- (21.0733341666667/100)-fg$headspace_L
  
  # Temperature in Kelvin at the time when headspace concentration is equalibrated
  fg$Temp_headspace_K <- 313.15
  
  # Total pressure in atm- calculated from amount of HS 
  # added at room temp then adjusted to presssure at agitator temperature of 40 C (p2 = (t2 *p1)/t1) 
  fg$tot_pressure_atm<-((0.001316*((fg$weather_station_bp*25.4)-(2.5*2053/100)))*(273.15+40))/(273.15+fg$lab_temp)
 
  
  ## Just for CH4
  
  # Partial Pressure for the CH4 gas
  fg$CH4_partial_pressure <- fg$tot_pressure_atm*(fg$CH4_GC_headspace_ppm/10^6)
  
  # mols compound in CH4 gas (calculated from partial pressure)
  fg$CH4_mol <- (fg$CH4_partial_pressure*fg$headspace_L)/(0.082057*fg$Temp_headspace_K)
  
  # Unnamed column. Check with Bobbie but used to calculate the mol compund in water
  fg$hen_law <- 101.325*(0.000014*exp(1600*((1/fg$Temp_headspace_K)-(1/298.15))))
  
  # mol compound in water (estimated from Henry's)
  fg$CH4_mol_water <- fg$CH4_partial_pressure*fg$hen_law*fg$vol_water_L

  # Mols of CH4 gas used to make the headspace. Needs to be taken out of the final concentration
  fg$CH4_headspace_mol <- 0
  
  # mols of CH4 in gas in vessel (calculated from partial pressure)
  fg$CH4_tot_vessel <- fg$CH4_mol + fg$CH4_mol_water
  
  ### Total Concentration of CH4.  
  # Total concentration of CH4 in the original aqueous sample in µmol/L
  fg$CH4_umolL <- 10^6*((fg$CH4_tot_vessel-fg$CH4_headspace_mol)/fg$vol_water_L)
  
  
  
  ## Just for CO2 
  
  # Partial Pressure for CO2 gas from amount of CO2 in the headspace
  fg$CO2_partial_pressure <- fg$tot_pressure_atm*(fg$CO2_GC_headspace_ppm/10^6)
  
  # mols of CO2 in gas (estimated from Henry's)
  fg$CO2_mol <- (fg$CO2_partial_pressure*fg$headspace_L)/(0.082057*fg$Temp_headspace_K)
  
  # mol of CO2 in water
  fg$CO2_mol_water <- fg$CO2_partial_pressure*fg$hen_law*fg$vol_water_L
  
  # mols of CO2 in gas in vessel (calculated from partial pressure)
  fg$CO2_tot_vessel <- fg$CO2_mol + fg$CO2_mol_water
  
  # mols compound in gas used to make headspace
  fg$CO2_headspace_mol <- 0
  
  ### Total Concentration of CH4.  
  # Total concentration of CH4 in the original aqueous sample in µmol/L
  fg$CO2_umolL <- 10^6*((fg$CO2_tot_vessel-fg$CO2_headspace_mol)/fg$vol_water_L)
  
  
  #### Match up vial numbers to Sampling Locations #####
  
  # Now read in the Digitized GHG vials spreadsheet from Google Drive
  
  gsheet_url2 <- "https://docs.google.com/spreadsheets/d/1HoBeXWUm0_hjz2bmd-ZmS0yhgF1WvLenpvwEa8dL008/edit#gid=1256821207"
  
  site_info <- gsheet::gsheet2tbl(gsheet_url2)
  
  # convert the date and DateTime into usable form
  site_info <- site_info%>%
    #dplyr::rename(clean_vial_number="Vial Number")%>%
    mutate(
      clean_vial_number= `Vial Number`,
      DateTime=parse_date_time(DateTime, orders = c('ymd HMS','ymd HM','ymd','mdy')),
      Date=as.Date(DateTime),
      Date_upper=Date+4)

  
  # samples that were take less than 3 days from the process date. Usually they are processed the next day
  # use the join_by and between to say that lab processing had to happened after the samples were collected
  # but less that 4 days after collection. That is what Date_upper is. 
  
  by<-join_by("clean_vial_number", between(date_acquired_comp ,Date, Date_upper))
  ab<-full_join(fg, site_info, by)
  
  # Check out how that worked and it did!!
  
  work_check <- ab%>%
    select(Reservoir, Site, Depth_m, Date.y, date_acquired, Date_upper, vial_number,`Vial Number`)%>%
    dplyr::rename(field_date=Date.y,
                  lab_date=date_acquired,
                  upper_date=Date_upper,
                  lab_vial_number=vial_number,
                  field_vial_number=`Vial Number`)%>%
    distinct()
  
  work_check$lab_date = as.Date(as.character(with_tz(work_check$lab_date, "America/Nome")))
  
  
    
  
  # Make a list of observations that don't fall with in the 3 days after collection
  
  out_range<-work_check%>%
    filter(is.na(lab_vial_number)|is.na(field_vial_number))%>%
    mutate(miss = ifelse(is.na(field_date),"no_field_obs", "no_lab_obs"),
           com_date=coalesce(lab_date, field_date))%>%
    filter(field_date>as.Date("2023-03-22")|is.na(field_date))%>%
    filter(lab_date>as.Date("2023-03-24")|is.na(lab_date))%>%
    select(com_date, miss, field_date, lab_date, upper_date, field_vial_number, lab_vial_number)
  
  
  # Count number of observations for each time
  field_sum<-work_check%>%
    group_by(field_date)%>%
    count(field_date)%>%
    dplyr::rename(field_lab=n)%>%
    filter(field_date>as.Date("2023-03-01"))
  
  # lab sum to see how many are missing
  lab_sum<-work_check%>%
    group_by(lab_date)%>%
    count(lab_date)%>%
    dplyr::rename(count_lab=n)%>%
    drop_na(lab_date)
  
  # merge to see if the number of samples from the field match with the lab 
  
  # find the closest date and then join the two data frames
  by<-join_by(closest(lab_date>= field_date))
  fgfgg<-full_join(field_sum, lab_sum, by)
  
  total_samples_field_lab<-bind_cols(field_sum,lab_sum)
  
  
  # Select only columns we want 
  
  # Make a working data frame for QAQC that goes into QAQC
  working_final_df<-ab%>%
    distinct()%>%
    mutate(field_lab_date_check = coalesce(DateTime, date_acquired))%>%
    mutate(Notes = str_c(coalesce(notes, Notes), coalesce(Notes, notes), sep="_"))%>% 
    select(field_lab_date_check,Reservoir, Site, DateTime, Depth_m, CH4_umolL, CO2_umolL, Notes)
    
  
  # Make a maintenance log to remove observations that had issues or are missing
  
  
  # Run through a QAQC
  
}
