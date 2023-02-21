qaqc_fcrweir <- function(VT_data_file, WVWA_data_file, maintenance_file, Staff_gauge_readings, output_file, start_date, end_date) {

#### Read in the VT streaming sensors ####  
  VTsens=VT_data_file
  
  if (is.character(VTsens)==T) {
    VTsens<-read_csv(VTsens, skip=4,col_names=c("DateTime","Record","BattV","PTemp_C","AirTemp_C","Lvl_psi","wtr_weir"))
   
    # Select the columns you want
    VTdat <- VTsens[,c("DateTime","Lvl_psi","wtr_weir")]
    # Rename the columns
    colnames(VTdat) <- c('DateTime', 'VT_Pressure_psia', 'VT_Temp_C')
    
  }else {# If the file is used in the markdown file then is is already read in and nothing needs to happen
    VTdat<-VTsens
  }
  
  # Add a column to calculate flow
  VTdat$VT_Flow_cms <- NA
  
#### Read in the WVWA sensors ####
  WVWA_sens<- read_csv(WVWA_data_file)
    #read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_Inflow/2023_Jan/misc_data_files/WVWA_pressure_readings_2013_current.csv")
  
  
  # Add a column to calculate flow
  WVWA_sens$WVWA_Flow_cms <- NA
  
#### read in maintenance file ####
  log_read <- #read_csv(file.path('https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-weir-data/Weir_MaintenanceLog.txt'),skip=0, col_types = cols(
    read_csv(maintenance_file, skip=0, col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer(),
  )) 
  
  log <- log_read
  
#### identify the date subsetting for the data ####
  if (!is.null(start_date)){
    Met <- Met %>% 
      filter(DateTime >= start_date)
    log <- log %>% 
      filter(TIMESTAMP_start >= start_date)
  }
  
  if(!is.null(end_date)){
    Met <- Met %>% 
      filter(DateTime <= end_date)
    log <- log %>% 
      filter(TIMESTAMP_end <= end_date)
  }
  
  if (nrow(log) == 0){
    log <- log_read
  }


### Combine the WVWA sensor and the VT sensor ######
All_Inflow <- merge(WVWA_sens, VTdat, by='DateTime', all=TRUE)

# Rearrange the columns

All_Inflow<-All_Inflow%>%
  select("DateTime", "WVWA_Pressure_psi", "WVWA_Baro_pressure_psi", "WVWA_Pressure_psia",
         "WVWA_Flow_cms", "WVWA_Temp_C", "VT_Pressure_psia", "VT_Flow_cms", "VT_Temp_C")

#### Add Flag columns ### 
# for loop to create flag columns
for(j in c(2:9)) { #for loop to create new columns in data frame
  All_Inflow[,paste0("Flag_",colnames(All_Inflow[j]))] <- 0 #creates flag column + name of variable
}


#### QAQC with the Maintenance Log ####

# separate the maintenance time from the dates and times for rating curves

main_log <- log%>%
  filter(DataStream=="WEIR")

### QAQC based on Maintenance log ###

# modify catdata based on the information in the log   

for(i in 1:nrow(main_log))
{
  # get start and end time of one maintenance event
  start <- main_log$TIMESTAMP_start[i]
  end <- main_log$TIMESTAMP_end[i]
  
  
  # get indices of columns affected by maintenance
  if(grepl("^\\d+$", main_log$colnumber[i])) # single num
  {
    maintenance_cols <- intersect(c(2:9), as.integer(main_log$colnumber[i]))
  }
  
  else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", main_log$colnumber[i])) # c(x;y;...)
  {
    maintenance_cols <- intersect(c(2:9), as.integer(unlist(regmatches(main_log$colnumber[i],
                                                                       gregexpr("\\d+", main_log$colnumber[i])))))
  }
  else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", main_log$colnumber[i])) # c(x:y)
  {
    bounds <- as.integer(unlist(regmatches(main_log$colnumber[i], gregexpr("\\d+", main_log$colnumber[i]))))
    maintenance_cols <- intersect(c(2:9), c(bounds[1]:bounds[2]))
  }
  else
  {
    warning(paste("Could not parse column colnumber in row", i, "of the maintenance main_log. Skipping maintenance for",
                  "that row. The value of colnumber should be in one of three formats: a single number (\"47\"), a",
                  "semicolon-separated list of numbers in c() (\"c(47;48;49)\"), or a range of numbers in c() (\"c(47:74)\").",
                  "Other values (even valid calls to c()) will not be parsed properly."))
    next
  }
  
  if(length(maintenance_cols) == 0)
  {
    warning(paste("Did not parse any valid data columns in row", i, "of the maintenance main_log. Valid columns have",
                  "indices 2 through 9. Skipping maintenance for that row."))
    next
  }
  
  #index the Flag columns
  if(grepl("^\\d+$", main_log$flagcol[i])) # single num
  {
    flag_cols <- intersect(c(10:17), as.integer(main_log$flagcol[i]))
    
  }
  else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", main_log$flagcol[i])) # c(x;y;...)
  {
    flag_cols <- intersect(c(10:17), as.integer(unlist(regmatches(main_log$flagcol[i],
                                                                  gregexpr("\\d+", main_log$flagcol[i])))))
  }
  
  else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", main_log$flagcol[i])) # c(x:y)
  {
    bounds_flag <- as.integer(unlist(regmatches(main_log$flagcol[i], gregexpr("\\d+", main_log$flagcol[i]))))
    flag_cols <- intersect(c(10:17), c(bounds_flag[1]:bounds_flag[2]))
  }
  else
  {
    warning(paste("Could not parse column flagcol in row", i, "of the maintenance log. Skipping maintenance for",
                  "that row. The value of colnumber should be in one of three formats: a single number (\"47\"), a",
                  "semicolon-separated list of numbers in c() (\"c(47;48;49)\"), or a range of numbers in c() (\"c(47:74)\").",
                  "Other values (even valid calls to c()) will not be parsed properly."))
    next
  }
  
  #Get the Maintenance Flag 
  
  flag <- main_log$flag[i]
  
  # replace relevant data with NAs and set flags while maintenance was in effect
  if(flag==1 && maintenance_cols==2){ # for down correcting
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, maintenance_cols] <- All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, maintenance_cols]-0.14
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, flag_cols] <- flag
    
    # correct flags for those that were down corrected and if the flow is too low based on pressure sensor
    All_Inflow <- All_Inflow%>%
      mutate(Flag_WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                                         WVWA_Pressure_psia<0.184,13, Flag_WVWA_Flow_cms))
    
    # correct flags for those that were down corrected and if the flow is too high and over tops the weir
    All_Inflow <- All_Inflow%>%
      mutate(Flag_WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                                         WVWA_Pressure_psia>0.611,16, Flag_WVWA_Flow_cms))
    
  }else if(flag==1 && maintenance_cols==7){ # for up correcting for VT sensor
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, maintenance_cols] <- All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, maintenance_cols]+0.033
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, flag_cols] <- flag
  }
  
  else{ # All other flags and change to NA
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, maintenance_cols] <- NA
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, flag_cols] <- flag
  }
}

#### Read in gague height #####

# Guage height csv
# Load in this spreadsheet and align with atmospheric corrected pressure measured by the WVWA sensor
rating_curve_dates <- read_csv(Staff_gauge_readings)
  #read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_Inflow/2023_Jan/misc_data_files/Inflow_Gauge_Height_at_Weir.csv")


# Round time to nearest 15 minutes
rating_curve_dates <- rating_curve_dates %>% 
  mutate(DateTime = round_date(DateTime,"15 minutes")) %>% 
  select(DateTime,GageHeight_cm)

rating_curve <- left_join(rating_curve_dates, All_Inflow, by="DateTime")
rating_curve <- rating_curve%>% 
  select(DateTime,GageHeight_cm, WVWA_Pressure_psia, VT_Pressure_psia)%>%
  drop_na(GageHeight_cm)
# This is the data you will use to update the rating curve!
# We will calculate a rating curve with data from each time period

# Filter out the DateTime for making rating curve
# The flag numbers are used for filtering out in the if statements
rat_log <- log%>%
  filter(DataStream=="RATING")

# Create a data frame for information about the rating curve and height for flow calculation can go 
if(is.null(start_date)){
 header<- c("Sensor", "Start", "End", "Slope", "Intercept", "Low_psi", "High_psi")
 df2<- as.data.frame(setNames(replicate(7,numeric(0), simplify = F), header))
}

### Get Dates and Times for rating curve ####

# modify catdata based on the information in the log   
for(i in 1:nrow(rat_log)){
  # get start and end time of one maintenance event
  start <- rat_log$TIMESTAMP_start[i]
  end <- rat_log$TIMESTAMP_end[i]
  
  # get indices of columns affected by maintenance
  
  main_cols <- as.integer(rat_log$colnumber[i])
  
  # get number to identify time and info for rating curve
  
  info_cols <- as.integer(rat_log$flag[i])
  
  # getting liner realtionship between psi and guage height and calculating flow
  if(info_cols==100){
    
    # the old weir equations are taken directly from MEL's Inflow Aggregation script
    # Use for pressure data prior to 2019-06-06: see notes above for description of equations
    # NOTE: Pressure_psia < 0.185 calculates -flows (and are automatically set to NA's)
    # THIS WILL NOT CHANGE EVER AGAIN! KEEP AS IS FOR RECTANGULAR WEIR
    
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, "flow1"]<-All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, "WVWA_Pressure_psia"]* 0.70324961490205 - 0.1603375 + 0.03048 
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, "flow_cfs"]<- (0.62 * (2/3) * (1.1) * 4.43 * (All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, "flow1"] ^ 1.5) * 35.3147)
    All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, main_cols]<-All_Inflow[All_Inflow$DateTime >= start & All_Inflow$DateTime <= end, "flow_cfs"]*0.028316847
    
    # take out columns you don't need any more
    All_Inflow<-subset(All_Inflow, select=-c(flow1, flow_cfs))
    if(main_cols==5){
      # correct flags for flow being too low based on pressure sensor
      All_Inflow <- All_Inflow%>%
        mutate(Flag_WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                                           Flag_WVWA_Flow_cms!=13 & WVWA_Pressure_psia<0.184,3, Flag_WVWA_Flow_cms))
      
      # Make flow NA when psi <= 0.184 (distance between pressure sensor and bottom of weir = 0.1298575 m = 0.18337 psi
      All_Inflow <- All_Inflow%>%
        mutate(WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                                      WVWA_Pressure_psia<0.184,NA, WVWA_Flow_cms))
      
      # Will also need to flag flows when water tops the weir: for the rectangular weir, head = 0.3 m + 0.1298575 m = 0.4298575 m
      # This corresponds to Pressure_psia <= 0.611244557965199
      # diff_pre$Flow_cms = ifelse(diff_pre$Pressure_psia > 0.611, NA, diff_pre$Flow_cms)
      # Decided to keep data from above the weir, but flag!
      All_Inflow <- All_Inflow%>%
        mutate(Flag_WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                                           Flag_WVWA_Flow_cms!=16 & WVWA_Pressure_psia>0.611,6, Flag_WVWA_Flow_cms))
    }
    
    if (main_cols==8){
      # correct flags for flow being too low based on pressure sensor
      All_Inflow <- All_Inflow%>%
        mutate(Flag_VT_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(VT_Pressure_psia) &
                                         VT_Pressure_psia<0.184,3, Flag_VT_Flow_cms))
      
      # Make flow NA when psi <= 0.184 (distance between pressure sensor and bottom of weir = 0.1298575 m = 0.18337 psi
      All_Inflow <- All_Inflow%>%
        mutate(VT_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(VT_Pressure_psia) &
                                    VT_Pressure_psia<0.184,NA, VT_Flow_cms))
      
      # Will also need to flag flows when water tops the weir: for the rectangular weir, head = 0.3 m + 0.1298575 m = 0.4298575 m
      # This corresponds to Pressure_psia <= 0.611244557965199
      # diff_pre$Flow_cms = ifelse(diff_pre$Pressure_psia > 0.611, NA, diff_pre$Flow_cms)
      # Decided to keep data from above the weir, but flag!
      All_Inflow <- All_Inflow%>%
        mutate(Flag_VT_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(VT_Pressure_psia) &
                                         VT_Pressure_psia>0.611,6, Flag_VT_Flow_cms))
    }
    # q = 2.391 * H^2.5
    # where H = head in meters above the notch
    # the head was 14.8 cm on June 24 at ~13:30
    #14.8 cm is 0.148 m 
    #14.9cm on Jun 27 at 3:49PM
    # WW: Used scaling relationship to determine head:pressure relationship
    # diff_post <- diff_post %>%  mutate(head = (0.149*Pressure_psia)/0.293) %>% 
    #   mutate(Flow_cms = 2.391* (head^2.5)) %>% 
    #   select(DateTime, Temp_C, Baro_pressure_psi, Pressure_psi, Pressure_psia, Flow_cms)
    
  }
  if(info_cols!=100 && main_cols==5){
    # Can use this for the rest of the time because everything is done here
    # This is for WVWA sensor
    # figure out the liner regression 
    
    # select rating curve based on date time
    sel_WVWA<- rating_curve[rating_curve$DateTime >= start & rating_curve$DateTime <= end,]
    
    # Let's calculate flow for WVWA
    #get linear relationship between pressure gage and 
    fit_WVWA <- lm(sel_WVWA$GageHeight_cm ~ sel_WVWA$WVWA_Pressure_psia)
    #plot(sel_WVWA$GageHeight_cm ~ sel_WVWA$WVWA_Pressure_psia)
    # Pull out intercept
    int_WVWA<-as.numeric(fit_WVWA$coefficients[1])  
    
    # Pull out slope
    slo_WVWA<-as.numeric(fit_WVWA$coefficients[2])
    
    # Calculate the Flow
    All_Inflow <- All_Inflow %>% mutate(head = ((slo_WVWA*WVWA_Pressure_psia)+int_WVWA)/100) %>% 
      mutate(WVWA_Flow_cms = 2.391 * (head^2.5)) %>% 
      select(-c(head))
    
    # take out values when flow is too low to detect
    # figure out when that is 
    low_WVWA<-(-int_WVWA)/slo_WVWA
    
    # take out the low values and flag
    # According to gage height vs. pressure relationship as calculated above
    
    All_Inflow<-All_Inflow%>%
      mutate(Flag_WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                                         WVWA_Pressure_psia<low_WVWA,3, Flag_WVWA_Flow_cms))%>%
      mutate(WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                                    WVWA_Pressure_psia<low_WVWA,NA, WVWA_Flow_cms))
    
    
    # flag when water is over top of the weir at 27.5 cm but not taking it out
    # figure out at what psia that is at 
    high_WVWA=(27.5-int_WVWA)/slo_WVWA
    
    # populate the data frame for the stats from the rating curve
    if(is.null(start_date)){
   df2[i,"Sensor"]<-"WVWA"
    df2[i, "Start"]<-as.character(start)
    df2[i, "End"]<-as.character(end)
    df2[i, "Slope"]<-slo_WVWA
    df2[i, "Intercept"]<-int_WVWA
    df2[i, "Low_psi"]<-low_WVWA
    df2[i, "High_psi"]<-high_WVWA
    }
    
    # flag those values but don't remove them
    All_Inflow<-All_Inflow%>%
      mutate(Flag_WVWA_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(WVWA_Pressure_psia) &
                                         WVWA_Pressure_psia>high_WVWA,6, Flag_WVWA_Flow_cms))
    
  }else if (info_cols!=100 && main_cols==8){# Let's do it for VT Sensor
    
    # select rating curve based on date time
    sel_VT<- rating_curve[rating_curve$DateTime >= start & rating_curve$DateTime <= end,]
    
    # What to do about if there is only one observation 
    
    fit_VT <- lm(sel_VT$GageHeight_cm ~ sel_VT$VT_Pressure_psia)
    #plot(sel_VT$GageHeight_cm ~ sel_VT$VT_Pressure_psia)
    
    # Pull out intercept
    int_VT<-as.numeric(fit_VT$coefficients[1])  
    
    # Pull out slope
    slo_VT<-as.numeric(fit_VT$coefficients[2])
    
    
    # Calculate Flow
    All_Inflow <- All_Inflow %>% mutate(head_VT = ((slo_VT*VT_Pressure_psia)+int_VT)/100) %>% 
      mutate(VT_Flow_cms = 2.391 * (head_VT^2.5)) %>% 
      select(-c(head_VT))
    
    # take out values when flow is too low to detect
    # figure out when that is 
    low_VT<-(-int_VT)/slo_VT
    
    # take out the low values and flag
    # According to gage height vs. pressure relationship as calculated above
    
    All_Inflow<-All_Inflow%>%
      mutate(Flag_VT_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(VT_Pressure_psia) &
                                       VT_Pressure_psia<low_VT,3, Flag_VT_Flow_cms))%>%
      mutate(VT_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(VT_Pressure_psia) &
                                  VT_Pressure_psia<low_VT,NA, VT_Flow_cms))
    
    
    # flag when water is over top of the weir at 27.5 cm but not taking it out
    # figure out at what psia that is at 
    high_VT=(27.5-int_VT)/slo_VT
    
    # populate the data frame for the stats from the rating curve
    if(is.null(start_date)){
    df2[i,"Sensor"]<-"VT"
    df2[i, "Start"]<-as.character(start)
    df2[i, "End"]<-as.character(end)
    df2[i, "Slope"]<-slo_VT
    df2[i, "Intercept"]<-int_VT
    df2[i, "Low_psi"]<-low_VT
    df2[i, "High_psi"]<-high_VT
    }
    
    # flag those values
    All_Inflow<-All_Inflow%>%
      mutate(Flag_VT_Flow_cms=ifelse(DateTime >= start & DateTime <= end & !is.na(VT_Pressure_psia) &
                                       VT_Pressure_psia>high_VT,6, Flag_VT_Flow_cms))
  }else 
    print(paste("Could not parse column flagcol in row", i, "of the rating log."))
  next
}

# Drop NA in the data frame of slope info
if(is.null(start_date)){
 df<-df2%>%
   drop_na(Sensor)%>%
   mutate_if(is.numeric, ~round(., 3))
}



### Add flags for missing values ####
# for loop to create flag columns
for(j in c(2:9)) { #for loop to create new columns in data frame
  All_Inflow[c(which(is.na(All_Inflow[,j]) & (All_Inflow[,paste0("Flag_",colnames(All_Inflow[j]))]==0))), paste0("Flag_",colnames(All_Inflow[j]))]=7#s #puts in flag 7 if value not collected
}

#### Add Reservoir and Site ####
# Get it ready for publishing and get the maintenance log ready. 
#creating columns for EDI
All_Inflow$Reservoir <- "FCR" #creates reservoir column to match other data sets
All_Inflow$Site <- 100  #creates site column to match other data sets

Final<-All_Inflow%>%
  select(Reservoir, Site, everything())

#### Write to CSV ####

write.csv(Final,file=output_file, row.names=F)

# once you return something your function ends. This always comes after write to csv
if(is.null(start_date)){return(df)}

}
