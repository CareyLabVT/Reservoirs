### Function to create a QAQC plot and
## clean up files produced by LiCor Eddy Pro

## Originally from Alex Hounshell's EddyPro_CleanUp script from 8 October 2021, A. Hounshell

# Edits: 
## A. Breef-Pilz updated on 26 Oct. 2022 to included a for loop to
## read in the EddyPro output, create QAQC plots and bind all the files together into on large data frame.
## A. Breef-Pilz modified the script to create a function that reads in the current EddyFlux files
## Makes QAQC plots if they haven't been created,
## then binds current files from the year,
## does a quick QAQC check and then creates an L1 file with all of the current fluxes
## A.Breef-Pilz edited to just make it the function 26 Jan 2024
## A.Breef-Pilz edits to filter by EDi package and make time conversion section work


# Additional notes: This script is included with this EDI package to show which QAQC has already been 
# applied to generate these data <and includes additional R scripts available with this package>. 
# This script is only for internal use by the data creator team and is provided as a reference; 
# it will not run as-is. 


#####################################################


# Download/load libraries
#  pacman::p_load(lubridate,tidyverse,hms,gridExtra,openair, googledrive)
# 
# library(EDIutils)
# library(xml2)


eddypro_cleaning_function<-function(directory, # Name of the directory where the data folder and the QAQC plot folder lives
                                    gdrive, # Are the files on Google Drive. True or False
                                    gshared_drive, # Name of the shared drive where the files are held or use as_id()and the ID of the folder
                                    #current_year, # Current Year. Must be numeric
                                    output_file,
                                    start_date, # date the EDI file ends
                                    end_date)  # sys.Date plus 1
{
  
  # Name the directory where the full output files are found. Ours are on GitHub 
  mydir <-directory
  
  # list of EddyPro full output files on Github
  rfiles <- list.files(path=paste0(mydir,"data/"),pattern="", full.names=TRUE)
  
  
  # Are the files on Google Drive? If so then download missing EddyPro Full Output files
  
  if(gdrive==T){
    # Get the file info of the EddyPro Full output files
    gdrive_files<-googledrive::drive_find(pattern = "_full_output_", 
                                          type="csv",
                                          n_max = Inf, 
                                          shared_drive = gshared_drive)
    
    
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
  
  ## Now for the QAQC Plots Section
  
  
  # Make a list of files on GitHub but only for the current year. Might need to think about transition time. 
  
  myfiles = list.files(path=paste0(mydir,"data/"),pattern= "", recursive = TRUE,full.names=TRUE)
  
  # Check this directory to see if there are plots of the data
  qaqc_files <- list.files(path=paste0(mydir, "QAQC plots/"),pattern="", full.names=TRUE)
  
  
  #taking out the the Temp Test files
  #myfilesBVR <- myfiles[ !grepl("CR6_BVR_TempTest*", myfiles) ]#exclude the Temp test data
  
  # Check to see if a QAQC plot has been made. If it hasn't don't have to make it again and that means
  # the data has already been cleaned and add to the current file
  
  #create an out.file for the combined data
  
  out.file <- NULL
  
  # read in header from an early file will all the columns we want
  #  columns <- colnames(read.csv(myfiles[1], skip=1, as.is=T))%>%
  #    map_dfr( ~tibble(!!.x := logical() ) )
  
  
  for(k in 1:length(myfiles)){
    
    # read in the file
    header2<-read.csv(myfiles[k], skip=1, as.is=T) #get header minus wonky Campbell rows
    data2<-read.csv(myfiles[k], skip=3, header=F) #get data minus wonky Campbell rows
    names(data2)<-names(header2) #combine the names to deal with Campbell logger formatting
    
    if(colnames(header2)[1]!="filename"){
      data2<-read.csv(myfiles[k], header=T)
      data2$date <- as.Date(data2$date, tryFormats = "%m/%d/%Y") # convert date 
      data2$date <- as.character(data2$date) # make it character so can bind files together
    }
    
    # Clean up and make it useable for plotting
    data2[data2 ==-9999] <- NA # Remove -9999 and replace with NAs
    
    # Make a datetime column
    data2$datetime <- as.POSIXct(paste(data2$date , paste(data2$time), sep=" "))
    
    #extract the beginning of the file name to see if a qaqc plot has been made
    dfile<-sub("\\_full.*", "",sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(myfiles[k])))
    
    
    # If a QAQC plot hasn't been made then make it and gave it in folder on GitHub
    if (any(grepl(dfile,qaqc_files))==F){
      
      # Find the end row of the file. This is so I can get the date of the last reading
      endrow=tail(data2,n=1)
      
      # Now create the PDF that is saved in the Google Drive
      #folder <- "G:/Shared drives/VT-UBC FCR Team/QAQC Plots/"
      pdf(paste0(mydir, "QAQC plots/",dfile," _ ", "plot", ".pdf"), width=8.5, height=11) #call PDF file
      
      
      #Create the first page
      plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
      
      text(5, 8, "EC Data Fluxes in Falling Creek Reservoir, Vinton, VA")
      text(5, 7, paste0("Start: ",data2[1,'date']))
      text(5, 6, paste0("End: ",endrow$date))
      
      # Now the plots for the first page
      p1=data2 %>% 
        ggplot(aes(datetime, ch4_flux)) + geom_point() + geom_line() + theme_bw()
      
      p2=data2 %>% 
        ggplot(aes(datetime, co2_flux)) + geom_point() + geom_line() + theme_bw()
      
      # CO2 and CH4 signal strength
      
      p3=data2 %>% 
        ggplot(aes(datetime, rssi_77_mean)) + geom_point() + geom_line() +
        ylab("CH4 signal strength (%)")+
        theme_bw()
      
      #CO2 signal changes when took out the 7200
      if("co2_signal_strength_7200_mean" %in% names(data2)) {
        p4=data2 %>% 
          ggplot(aes(datetime, co2_signal_strength_7200_mean)) + geom_point() + geom_line() +
          ylab("CO2 signal strength (%)")+
          theme_bw()
      } else {
        p4=data2 %>% 
          ggplot(aes(datetime, co2_signal_strength_7500_mean)) + geom_point() + geom_line() +
          ylab("CO2 signal strength (%)")+
          theme_bw()
      }
      
      #Smartflux voltage in 
      p5=data2 %>% 
        ggplot(aes(datetime, vin_sf_mean)) + geom_point() +
        theme_bw() +
        geom_line() +
        xlab(" ") +
        ylab('Smartflux voltage (V)') +
        scale_x_datetime(date_breaks = "1 days", date_labels = "%m-%d") 
      #theme(axis.text.x = element_text(size = 14, color = "black", angle = 60, hjust = 1),
      #     axis.text.y = element_text(size = 14, color = "black"),
      #      axis.title = element_text(size = 16, color = "black"))
      
      grid.arrange(p1,p2,p3,p4,p5,nrow=5)
      
      # plot ustar distribution, wind speed, windrose and temperature
      # Create a histogram of u*
      p6=data2 %>% 
        ggplot(aes(x=u.)) + geom_histogram()+
        geom_histogram(color="black", fill="white")+
        xlab('u star (m s^-1)')+
        theme_bw()
      
      # Wind Speed U,V,W
      p7=data2 %>% 
        ggplot(aes(datetime, u_unrot)) + geom_point() + geom_line() +
        ylab("U (m/s)")+
        theme_bw()
      p8=data2 %>% 
        ggplot(aes(datetime, v_unrot)) + geom_point() + geom_line() +
        ylab("V (m/s)")+
        theme_bw()
      p9=data2 %>% 
        ggplot(aes(datetime, w_unrot)) + geom_point() + geom_line() +
        ylab("W (m/s)")+
        theme_bw()
      
      grid.arrange(p6,p7,p8,p9,nrow=4)
      
      # Visualize wind directions that 
      chicago_wind=data2%>%
        select(datetime,wind_speed,wind_dir)%>%
        dplyr::rename(date = datetime, ws = wind_speed, wd = wind_dir)
      pollutionRose(chicago_wind, pollutant="ws")
      
      # Sonic Temperature
      p10=data2 %>% mutate(sonicC=sonic_temperature-273.15)%>%
        ggplot(aes(datetime, sonicC)) + geom_point() + geom_line() +
        ylab("Sonic Temperature (Degrees C)")+
        theme_bw()
      
      # H and LE
      p11=data2 %>% 
        ggplot(aes(datetime, H)) + geom_point() + 
        ylab("Sensible heat flux (W m^(-2))")+
        theme_bw()
      
      p12=data2 %>% 
        ggplot(aes(datetime, LE)) + geom_point() +
        ylab("Latent heat flux (W m^(-2))")+
        theme_bw()
      
      grid.arrange(p11,p12,nrow=2)
      
      # Flow Rate
      if("flowrate_mean" %in% names(data2)) {
        p13=data2 %>%
          ggplot(aes(datetime, flowrate_mean*60000)) + geom_point() +
          theme_bw() +
          geom_line() +
          xlab(" ") +
          ylab('Flowrate (L min-1)') 
      } else {
        p13=data2%>%
          ggplot(aes(datetime,60000))+geom_blank()+theme_bw()+
          labs(title="CO2 sensor is out for service and there is no flowrate")
      }
      grid.arrange(p10,p13,nrow=2)
      
      dev.off()
      
    }
    
    # Now that you have created the QAQC plots only pick the variables you want for EDI and for 
    # Processing
    
    # If there was no flow rate than add that column. It means the 7200-101 flow module wasn't working. 
    if(!("flowrate_mean" %in% names(data2))) {
      data2$flowrate_mean<-NA
      
    }
    
    data3<-data2%>%select(date,time,DOY,Tau,qc_Tau,H,qc_H,LE,qc_LE,co2_flux,qc_co2_flux,h2o_flux,
                          qc_h2o_flux,ch4_flux,qc_ch4_flux,`co2_v.adv`,`h2o_v.adv`,`ch4_v.adv`,
                          co2_molar_density,co2_mole_fraction,co2_mixing_ratio,co2_time_lag,
                          co2_def_timelag,h2o_molar_density,h2o_mole_fraction,h2o_mixing_ratio,
                          h2o_time_lag,h2o_def_timelag,ch4_molar_density,ch4_mole_fraction,
                          ch4_mixing_ratio,ch4_time_lag,ch4_def_timelag,sonic_temperature,
                          air_temperature,air_pressure,air_density,air_heat_capacity,
                          air_molar_volume,ET,water_vapor_density,e,es,specific_humidity,RH,VPD,
                          Tdew,wind_speed,max_wind_speed,wind_dir,`u.`,TKE,L,`X.z.d..L`,bowen_ratio,
                          `T.`,x_peak,x_offset,`x_10.`,`x_30.`,`x_50.`,`x_70.`,`x_90.`,un_Tau,
                          Tau_scf,un_H,H_scf,un_LE,LE_scf,un_co2_flux,co2_scf,un_h2o_flux,
                          h2o_scf,un_ch4_flux,ch4_scf,u_var,v_var,w_var,rssi_77_mean, flowrate_mean)
    out.file=bind_rows(data3, out.file)
  }
  
  # # Get rid of rows with no date
  # out.file2 <- subset(out.file, out.file$date != "")
  
  # change columns to numeric instead of character
  out.file[, c(3:80)] <- sapply(out.file[, c(3:80)], as.numeric)
  
  
  current.ec<-out.file%>%
    mutate(date=ymd(date), #converts date to correct format
           time=strptime(time, format = "%H:%M"), #converts time to postix
           time=as_hms(time), #takes out the date and just leaves the time
           Year = year(date),
           flowrate_mean = flowrate_mean *60000)%>% # convert the flowrate to L min-1
    dplyr::rename(Tau_kgms2 = Tau,
                  H_wm2 = H,
                  LE_wm2 = LE,
                  co2_flux_umolm2s = co2_flux,
                  h2o_flux_umolm2s = h2o_flux,
                  ch4_flux_umolm2s = ch4_flux,
                  co2_v_adv_umolm2s = `co2_v.adv`,
                  h2o_v_adv_umolm2s = `h2o_v.adv`,
                  ch4_v_adv_umolm2s = `ch4_v.adv`,
                  co2_molar_density_mmolm3 = co2_molar_density,
                  co2_mole_fraction_umolmol = co2_mole_fraction,
                  co2_mixing_ratio_umolmol = co2_mixing_ratio,
                  co2_time_lag_s = co2_time_lag,
                  h2o_molar_density_mmolm3 = h2o_molar_density,
                  h2o_mole_fraction_umolmol = h2o_mole_fraction,
                  h2o_mixing_ratio_umolmol = h2o_mixing_ratio,
                  h2o_time_lag_s = h2o_time_lag,
                  ch4_molar_density_mmolm3 = ch4_molar_density,
                  ch4_mole_fraction_umolmol = ch4_mole_fraction,
                  ch4_mixing_ratio_umolmol = ch4_mixing_ratio,
                  ch4_time_lag_s = ch4_time_lag,
                  sonic_temperature_k = sonic_temperature,
                  air_temperature_k = air_temperature,
                  air_pressure_pa = air_pressure,
                  air_density_kgm3 = air_density,
                  air_heat_capacity_jkkg = air_heat_capacity,
                  air_molar_volume_m3mol = air_molar_volume,
                  ET_mmhr = ET,
                  water_vapor_density_kgm3 = water_vapor_density,
                  e_pa = e,
                  es_pa = es,
                  specific_humidity_kgkg = specific_humidity,
                  VPD_pa = VPD,
                  Tdew_k = Tdew,
                  wind_speed_ms = wind_speed,
                  max_wind_speed_ms = max_wind_speed,
                  u_star_ms = `u.`,
                  TKE_m2s2 = TKE,
                  L_m = L,
                  MO_stability = `X.z.d..L`,
                  scale_T_k = `T.`,
                  x_peak_m = x_peak,
                  x_offset_m = x_offset,
                  x_10_m = `x_10.`,
                  x_30_m = `x_30.`,
                  x_50_m = `x_50.`,
                  x_70_m = `x_70.`,
                  x_90_m = `x_90.`,
                  un_Tau_kgms2 = un_Tau,
                  un_H_wm2 = un_H,
                  un_LE_wm2 = un_LE,
                  un_co2_flux_umolm2s = un_co2_flux,
                  un_h2o_flux_umolm2s = un_h2o_flux,
                  un_ch4_flux_umolm2s = un_ch4_flux,
                  u_var_ms = u_var,
                  v_var_ms = v_var,
                  w_var_ms = w_var)
  
  # Make a datetime column
  #current.ec$datetime <- as.POSIXct(paste(current.ec$date , paste(current.ec$time), sep=" "))
  
  # Fix DateTime issues. From 2020-04-04 to 2020-09-02 17:30 the system was in Est/GMT +5. 
  # System is in US/Eastern with daylight savings observed from 2020-09-02 12:00 to current.
  
  # We want to convert the time in Est/GMT +5 to GMT+4  so we need to add an hour. 
  #Change DateTime when it was changed from EDT to EST
  # Set everything to Etc/GMT+4
  # Make a datetime column
  
  current.ec$datetime <- ymd_hms(paste0(as.character(current.ec$date), " " , as.character(current.ec$time)), tz="Etc/GMT+4")
  
  # order the datetime column
  # reorder 
  current.ec <-  current.ec[order(current.ec$datetime),]
  
  if("2020-09-02 17:30:00" %in% current.ec$datetime){
    # shows time point when met station was switched from GMT -4(EST) to GMT -5(EDT) then -2 to get the row number right
    flux_timechange=max(which(current.ec$datetime=="2020-09-02 17:30:00")-2) 
    #Met$DateTime<-as.POSIXct(strptime(Met$DateTime, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5") #get dates aligned
    current.ec$datetime[c(0:flux_timechange+1)]<-with_tz(force_tz(current.ec$datetime[c(0:flux_timechange+1)],"Etc/GMT+5"), "Etc/GMT+4") #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set
  }
  # else if (min(current.ec$datetime, na.rm = TRUE)<"2020-09-02 17:30:00"){
  #   #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set
  #   current.ec$datetime<-with_tz(force_tz(current.ec$datetime,"Etc/GMT+5"), "Etc/GMT+4")
  # }else if(min(current.ec$datetime, na.rm = TRUE)>"2020-09-02 17:30:00"){
  #   # Do nothing because already in EST
  # }
  
  # Set timezone as America/New_York because 
  current.ec$datetime <- force_tz(current.ec$datetime, tzone = "America/New_York")
  
  # Because we changed times in the datetime column, we need to make sure we get the new time
  # convert to character 
  current.ec$datetime2 <-as.character(format(current.ec$datetime))
  # split into 2 columns   
  current.ec <- current.ec %>% separate(datetime2, c('date', 'time'), sep = " ")  
  
  
  
  # Filter for just the unprocessed files
  ### identify the date subsetting for the data
  if (!is.null(start_date)){
    current.ec <- current.ec %>% 
      filter(datetime >= start_date)
  }
  
  if(!is.null(end_date)){
    current.ec <- current.ec %>% 
      filter(datetime <= end_date)
  }
  
  
  ## Add flag for missing data: 3 = missing data, 4= instrument malfunction 
  # For: qc_tau, qc_H, qc_LE, qc_co2_flux, qc_h2o_flux, qc_ch4_flux
  ec_all <- current.ec %>% 
    mutate(qc_Tau = ifelse(is.na(Tau_kgms2), 3, qc_Tau),
           qc_H = ifelse(is.na(H_wm2), 3, qc_H),
           qc_LE = ifelse(is.na(LE_wm2), 3, qc_LE),
           qc_co2_flux = ifelse(is.na(co2_flux_umolm2s), 3, qc_co2_flux),
           qc_h2o_flux = ifelse(is.na(h2o_flux_umolm2s), 3, qc_h2o_flux),
           qc_ch4_flux = ifelse(is.na(ch4_flux_umolm2s), 3, qc_ch4_flux),
           qc_co2_flux = ifelse(flowrate_mean<10 & !is.na(flowrate_mean) | flowrate_mean>20 & !is.na(flowrate_mean), 4, qc_co2_flux), # take out fluxes when the blower motor malfunction
           co2_flux_umolm2s = ifelse(flowrate_mean<10 & !is.na(flowrate_mean) | flowrate_mean>20 & !is.na(flowrate_mean),NA, co2_flux_umolm2s),  # take out fluxes when the blower motor malfunction
           qc_h2o_flux = ifelse(flowrate_mean<10 & !is.na(flowrate_mean) | flowrate_mean>20 & !is.na(flowrate_mean), 4, qc_h2o_flux),  # take out fluxes when the blower motor malfunction
           h2o_flux_umolm2s = ifelse(flowrate_mean<10 & !is.na(flowrate_mean) | flowrate_mean>20 & !is.na(flowrate_mean),NA, h2o_flux_umolm2s),  # take out fluxes when the blower motor malfunction
           qc_ch4_flux = ifelse(ch4_flux_umolm2s> 1 & !is.na(ch4_flux_umolm2s) | ch4_flux_umolm2s< -0.25 & !is.na(ch4_flux_umolm2s), 4, qc_ch4_flux),
           ch4_flux_umolm2s = ifelse(ch4_flux_umolm2s> 1 & !is.na(ch4_flux_umolm2s)| ch4_flux_umolm2s< -0.25 & !is.na(ch4_flux_umolm2s), NA, ch4_flux_umolm2s), # take out very high and low fluxes
           qc_co2_flux = ifelse(co2_flux_umolm2s > 300 & !is.na(co2_flux_umolm2s) | co2_flux_umolm2s < -300 & !is.na(co2_flux_umolm2s), 4, qc_co2_flux),
           co2_flux_umolm2s = ifelse(co2_flux_umolm2s > 300 & !is.na(co2_flux_umolm2s) | co2_flux_umolm2s < -300 & !is.na(co2_flux_umolm2s), NA, co2_flux_umolm2s), # take out very high and low fluxes
           qc_h2o_flux = ifelse(h2o_flux_umolm2s > 40 & !is.na(h2o_flux_umolm2s) | h2o_flux_umolm2s < -40 & !is.na(h2o_flux_umolm2s), 4, qc_h2o_flux),
           h2o_flux_umolm2s = ifelse(h2o_flux_umolm2s > 40 & !is.na(h2o_flux_umolm2s) | h2o_flux_umolm2s < -40 & !is.na(h2o_flux_umolm2s), NA, h2o_flux_umolm2s))%>% # take out very high and low fluxes
    distinct()%>% # take out duplicates
    select(-Year, -flowrate_mean, -datetime,)%>% # take out the columns not in EDI
    select(date, time, everything())
  
  
  
  # Double check that -9999 got changed to NA
  # Clean up and make it useable for plotting
  ec_all[ec_all ==-9999] <- NA # Remove -9999 and replace with NAs
  
  # order the observations
  ec_all<-ec_all[order(ec_all$date),]
  
  
  # ## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
  #  package_ID <- 'edi.1061.2'
  # eml <- read_metadata(package_ID)
  #  date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
  #  last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)
  
  # ec_all <- ec_all |> filter(date> last_edi_date)
  
  # convert datetimes to characters so that they are properly formatted in the output file
  ec_all$date <- as.character(format(ec_all$date))
  ec_all$time <- as.character(format(ec_all$time))
  
  # Output data
  #write_csv(ec_all, paste0(mydir,output_file), row.names = FALSE)
  # save data if there is an output)file path. If not then the file is returned. 
  if (is.null(output_file)){
    return(ec_all)
  }else{
    write_csv(ec_all, paste0(mydir,output_file))
  }
  
}

# ## Function Example
# eddypro_cleaning_function(
#   directory = "./Data/DataNotYetUploadedToEDI/EddyFlux_Processing/",
#   gdrive = F, # Are the files on Google Drive. True or False
#   gshared_drive = as_id("0ACybYKbCwLRPUk9PVA"),
#   current_year = 2023,
#   output_file = "/EddyPro_Cleaned_L1.csv",
#   start_date = as.Date("2022-12-31") + lubridate::days(1),
#   end_date = sys.Date() + lubridate::days(1))
# 
# 
# ## Call healthcheck
# RCurl::url.exists("https://hc-ping.com/f0ba1278-7b06-4b3b-b8aa-5486e778abc3", timeout = 5)

