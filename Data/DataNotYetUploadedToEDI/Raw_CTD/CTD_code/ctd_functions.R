trim_ctd <- function(SCAN_NUMBER, DATE_TEXT, AUTO_NAME, SITE, REP, NAME_OVERRIDE){
  ### Format date and names for the rest of the automated script
  DATE<- dmy(DATE_TEXT)
  DATE_NUM <- format(DATE, "%m%d%y")
  
  if(AUTO_NAME == TRUE){
    NAME <- paste("../RawDownloads/",DATE_NUM,"_",SITE, REP, sep = "")
  } else {NAME <- NAME_OVERRIDE}
  
  ### Upload the raw CTD file as a .cnv format ###
  name_cnv <- paste(NAME,"cnv", sep = ".")
  ctdRaw <- read.ctd.sbe(name_cnv,            
                         columns = list(), 
                         monitor = T,
                         debug = getOption("oceDebug"))
  
  ### Find the range of the cast that only includes the downcast ###
  par(mfrow=c(1,1))
  
  ### Trim the dataframe to include ONLY the downcast (Numbers "from = XXX," will need to be adjusted)
  ctdTrimmed <- ctdTrim(ctdRaw, "range",
                        parameters=list(item="scan", from=SCAN_NUMBER))
  
  ### Verify that we only include the downcast
  plotScan(ctdTrimmed)
  
  files = list.files()
  if("saved_scan_numbers.csv" %in% files){
    scans = read.csv("saved_scan_numbers.csv")
    if(name_cnv %in% scans$file){
      scans$scan[scans$file == name_cnv] <- SCAN_NUMBER
    } else {
      new_scan = data.frame(file = name_cnv, scan = SCAN_NUMBER)
      scans = full_join(scans, new_scan)
    }
    write.csv(scans, "saved_scan_numbers.csv", row.names = F)
  }else{
    new_scan = data.frame(file = name_cnv, scan = SCAN_NUMBER)
    write.csv(new_scan, "saved_scan_numbers.csv", row.names = F)
  }
  
  return(ctdTrimmed)
}






epic_ctd_function <- function(ctdTrimmed, DATE_TEXT, SITE, SAMPLER, 
                              REP, AUTO_NAME, NAME_OVERRIDE, AUTO_FOLDER, 
                              CSV_FOLDER_OVERRIDE, MAX_DEPTH){
  ### Format date and names for the rest of the automated script
  DATE<- dmy(DATE_TEXT)
  DATE_NUM <- format(DATE, "%m%d%y")
  
  if(AUTO_NAME == TRUE){
    NAME <- paste(DATE_NUM,"_",SITE, REP, sep = "")
  } else {NAME <- NAME_OVERRIDE}
  
  ### extract only the raw data from the .cnv file
  data <- data.frame(ctdTrimmed@data)
  
  ### Add a date column to the dataframe. This will actually give the EXACT TIME of the cast
  data$Date <- as_datetime(ctdTrimmed@metadata$startTime)
  
  ### Extract and order columns we want
  data <- data %>% 
    dplyr::select(Date,
          depth, 
          temperature, 
          fluorescence, 
          turbidity, 
          conductivity, 
          conductivity2,
          oxygen,
          oxygen2,
          pH,
          orp,
          par.sat.log,
          salinity, 
          descentRate,
          #density,
          #pressurePSI,
          flag)
  
  ### rename the columns so they are easy to decifer and reeproducible for future appending                
  names(data) <- c("Date", 
                   "Depth_m",
                   "Temp_C", 
                   "Chla_ugL", 
                   "Turb_NTU", 
                   "Cond_uScm", 
                   "Spec_Cond_uScm", 
                   "DO_mgL", 
                   "DO_pSat", 
                   "pH", 
                   "ORP_mV", 
                   "PAR", 
                   "Salinity",
                   "Descent Rate (m/s)",
                   #"Pressure (PSI)",
                   #"Density_mg_m3",
                   "Flag")
  
  ### REMOVE THE BOTTOM NA values ###
  data <- data[!is.na(data$DO_mgL),]
  
  ### create a file for both upcast and downcast
  data_wtr_both <- data  %>% 
    filter(Depth_m <= MAX_DEPTH) #%>% 
    #filter(`Descent Rate (m/s)` >= 0) #NOTE: I got rid of this 21 Jun 20
  
  # create a file for just downcast
  data_wtr <- data %>% #filter(Depth_m > 0) %>% <- got rid of this 23 Sep 20
    filter(Depth_m <= MAX_DEPTH) 
  data_wtr <- data_wtr[1:which.max(data_wtr$Depth_m),]
  #data_wtr$DO_mgL[data_wtr$DO_mgL < 0] <- 0 Stopped doing this at the start of 2020
  #data_wtr$DO_pSat[data_wtr$DO_pSat < 0] <- 0
  
  
  ### PLOT THE CTD DATA AND SAVE IT AS A PDF FILE ###
  name_pdf <- paste("../PDF_outputs/",NAME,".pdf", sep = "")
  pdf(name_pdf, width=12, height=12)
  
  par(mfrow=c(4,4))
  cl <- rainbow(16)
  for(i in seq(1,length(data_wtr),1)) plot(data_wtr[,i],-data_wtr$Depth_m, xlab=names(data_wtr[i]),type="l", col = cl[i], ylab= "Depth (m)")
  
  dev.off()
  
  ### Plot the downcast and upcast for troubleshooting
  name_pdf <- paste("../PDF_outputs/",NAME,"_WithUpcast.pdf", sep = "")
  pdf(name_pdf, width=12, height=12)
  
  par(mfrow=c(4,4))
  cl <- rainbow(16)
  for(i in seq(1,length(data_wtr_both),1)) plot(data_wtr_both[,i],-data_wtr_both$Depth_m, xlab=names(data_wtr_both[i]),type="l", col = cl[i], ylab= "Depth (m)")
  
  dev.off()
  
  ### write the dataframe of the downcast as a .csv 
  CTD_FOLDER <- "../csv_outputs/"
  if(AUTO_FOLDER == FALSE){
    CTD_FOLDER <- paste(CSV_FOLDER_OVERRIDE,"/", sep = "")
  }
  if(substring(NAME,1,1) == ".") {
    name_csv <- substring(paste(CTD_FOLDER,NAME,".csv", sep = ""),2)
  } else {
    name_csv <- paste(CTD_FOLDER,NAME,".csv", sep = "")
  }
  
  if(min(data_wtr$Depth_m)>0){ #if the chosen scan number is underwater (no above the surface PAR) we need to load the whole cast and get just a PAR value
    ### Upload the raw CTD file as a .cnv format ###
    if(AUTO_NAME == TRUE){
      FILE_NAME <- paste("../RawDownloads/",DATE_NUM,"_",SITE, REP, sep = "")
    } else {FILE_NAME <- NAME_OVERRIDE}
    name_cnv <- paste(FILE_NAME,"cnv", sep = ".")
    ctdRaw <- read.ctd.sbe(name_cnv,            
                           columns = list(), 
                           monitor = T,
                           debug = getOption("oceDebug"))
    ctdRaw <- ctdTrim(ctdRaw, "range",
                          parameters=list(item="scan", to=SCAN_NUMBER)) # we only want values before the selected scan number (air)
    
    ### extract only the raw data from the .cnv file
    data_air <- data.frame(ctdRaw@data) %>%
      mutate(Date = as_datetime(ctdRaw@metadata$startTime))%>%
      dplyr::select(Date,
                    depth, 
                    temperature, 
                    fluorescence, 
                    turbidity, 
                    conductivity, 
                    conductivity2,
                    oxygen,
                    oxygen2,
                    pH,
                    orp,
                    par.sat.log,
                    salinity, 
                    descentRate,
                    density,
                    #pressurePSI,
                    flag)
    
    ### rename the columns so they are easy to decifer and reeproducible for future appending                
    names(data_air) <- c("Date", 
                     "Depth_m",
                     "Temp_C", 
                     "Chla_ugL", 
                     "Turb_NTU", 
                     "Cond_uScm", 
                     "Spec_Cond_uScm", 
                     "DO_mgL", 
                     "DO_pSat", 
                     "pH", 
                     "ORP_mV", 
                     "PAR", 
                     "Salinity",
                     "Descent Rate (m/s)",
                     #"Pressure (PSI)",
                     "Flag")
    
    ### REMOVE THE BOTTOM NA values ###
    data_air <- data_air%>%
      filter(!is.na(DO_mgL))
    
    data_air$scan_num <- as.numeric(row.names(data_air))
    
    # create a file for just downcast
    data_air <- data_air[max(data_air$scan_num[data_air$Depth_m < (-0.1)]):nrow(data_air),]
    data_air <- data_air[data_air$scan_num < min(data_air$scan_num[data_air$Depth_m > 0]),]
    data_air = data_air%>%
      select(-scan_num)%>%
      filter(Depth_m<0)
    
    data_air[!names(data_air) %in% c("Date", "Depth_m", "Temp_C","PAR", "Descent Rate (m/s)", "Flag")] <- NA
    data_wtr = data_air%>%
      full_join(data_wtr)
  }
  
  write_csv(data_wtr, name_csv)
  
  ### Save csv in the main folder as well (OLD: when everything was in one folder)
  #name_csv <- paste(NAME,".csv", sep = "")
  #write_csv(data_wtr, name_csv)
  
  ### Plot clarity for QAQC
  
  par <- data_wtr %>% dplyr::select(Date,Depth_m,PAR,Chla_ugL)
  
  ### plot and save PAR/CHLA for clarity and QCQA ###
  name_par_pdf <- paste("../PAR_files/",NAME,"_PAR.pdf", sep = "")
  pdf(name_par_pdf, width=10, height=10)
  
  par(mar=c(4,4,4,4))
  plot(par$PAR, -par$Depth_m, type = "l", ylab = "Depth (m)", xlab = "PAR")
  par(new = TRUE)
  plot(par$Chla_ugL, -par$Depth_m, type = "l", ylab = "Depth (m)",col = "green", lwd = 2, xlab = "", xaxt = "n")
  axis(3)
  mtext(side = 3, "CHLA_ugL", line = 2)
  
  dev.off()
  
  
  ### Write it as a csv to send to MEL and save in the DROPBOX ### Stopped doing this 23 Sep 20 because MEL uses file from EDI
  #name_par_csv <- paste("../PAR_files/",NAME,"_PAR.csv", sep = "")
  #write_csv(par, name_par_csv)
  
  ### New feature, This saves the Metadata text from the .cnv as a raw .txt file 
  ### so we can refer to prior dates to see if there are any discrepancies or to use for EDI
  
  ### Change the MetaData to clarify who, when, and where cast was taken ###
  # specify the day sampled
  ctdTrimmed[["metadata"]]$cruise <- paste("Sampling on", DATE_TEXT)            
  
  # Who took the cast?
  ctdTrimmed[["metadata"]]$scientist <- SAMPLER                        
  
  # What group was the cast affiliated with?
  ctdTrimmed[["metadata"]]$institute <- "Carey Lab, Virginia Tech"
  
  # What site was the cast taken from?
  ctdTrimmed[["metadata"]]$station <- SITE 
  
  ### Specify names ###
  CTD_SN <- ctdTrimmed@metadata$serialNumberTemperature
  CTD_type <- ctdTrimmed@metadata$type
  Sampling_event <- ctdTrimmed@metadata$cruise
  User <- ctdTrimmed@metadata$scientist
  Institute <- ctdTrimmed@metadata$institute
  Cast_location <- ctdTrimmed@metadata$station
  
  ### Bind the metadata together ###
  metadata <- as.data.frame(rbind(CTD_SN,CTD_type,Institute,Sampling_event,Cast_location,User))
  names(metadata) <- c("")
  
  ### write the simple metadata table ###
  name_txt <- paste("../metadata_files/",NAME, "_metadata.txt", sep = "")
  write.table(metadata, file = name_txt, sep = "\t")
  
  print(paste("Success! Data have been processed for ",NAME, ". ", "Double check the files on github to make sure everything looks right.", sep = ""))
}


