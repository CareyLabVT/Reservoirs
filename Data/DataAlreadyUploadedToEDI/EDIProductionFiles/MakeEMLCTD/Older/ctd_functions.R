trim_ctd <- function(SCAN_NUMBER, DATE_TEXT, AUTO_NAME, SITE, REP, NAME_OVERRIDE){
  ### Format date and names for the rest of the automated script
  DATE<- dmy(DATE_TEXT)
  DATE_NUM <- format(DATE, "%m%d%y")
  
  if(AUTO_NAME == TRUE){
    NAME <- paste(DATE_NUM,"_",SITE, REP, sep = "")
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
  ctdTrimmed@metadata$startTime
  
  ### Extract and order columns we want
  data <- data %>% select(Date,
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
                   "Density_mg_m3",
                   "Flag")
  
  ### REMOVE THE BOTTOM negative values ###
  data <- na.omit(data)
  
  ### REMOVE the top negative values and only include the downcast from the water
  data_wtr <- data %>% filter(Depth_m > 0) %>% 
    filter(Depth_m <= MAX_DEPTH) %>% 
    filter(`Descent Rate (m/s)` >= 0)
  data_wtr$DO_mgL[data_wtr$DO_mgL <= 0] <- 0.01
  data_wtr$DO_pSat[data_wtr$DO_pSat <= 0] <- 0.01
  
  
  
  
  ### PLOT THE CTD DATA AND SAVE IT AS A PDF FILE ###
  name_pdf <- paste(NAME,"pdf", sep = ".")
  pdf(name_pdf, width=12, height=12)
  
  par(mfrow=c(4,4))
  cl <- rainbow(16)
  for(i in seq(1,length(data_wtr),1)) plot(data_wtr[,i],-data_wtr$Depth_m, xlab=names(data_wtr[i]),type="l", col = cl[i])
  
  dev.off()
  
  ### write the dataframe of the downcast as a .csv 
  if(SITE == "fcr50"){CTD_FOLDER <- "CTD_CASTS_CSV/"
  } else {CTD_FOLDER <- ""}
  if(AUTO_FOLDER == FALSE){
    CTD_FOLDER <- paste(CSV_FOLDER_OVERRIDE,"/", sep = "")
  }
  if(substring(NAME,1,1) == ".") {
    name_csv <- substring(paste(CTD_FOLDER,NAME,".csv", sep = ""),2)
  } else {
    name_csv <- paste(CTD_FOLDER,NAME,".csv", sep = "")
  }
  write_csv(data_wtr, name_csv)
  
  ### Save csv in the main folder as well
  name_csv <- paste(NAME,".csv", sep = "")
  write_csv(data_wtr, name_csv)
  
  ### Pull the PAR for MEL from the original cleaned data that but includes the few depths above the surface ###
  
  par <- data %>% select(Date,Depth_m,PAR,Chla_ugL)
  
  ### plot and save PAR/CHLA for clarity and QCQA ###
  name_par_pdf <- paste(NAME,"PAR.pdf", sep = "_")
  pdf(name_par_pdf, width=10, height=10)
  
  par(mar=c(4,4,4,4))
  plot(par$PAR, -par$Depth_m, type = "l", ylab = "Depth (m)", xlab = "PAR")
  par(new = T)
  plot(par$Chla_ugL, -par$Depth_m, type = "l", ylab = "Depth (m)",col = "green", lwd = 2, xlab = "", xaxt = "n")
  axis(3)
  mtext(side = 3, "CHLA_ugL", line = 2)
  
  dev.off()
  
  
  ### Write it as a csv to send to MEL and save in the DROPBOX ###
  name_par_csv <- paste(NAME,"PAR.csv", sep = "_")
  write_csv(par, name_par_csv)
  
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
  name_txt <- paste(NAME, "metadata.txt", sep = "_")
  write.table(metadata, file = name_txt, sep = "\t")
  
  print(paste("Success! Data have been processed for ",NAME, ". ", "Double check the files in dropbox to make sure everything looks right.", sep = ""))
}


