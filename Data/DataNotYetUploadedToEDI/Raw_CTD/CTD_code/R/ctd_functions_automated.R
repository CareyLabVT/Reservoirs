trim_ctd <- function(file, raw_downloads){
  
  ### Pull in the raw CTD file as a .cnv format ###
  NAME <- paste(raw_downloads, "/", file, sep = "")
  name_cnv <- paste(NAME,"cnv", sep = ".")
 
  # Wrap read.ctd.sbe in a try() function so if there is a warning it keeps running
  ctdRaw<-try(read.ctd.sbe(name_cnv,            
                         columns = list(), 
                         monitor = T,
                         debug = getOption("oceDebug")), silent = T)
  
  ### If the file fails then there is a issue with the conductivity reading and it needs an extra space
  ### Read in all the lines and then find the dashes and add a space but then remove it for e-0001
  if(is.null(nrow(ctdRaw))){
    lines <- readLines(name_cnv)
    
  ### For loop to find the dashes and add a space to sort out column issues
  for(i in 1:length(lines)){
      if(grepl('^[*]|^[#]', lines[i])==F){
      lines[i] <-  gsub("e\\s","e",gsub("-"," -",lines[i]))
      }
      else{}
  }
    
    ### Let's try reading it in again
    ctdRaw <- read.ctd.sbe(textConnection(lines),            
                           columns = list(), 
                           monitor = T,
                           debug = getOption("oceDebug"))
  }
  
  ### Find the range of the cast that only includes the downcast ###
  par(mfrow=c(1,1))
  
  ctdTrimmed <- ctdTrim(ctdRaw, trim_function,
                        parameters = list(depth = -0.1))
  
  ### Verify that we only include the downcast
  plotScan(ctdTrimmed)
  
  return(ctdTrimmed)
}

### Trim the dataframe to include ONLY the downcast (SCAN_NUMBERS "from = XXX," will need to be adjusted)
trim_function <- function(data, parameters){
  max_depth_index <- data$scan[which.max(data$depth)]
  
  #Identify the scan number where you want to start the cast
  scan_index <- data$scan[max(which(data$depth < parameters[1] & data$scan < max_depth_index))]
  if(!is.finite(scan_index)){scan_index <- 1}
  #Filter to after that scan number
  data$scan >= scan_index
}


epic_ctd_function <- function(ctdTrimmed, file, SN, AUTO_FOLDER, 
                              CSV_FOLDER_OVERRIDE, MAX_DEPTH, CTD_FOLDER){
  ### Format date and names for the rest of the automated script
  NAME <- file
  
  ### extract only the raw data from the .cnv file
  data <- data.frame(ctdTrimmed@data)
  
  ### Add a date column to the dataframe. This will actually give the EXACT TIME of the cast
  data$Date <- as_datetime(ctdTrimmed@metadata$startTime)
  
  # If statement to decide which variables to select based on which CTD is being processed
  
  if(SN==7809){
  
  ### Extract and order columns we want
  data <- data %>% 
    dplyr::select(Date,
          depth, 
          temperature, 
          fluorescence, 
          turbidity, 
          conductivity2, 
          conductivity,
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
  names(data) <- c("DateTime", 
                   "Depth_m",
                   "Temp_C", 
                   "Chla_ugL", 
                   "Turbidity_NTU", 
                   "Cond_uScm", 
                   "SpCond_uScm", 
                   "DO_mgL", 
                   "DOsat_percent", 
                   "pH", 
                   "ORP_mV", 
                   "PAR", 
                   "Salinity",
                   "DescRate_ms ",
                   #"Pressure (PSI)",
                   #"Density_mg_m3",
                   "Flag")
  }else if(SN==8188){
    
    ### Extract and order columns we want
    data <- data %>% 
      dplyr::select(Date,
                    depth, 
                    temperature, 
                    fluorescence, 
                    fluorescence2,
                    fluorescence3,
                    fluorescence4,
                    turbidity, 
                    conductivity2, 
                    conductivity,
                    oxygen,
                    oxygen2,
                    #pH,
                    #orp,
                    par.sat.log,
                    salinity, 
                    descentRate,
                    #density,
                    #pressurePSI,
                    flag)
    
    ### rename the columns so they are easy to decipher and reproducible for future appending                
    names(data) <- c("DateTime", 
                     "Depth_m",
                     "Temp_C", 
                     "CDOM_ugL",
                     "Chla_ugL",
                     "Phycoerythrin_ugL",
                     "Phycocyanin_ugL",
                     "Turbidity_NTU", 
                     "Cond_uScm", 
                     "SpCond_uScm", 
                     "DO_mgL", 
                     "DOsat_percent", 
                     #"pH", 
                     #"ORP_mV", 
                     "PAR", 
                     "Salinity",
                     "DescRate_ms ",
                     #"Pressure (PSI)",
                     #"Density_mg_m3",
                     "Flag")
    
  }else{
    warning("CTD Serial Number did not match the current serial numbers we have. Could not select and rename columns. 
            Check that the serial number is used in the file name of the cnv")
  }
  
  
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
  name_pdf <- paste(CTD_FOLDER, "PDF_outputs/",NAME,".pdf", sep = "")
  pdf(name_pdf, width=12, height=12)
  
  par(mfrow=c(4,4))
  cl <- rainbow(16)
  for(i in seq(1,length(data_wtr),1)) plot(data_wtr[,i],-data_wtr$Depth_m, xlab=names(data_wtr[i]),type="l", col = cl[i], ylab= "Depth (m)")
  
  dev.off()
  
  ### Plot the downcast and upcast for troubleshooting
  name_pdf <- paste(CTD_FOLDER, "PDF_outputs/",NAME,"_WithUpcast.pdf", sep = "")
  pdf(name_pdf, width=12, height=12)
  
  par(mfrow=c(4,4))
  cl <- rainbow(16)
  for(i in seq(1,length(data_wtr_both),1)) plot(data_wtr_both[,i],-data_wtr_both$Depth_m, xlab=names(data_wtr_both[i]),type="l", col = cl[i], ylab= "Depth (m)")
  
  dev.off()
  
  ### write the dataframe of the downcast as a .csv 
  CSV_FOLDER <- paste0(CTD_FOLDER,"csv_outputs/")
  if(AUTO_FOLDER == FALSE){
    CSV_FOLDER <- paste(CSV_FOLDER_OVERRIDE,"/", sep = "")
  }
  if(substring(NAME,1,1) == ".") {
    name_csv <- substring(paste(CSV_FOLDER,NAME,".csv", sep = ""),2)
  } else {
    name_csv <- paste(CSV_FOLDER,NAME,".csv", sep = "")
  }
  
  write_csv(data_wtr, name_csv)
  
  print(paste("Success! Data have been processed for ",NAME, ". ", "Double check the files on github to make sure everything looks right.", sep = ""))
}


