### 2019 CTD Processing for cnv files ###
### First written by RPM ###
### April, 2018: UPDATED 06-June-2019: By RPM ###

### Script cannot be sourced because 
### scan numbers need to be changed 
### to collect only the downcast

### Load OCE Libraries ###
pacman::p_load(oce, ocedata, tidyverse, lubridate)

### Upload the raw CTD file as a .cnv format ###
ctdRaw <- read.ctd.sbe("081919_fcr10.cnv",            
                       columns = list(), 
                       monitor = T,
                       debug = getOption("oceDebug"))

### Find the range of the cast that only includes the downcast ###
par(mfrow=c(1,1))

### Plot the Scan number (X) against CTD pressure (Y) to see when the downcast started
plotScan(ctdTrim(ctdRaw, "range",
                 parameters=list(item="scan", from=0, to=100000))) # if it was a long cast (SHR), go higher than 10000

### Trim the dataframe to include ONLY the downcast (Numbers "from = XXX," will need to be adjusted)
ctdTrimmed <- ctdTrim(ctdRaw, "range",
                      parameters=list(item="scan", from=250, to=100000))


### Verify that we only include the downcast
plotScan(ctdTrimmed)

### extract only the raw data from the .cnv file
data <- data.frame(ctdTrimmed@data)

### Add a date column to the dataframe. This will actually give the EXACT TIME of the cast
data$Date <- as_datetime(ctdTrimmed@metadata$startTime)

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
  filter(Depth_m <= 9.3) %>% #### CHANGE ME FOR BVR to 11
  filter(`Descent Rate (m/s)` >= 0)
data_wtr$DO_mgL[data_wtr$DO_mgL <= 0] <- 0.01
data_wtr$DO_pSat[data_wtr$DO_pSat <= 0] <- 0.01




### PLOT UP THE CTD DATA AND SAVE IT AS A PDF FILE ###
pdf("081919_fcr10.pdf", width=12, height=12)

par(mfrow=c(4,4))
cl <- rainbow(16)
for(i in seq(1,length(data_wtr),1)) plot(data_wtr[,i],-data_wtr$Depth_m, xlab=names(data_wtr[i]),type="l", col = cl[i])

dev.off()

### write the dataframe of the downcast as a .csv 
write_csv(data_wtr, "081919_fcr10.csv")

### Pull the PAR for MEL from the original cleaned data that but includes the few depths above the surface ###

par <- data %>% select(Date,Depth_m,PAR,Chla_ugL)

### plot and save PAR/CHLA for clarity and QCQA ###

pdf("081919_fcr10_PAR.pdf", width=10, height=10)

par(mar=c(4,4,4,4))
plot(par$PAR, -par$Depth_m, type = "l", ylab = "Depth (m)", xlab = "PAR")
par(new = T)
plot(par$Chla_ugL, -par$Depth_m, type = "l", ylab = "Depth (m)",col = "green", lwd = 2, xlab = "", xaxt = "n")
axis(3)
mtext(side = 3, "CHLA_ugL", line = 2)

dev.off()


### Write it as a csv to send to MEL and save in the DROPBOX ###
write_csv(par, "081919_fcr10_PAR.csv")

### New feature, This saves the Metadata text from the .cnv as a raw .txt file 
### so we can refer to prior dates to see if there are any discrepancies or to use for EDI

### Change the MetaData to clarify who, when, and where cast was taken ###
# specify the day sampled
ctdTrimmed[["metadata"]]$cruise <- "Sampling on 19-Aug-19"            

# Who took the cast?
ctdTrimmed[["metadata"]]$scientist <- "RPM"                         

# What group was the cast affiliated with?
ctdTrimmed[["metadata"]]$institute <- "Carey Lab, Virginia Tech"

# What site was the cast taken from?
ctdTrimmed[["metadata"]]$station <- "fcr10"  

### Specify names ###
CTD_SN <- ctdTrimmed@metadata$serialNumberTemperature
CTD_type <- ctdTrimmed@metadata$type
Sampling_event <- ctdTrimmed@metadata$cruise
User <- ctdTrimmed@metadata$scientist
Insitute <- ctdTrimmed@metadata$institute
Cast_location <- ctdTrimmed@metadata$station

### Bind the metadata together ###
metadata <- as.data.frame(rbind(CTD_SN,CTD_type,Insitute,Sampling_event,Cast_location,User))
names(metadata) <- c("")

### write the simple metadata table ###
write.table(metadata, file = "081919_fcr10_metadata.txt", sep = "\t")

