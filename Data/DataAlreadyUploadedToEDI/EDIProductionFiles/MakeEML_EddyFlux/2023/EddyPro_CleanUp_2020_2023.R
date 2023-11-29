### Script to create a QAQC plot and
## clean up files produced by LiCor Eddy Pro

### 8 October 2021, A. Hounshell

### Updated with new data on 21 Jan 2022

###A. Breef-Pilz updated on 26 Oct. 2022 to included a for loop to
###read in the EddyPro output, create QAQC plots and bind all the files together into on large data frame

#####################################################

# Clear workspace
rm(list = ls())

# Download/load libraries
pacman::p_load(lubridate,tidyverse,hms,gridExtra,openair)

# Set working directory
#wd <- getwd()
#setwd(wd)

# Stitch together out puts from EddyPro


#All files including the one from 08 Nov 21(last reading actually happened on 16:10EST ) is EDT 
#so need to change to EST -ABP needs to double check this is added files

mydir = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_EddyFlux/Data/"
myfiles = list.files(path=paste0(mydir,"2023_processed_files/"), pattern="", full.names=TRUE)

# The current csv with all of the data from the EC system read in later on 
currentfile=list.files(path=mydir, pattern=".csv",full.names = T)

#taking out the the Temp Test files
#myfilesBVR <- myfiles[ !grepl("CR6_BVR_TempTest*", myfiles) ]#exclude the Temp test data

# Create the pdf output of QAQC plots and then combine data

#create an out.file for the combined data
out.file<-""
#for loop to create PDF and then combine the files
for(k in 1:length(myfiles)){
  header2<-read.csv(myfiles[k], skip=1, as.is=T) #get header minus wonky Campbell rows
  data2<-read.csv(myfiles[k], skip=3, header=F) #get data minus wonky Campbell rows
  names(data2)<-names(header2) #combine the names to deal with Campbell logger formatting
  
  # Clean up and make it useable for plotting
  data2[data2 ==-9999] <- NA # Remove -9999 and replace with NAs
  
  # Make a datetime column
  data2$datetime <- as.POSIXct(paste(data2$date , paste(data2$time), sep=" "))
  
  # Find the end row of the file. This is so I can get the date of the last reading
  endrow=tail(data2,n=1)
  
  # Now create the PDF that is saved in the Google Drive
  #folder <- "G:/Shared drives/VT-UBC FCR Team/QAQC Plots/"
  #pdf(paste0(folder, "EddyFlux_QAQC_Figures_",data2[1,'date']," - ", endrow$date, ".pdf"), width=8.5, height=11) #call PDF file
  
  endrow=tail(data2,n=1)
  
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
  rename(date = datetime, ws = wind_speed, wd = wind_dir)
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
  
  # Now that you have created the QAQC plots only pick the variables you want for EDI and for 
  # Processing
  
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
         h2o_scf,un_ch4_flux,ch4_scf,u_var,v_var,w_var,rssi_77_mean)
  out.file=rbind(data3, out.file)
}

out.file2 <- subset(out.file, out.file$date != "")

# change columns to numeric instead of character
out.file2[, c(3:79)] <- sapply(out.file2[, c(3:79)], as.numeric)

# Now read in the current CSV
bigcsv= read_csv(currentfile)

# order the observations
bigcsv=bigcsv[order(bigcsv$date),]

current.ec <- bigcsv
  
# Now that you have the columns you want let's put them into a usable format

current.ec<-out.file2%>%
  mutate(date=ymd(date))%>% #converts date to correct format
  mutate(time=strptime(time, format = "%H:%M"))%>% #converts time to postix
  mutate(time=as_hms(time))%>% #takes out the date and just leaves the time
  rename(Tau_kgms2 = Tau,
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
         w_var_ms = w_var)%>%
        rbind(.,bigcsv)

## Add flag for missing data: 3 = missing data
# For: qc_tau, qc_H, qc_LE, qc_co2_flux, qc_h2o_flux, qc_ch4_flux
ec_all <- current.ec %>% 
  mutate(qc_Tau = ifelse(is.na(Tau_kgms2), 3, qc_Tau),
         qc_H = ifelse(is.na(H_wm2), 3, qc_H),
         qc_LE = ifelse(is.na(LE_wm2), 3, qc_LE),
         qc_co2_flux = ifelse(is.na(co2_flux_umolm2s), 3, qc_co2_flux),
         qc_h2o_flux = ifelse(is.na(h2o_flux_umolm2s), 3, qc_h2o_flux),
         qc_ch4_flux = ifelse(is.na(ch4_flux_umolm2s), 3, qc_ch4_flux))

# Take out duplicates. 

ec_all<- ec_all%>%distinct()

# Double check that -9999 got changed to NA
# Clean up and make it useable for plotting
ec_all[ec_all ==-9999] <- NA # Remove -9999 and replace with NAs

# Subset to date for publishing

ec_all<-ec_all%>%filter(date<as.Date("2024-01-01"))

# Output data
write_csv(ec_all, paste0(mydir,"/EddyPro_Cleaned_", Sys.Date(),".csv"))

#write_csv(ec, "./EDI/20220130_EddyPro_Cleaned.csv")

#######Plot Year or Multi Year long fluxes###########

# Use the csv of all the data that was read in earlier
ec_all$datetime <- as.POSIXct(paste(ec_all$date , paste(ec_all$time), sep=" "))
 
bv=ec_all%>%
  filter(co2_flux_umolm2s>-100)%>%
ggplot(., aes(datetime,co2_flux_umolm2s))+
  geom_point()+
  labs(title="Co2 Fluxes from 2020-2022")


bb=ec_all%>%
  filter(co2_flux_umolm2s>-100)%>%
  filter(datetime>"2021-12-31 23:30")%>%
  ggplot(., aes(datetime,co2_flux_umolm2s))+
  geom_point()+
  labs(title="Co2 Fluxes from 2022")

bc=ec_all%>%
  filter(ch4_flux_umolm2s>-100)%>%
  filter(datetime>"2021-12-31 23:30")%>%
  ggplot(., aes(datetime,ch4_flux_umolm2s))+
  geom_point()+
  labs(title="CH4 Fluxes from 2022")
bv


