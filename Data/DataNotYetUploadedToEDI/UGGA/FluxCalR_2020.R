## Script to *try* FluxCalR for calculating fluxes from UGGA data
## For 2020 data! Will need to updated for 2021 data : )
## A Hounshell, 25 Jan 2021

# Script following: https://github.com/junbinzhao/FluxCalR

# Load in 'remotes' package
pacman::p_load(remotes,tidyverse)

# Install FluxCalR
remotes::install_github("junbinzhao/FluxCalR",build_vignettes = TRUE)
library(FluxCalR)

# Load in data: will need to load in individual files - this can serve as a log for what files have been corrected
wd <- getwd()
setwd(wd)

# Load in flux data
# NOTE: Did not sure data from 2002-05-11 (aka: 2020-06-29 - datetime messed up!)
#flux_lgr_1 <- LoadLGR(file ="./gga_2002-05-11_f0001.txt",
#                    time_format = "mdy_HMS")
flux_lgr_2 <- LoadLGR(file ="./gga_2020-07-06_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_3 <- LoadLGR(file ="./gga_2020-07-13_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_4 <- LoadLGR(file ="./gga_2020-07-20_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_5 <- LoadLGR(file ="./gga_2020-07-27_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_6 <- LoadLGR(file ="./gga_2020-08-10_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_7 <- LoadLGR(file ="./gga_2020-08-24_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_8 <- LoadLGR(file ="./gga_2020-09-02_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_9 <- LoadLGR(file ="./gga_2020-09-11_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_10 <- LoadLGR(file ="./gga_2020-10-30_f0000.txt",
                      time_format = "mdy_HMS")

# Select times when the UGGA was on/off the water
# Select the time point BEFORE the peak for both reps
# Instructions: Use the cursor to select the timepoint before the peak; click once for the first peak and again for
# the second peak. When finished, click on 'Stop' in the upper left-hand corner and then click 'Stop locator'
# This generates a list of 'end' times for each peak
#time_cue_1 <- SelCue(flux_lgr_1,flux="CO2",cue="End",save=F)
time_cue_2 <- SelCue(flux_lgr_2,flux="CO2",cue="End",save=F)
time_cue_3 <- SelCue(flux_lgr_3,flux="CO2",cue="End",save=F)
time_cue_4 <- SelCue(flux_lgr_4,flux="CO2",cue="End",save=F)
time_cue_5 <- SelCue(flux_lgr_5,flux="CO2",cue="End",save=F)
time_cue_6 <- SelCue(flux_lgr_6,flux="CO2",cue="End",save=F)
time_cue_7 <- SelCue(flux_lgr_7,flux="CO2",cue="End",save=F)
time_cue_8 <- SelCue(flux_lgr_8,flux="CO2",cue="End",save=F)
time_cue_9 <- SelCue(flux_lgr_9,flux="CO2",cue="End",save=F)
time_cue_10 <- SelCue(flux_lgr_10,flux="CO2",cue="End",save=F)

# Then calculate fluxes
#Flux_output1 <- FluxCal(data = flux_lgr_1, # Dataframe loaded in
#                        win = 4, # Window length = 4 minutes
#                        vol = 0.020876028*1000, # Volume of trap in liters
#                        area = 0.1451465, # Area of trap in m^2
#                        df_cue = time_cue_1, # End times selected using SelCue
#                        cue_type = "End", # Designate that these times are for the end
#                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
#                        output = FALSE)

Flux_output2 <- FluxCal(data = flux_lgr_2, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_2, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        output = FALSE)

Flux_output3 <- FluxCal(data = flux_lgr_3, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_3, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        output = FALSE)

Flux_output4 <- FluxCal(data = flux_lgr_4, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_4, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        output = FALSE)

Flux_output5 <- FluxCal(data = flux_lgr_5, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_5, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        output = FALSE)

Flux_output6 <- FluxCal(data = flux_lgr_6, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_6, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        output = FALSE)

Flux_output7 <- FluxCal(data = flux_lgr_7, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_7, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        output = FALSE)

Flux_output8 <- FluxCal(data = flux_lgr_8, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_8, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        output = FALSE)

Flux_output9 <- FluxCal(data = flux_lgr_9, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_9, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        output = FALSE)

Flux_output10 <- FluxCal(data = flux_lgr_10, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_10, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        output = FALSE)

flux_output <- rbind(Flux_output2,Flux_output3,Flux_output4,Flux_output5,Flux_output6,Flux_output7,
                     Flux_output8,Flux_output9,Flux_output10)

# Get together for publication to EDI
flux_co2 <- flux_output %>% 
  filter(Gas == "CO2") %>% 
  rename(co2_slope = Slope, co2_R2 = R2, co2_flux_umolCm2s = Flux) %>% 
  select(-Gas)

flux_ch4 <- flux_output %>% 
  filter(Gas == "CH4") %>% 
  rename(ch4_slope = Slope, ch4_R2 = R2, ch4_flux_umolCm2s = Flux) %>% 
  select(-Gas)

flux_all <- left_join(flux_co2,flux_ch4,by=c("Num","Date","Start","End","Ta"))

flux_all <- flux_all %>% 
  rename(Rep = Num, Start_time = Start, End_time = End, Temp_C = Ta) %>% 
  mutate (Reservoir = "FCR", Site = 50, co2_flux_umolCm2s_flag = NA, ch4_flux_umolCm2s_flag = NA)

col_order <- c("Reservoir","Site","Date","Rep","Start_time","End_time","Temp_C","co2_slope","co2_R2",
               "co2_flux_umolCm2s","ch4_slope","ch4_R2","ch4_flux_umolCm2s","co2_flux_umolCm2s_flag",
               "ch4_flux_umolCm2s_flag")

flux_all_2 <- flux_all[,col_order]

# Plots to double check data!
flux_all_2$Date <- as.POSIXct(strptime(flux_all_2$Date,"%Y-%m-%d"))

ggplot()+
  geom_point(flux_all_2,mapping=aes(x=Date,y=co2_flux_umolCm2s,color="CO2"))+
  geom_point(flux_all_2,mapping=aes(x=Date,y=ch4_flux_umolCm2s,color="CH4"))+
  ylab("flux_umolCm2s")

# Export out fluxes
write_csv(flux_all_2,"./20210125_Flux_Output.csv")
