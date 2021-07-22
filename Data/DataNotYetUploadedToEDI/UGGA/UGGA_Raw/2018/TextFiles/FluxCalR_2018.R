## Script to *try* FluxCalR for calculating fluxes from UGGA data
## For 2020 data! Will need to updated for 2021 data : )
## A Hounshell, 25 Jan 2021

# Script following: https://github.com/junbinzhao/FluxCalR

# Load in 'remotes' package
pacman::p_load(remotes,tidyverse,FluxCalR)

# Install FluxCalR (if not already installed! If installed, skip to library(FluxCalR))
remotes::install_github("junbinzhao/FluxCalR",build_vignettes = TRUE)
library(FluxCalR)

# Load in data: will need to load in individual files - I recommend doing this by year
# SET TO YOUR OWN WD!

#Arpita: go to Session then Set Working Directory then To Source File Location and paste the output here
wd <- setwd("~/Desktop/Reservoirs/Data/DataNotYetUploadedToEDI/UGGA/UGGA_Raw/2018/TextFiles")
#wd <- setwd("~/Desktop/github/Reservoirs/Data/DataNotYetUploadedToEDI/UGGA/UGGA_Raw/2020/TextFiles")
#wd <- setwd("C:/Users/ahoun/Desktop/Reservoirs/Data/DataNotYetUploadedToEDI/UGGA/UGGA_Raw/2020/TextFiles")
# You'll want to save this script in the same working directory to keep a record of what files
# you have corrected.

# Load in flux data - load in all text files from the 'TextFiles' folder here.
# This can be used as a record of what files you have already corrected.
flux_lgr_2 <- LoadLGR(file ="./gga_2018-05-31_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_3 <- LoadLGR(file ="./gga_2018-05-30_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_4 <- LoadLGR(file ="./gga_2018-05-29_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_5 <- LoadLGR(file ="./gga_2018-05-28_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_6 <- LoadLGR(file ="./gga_2018-05-24_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_7 <- LoadLGR(file ="./gga_2018-05-21_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_8 <- LoadLGR(file ="./gga_2018-05-14_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_9 <- LoadLGR(file ="./gga_2018-05-07_f0000.txt",
                      time_format = "mdy_HMS")
flux_lgr_10 <- LoadLGR(file ="./gga_2018-03-23_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_11 <- LoadLGR(file ="./gga_2018-03-05_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_12 <- LoadLGR(file ="./gga_2018-02-23_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_13 <- LoadLGR(file ="./gga_2018-02-12_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_14 <- LoadLGR(file ="./gga_2018-01-26_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_15 <- LoadLGR(file ="./gga_2018-01-25_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_16 <- LoadLGR(file ="./gga_2018-06-04_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_17 <- LoadLGR(file ="./gga_2018-07-02_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_18 <- LoadLGR(file ="./gga_2018-07-05_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_19 <- LoadLGR(file ="./gga_2018-07-09_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_20 <- LoadLGR(file ="./gga_2018-07-16_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_21 <- LoadLGR(file ="./gga_2018-07-18_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_22 <- LoadLGR(file ="./gga_2018-07-23_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_23 <- LoadLGR(file ="./gga_2018-07-30_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_27 <- LoadLGR(file ="./gga_2018-09-03_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_28 <- LoadLGR(file ="./gga_2018-09-17_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_29 <- LoadLGR(file ="./gga_2018-09-21_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_31 <- LoadLGR(file ="./gga_2018-09-24_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_33 <- LoadLGR(file ="./gga_2018-10-01_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_34 <- LoadLGR(file ="./gga_2018-10-05_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_35 <- LoadLGR(file ="./gga_2018-10-08_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_37 <- LoadLGR(file ="./gga_2018-10-15_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_38 <- LoadLGR(file ="./gga_2018-10-19_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_39 <- LoadLGR(file ="./gga_2018-10-22_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_40 <- LoadLGR(file ="./gga_2018-10-29_f0000.txt",
                       time_format = "mdy_HMS")
flux_lgr_41 <- LoadLGR(file ="./gga_2018-11-02_f0000.txt",
                       time_format = "mdy_HMS")


# Select times when the UGGA was on/off the water
# Select the time point BEFORE the peak for both reps
# Instructions: Use the cursor to select the timepoint before the peak; click once for the first peak and again for
# the second peak. When finished, click on 'Stop' in the upper left-hand corner and then click 'Stop locator'
# This generates a list of 'end' times for each peak saved as time_cue_x

# Repeat this for all timepoints
time_cue_2 <- SelCue(flux_lgr_2,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c()) #Need to fill in reservoir and site!
time_cue_3 <- SelCue(flux_lgr_3,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_4 <- SelCue(flux_lgr_4,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_5 <- SelCue(flux_lgr_5,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_6 <- SelCue(flux_lgr_6,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_7 <- SelCue(flux_lgr_7,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_8 <- SelCue(flux_lgr_8,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_9 <- SelCue(flux_lgr_9,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_10 <- SelCue(flux_lgr_10,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_11 <- SelCue(flux_lgr_11,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_12 <- SelCue(flux_lgr_12,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_13 <- SelCue(flux_lgr_13,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_14 <- SelCue(flux_lgr_14,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_15 <- SelCue(flux_lgr_15,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_16 <- SelCue(flux_lgr_16,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_17 <- SelCue(flux_lgr_17,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_18 <- SelCue(flux_lgr_18,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_19 <- SelCue(flux_lgr_19,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_20 <- SelCue(flux_lgr_20,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_21 <- SelCue(flux_lgr_21,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_22 <- SelCue(flux_lgr_22,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_23 <- SelCue(flux_lgr_23,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_24 <- SelCue(flux_lgr_27,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_25 <- SelCue(flux_lgr_28,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_26 <- SelCue(flux_lgr_29,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_27 <- SelCue(flux_lgr_31,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_28 <- SelCue(flux_lgr_33,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_29 <- SelCue(flux_lgr_34,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_30 <- SelCue(flux_lgr_35,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_31 <- SelCue(flux_lgr_37,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_32 <- SelCue(flux_lgr_38,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_33 <- SelCue(flux_lgr_39,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_34 <- SelCue(flux_lgr_40,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())
time_cue_35 <- SelCue(flux_lgr_41,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c(), Site = c())


# Then calculate fluxes
Flux_output2 <- FluxCal(data = flux_lgr_2, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_2, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site"), #Other columns from the data file to pass on
                        output = FALSE)

Flux_output3 <- FluxCal(data = flux_lgr_3, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_3, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site"), #Other columns from the data file to pass on
                        output = FALSE)

Flux_output4 <- FluxCal(data = flux_lgr_4, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_4, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site"), #Other columns from the data file to pass on
                        output = FALSE)

Flux_output5 <- FluxCal(data = flux_lgr_5, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_5, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site"), #Other columns from the data file to pass on
                        output = FALSE)

Flux_output6 <- FluxCal(data = flux_lgr_6, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_6, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site"), #Other columns from the data file to pass on
                        output = FALSE)

Flux_output7 <- FluxCal(data = flux_lgr_7, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_7, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site"), #Other columns from the data file to pass on
                        output = FALSE)

Flux_output8 <- FluxCal(data = flux_lgr_8, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_8, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site"), #Other columns from the data file to pass on
                        output = FALSE)

Flux_output9 <- FluxCal(data = flux_lgr_9, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_9, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site"), #Other columns from the data file to pass on
                        output = FALSE)

Flux_output10 <- FluxCal(data = flux_lgr_10, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_10, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output11 <- FluxCal(data = flux_lgr_11, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_11, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output12 <- FluxCal(data = flux_lgr_12, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_12, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output13 <- FluxCal(data = flux_lgr_13, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_13, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output14 <- FluxCal(data = flux_lgr_14, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_14, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output15 <- FluxCal(data = flux_lgr_15, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_15, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output16 <- FluxCal(data = flux_lgr_16, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_16, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output17 <- FluxCal(data = flux_lgr_17, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_17, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output18 <- FluxCal(data = flux_lgr_18, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_18, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output19 <- FluxCal(data = flux_lgr_19, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_19, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output20 <- FluxCal(data = flux_lgr_20, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_20, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output21 <- FluxCal(data = flux_lgr_21, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_21, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output22 <- FluxCal(data = flux_lgr_22, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_22, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output23 <- FluxCal(data = flux_lgr_23, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_23, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output24 <- FluxCal(data = flux_lgr_27, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_27, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output25 <- FluxCal(data = flux_lgr_28, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_28, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output26 <- FluxCal(data = flux_lgr_29, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_29, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output27 <- FluxCal(data = flux_lgr_31, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_31, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output28 <- FluxCal(data = flux_lgr_33, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_33, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output29 <- FluxCal(data = flux_lgr_34, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_34, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output30 <- FluxCal(data = flux_lgr_35, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_35, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output31 <- FluxCal(data = flux_lgr_37, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_37, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output32 <- FluxCal(data = flux_lgr_38, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_38, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output33 <- FluxCal(data = flux_lgr_39, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_39, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output34 <- FluxCal(data = flux_lgr_40, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_40, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

Flux_output35 <- FluxCal(data = flux_lgr_41, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_41, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site"), #Other columns from the data file to pass on
                         output = FALSE)

####
#MAKE SURE ALL FLUX OUTPUT FILES ARE INCLUDED HERE:

# Combine all flux outputs
flux_output <- rbind(Flux_output2,Flux_output3,Flux_output4,Flux_output5,Flux_output6,Flux_output7,
                     Flux_output8,Flux_output9,Flux_output10)

# Get together for publication to EDI
flux_co2 <- flux_output %>% 
  filter(Gas == "CO2") %>% 
  rename(co2_slope_ppmS = Slope, co2_R2 = R2, co2_flux_umolCm2s = Flux) %>% 
  select(-Gas)

flux_ch4 <- flux_output %>% 
  filter(Gas == "CH4") %>% 
  rename(ch4_slope_ppmS = Slope, ch4_R2 = R2, ch4_flux_umolCm2s = Flux) %>% 
  select(-Gas)

flux_all <- left_join(flux_co2,flux_ch4,by=c("Num","Date","Start","End","Ta"))

# NOTE: For 2020 - all data came from FCR at site 50
# THIS IS NOT THE CASE FOR ALL YEARS - you'll have to go back into the field sheets and
# look to see what reservoir the data came from (should all be at site 50 : )
flux_all <- flux_all %>% 
  rename(Rep = Num, Start_time = Start, End_time = End, Temp_C = Ta) %>% 
  mutate (Reservoir = "FCR", Site = 50, co2_flux_umolCm2s_flag = 0, ch4_flux_umolCm2s_flag = 0)

col_order <- c("Reservoir","Site","Date","Rep","Start_time","End_time","Temp_C","co2_slope_ppmS","co2_R2",
               "co2_flux_umolCm2s","ch4_slope_ppmS","ch4_R2","ch4_flux_umolCm2s","co2_flux_umolCm2s_flag",
               "ch4_flux_umolCm2s_flag")

flux_all_2 <- flux_all[,col_order]

# Plots to double check data!
flux_all_2$Date <- as.POSIXct(strptime(flux_all_2$Date,"%Y-%m-%d", tz="EST"))

ggplot()+
  geom_point(flux_all_2,mapping=aes(x=Date,y=co2_flux_umolCm2s,color="CO2"))+
  geom_point(flux_all_2,mapping=aes(x=Date,y=ch4_flux_umolCm2s,color="CH4"))+
  ylab("flux_umolCm2s")

# Export out fluxes
write_csv(flux_all_2,"./2018_season_Flux_Output.csv") #change this!
