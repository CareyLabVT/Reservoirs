#Load packages
library(akima)
library(colorRamps)
library(plotly)
library(tidyverse)
library(lubridate)
setwd("./Data/DataNotYetUploadedToEDI/Raw_CTD/CTD_code/")
source("ctd_QAQC.R")

THIS_YEAR <- 2023 #Latest year of data
# EDI publication that we are using for historical data
# NOTE: I THINK we do NOT want to change this link in future revisions, given our decision in 2024 to 
# Re-process all casts 2018-present rather than pulling the most recent data on EDI
ctd_edi <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/200/13/27ceda6bc7fdec2e7d79a6e4fe16ffdf")

#Make sure all files have been processed
processed_files <- sub(".csv","",list.files("../csv_outputs"))
raw_files <- list.files("../RawDownloads")
raw_files <- sub(".cnv", "", raw_files[grepl(".cnv", raw_files)])
processed_files[!processed_files %in% raw_files]
raw_files[!raw_files %in% processed_files]

# Re-process all data. This will take a long time, particularly if you set force_reprocessing to T
# Can be skipped if you only want to visualize data and have already run this once this year
ctd_reprocessed <- ctd_QAQC(raw_downloads = "../RawDownloads",
                            ctd_cast_csvs = "../csv_outputs",
                            ctd_season_csvs = "../CTD_season_csvs",
                            CTD_FOLDER = "../",
                            start_date = as.Date("2012-01-01"), #Since the beginning of the reservoir monitoring program
                            force_reprocessing = F, #Re-process all files
                            output_file_name = paste0("CTD_2018_", THIS_YEAR,".csv"),
                            intermediate_file_name = paste0("CTD_L0_2018_", THIS_YEAR,".csv"))

#Load data
ctd_reprocessed <- read.csv(paste0("../CTD_2018_", THIS_YEAR,".csv"))
#Gut check
min(ctd_reprocessed$DateTime) # we've re-processed files since 2018
unique(ctd_reprocessed$Site)
unique(ctd_reprocessed$Reservoir)

#Add SN to historical EDI data
ctd_edi <- ctd_edi %>%
  mutate(SN = ifelse(year(DateTime) %in% c(2013:2016), 4397, 7809))

#Check that all files have gotten re-processed
check = ctd_edi %>% #Join new data with published dataset
  arrange(desc(DateTime)) %>%
  filter(year(DateTime) >= 2018,
         !DateTime %in% unique(ctd_reprocessed$DateTime))
unique(check$DateTime) 
# the only file since 2018 that is on EDI but not in the re-processed data is 
# "2021-12-14 10:45:36" the reason this is not in the re-processed data is that 
# the time was later adjusted to 12:45:36. I.e., the historical data includes an 
# incorrect time. The version with 10:45:36 should be removed (which is done in 
# "Combine with historical", below)

#Now check for files that are in the re-processed data but not the historical data
check2 <- ctd_reprocessed %>%
  arrange(desc(DateTime)) %>%
  filter(year(DateTime) <= 2022, #Since we are using the 2022 data publication
         !DateTime %in% unique(ctd_edi$DateTime), 
         #SN == "7809"
         ) %>%
  mutate(Name = paste0(Reservoir, Site, " ", SN, " ", DateTime))
unique(check2$Name) 
# 54 new files. In 2022, many of these are the new CTD, which didn't get published
# Also lots of CCR from 2019

#Combine with historical
ctd_comb <- ctd_edi %>%
  mutate(Flag_CDOM_ugL = 5,
         Flag_Phycoerythrin_ugL = 5,
         Flag_Phycocyanin_ugL = 5) %>%
  filter(!DateTime %in% ctd_reprocessed$DateTime) %>% #Remove files that have been re-processed
  filter(!DateTime == "2021-12-14 10:45:36") %>% #Fix the time issue identified above
  full_join(ctd_reprocessed) %>%
  select(Reservoir, Site, DateTime, Depth_m, SN, Temp_C, DO_mgL, DOsat_percent,
         Cond_uScm, SpCond_uScm, Chla_ugL, Turbidity_NTU, pH, ORP_mV, PAR_umolm2s,
         CDOM_ugL, Phycoerythrin_ugL, Phycocyanin_ugL, DescRate_ms, Flag_DateTime, 
         Flag_Temp_C, Flag_DO_mgL, Flag_DOsat_percent, Flag_Cond_uScm, 
         Flag_SpCond_uScm, Flag_Chla_ugL, Flag_Turbidity_NTU, Flag_pH, 
         Flag_ORP_mV, Flag_PAR_umolm2s, Flag_CDOM_ugL, Flag_Phycoerythrin_ugL, 
         Flag_Phycocyanin_ugL, Flag_DescRate_ms)
write.csv(ctd_comb, paste0("../CTD_2013_", THIS_YEAR,".csv"), row.names = F)

# Begin visualization
qaqc = ctd_comb %>%
  mutate(Date = as_datetime(DateTime))

# Basic checks
unique(qaqc$Site)
unique(qaqc$Reservoir)

#Function to plot all data for a given year as a heatmap
plot_var <- function(var_name, 
                     year, 
                     reservoirs = c("FCR", "BVR", "CCR"),
                     data = qaqc){
  var <- sym(var_name)
  flag <- sym(paste0("Flag_", var_name))
  qaqc_plotly <- data %>%
    filter(Reservoir %in% reservoirs, Site == 50, year(Date) %in% year) %>%
    sample_frac(.1) %>%
    ggplot(aes(x = Date, y = Depth_m, color = !!var, shape = as.factor(!!flag))) +
    scale_color_gradientn(colours = blue2green2red(100), na.value="gray") +
    scale_y_reverse() +
    geom_point() +
    facet_grid(cols = vars(Reservoir), rows = vars(SN))
  ggplotly(qaqc_plotly)
}

vars_to_plot <- c("Temp_C", "DO_mgL", "DOsat_percent", "Cond_uScm", "SpCond_uScm",
                  "Chla_ugL", "Turbidity_NTU", "pH", "ORP_mV", "PAR_umolm2s", 
                  "DescRate_ms")
library(colorRamps)
library(plotly)
vars_to_plot %>%
  map(plot_var, year = 2023)

vars_to_plot %>%
  map(plot_var, year = 2022)

vars_to_plot %>%
  map(plot_var, year = 2021)

vars_to_plot %>%
  map(plot_var, year = 2020)

vars_to_plot %>%
  map(plot_var, year = 2019)

vars_to_plot %>%
  map(plot_var, year = 2018)

qaqc %>%
  select(DateTime, SN) %>%
  unique() %>%
  filter(year(DateTime) == 2023) %>%
  ggplot(aes(x = hour(DateTime))) +
  geom_histogram()+
  facet_wrap(~SN)
