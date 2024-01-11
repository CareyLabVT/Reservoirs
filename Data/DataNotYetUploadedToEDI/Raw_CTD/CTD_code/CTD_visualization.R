#Load packages
library(akima)
library(colorRamps)
library(plotly)
library(tidyverse)
library(lubridate)
source("./ctd_QAQC.R")

#Re-process all data. This will take a long time!
ctd_QAQC(raw_downloads = "../RawDownloads",
         ctd_cast_csvs = "../csv_outputs",
         ctd_season_csvs = "../CTD_season_csvs",
         CTD_FOLDER = "../",
         start_date = as.Date("2012-01-01"), #Since the beginning of the reservoir monitoring program
         force_reprocessing = F, #Re-process all files
         output_file_name = "CTD_2013_2023.csv",
         intermediate_file_name = "CTD_L0_2013_2023.csv")

#Load data
ctd_reprocessed <- read.csv("../CTD_2013_2023.csv")
ctd_edi <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/200/13/27ceda6bc7fdec2e7d79a6e4fe16ffdf")

#For 2023, add SN to EDI data
ctd_edi <- ctd_edi %>%
  mutate(SN = 7809)

final = ctd_edi %>% #Join new data with published dataset
  arrange(desc(DateTime)) %>%
  filter(!DateTime %in% unique(ctd_reprocessed$DateTime))
unique(final$DateTime) 
# only issue is "2021-12-14 10:45:36"

final2 <- ctd_reprocessed %>%
  arrange(desc(DateTime)) %>%
  filter(!DateTime %in% unique(ctd_edi$DateTime))
unique(final2$DateTime) 
# Good number of new files, especially in 2022

#Combine with historical
ctd_comb <- ctd_edi %>%
  filter(!DateTime %in% ctd_reprocessed$DateTime)%>%
  full_join(ctd_reprocessed)
qaqc = ctd_comb %>%
  mutate(Date = as_datetime(DateTime))

#Plot all data for this year as a heatmap
plot_var <- function(var_name, year){
  var <- sym(var_name)
  flag <- sym(paste0("Flag_", var_name))
  qaqc_plotly <-qaqc %>%
    filter(Reservoir %in% c("FCR", "BVR"), Site == 50, year(Date) %in% year) %>%
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
#Issues with pH

#Something got mis-labeled as BVR
test <- ctd_reprocessed %>%
  filter(as.Date(DateTime) == "2022-04-20")

test %>%
  pivot_wider(values_from = "DO_mgL", names_from = "Reservoir") %>%
  ggplot(aes(x = BVR, y = CCR))+
  geom_point()

max(test$Depth_m[test$Reservoir == "BVR"])
unique(test$Reservoir)

# DO and temp are messed up for Jul 24 at BVR, but FCR looks fine. Added to maintenance log.
