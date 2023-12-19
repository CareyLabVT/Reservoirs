#Load packages
library(akima)
library(colorRamps)
library(plotly)
library(tidyverse)
library(lubridate)

#Load data
ctd_l1 <- read.csv("../../ctd_L1.csv")
ctd_edi <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/200/13/27ceda6bc7fdec2e7d79a6e4fe16ffdf")

#For 2023, add SN to EDI data
ctd_edi <- ctd_edi %>%
  mutate(SN = 7809)
#Combine with historical
ctd_comb <- ctd_edi %>%
  full_join(ctd_l1)
qaqc = ctd_comb %>%
  mutate(Date = as_datetime(DateTime))

#Plot all data for this year as a heatmap
plot_var <- function(var_name){
  var <- sym(var_name)
  flag <- sym(paste0("Flag_", var_name))
  qaqc_plotly = qaqc %>%
    filter(Reservoir %in% c("FCR", "BVR"), Site == 50, year(Date) %in% c(2023)) %>%
    sample_frac(.1) %>%
    ggplot(aes(x = Date, y = Depth_m, color = !!var, shape = as.factor(!!flag))) +
    scale_color_gradientn(colours = blue2green2red(100), na.value="gray") +
    scale_y_reverse() +
    geom_point() +
    facet_wrap(~Reservoir)
  ggplotly(qaqc_plotly)
}

vars_to_plot <- c("Temp_C", "DO_mgL", "DOsat_percent", "Cond_uScm", "SpCond_uScm",
                  "Chla_ugL", "Turbidity_NTU", "pH", "ORP_mV", "PAR_umolm2s", 
                  "DescRate_ms")
vars_to_plot %>%
  map(plot_var)

# DO and temp are messed up for Jul 24 at BVR, but FCR looks fine