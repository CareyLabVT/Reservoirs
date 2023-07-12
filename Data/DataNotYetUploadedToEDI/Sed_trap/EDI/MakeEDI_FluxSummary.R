#set up
rm(list=ls(all=TRUE))
library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)

#add sed flux
setwd('./Data/DataNotYetUploadedToEDI/Sed_trap/EDI')
fluxes <- read.csv('CN_Metals_Flux_EDI.csv')
fluxes$Date <- as.Date(fluxes$Date)
fluxes <- fluxes %>% 
  mutate(SedFlux_gm2d = (((CombinedSedMass_g/CombinedFilterVol_L) * CombinedCollectionVol_L) / (CombinedXSA_m2 * Duration_days)))

flux_summary <- fluxes %>% 
  group_by(Date, Reservoir, Depth_m) %>% 
  summarise(n_Fe = sum(!is.na(TFeFlux_gm2d)),
            n_CN = sum(!is.na(TOCFlux_gm2d)),
            AvgSedFlux_gm2d = mean(SedFlux_gm2d, na.rm = TRUE),
            TFeFlux_gm2d = mean(TFeFlux_gm2d, na.rm = TRUE), 
            TMnFlux_gm2d = mean(TMnFlux_gm2d, na.rm = TRUE), 
            TOCFlux_gm2d = mean(TOCFlux_gm2d, na.rm = TRUE),
            TNFlux_gm2d = mean(TNFlux_gm2d, na.rm = TRUE),
            Flag_CombinedCollectionVol_L = paste0(unique(Flag_CombinedCollectionVol_L), collapse = ""),
            Flag_Duration_days = paste0(unique(Flag_Duration_days), collapse = ""),
            Flag_Filter2ID = paste0(unique(Flag_Filter2ID), collapse = ''),
            Flag_CombinedSedMass_g = paste0(unique(Flag_CombinedSedMass_g), collapse = '')
            )
  
#replace NaN created by summarise function with NA
flux_summary$TFeFlux_gm2d <- na_if(flux_summary$TFeFlux_gm2d, NaN)
flux_summary$TMnFlux_gm2d <- na_if(flux_summary$TMnFlux_gm2d, NaN)
flux_summary$TOCFlux_gm2d <- na_if(flux_summary$TOCFlux_gm2d, NaN)
flux_summary$TNFlux_gm2d <- na_if(flux_summary$TNFlux_gm2d, NaN)
flux_summary$Flag_CombinedCollectionVol_L <- as.integer(flux_summary$Flag_CombinedCollectionVol_L)

#flags for sed flux, Fe flux, Mn flux, TOC flux, and TN flux; need 'summary variable' flag for all
flux_summary_final <- flux_summary %>% 
  mutate(Flag_TFeFlux_gm2d = 1,
         Flag_TFeFlux_gm2d = ifelse(n_Fe > 1, 3, Flag_TFeFlux_gm2d),
         Flag_TFeFlux_gm2d = ifelse(is.na(TFeFlux_gm2d), 2, Flag_TFeFlux_gm2d),
         Flag_TMnFlux_gm2d = 1,
         Flag_TMnFlux_gm2d = ifelse(n_Fe > 1, 3, Flag_TMnFlux_gm2d),
         Flag_TMnFlux_gm2d = ifelse(is.na(TMnFlux_gm2d), 2, Flag_TMnFlux_gm2d),
         Flag_TOCFlux_gm2d = 1,
         Flag_TOCFlux_gm2d = ifelse(is.na(TOCFlux_gm2d), 2, Flag_TOCFlux_gm2d),
         Flag_TNFlux_gm2d = 1,
         Flag_TNFlux_gm2d = ifelse(is.na(TNFlux_gm2d), 2, Flag_TNFlux_gm2d),
         Flag_AvgSedFlux_gm2d = 1,
         Flag_AvgSedFlux_gm2d = ifelse(Flag_CombinedCollectionVol_L != 1 | Flag_Duration_days != 1 | Flag_Filter2ID != 1 | Flag_CombinedSedMass_g != 1, 4, Flag_AvgSedFlux_gm2d),
         Flag_AvgSedFlux_gm2d = ifelse(is.na(AvgSedFlux_gm2d), 2, Flag_AvgSedFlux_gm2d)) %>% 
  select(Date, Reservoir, Depth_m, AvgSedFlux_gm2d, TFeFlux_gm2d, TMnFlux_gm2d, TOCFlux_gm2d,
         TNFlux_gm2d, Flag_AvgSedFlux_gm2d, Flag_TFeFlux_gm2d, Flag_TMnFlux_gm2d, Flag_TOCFlux_gm2d,
         Flag_TNFlux_gm2d)

write_csv(flux_summary_final, file = 'FluxSummary.csv')

             