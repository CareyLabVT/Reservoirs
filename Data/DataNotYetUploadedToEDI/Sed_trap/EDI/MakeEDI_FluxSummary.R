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
  summarise(n_Li = sum(!is.na(TLiFlux_gm2d)),
            n_Na = sum(!is.na(TNaFlux_gm2d)),
            n_Mg = sum(!is.na(TMgFlux_gm2d)),
            n_Al = sum(!is.na(TAlFlux_gm2d)),
            n_Si = sum(!is.na(TSiFlux_gm2d)),
            n_K = sum(!is.na(TKFlux_gm2d)),
            n_Ca = sum(!is.na(TCaFlux_gm2d)),
            n_Fe = sum(!is.na(TFeFlux_gm2d)),
            n_Mn = sum(!is.na(TMnFlux_gm2d)),
            n_Cu = sum(!is.na(TCuFlux_gm2d)),
            n_Sr = sum(!is.na(TSrFlux_gm2d)),
            n_Ba = sum(!is.na(TBaFlux_gm2d)),
            AvgSedFlux_gm2d = mean(SedFlux_gm2d, na.rm = TRUE),
            TLiFlux_gm2d = mean(TLiFlux_gm2d, na.rm = TRUE),
            TNaFlux_gm2d = mean(TNaFlux_gm2d, na.rm = TRUE), 
            TMgFlux_gm2d = mean(TMgFlux_gm2d, na.rm = TRUE), 
            TAlFlux_gm2d = mean(TAlFlux_gm2d, na.rm = TRUE), 
            TSiFlux_gm2d = mean(TSiFlux_gm2d, na.rm = TRUE),
            TKFlux_gm2d = mean(TKFlux_gm2d, na.rm = TRUE),
            TCaFlux_gm2d = mean(TCaFlux_gm2d, na.rm = TRUE),
            TFeFlux_gm2d = mean(TFeFlux_gm2d, na.rm = TRUE), 
            TMnFlux_gm2d = mean(TMnFlux_gm2d, na.rm = TRUE), 
            TCuFlux_gm2d = mean(TCuFlux_gm2d, na.rm = TRUE),
            TSrFlux_gm2d = mean(TSrFlux_gm2d, na.rm = TRUE),
            TBaFlux_gm2d = mean(TBaFlux_gm2d, na.rm = TRUE),
            TOCFlux_gm2d = mean(TOCFlux_gm2d, na.rm = TRUE),
            TNFlux_gm2d = mean(TNFlux_gm2d, na.rm = TRUE),
            Flag_CombinedCollectionVol_L = paste0(unique(Flag_CombinedCollectionVol_L), collapse = ""),
            Flag_Duration_days = paste0(unique(Flag_Duration_days), collapse = ""),
            Flag_Filter2ID = paste0(unique(Flag_Filter2ID), collapse = ''),
            Flag_CombinedSedMass_g = paste0(unique(Flag_CombinedSedMass_g), collapse = '')
            )
  
#replace NaN created by summarise function with NA
flux_summary$TLiFlux_gm2d <- na_if(flux_summary$TLiFlux_gm2d, NaN)
flux_summary$TNaFlux_gm2d <- na_if(flux_summary$TNaFlux_gm2d, NaN)
flux_summary$TMgFlux_gm2d <- na_if(flux_summary$TMgFlux_gm2d, NaN)
flux_summary$TAlFlux_gm2d <- na_if(flux_summary$TAlFlux_gm2d, NaN)
flux_summary$TSiFlux_gm2d <- na_if(flux_summary$TSiFlux_gm2d, NaN)
flux_summary$TKFlux_gm2d <- na_if(flux_summary$TKFlux_gm2d, NaN)
flux_summary$TCaFlux_gm2d <- na_if(flux_summary$TCaFlux_gm2d, NaN)
flux_summary$TFeFlux_gm2d <- na_if(flux_summary$TFeFlux_gm2d, NaN)
flux_summary$TMnFlux_gm2d <- na_if(flux_summary$TMnFlux_gm2d, NaN)
flux_summary$TCuFlux_gm2d <- na_if(flux_summary$TCuFlux_gm2d, NaN)
flux_summary$TSrFlux_gm2d <- na_if(flux_summary$TSrFlux_gm2d, NaN)
flux_summary$TBaFlux_gm2d <- na_if(flux_summary$TBaFlux_gm2d, NaN)
flux_summary$TOCFlux_gm2d <- na_if(flux_summary$TOCFlux_gm2d, NaN)
flux_summary$TNFlux_gm2d <- na_if(flux_summary$TNFlux_gm2d, NaN)
flux_summary$Flag_CombinedCollectionVol_L <- as.integer(flux_summary$Flag_CombinedCollectionVol_L)

#flags for sed flux, Fe flux, Mn flux, TOC flux, and TN flux; need 'summary variable' flag for all
flux_summary_final <- flux_summary %>% 
  mutate(Flag_TLiFlux_gm2d = 1,
         Flag_TLiFlux_gm2d = ifelse(n_Li > 1, 3, Flag_TLiFlux_gm2d),
         Flag_TLiFlux_gm2d = ifelse(is.na(TLiFlux_gm2d), 2, Flag_TLiFlux_gm2d),
         Flag_TNaFlux_gm2d = 1,
         Flag_TNaFlux_gm2d = ifelse(n_Na > 1, 3, Flag_TNaFlux_gm2d),
         Flag_TNaFlux_gm2d = ifelse(is.na(TNaFlux_gm2d), 2, Flag_TNaFlux_gm2d),
         Flag_TMgFlux_gm2d = 1,
         Flag_TMgFlux_gm2d = ifelse(n_Mg > 1, 3, Flag_TMgFlux_gm2d),
         Flag_TMgFlux_gm2d = ifelse(is.na(TMgFlux_gm2d), 2, Flag_TMgFlux_gm2d),
         Flag_TAlFlux_gm2d = 1,
         Flag_TAlFlux_gm2d = ifelse(n_Al > 1, 3, Flag_TAlFlux_gm2d),
         Flag_TAlFlux_gm2d = ifelse(is.na(TAlFlux_gm2d), 2, Flag_TAlFlux_gm2d),
         Flag_TSiFlux_gm2d = 1,
         Flag_TSiFlux_gm2d = ifelse(n_Si > 1, 3, Flag_TSiFlux_gm2d),
         Flag_TSiFlux_gm2d = ifelse(is.na(TSiFlux_gm2d), 2, Flag_TSiFlux_gm2d),
         Flag_TKFlux_gm2d = 1,
         Flag_TKFlux_gm2d = ifelse(n_K > 1, 3, Flag_TKFlux_gm2d),
         Flag_TKFlux_gm2d = ifelse(is.na(TKFlux_gm2d), 2, Flag_TKFlux_gm2d),
         Flag_TCaFlux_gm2d = 1,
         Flag_TCaFlux_gm2d = ifelse(n_Ca > 1, 3, Flag_TCaFlux_gm2d),
         Flag_TCaFlux_gm2d = ifelse(is.na(TCaFlux_gm2d), 2, Flag_TCaFlux_gm2d),
         Flag_TFeFlux_gm2d = 1,
         Flag_TFeFlux_gm2d = ifelse(n_Fe > 1, 3, Flag_TFeFlux_gm2d),
         Flag_TFeFlux_gm2d = ifelse(is.na(TFeFlux_gm2d), 2, Flag_TFeFlux_gm2d),
         Flag_TMnFlux_gm2d = 1,
         Flag_TMnFlux_gm2d = ifelse(n_Mn > 1, 3, Flag_TMnFlux_gm2d),
         Flag_TMnFlux_gm2d = ifelse(is.na(TMnFlux_gm2d), 2, Flag_TMnFlux_gm2d),
         Flag_TCuFlux_gm2d = 1,
         Flag_TCuFlux_gm2d = ifelse(n_Cu > 1, 3, Flag_TCuFlux_gm2d),
         Flag_TCuFlux_gm2d = ifelse(is.na(TCuFlux_gm2d), 2, Flag_TCuFlux_gm2d),
         Flag_TSrFlux_gm2d = 1,
         Flag_TSrFlux_gm2d = ifelse(n_Sr > 1, 3, Flag_TSrFlux_gm2d),
         Flag_TSrFlux_gm2d = ifelse(is.na(TSrFlux_gm2d), 2, Flag_TSrFlux_gm2d),
         Flag_TBaFlux_gm2d = 1,
         Flag_TBaFlux_gm2d = ifelse(n_Ba > 1, 3, Flag_TBaFlux_gm2d),
         Flag_TBaFlux_gm2d = ifelse(is.na(TBaFlux_gm2d), 2, Flag_TBaFlux_gm2d),
         Flag_TOCFlux_gm2d = 1,
         Flag_TOCFlux_gm2d = ifelse(is.na(TOCFlux_gm2d), 2, Flag_TOCFlux_gm2d),
         Flag_TNFlux_gm2d = 1,
         Flag_TNFlux_gm2d = ifelse(is.na(TNFlux_gm2d), 2, Flag_TNFlux_gm2d),
         Flag_AvgSedFlux_gm2d = 1,
         Flag_AvgSedFlux_gm2d = ifelse(Flag_CombinedCollectionVol_L != 1 | Flag_Duration_days != 1 | Flag_Filter2ID != 1 | Flag_CombinedSedMass_g != 1, 3, Flag_AvgSedFlux_gm2d),
         Flag_AvgSedFlux_gm2d = ifelse(is.na(AvgSedFlux_gm2d), 2, Flag_AvgSedFlux_gm2d)) %>% 
  select(Date, Reservoir, Depth_m, AvgSedFlux_gm2d, 
         TLiFlux_gm2d, TNaFlux_gm2d,
         TMgFlux_gm2d, TAlFlux_gm2d,
         TSiFlux_gm2d, TKFlux_gm2d,
         TCaFlux_gm2d, TFeFlux_gm2d,
         TMnFlux_gm2d, TCuFlux_gm2d, 
         TSrFlux_gm2d, TBaFlux_gm2d,TOCFlux_gm2d,
         TNFlux_gm2d, Flag_AvgSedFlux_gm2d, 
         Flag_TLiFlux_gm2d, Flag_TNaFlux_gm2d,
         Flag_TSiFlux_gm2d, Flag_TKFlux_gm2d,
         Flag_TCaFlux_gm2d, Flag_TFeFlux_gm2d,
         Flag_TMnFlux_gm2d, Flag_TCuFlux_gm2d,
         Flag_TSrFlux_gm2d, Flag_TBaFlux_gm2d, 
         Flag_TOCFlux_gm2d,
         Flag_TNFlux_gm2d)

write_csv(flux_summary_final, file = 'FluxSummary.csv')

             