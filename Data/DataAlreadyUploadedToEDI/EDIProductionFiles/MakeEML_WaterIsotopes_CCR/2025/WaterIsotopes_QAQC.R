#### CCR isotope publishing 
# DWH january 2026


##libraries
library(tidyverse)


##read in data
iso <- read.csv("https://raw.githubusercontent.com/dwh77/Reservoir_EEMs/refs/heads/main/Water_Isotopes/isotopes_named.csv")


#### Make precipitation iso data frame
iso_precip <- iso |> 
  rename(Site = Site_number) |> 
  filter(Site == 51) |> 
  mutate(Iso_Flag = ifelse(Run_Name == "CCR3", 1, 0)) |> #Flag values for run date that was rerun
  select(Reservoir, Site, Date, SampleName, d18O_VSMOW, d18O_VSMOW_1sd, d2H_VSMOW, d2H_VSMOW_1sd, Iso_Flag) 

#write.csv(iso_precip, "./Water_Isotopes/EDI_2025/WaterIsotopes_precip_DRAFT.csv", row.names = F)

#### Make reservoir iso data frame
iso_reservoir <- iso |> 
  rename(Site = Site_number) |> 
  filter(Site != 51) |> 
  mutate(Iso_Flag = ifelse(Run_Name == "CCR3", 1, 0)) |> #Flag values for run date that was rerun
  select(Reservoir, Site, Date, Depth_m, d18O_VSMOW, d18O_VSMOW_1sd, d2H_VSMOW, d2H_VSMOW_1sd, Iso_Flag) |> 
  mutate(Date = mdy(Date)) 

write.csv(iso_reservoir, "./Data/DataAlreadyUPloadedToEDI/EDIProductionFiles/MakeEML_WaterIsotopes_CCR/2025/WaterIsotopes_CCR_2024_2025.csv", row.names = F)

