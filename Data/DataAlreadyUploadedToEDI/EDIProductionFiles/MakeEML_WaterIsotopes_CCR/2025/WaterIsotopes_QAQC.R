#### CCR isotope publishing 
# DWH january 2026


##libraries
library(tidyverse)


##read in data
iso <- read.csv("./Water_Isotopes/isotopes_named.csv")


#### Make precipitation iso data frame
iso_precip <- iso |> 
  select(Reservoir, Site_number, Date, SampleName, d18O_VSMOW, d18O_VSMOW_1sd, d2H_VSMOW, d2H_VSMOW_1sd) |> 
  rename(Site = Site_number) |> 
  filter(Site == 51)

#write.csv(iso_precip, "./Water_Isotopes/EDI_2025/WaterIsotopes_precip_DRAFT.csv", row.names = F)

#### Make reservoir iso data frame
iso_reservoir <- iso |> 
  select(Reservoir, Site_number, Date, Depth_m, d18O_VSMOW, d18O_VSMOW_1sd, d2H_VSMOW, d2H_VSMOW_1sd) |> 
  rename(Site = Site_number) |> 
  filter(Site != 51) |> 
  mutate(Date = mdy(Date)) |> 
  mutate(Iso_Flag = 0)

# write.csv(iso_reservoir, "./Water_Isotopes/EDI_2025/WaterIsotopes_CCR_2024_2025.csv", row.names = F)

