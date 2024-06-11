# Check for missing dates between Chemistry, YSI, Secchi files
pacman::p_load(tidyverse, lubridate)

chemistry <- read_csv('./Formatted_Data/chemistry.csv') %>%
  mutate(Chemistry = "Yes", 
         DateTime = as.Date(DateTime)) %>%
  group_by(Reservoir, Site, DateTime) %>%
  distinct(Reservoir, Site, DateTime, Depth_m, .keep_all = T) %>%
  select(Reservoir:Depth_m, Chemistry)

Secchi <- read_csv('./Formatted_Data/Secchi_depth.csv') %>%
  mutate(Secchi = "Yes",
         DateTime = as.Date(DateTime)) %>%
  select(Reservoir:DateTime, Secchi)

YSI <- read_csv('./Formatted_Data/YSI_PAR_profiles.csv') %>%
  mutate(DateTime = as.Date(DateTime),
         Temp = ifelse(!is.na(Temp_C), "Yes", "No"),
         DO = ifelse(!is.na(DO_mgL), "Yes", "No"),
         DOsat = ifelse(!is.na(DOSat), "Yes", "No"),
         Cond = ifelse(!is.na(Cond_uScm), "Yes", "No"),
         PAR = ifelse(!is.na(PAR_umolm2s), "Yes", "No"),
         ORP = ifelse(!is.na(ORP), "Yes", "No"),
         PH = ifelse(!is.na(pH), "Yes", "No")) %>%
  select(Reservoir:pH, Notes)

all <- full_join(YSI, chemistry) %>% 
  full_join(., Secchi) %>%
  mutate(Chemistry = ifelse(!is.na(Chemistry), Chemistry, "No"),
         Secchi = ifelse(!is.na(Secchi), Secchi, "No")) %>%
  group_by(Reservoir, Site, DateTime) %>%
  #distinct(Reservoir, Site, DateTime, Depth_m, .keep_all= T) %>%
  select(Reservoir:Depth_m, Secchi, Chemistry, Temp_C:pH, Notes) %>%
  arrange(Reservoir, Site, DateTime, Depth_m) %>%
  #mutate_at(.vars = vars(Secchi:pH),
  #          .funs = funs(ifelse(is.na(.),"No", .)))%>%
  write_csv('./Formatted_Data/Chemistry_YSI_PAR_Secchi_matchedDates.csv')
