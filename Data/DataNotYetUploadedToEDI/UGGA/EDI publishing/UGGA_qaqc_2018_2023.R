## AUTOMATED L1 QAQC SCRIPT FOR UGGA 
## Author: Austin Delany 
## Last edited: 01-03-24


ugga_qaqc <- function(current_file, log_file, current_year){

ugga_current = read_csv(current_file)%>%
  mutate(Date=as.Date(Date),
         Start=paste0(Start),
         End = paste0(End))%>%
  rename(Flag_co2_flux_umolCm2s = co2_flux_umolCm2s_flag,
         Flag_ch4_flux_umolCm2s = ch4_flux_umolCm2s_flag)%>%
  #filter(!Date == "2022-06-13"|!(Start %in% c("11:14:08","11:22:16","11:30:14")))%>%#only one real sampling on these three takes, removing issues caused by data processing
  rename(Start_time = Start,
         End_time = End) |> 
  dplyr::select("Reservoir",
                "Site",
                "Date",
                "Rep",
                "Start_time",
                "End_time",
                "Temp_C",
                "co2_slope_ppmS",
                "co2_R2",
                "co2_flux_umolCm2s",
                "ch4_slope_ppmS",
                "ch4_R2",
                "ch4_flux_umolCm2s",
                "Flag_co2_flux_umolCm2s",
                "Flag_ch4_flux_umolCm2s") %>%
  arrange(Date)



ugga_flagged <- ugga_current %>%
  mutate(Flag_co2_flux_umolCm2s = ifelse(co2_R2<0.9,1,Flag_co2_flux_umolCm2s),
         Flag_ch4_flux_umolCm2s = ifelse(ch4_R2<0.9,1,Flag_ch4_flux_umolCm2s),
         Site = ifelse(Site==1&Reservoir=="BVR",40,Site))%>%
  rename(CO2Slope_ppmS = co2_slope_ppmS,
         CH4Slope_ppmS = ch4_slope_ppmS,
         CO2_R2 = co2_R2,
         CH4_R2 = ch4_R2,
         CH4Flux_umolCm2s = ch4_flux_umolCm2s,
         CO2Flux_umolCm2s = co2_flux_umolCm2s,
         Flag_CO2Flux_umolCm2s = Flag_co2_flux_umolCm2s,
         Flag_CH4Flux_umolCm2s = Flag_ch4_flux_umolCm2s)%>%
  mutate(End_dif = as.POSIXct(End_time, format = "%H:%M:%S")-as.POSIXct(Start_time, format = "%H:%M:%S"),
         Start_time = ifelse(year(Date)==current_year,"12:00:00",Start_time),
         End_time = ifelse(year(Date)==current_year,format(as.POSIXct("12:00:00", format = "%H:%M:%S")+End_dif,"%H:%M:%S"),End_time),
         Flag_Start_time = ifelse(year(Date)==current_year,1,0),
         Flag_End_time = ifelse(year(Date)==current_year,1,0))%>%
  dplyr::select(-End_dif)

# drop inflow and saw grass sites
ugga_final <- ugga_flagged |> 
  filter(!(Site %in% c('inflow', 'saw grass'))) |> 
  mutate(Site = as.numeric(Site), 
         Start_time = as.character(Start_time), 
         End_time = as.character(End_time)) 

## Maintenance Log is handled in the FluxCalR script

save_path <- file.path("../../Data/DataNotYetUploadedToEDI/UGGA/UGGA_Raw","UGGA_L1.csv")

write.csv(ugga_final, save_path, row.names = F)

}


current_file <- 'https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/UGGA/UGGA_Raw/2023/2023_season_Flux_Output.csv'
current_year <- 2023

log_file <- 'https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/UGGA/UGGA_Raw/2023/UGGA_log_2023.csv'


ugga_qaqc(current_file = current_file, log_file = log_file, current_year = current_year)
