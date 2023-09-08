## QAQC secchi data -- pull from google sheets and save as csv 

library(tidyverse)
library(gsheet)

secchi_qaqc <- function(){
  
gsheet_url <- 'https://docs.google.com/spreadsheets/d/1fvM0fDRliuthicQWZT7c9RYErikI5DwrzbOC7TCoMGI/edit#gid=1172894977'
secchi_df <- gsheet::gsheet2tbl(gsheet_url)

secchi_df$Flag_Datetime <- NA

secchi_reformat <- secchi_df |> 
  rename(Flag_Secchi_m = Flag_Secchi)

write.csv(secchi_reformat, './Data/DataNotYetUploadedToEDI/Secchi/secchi_L1.csv')

}

secchi_qaqc()
