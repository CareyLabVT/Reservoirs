metal_read<-read.csv("~/Documents/GitHub/Reservoirs/Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/2023/ICPMS_230203_230501.csv",
                     skip = 5) %>% 
  select(X, X.14, X.15) %>%
  rename(Jeff_ids = X, Fe_ppb = X.14, Mn_ppb = X.15) %>%
  separate(Jeff_ids,c("Date","Sample"),"-") %>%
  mutate(Reservoir = ifelse(Sample %in% c(1:18),"FCR","BVR"),
         Site = ifelse(Sample %in% c(15,16,29,30),100,
                       ifelse(Sample %in% c(17,18),200,50)))
