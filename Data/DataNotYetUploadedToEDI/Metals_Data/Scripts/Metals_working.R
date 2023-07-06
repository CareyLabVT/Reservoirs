#read in most recent ICPMS sheet
ICP<-read.csv("~/Documents/GitHub/Reservoirs/Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/2023/ICPMS_230203_230501.csv",
                     skip = 5) %>% 
  select(X, X.14, X.15) %>%
  rename(Jeff_ID = X, Fe_mgL = X.14, Mn_mgL = X.15) %>% #note: Fe and Mn are still ppb or ug/L
  separate(Jeff_ID,c("DateTime","Sample")," - ")
 ICP$Sample <- as.numeric(ICP$Sample) #must be numeric for join to work
 ICP$Fe_mgL <- ICP$Fe_mgL/1000 #converting to ppm
 ICP$Mn_mgL <- ICP$Mn_mgL/1000 #converting to ppm
 ICP$DateTime <-as.Date(ICP$DateTime, format = "%m/%d/%Y")
   
#read in metals ID, reservoir, site, depth, and total/soluble key
 metals_key <- read_csv('~/Documents/GitHub/Reservoirs/Data/DataNotYetUploadedToEDI/Metals_Data/Scripts/Metals_Sample_Depth.csv') %>%
   rename(Depth_m = `Sample Depth (m)`)
 
#set up final data frame with correct formatting!
 frame1 <- ICP %>%
   group_by(DateTime) %>% 
   expand(Sample = 1:30) %>% #note - 1:30 includes ALL of the FCR and BVR sites that are sampled
   left_join(ICP, by = c('Sample', 'DateTime')) %>% 
   full_join(metals_key, by = 'Sample') %>% 
   pivot_wider(names_from = 'Filter', values_from = c('Fe_mgL', 'Mn_mgL')) %>% 
   rename(TFe_mgL = Fe_mgL_T, SFe_mgL = Fe_mgL_S, TMn_mgL = Mn_mgL_T, SMn_mgL = Mn_mgL_S) %>% 
   group_by(DateTime, Reservoir, Depth_m, Site) %>% 
   summarise(TFe_mgL = sum(TFe_mgL, na.rm = TRUE), TMn_mgL = sum(TMn_mgL, na.rm = TRUE), 
             SFe_mgL = sum(SFe_mgL, na.rm = TRUE), SMn_mgL = sum(SMn_mgL, na.rm = TRUE)) %>% 
   arrange(DateTime, Reservoir, Site, Depth_m) %>% 
   select(Reservoir, Site, DateTime, Depth_m, TFe_mgL, TMn_mgL, SFe_mgL, SMn_mgL)

 #flags are next! also need to figure out a way to 'expand' only to the number of samples that
 #were actually collected...