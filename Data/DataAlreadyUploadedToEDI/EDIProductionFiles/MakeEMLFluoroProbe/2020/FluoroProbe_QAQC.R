#Title: Script for aggregating and QAQCing FP txt files for publication to EDI
#Author: Mary Lofton
#Date: 16DEC19
#Last updated: 05JAN22

rm(list=ls())

# load packages
#install.packages('pacman')
pacman::p_load(tidyverse, lubridate)

# Load in column names for .txt files to get template
col_names <- names(read_tsv("./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/20200525_FCR_50_1.txt", n_max = 0))

# Load in all txt files
fp_casts <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe", pattern = paste0("*.txt")) %>%
  map_df(~ data_frame(x = .x), .id = "cast")

raw_fp <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe", pattern = paste0("*.txt")) %>% 
  map_df(~ read_tsv(file.path(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe", .), 
                    col_types = cols(.default = "c"), col_names = col_names, skip = 2), .id = "cast")

#split out column containing filename to get Reservoir and Site data
fp2 <- left_join(raw_fp, fp_casts, by = c("cast")) %>%
  rowwise() %>% 
  mutate(Reservoir = unlist(strsplit(x, split='_', fixed=TRUE))[2],
         Site = unlist(strsplit(x, split='_', fixed=TRUE))[3],
         Site = unlist(strsplit(Site, split='.', fixed=TRUE))[1]) %>%
  ungroup()
fp2$Site <- as.numeric(fp2$Site)

#check to make sure strsplit function worked for all casts
check <- subset(fp2, is.na(fp2$Site))
unique(fp2$Reservoir)
unique(fp2$Site)
  
# Rename and select useful columns; drop metrics we don't use or publish such as cell count;
# eliminate shallow depths because of quenching
fp3 <- fp2 %>%
  mutate(DateTime = `Date/Time`, GreenAlgae_ugL = as.numeric(`Green Algae`), Bluegreens_ugL = as.numeric(`Bluegreen`),
         BrownAlgae_ugL = as.numeric(`Diatoms`), MixedAlgae_ugL = as.numeric(`Cryptophyta`), YellowSubstances_ugL = as.numeric(`Yellow substances`),
         TotalConc_ugL = as.numeric(`Total conc.`), Transmission = as.numeric(`Transmission`), Depth_m = as.numeric(`Depth`), Temp_degC = as.numeric(`Temp. Sample`),
         RFU_525nm = as.numeric(`LED 3 [525 nm]`), RFU_570nm = as.numeric(`LED 4 [570 nm]`), RFU_610nm = as.numeric(`LED 5 [610 nm]`),
         RFU_370nm = as.numeric(`LED 6 [370 nm]`), RFU_590nm = as.numeric(`LED 7 [590 nm]`), RFU_470nm = as.numeric(`LED 8 [470 nm]`),
         Pressure_unit = as.numeric(`Pressure`)) %>%
  select(x,cast, Reservoir, Site, DateTime, GreenAlgae_ugL, Bluegreens_ugL, BrownAlgae_ugL, MixedAlgae_ugL, YellowSubstances_ugL,
         TotalConc_ugL, Transmission, Depth_m, Temp_degC, RFU_525nm, RFU_570nm, RFU_610nm,
         RFU_370nm, RFU_590nm, RFU_470nm) %>%
  mutate(DateTime = as.POSIXct(as_datetime(DateTime, tz = "", format = "%m/%d/%Y %I:%M:%S %p"))) %>%
  filter(Depth_m >= 0.2) 

# #eliminate upcasts if they exist; this can also be done manually as .txt files
# #are uploaded each field day
# fp_downcasts <- fp3[0,]
# upcasts <- c("20160617_FCR_50.txt","20180907_BVR_50.txt")
# 
# for (i in 1:length(unique(fp3$cast))){
#   
#   if(unique(fp3$cast)[i] %in% upcasts){}else{
#   profile = subset(fp3, cast == unique(fp3$cast)[i])
#   
#   bottom <- max(profile$Depth_m)
#   
#   idx <- which( profile$Depth_m == bottom ) 
#   if( length( idx ) > 0L ) profile <- profile[ seq_len( idx ) , ]
#   
#   fp_downcasts <- bind_rows(fp_downcasts, profile)
#   }
#   
# }

#create png plots for every cast for QAQC purposes (algal biomass)
#these are just written to a file I temporarily create on my desktop
#for EDI day
for (i in 1:length(unique(fp3$cast))){
  profile = subset(fp3, cast == unique(fp3$cast)[i])
  castname = profile$x[1]
  profile2 = profile %>%
    select(Depth_m, GreenAlgae_ugL, Bluegreens_ugL, BrownAlgae_ugL, MixedAlgae_ugL, TotalConc_ugL)%>%
    gather(GreenAlgae_ugL:TotalConc_ugL, key = spectral_group, value = ugL)
  profile_plot <- ggplot(data = profile2, aes(x = ugL, y = Depth_m, group = spectral_group, colour = spectral_group))+
    geom_path(size = 1)+
    scale_y_reverse()+
    ggtitle(castname)+
    theme_bw()
  filename = paste0("C:/Users/Mary Lofton/Desktop/FP_plots_2020/",castname,".png")
  ggsave(filename = filename, plot = profile_plot, device = "png")

}

#create png plots for every cast for QAQC purposes (temperature)
#these are just written to a file I temporarily create on my desktop
#for EDI day
for (i in 1:length(unique(fp3$cast))){
  profile = subset(fp3, cast == unique(fp3$cast)[i])
  castname = profile$x[1]
  profile_plot <- ggplot(data = profile, aes(x = Temp_degC, y = Depth_m))+
    geom_path(size = 1)+
    scale_y_reverse()+
    ggtitle(castname)+
    theme_bw()
  filename = paste0("C:/Users/Mary Lofton/Desktop/FP_temp_plots_2020/",castname,".png")
  ggsave(filename = filename, plot = profile_plot, device = "png")
  
}


#create png plots for every cast for QAQC purposes (transmission)
for (i in 1:length(unique(fp3$cast))){
  profile = subset(fp3, cast == unique(fp3$cast)[i])
  castname = profile$x[1]
  profile_plot <- ggplot(data = profile, aes(x = Transmission, y = Depth_m))+
    geom_path(size = 1)+
    scale_y_reverse()+
    ggtitle(castname)+
    theme_bw()
  filename = paste0("C:/Users/Mary Lofton/Desktop/FP_trans_plots_2020/",castname,".png")
  ggsave(filename = filename, plot = profile_plot, device = "png")
  
}

#QAQC note that most of the 20200604_BVR_50 cast is missing so am
#including upcast to give a cursory sense of what is happening in rest
#of water column

#20200824_FCR_50 temp cast problematic (max of 18-20 degrees?)
bad_temp_casts <- c("20200824_FCR_50.txt")

fp4 <- fp3 %>%
  mutate(Temp_degC = ifelse(x %in% bad_temp_casts,NA,Temp_degC))

check <- fp4 %>%
  filter(x %in% bad_temp_casts)

#if need to alter any of the transmission data, do so here
#not necessary for 2020
# fp5 <- fp4 %>%
#   mutate(Transmission = ifelse(date(DateTime)>= "2019-08-28",NA,Transmission))
fp5 <- fp4

#merge two datasets - previously published data package + this year's data

#read in old data and check column names of old and new and DateTime format
fp_og <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFluoroProbe/2019/FluoroProbe.csv")

#convert time zone of old data; usually defaults to UTC when read in
attr(fp_og$DateTime, "tzone") <- "America/New_York"

#check that time zones are correct (EDT and EST for both old and new)
head(fp5$DateTime)
tail(fp5$DateTime)
head(fp_og$DateTime)
check <- subset(fp_og, Reservoir == "FCR")
tail(check$DateTime)

#check column names
colnames(fp_og)
colnames(fp5)

#get rid of columns we don't need for final publication
fp6 <- fp5 %>%
  select(-x, -cast) %>%
  mutate(Site = as.numeric(Site))

colnames(fp6)

#reorder columns to match current EDI data package
fp7 <- fp6[,c(1,2,3,11,4,5,6,7,8,9,10,12,13,14,15,16,17,18)]

colnames(fp7)

#ADD FLAGS

#2020: no need to flag algal profiles at this time
#2020: need to flag temp profile from 08-24 that is missing due to too-cold temp readings
#(best guess is probe was not given time to equilibrate before cast was taken)
#2020: no need to flag transmission at this time
bad_temp_days <- c("2020-08-24")

fp8 <- fp7 %>%
  mutate(Flag_GreenAlgae = 0,
         Flag_BluegreenAlgae = 0,
         Flag_BrownAlgae = 0,
         Flag_MixedAlgae = 0,
         Flag_TotalConc = 0,
         Flag_Temp = ifelse(date(DateTime) %in% bad_temp_days,2,0),
         Flag_Transmission = 0,
         Flag_525nm = 0,
         Flag_570nm = 0,
         Flag_610nm = 0,
         Flag_370nm = 0,
         Flag_590nm = 0,
         Flag_470nm = 0)

colnames(fp8)

#final merge!
fp_final <- bind_rows(fp_og, fp8) %>%
  arrange(Reservoir, Site, DateTime)

#write the csv for publication!
write.csv(fp_final, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFluoroProbe/2020/FluoroProbe.csv", row.names = FALSE)
fp <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFluoroProbe/2020/FluoroProbe.csv")

#Congrats you are done! Go have a cookie :-)

  