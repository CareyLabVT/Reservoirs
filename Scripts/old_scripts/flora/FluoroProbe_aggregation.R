#Title: Script for aggregating FP txt files for publication to EDI
#Author: Mary Lofton
#Date: 13DEC18

rm(list=ls())

# load packages
#install.packages('pacman')
pacman::p_load(tidyverse, lubridate)

# Load in column names for .txt files 
col_names <- names(read_tsv("./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/20190208_FCR_50.txt", n_max = 0))

# Load in txt files
## NEED TO FIGURE OUT HOW TO ADD FILENAME AS A COLUMN SO CAN SPLIT UP BY CAST
fp_casts <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe", pattern = paste0("*.txt")) %>%
  map_df(~ data_frame(x = .x), .id = "cast")

raw_fp <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe", pattern = paste0("*.txt")) %>% 
  map_df(~ read_tsv(file.path(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe", .), 
                    col_types = cols(.default = "c"), col_names = col_names, skip = 2), .id = "cast")

fp2 <- left_join(raw_fp, fp_casts, by = c("cast")) %>%
  rowwise() %>% 
  mutate(Reservoir = unlist(strsplit(x, split='_', fixed=TRUE))[2],
         Site = unlist(strsplit(x, split='_', fixed=TRUE))[3],
         Site = unlist(strsplit(Site, split='.', fixed=TRUE))[1],
         Reservoir = ifelse(x == "20190710__BVR_50_MSN_1138.txt","BVR",Reservoir),
         Site = ifelse(x == "20190710__BVR_50_MSN_1138.txt","50",Site)) %>%
  ungroup()
fp2$Site <- as.numeric(fp2$Site)

check <- subset(fp2, is.na(fp2$Site))
unique(fp2$Reservoir)
unique(fp2$Site)
  
# Rename and select useful columns; drop metrics we don't like such as cell count;
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

# #eliminate upcasts 
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

#look at casts
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
  filename = paste0("C:/Users/Mary Lofton/Desktop/FP_plots_2019/",castname,".png")
  ggsave(filename = filename, plot = profile_plot, device = "png")

}

##look at temperature data
for (i in 1:length(unique(fp3$cast))){
  profile = subset(fp3, cast == unique(fp3$cast)[i])
  castname = profile$x[1]
  profile_plot <- ggplot(data = profile, aes(x = Temp_C, y = Depth_m))+
    geom_path(size = 1)+
    scale_y_reverse()+
    ggtitle(castname)+
    theme_bw()
  filename = paste0("C:/Users/Mary Lofton/Desktop/FP_temp_plots_2019/",castname,".png")
  ggsave(filename = filename, plot = profile_plot, device = "png")
  
}

##look at transmission data
for (i in 1:length(unique(fp3$cast))){
  profile = subset(fp3, cast == unique(fp3$cast)[i])
  castname = profile$x[1]
  profile_plot <- ggplot(data = profile, aes(x = Transmission, y = Depth_m))+
    geom_path(size = 1)+
    scale_y_reverse()+
    ggtitle(castname)+
    theme_bw()
  filename = paste0("C:/Users/Mary Lofton/Desktop/FP_trans_plots_2019/",castname,".png")
  ggsave(filename = filename, plot = profile_plot, device = "png")
  
}

#need to get rid of temp data between 02SEP and 11SEP because calibration was bad
bad_temp_casts <- c("20190902_FCR_50.txt","20190904_BVR_50.txt","20190911_FCR_50.txt")

fp4 <- fp3 %>%
  mutate(Temp_degC = ifelse(x %in% bad_temp_casts,NA,Temp_degC))

check <- fp4 %>%
  filter(x %in% bad_temp_casts)



#merge two datasets
fp_og <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFluoroProbe/2014-2018/FluoroProbe.csv")
colnames(fp_og)
colnames(fp4)
head(fp4$DateTime)

fp5 <- fp4 %>%
  select(-x, -cast) %>%
  mutate(Site = as.numeric(Site))

colnames(fp5)

fp6 <- fp5[,c(1,2,3,11,4,5,6,7,8,9,10,12,13,14,15,16,17,18)]

colnames(fp6)


#need to flag transmission data after 28Aug19 because just reads 100% all the time
#attr(fp_final$DateTime, "tzone") <- "America/New_York"

#2019: need to flag transmission data after 28Aug19 because just reads 100% all the time
bad_temp_days <- c("2019-09-02","2019-09-04","2019-09-11")

fp7 <- fp6 %>%
  mutate(Flag_GreenAlgae = 0,
         Flag_BluegreenAlgae = 0,
         Flag_BrownAlgae = 0,
         Flag_MixedAlgae = 0,
         Flag_TotalConc = 0,
         Flag_Temp = ifelse(date(DateTime) %in% bad_temp_days,2,0),
         Flag_Transmission = ifelse(date(DateTime)>= "2019-08-28",2,0),
         Flag_525nm = 0,
         Flag_570nm = 0,
         Flag_610nm = 0,
         Flag_370nm = 0,
         Flag_590nm = 0,
         Flag_470nm = 0)

colnames(fp7)

fp_final <- bind_rows(fp_og, fp7) %>%
  arrange(Reservoir, Site, DateTime)


write.csv(fp_final, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFluoroProbe/2019/FluoroProbe.csv", row.names = FALSE)
fp <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFluoroProbe/2019/FluoroProbe.csv")



  