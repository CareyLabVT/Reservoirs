##Flora/EXO overlap
##Author: Mary Lofton
##Date: 08JUL19

#load packages
pacman::p_load(tidyverse, lubridate)
source("C:/Users/Mary Lofton/Documents/RScripts/ggplot_smooth_func.R")

#read in EXO data
exo <- read_csv("./Ch3_submodel/Catwalk.csv", skip = 1)%>%
  select(TIMESTAMP, Chla_1, BGAPC_1) %>%
  filter(Chla_1 != "NAN")
exo <- exo[-c(1,2),]
exo <- exo %>%
  mutate(Day = date(TIMESTAMP),
         Hour = hour(TIMESTAMP)) %>%
  select(-TIMESTAMP)
exo$Chla_1 <- as.numeric(exo$Chla_1)
exo$BGAPC_1 <- as.numeric(exo$BGAPC_1)
exo <- aggregate(cbind(Chla_1, BGAPC_1) ~ Day + Hour, data = exo, mean) %>%
  mutate(Depth_m = 1)

#read in FP data
# Load .txt files for appropriate reservoir 
Reservoir = 'FCR'

#NOTE: this script is not currently set up to handle upstream sites in FCR
col_names <- names(read_tsv("C:/Users/Mary Lofton/Documents/Github/Reservoirs/Data/DataAlreadyUploadedToEDI/CollatedDataForEDI/FluoroProbeData/20180410_FCR_50.txt", n_max = 0))

raw_fp <- dir(path = "C:/Users/Mary Lofton/Documents/Github/Reservoirs/Data/DataAlreadyUploadedToEDI/CollatedDataForEDI/FluoroProbeData", pattern = paste0("*_",Reservoir,"_50.txt")) %>% 
  map_df(~ read_tsv(file.path(path = "C:/Users/Mary Lofton/Documents/Github/Reservoirs/Data/DataAlreadyUploadedToEDI/CollatedDataForEDI/FluoroProbeData", .), col_types = cols(.default = "c"), col_names = col_names, skip = 2))

raw_fp1 <- dir(path = "C:/Users/Mary Lofton/Documents/Github/Reservoirs/Data/DataNotYetUploadedToEDI/Raw_fluoroprobe", pattern = paste0("*_",Reservoir,"_50.txt")) %>% 
  map_df(~ read_tsv(file.path(path = "C:/Users/Mary Lofton/Documents/Github/Reservoirs/Data/DataNotYetUploadedToEDI/Raw_fluoroprobe", .), col_types = cols(.default = "c"), col_names = col_names, skip = 2))

raw_fp2 <- bind_rows(raw_fp, raw_fp1)

fp <- raw_fp2 %>%
  mutate(DateTime = `Date/Time`, GreenAlgae_ugL = as.numeric(`Green Algae`), Bluegreens_ugL = as.numeric(`Bluegreen`),
         Browns_ugL = as.numeric(`Diatoms`), Mixed_ugL = as.numeric(`Cryptophyta`), YellowSubstances_ugL = as.numeric(`Yellow substances`),
         TotalConc_ugL = as.numeric(`Total conc.`), Transmission_perc = as.numeric(`Transmission`), Depth_m = `Depth`) %>%
  select(DateTime, GreenAlgae_ugL, Bluegreens_ugL, Browns_ugL, Mixed_ugL, YellowSubstances_ugL,
         TotalConc_ugL, Transmission_perc, Depth_m) %>%
  mutate(DateTime = as.POSIXct(as_datetime(DateTime, tz = "", format = "%m/%d/%Y %I:%M:%S %p"))) %>%
  mutate(Day = date(DateTime), Hour = hour(DateTime)) %>%
  mutate(Depth_m = ifelse(Depth_m <= 2 & Depth_m >= 0.5, 1, Depth_m))
fp$Depth_m <- as.numeric(fp$Depth_m)

fp <- aggregate(cbind(GreenAlgae_ugL, Bluegreens_ugL, Browns_ugL, Mixed_ugL, TotalConc_ugL) ~ Day + Hour + Depth_m, data = fp, mean) 

#combine EXO and FP data
overlap <- left_join(fp, exo, by = c("Day","Hour","Depth_m")) %>%
  filter(!is.na(Chla_1))

#plotting theme
mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 legend.key = element_blank(),legend.background = element_blank(),
                 text = element_text(size=14), axis.text = element_text(size = 16))

#run overlap between logical combinations of vars
plot1 <- ggplot(data = overlap, aes(x = BGAPC_1*10, y = Bluegreens_ugL))+
  geom_point(size = 3)+
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, size = 5)+
  geom_abline(slope = 1, intercept = 0, size = 1)+
  geom_smooth(method='lm',formula=y~x, se = FALSE, size = 1, colour = "cyan4")+
  xlab("Cyanobacteria (EXO sonde) x10!!!!! - ug/L")+
  ylab("Cyanobacteria (FluoroProbe) - ug/L")+
  mytheme
plot1
ggsave(plot1, filename = "./Ch3_submodel/cyanobacteria.png",device = "png",
       height = 4, width = 6, units = "in")

plot2 <- ggplot(data = overlap, aes(x = Chla_1, y = GreenAlgae_ugL))+
  geom_point(size = 3)+
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, size = 5)+
  geom_abline(slope = 1, intercept = 0, size = 1)+
  geom_smooth(method='lm',formula=y~x, se = FALSE, size = 1, colour = "springgreen3")+
  xlab("Chl-a (EXO sonde) - ug/L")+
  ylab("Green Algae (FluoroProbe) - ug/L")+
  mytheme
plot2
ggsave(plot2, filename = "./Ch3_submodel/green_algae.png",device = "png",
       height = 4, width = 6, units = "in")

plot3 <- ggplot(data = overlap, aes(x = Chla_1, y = TotalConc_ugL))+
  geom_point(size = 3)+
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, size = 5)+
  geom_abline(slope = 1, intercept = 0, size = 1)+
  geom_smooth(method='lm',formula=y~x, se = FALSE, size = 1, colour = "darkgreen")+
  xlab("Chl-a (EXO sonde) - ug/L")+
  ylab("Total Phytoplankton (FluoroProbe) - ug/L")+
  mytheme
plot3
ggsave(plot3, filename = "./Ch3_submodel/total_phytos.png",device = "png",
       height = 4, width = 6, units = "in")

plot4 <- ggplot(data = overlap, aes(x = Chla_1, y = Browns_ugL))+
  geom_point(size = 3)+
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, size = 5)+
  geom_abline(slope = 1, intercept = 0, size = 1)+
  geom_smooth(method='lm',formula=y~x, se = FALSE, size = 1, colour = "goldenrod4")+
  xlab("Chl-a (EXO sonde) - ug/L")+
  ylab("Brown Algae (FluoroProbe) - ug/L")+
  mytheme
plot4
ggsave(plot4, filename = "./Ch3_submodel/brown_algae.png",device = "png",
       height = 4, width = 6, units = "in")

plot5 <- ggplot(data = overlap, aes(x = Chla_1, y = Mixed_ugL))+
  geom_point(size = 3)+
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, size = 5)+
  geom_abline(slope = 1, intercept = 0, size = 1)+
  geom_smooth(method='lm',formula=y~x, se = FALSE, size = 1, colour = "purple")+
  xlab("Chl-a (EXO sonde) - ug/L")+
  ylab("Cryptophytes (FluoroProbe) - ug/L")+
  mytheme
plot5
ggsave(plot5, filename = "./Ch3_submodel/cryptophytes.png",device = "png",
       height = 4, width = 6, units = "in")

#plot to illustrate extreme event
fp1 <- fp %>% filter(Depth_m == 1)
bloom <- ggplot(data = fp1, aes(x = Day, y = TotalConc_ugL))+
  geom_line(color = "black", size = 1.5)+
  xlab("")+
  ylab("Algae Biomass (ugL)")+
  ylim(c(0,30))+
  geom_smooth(method = "loess", lty = 2, size = 2, colour = "red")+
  mytheme
bloom  

#plot to illustrate depth profile
fp2 <- fp %>% filter(Day == "2018-07-19") %>%
  select(-YellowSubstances_ugL) %>%
  gather(GreenAlgae_ugL:TotalConc_ugL, key = "Spectral_group", value = "ugL")
  
profile <- ggplot(data = fp2, aes(x = ugL, y = -Depth_m, group = Spectral_group, 
                                  colour = Spectral_group))+
  geom_path(size = 1)+
  geom_hline(yintercept=-0.1,size=1.1)+
  geom_hline(yintercept=-0.8,size=1.1)+
  geom_hline(yintercept=-1.6,size=1.1)+
  geom_hline(yintercept=-3.8,size=1.1)+
  geom_hline(yintercept=-5.0,size=1.1)+
  geom_hline(yintercept=-6.2,size=1.1)+
  xlab("Biomass (ug/L)")+
  ylab("Depth (m)")+
  mytheme
profile

#plot to illustrate data streams
x <- c(1:100)
y <- 1+runif(100, -0.5, 0.5)
dat <- data.frame(cbind(x,y))
hist(fp)

stream <- ggplot(data = dat, aes(x = x, y = y))+
  geom_line(colour = "springgreen3", size = 1.5)+
  xlab("Time")+
  ylab("Depth")+
  scale_y_reverse(limits=c(10,0))+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  mytheme
stream
