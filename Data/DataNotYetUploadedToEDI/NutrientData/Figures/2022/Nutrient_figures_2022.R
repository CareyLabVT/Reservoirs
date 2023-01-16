#Script to visualize 2022 collated TN/TP, np, and DOC data 

#install.packages('pacman')
pacman::p_load(tidyverse, lubridate, scales, viridis)

#load in collated 2019 data 
raw_chem <- read.csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2022/Data/2022_chemistry_collation_final_nocommas.csv")

#date format
raw_chem$DateTime <- as.Date(raw_chem$DateTime, "%Y-%m-%d %H:%M:%S")

#select columns for plotting
raw_chem <- raw_chem [,(names(raw_chem) %in% c("Reservoir","Site","DateTime",
  "Depth_m","Rep","TP_ugL","TN_ugL","NH4_ugL","SRP_ugL","NO3NO2_ugL","DOC_mgL"))]

  #### Chemistry diagnostic plots ####
chemistry_long <- raw_chem %>% 
  gather(metric, value, TP_ugL:DOC_mgL) %>% #DOC_mgL
  mutate(month = strftime(DateTime, "%b"))

chemistry_long$value <- as.numeric(chemistry_long$value)


# Plot range of values per constituent for each reservoir; 
#mean value per sampling day indicated with large black dot
ggplot(subset(chemistry_long, Site == 50), aes(x = DateTime, y = value, col=Reservoir)) +
  geom_point(size=1) +
  stat_summary(fun.y="mean", geom="point",pch=21,  size=3, fill='black') +
  facet_grid(metric ~ Reservoir, scales= 'free_y') +
  scale_x_date(labels = date_format("%d %b")) +
  scale_y_continuous("Concentration at site 50") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='none')
ggsave(file.path(getwd(),"./Data/DataNotYetUploadedToEDI/NutrientData/Figures/2022/FCR_BVR_CCR_all_nutrients_summer2022.jpg"), width=3.5, height=4)

#----------------------------------------------------------------------------------------------------#
# FCR deep hole data time series
ggplot(subset(chemistry_long, Reservoir=='FCR' & Site=="50"), aes(x = DateTime, y = value, col=as.factor(Depth_m))) +
  geom_point(cex=2) + theme_bw()+
  facet_grid(metric ~ ., scales='free') +
  scale_x_date("Date", date_breaks="1 month", date_labels = "%d %b") +
  scale_y_continuous("Concentration at site 50") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_manual("FCR_Depth (m)", values = viridis(10)) 
  #geom_vline(xintercept=as.numeric(as.Date(c("2021-06-11", "2021-07-12", "2021-07-26", "2022-03-22")))) +
  #geom_vline(xintercept=as.numeric(as.Date(c("2021-06-26", "2021-07-14","2021-12-06", "2022-03-24"))), linetype="dotted")
ggsave(file.path(getwd(),"./Data/DataNotYetUploadedToEDI/NutrientData/Figures/2022/FCR_all_nutrients_summer2022.jpg"), width=3.5, height=4)


# FCR other sites nutrient data time series
ggplot(subset(chemistry_long, Reservoir=='FCR' & Site!="50"), aes(x = DateTime, y = value, col=as.factor(Site))) +
  geom_point(cex=2) + theme_bw() +
  facet_grid(metric ~ ., scales='free') +
  scale_x_date("Date", date_breaks="1 month", date_labels = "%d %b") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_color_manual("FCR_Site", values = rainbow(10)) 
ggsave(file.path(getwd(),"./Data/DataNotYetUploadedToEDI/NutrientData/Figures/2022/FCR_sites_all_nutrients_summer2022.jpg"), width=3.5, height=4)


# BVR deep hole data time series
ggplot(subset(chemistry_long, Reservoir=='BVR' & Site=="50"), aes(x = DateTime, y = value, col=as.factor(Depth_m))) +
  geom_point(cex=2) + theme_bw() +
  facet_grid(metric ~ ., scales='free') +
  scale_x_date("Date", date_breaks="1 month", date_labels = "%d %b") +
  scale_y_continuous("Concentration at site 50") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_color_manual("BVR_Depth (m)", values = plasma(10)) 
ggsave(file.path(getwd(),"./Data/DataNotYetUploadedToEDI/NutrientData/Figures/2022/BVR_all_nutrients_summer2022.jpg"), width=3.5, height=4)


# BVR other sites nutrient time series #no 2021 inflow samples so no figure :)
#jpeg("./Figures/BVR_sites_all_nutrients_summer2021.jpg", width = 6, height = 5, units = "in",res = 300)
#ggplot(subset(chemistry_long, Reservoir=='BVR' & Site!="50"), aes(x = DateTime, y = value, col=as.factor(Site))) +
#  geom_point(cex=2) + theme_bw() +
#  facet_grid(metric ~ ., scales='free') +
#  scale_x_date("Date", date_labels = "%d %b") +
#  scale_y_continuous("Concentration") +
#  theme(axis.text.x = element_text(angle = 0, hjust=0.5)) + 
#  scale_color_manual("BVR_Site", values = plasma(8)) 
#dev.off()

# CCR site 50 nutrient time series
#jpeg("./Figures/2021/CCR_50_all_nutrients_summer2021.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(chemistry_long, Reservoir=='CCR' & Site=="50"), aes(x = DateTime, y = value, col=as.factor(Depth_m))) +
  geom_point(cex=2) + theme_bw() +
  facet_grid(metric ~ ., scales='free') +
  scale_x_date("Date", date_breaks="1 month", date_labels = "%d %b") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_color_manual("CCR_Depth (m)", values = plasma(7)) 
ggsave(file.path(getwd(),"./Data/DataNotYetUploadedToEDI/NutrientData/Figures/2022/CCR_50_all_nutrients_summer2022.jpg"), width=3.5, height=4)


# CCR other sites nutrient time series
ggplot(subset(chemistry_long, Reservoir=='CCR' & Site!="50"), aes(x = DateTime, y = value, col=as.factor(Site))) +
  geom_point(cex=2) + theme_bw() +
  facet_grid(metric ~ ., scales='free') +
  scale_x_date("Date", date_breaks="1 month", date_labels = "%d %b") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_color_manual("CCR_Site", values = plasma(5)) 
ggsave(file.path(getwd(),"./Data/DataNotYetUploadedToEDI/NutrientData/Figures/2022/CCR_sites_all_nutrients_summer2022.jpg"), width=3.5, height=4)


