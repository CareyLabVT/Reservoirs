#Script to visualize 2020 collated TN/TP, np, and DOC data 

#install.packages('pacman')
pacman::p_load(tidyverse, lubridate, scales, viridis)

#load in collated 2019 data 
raw_chem <- read.csv("./FinalData/2020_chemistry_collation_final_nocommas.csv")

#date format
raw_chem$DateTime <- as.Date(raw_chem$DateTime, "%Y-%m-%d %H:%M:%S")

#change rightarmwest to 175, rightarmeast to 215, and remove s and s 0 s site names
#raw_chem$Site<- ifelse(raw_chem$Site=="rightarmwest",175,
#                       ifelse(raw_chem$Site=="rightarmeast",215,
 #                             ifelse(raw_chem$Site=="s", NA,
  #                                   ifelse(raw_chem$Site=="s 0 s",NA, paste(raw_chem$Site)))))

#select columns for plotting
raw_chem <- raw_chem [,(names(raw_chem) %in% c("Reservoir","Site","DateTime",
  "Depth_m","Rep","TP_ugL","TN_ugL","NH4_ugL","SRP_ugL","NO3NO2_ugL","DOC_mgL"))]
                             
  
  #### Chemistry diagnostic plots ####
chemistry_long <- raw_chem %>% 
  gather(metric, value, TP_ugL:DOC_mgL) %>% 
  mutate(month = strftime(DateTime, "%b"))

chemistry_long$value <- as.numeric(chemistry_long$value)

#only plot 2019 data
#jpeg("./Figures/FCR_0.1m_nutrient_reruns_summer2019.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(chemistry_long, substr(chemistry_long$DateTime,1,4)=="2019"),aes(x = DateTime, y = value, col=as.factor(Site))) +
  geom_point(cex=2) + theme_bw()+
  facet_grid(metric ~ ., scales='free') +
  scale_x_date("Date", date_breaks="1 month", date_labels = "%d %b") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) 
#dev.off()

# Plot range of values per constituent for each reservoir; 
#mean value per sampling day indicated with large black dot
#jpeg("./Figures/FCR_and_BVR_all_nutrients_summer2020.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(chemistry_long, Site == 50 & substr(chemistry_long$DateTime,1,4)=="2020"), aes(x = DateTime, y = value, col=Reservoir)) +
  geom_point(size=1) +
  stat_summary(fun.y="mean", geom="point",pch=21,  size=3, fill='black') +
  facet_grid(metric ~ Reservoir, scales= 'free_y') +
  scale_x_date(labels = date_format("%d %b")) +
  scale_y_continuous("Concentration at site 50") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='none')
#dev.off()

#----------------------------------------------------------------------------------------------------#
# FCR deep hole data time series
#jpeg("./Figures/FCR_all_nutrients_summer2020.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(chemistry_long, Reservoir=='FCR' & Site=="50" & substr(chemistry_long$DateTime,1,4)=="2020"), aes(x = DateTime, y = value, col=as.factor(Depth_m))) +
  geom_point(cex=2) + theme_bw()+
  facet_grid(metric ~ ., scales='free') +
  scale_x_date("Date", date_breaks="1 month", date_labels = "%d %b") +
  scale_y_continuous("Concentration at site 50") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_manual("FCR_Depth (m)", values = viridis(10)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-06-29", "2020-07-13", "2020-07-23", "2020-09-25")))) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-07-12", "2020-07-22", "2020-09-11","2020-12-2"))), linetype="dotted")
#dev.off()

# FCR other sites nutrient data time series
#jpeg("./Figures/FCR_sites_all_nutrients_summer2020.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(chemistry_long, Reservoir=='FCR' & Site!="50" & substr(chemistry_long$DateTime,1,4)=="2020"), aes(x = DateTime, y = value, col=as.factor(Site))) +
  geom_point(cex=2) + theme_bw() +
  facet_grid(metric ~ ., scales='free') +
  scale_x_date("Date", date_breaks="1 month", date_labels = "%d %b") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_color_manual("FCR_Site", values = rainbow(10)) 
#dev.off()

# BVR deep hole data time series
#jpeg("./Figures/BVR_all_nutrients_summer2020.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(chemistry_long, Reservoir=='BVR' & Site=="50" & substr(chemistry_long$DateTime,1,4)=="2020"), aes(x = DateTime, y = value, col=as.factor(Depth_m))) +
  geom_point(cex=2) + theme_bw() +
  facet_grid(metric ~ ., scales='free') +
  scale_x_date("Date", date_breaks="1 month", date_labels = "%d %b") +
  scale_y_continuous("Concentration at site 50") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_color_manual("BVR_Depth (m)", values = plasma(10)) 
#dev.off()

# BVR other sites nutrient time series
#jpeg("./Figures/BVR_sites_all_nutrients_summer2020.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(chemistry_long, Reservoir=='BVR' & Site!="50" & substr(chemistry_long$DateTime,1,4)=="2020"), aes(x = DateTime, y = value, col=as.factor(Site))) +
  geom_point(cex=2) + theme_bw() +
  facet_grid(metric ~ ., scales='free') +
  scale_x_date("Date", date_labels = "%d %b") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 0, hjust=0.5)) + 
  scale_color_manual("BVR_Site", values = plasma(8)) 
#dev.off()

# CCR site 50 nutrient time series
#jpeg("./Figures/CCR_50_all_nutrients_summer2020.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(chemistry_long, Reservoir=='CCR' & Site=="50" & substr(chemistry_long$DateTime,1,4)=="2020"), aes(x = DateTime, y = value, col=as.factor(Depth_m))) +
  geom_point(cex=2) + theme_bw() +
  facet_grid(metric ~ ., scales='free') +
  scale_x_date("Date", date_breaks="1 month", date_labels = "%d %b") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_color_manual("CCR_Depth (m)", values = plasma(8)) 
#dev.off()

# CCR other sites nutrient time series
#jpeg("./Figures/CCR_sites_all_nutrients_summer2020.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(chemistry_long, Reservoir=='CCR' & Site!="50" & substr(chemistry_long$DateTime,1,4)=="2020"), aes(x = DateTime, y = value, col=as.factor(Site))) +
  geom_point(cex=2) + theme_bw() +
  facet_grid(metric ~ ., scales='free') +
  scale_x_date("Date", date_breaks="1 month", date_labels = "%d %b") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_color_manual("CCR_Site", values = plasma(8)) 
#dev.off()

