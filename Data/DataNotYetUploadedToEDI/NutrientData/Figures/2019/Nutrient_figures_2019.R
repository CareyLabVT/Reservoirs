#Script to visualize collated TN/TP and DOC data 
#17 Apr 2020

#install.packages('pacman')
pacman::p_load(tidyverse, lubridate, scales, viridis)

#load in collated 2019 data 
raw_chem <- read.csv("/Users/heatherwander/Documents/VirginiaTech/research/2019 nutrient data collation/2019_chemistry_collation_final.csv")

#date format
raw_chem$DateTime <- as.Date(raw_chem$DateTime) 
#raw_chem$RunDate <- as.Date(raw_chem$RunDate, "%m/%d/%y") 
#raw_chem$RunDate_DOC <- as.Date(raw_chem$RunDate_DOC) 

#change rightarmwest to 175, rightarmeast to 215, and remove s and s 0 s site names
raw_chem$Site<- ifelse(raw_chem$Site=="rightarmwest",175,
                       ifelse(raw_chem$Site=="rightarmeast",215,
                              ifelse(raw_chem$Site=="s", NA,
                                     ifelse(raw_chem$Site=="s 0 s",NA, paste(raw_chem$Site)))))

#select columns for plotting
raw_chem <- raw_chem [,(names(raw_chem) %in% c("Reservoir","Site","DateTime",
  "Depth_m","Rep","TP_ugL","TN_ugL","NH4_ugL","SRP_ugL","NO3NO2_ugL","DOC_mgL"))]
                             
  
  #### Chemistry diagnostic plots ####
chemistry_long <- raw_chem %>% 
  gather(metric, value, TP_ugL:DOC_mgL) %>% 
  mutate(month = strftime(DateTime, "%b"))

chemistry_long$value <- as.numeric(chemistry_long$value)

# Plot range of values per constituent for each reservoir; 
#mean value per sampling day indicated with large black dot
#jpeg("FCR_and_BVR_all_nutrients_summer2019.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(chemistry_long, Site == 50), aes(x = DateTime, y = value, col=Reservoir)) +
  geom_point(size=1) +
  stat_summary(fun.y="mean", geom="point",pch=21,  size=3, fill='black') +
  facet_grid(metric ~ Reservoir, scales= 'free_y') +
  scale_x_date(labels = date_format("%d %b")) +
  scale_y_continuous("Concentration at site 50") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='none')
#dev.off()

#jpeg("FCR_soluble_all_nutrients_summer2019.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(chemistry_long, Reservoir=='FCR' & Site=="50"), aes(x = DateTime, y = value, col=as.factor(Depth_m))) +
  geom_point(cex=2) + theme_bw()+
  facet_grid(metric ~ ., scales='free') +
  scale_x_date("Date", date_breaks="1 month", date_labels = "%d %b") +
  scale_y_continuous("Concentration at site 50") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_manual("FCR_Depth (m)", values = viridis(10)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2019-06-03", "2019-07-08", "2019-08-05", "2019-09-02")))) +
  geom_vline(xintercept=as.numeric(as.Date(c("2019-06-17", "2019-07-22", "2019-08-19"))), linetype="dotted")
#dev.off()

# BVR deep hole data time series
#jpeg("BVR_soluble_all_nutrients_summer2019.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(chemistry_long, Reservoir!='FCR' & Site=="50"), aes(x = DateTime, y = value, col=as.factor(Depth_m))) +
  geom_point(cex=2) + theme_bw() +
  facet_grid(metric ~ ., scales='free') +
  scale_x_date("Date", date_breaks="1 month", date_labels = "%d %b") +
  scale_y_continuous("Concentration at site 50") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_color_manual("BVR_Depth (m)", values = plasma(8), breaks=c("0.1","3","6","8","9","10","10.5","11")) 
#dev.off()

# BVR inflows only (100,175,200,215)
#jpeg("BVR_inflows_all_nutrients_summer2019.jpg", width = 6, height = 5, units = "in",res = 300)
ggplot(subset(chemistry_long, Reservoir=='BVR'& Site %in% c(100,175,200,215)), aes(x = DateTime, y = value, col=Site)) +
  geom_point(cex=2) + theme_bw() +
  facet_grid(metric ~ ., scales='free') +
  scale_x_date("Date", date_breaks="2 months", date_labels = "%b") +
  scale_y_continuous("Concentration") +
  scale_color_manual("BVR_Site", values = plasma(4), breaks=c("1","100","175","200","215"))
#dev.off()

