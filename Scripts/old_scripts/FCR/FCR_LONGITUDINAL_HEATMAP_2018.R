# FCR longitudinal CTD Heatmaps
# Author: Mary Lofton riffing off of Ryan McClure
# Date last updated: 04SEP18


#########################
#NOTE: This is a template draft that you will need to modify for your own purposes.
#i.e., set your own working directory, pull the appropriate data, etc.

# Makes heatmaps of the CTD data in Falling Creek reservoir

#install.packages('pacman')
pacman::p_load(tidyverse, lubridate, akima, reshape2, pracma,
               gridExtra, grid, colorRamps,RColorBrewer, rLakeAnalyzer, cowplot)

# Set the WD 
setwd("C:/Users/Mary Lofton/Documents/Ch_1/New_heatmaps")

# load in appended CTD data from the season (using EDI data here)
# Appended means that each cast is stacked on top of one another
ctd <- read_csv("C:/Users/Mary Lofton/Documents/Ch_1/LakeAnalyzer_BACI_new/CTD_check/CTD_Meta_13_17.csv") %>%
  select(Reservoir, Site, Date, Depth_m, Temp_C, Turb_NTU) %>%
  mutate(Day = date(Date), DOY = yday(Date))%>%
  filter(Day >= "2016-05-19" & Date <= "2016-07-16")

##########Turbidity Longitudinal Plots - need to already have ctd loaded
fcr_turb <- ctd %>%
  filter(Reservoir == "FCR") %>%
  filter(Day == "2016-06-03" |
           Day == "2016-06-06") %>%
  mutate(x = ifelse(Site == 10, 153, ifelse(Site == 20, 320, ifelse(Site == 30, 510, ifelse(Site == 45, 725, ifelse(Site == 50, 820, NA))))))

#FCR

depths = seq(0.1, 10, by = 0.3)
df.final.fcr.turb<-data.frame()

for (i in 1:length(depths)){
  
  fcr_layer<-fcr_turb %>% 
    group_by(Date) %>% 
    slice(which.min(abs(as.numeric(Depth_m) - depths[i]))) %>%
    mutate(Depth_m = depths[i])
  
  # Bind each of the data layers together.
  df.final.fcr.turb = bind_rows(df.final.fcr.turb, fcr_layer)
  
}

# Re-arrange the data frame by date
fcr_new_turb <- arrange(df.final.fcr.turb, Date)

# Round each extracted depth to the nearest 10th. 
fcr_new_turb$Depth_m <- round(as.numeric(fcr_new_turb$Depth_m), digits = 0.5)

# Select and make each CTD variable a separate dataframe
# I have done this for the heatmap plotting purposes. 
turb_fcr <- select(fcr_new_turb, x, Depth_m, Turb_NTU, Day)
unique(turb_fcr$Day)

##create polygon of FCR
x = c(0,   150,  295,  464,  568,  726,  820,  905, 905, 0)
y = c(0.6,  1,   3.6,  5.2,  5.6,  7.4,  9.25,  9.3,   0,  0)

polygon = as_tibble(cbind(x,y))

# Complete data interpolation for the heatmaps
# interative processes here

#turbidity
turb_fcr1 <- turb_fcr %>% filter(Day == "2016-06-03")
interp_turb_fcr1 <- interp(x=turb_fcr1$x, y = turb_fcr1$Depth_m, z = turb_fcr1$Turb_NTU,
                           xo = seq(min(turb_fcr1$x), max(turb_fcr1$x), by = 1), 
                           yo = seq(0.1, 9.3, by = 0.01),
                           extrap = F, linear = T, duplicate = "strip")
interp_turb_fcr1 <- interp2xyz(interp_turb_fcr1, data.frame=T)

turb_fcr2 <- turb_fcr %>% filter(Day == "2016-06-06")
interp_turb_fcr2 <- interp(x=turb_fcr2$x, y = turb_fcr2$Depth_m, z = turb_fcr2$Turb_NTU,
                           xo = seq(min(turb_fcr2$x), max(turb_fcr2$x), by = 1), 
                           yo = seq(0.1, 9.3, by = 0.01),
                           extrap = F, linear = T, duplicate = "strip")
interp_turb_fcr2 <- interp2xyz(interp_turb_fcr2, data.frame=T)


##limited interpolated to data within the polygon

plotdata1 <- interp_turb_fcr1 %>%
  mutate(poly = inpolygon(x,y,polygon$x,polygon$y))%>%
  filter(poly == "TRUE")

plotdata2 <- interp_turb_fcr2 %>%
  mutate(poly = inpolygon(x,y,polygon$x,polygon$y))%>%
  filter(poly == "TRUE")

#plot!

#FCR
mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 legend.key = element_blank(),legend.background = element_blank(),
                 axis.text=element_text(size=16),
                 axis.title=element_text(size=16,face="bold"))

p3 = ggplot(data = plotdata1, aes(x = x, y = y))+
  geom_raster(aes(fill = z))+
  scale_y_reverse(expand = c(0,0))+
  scale_x_continuous(expand = c(0, 0), breaks = c(153,320,510,725,820), labels = c("Site 10","Site 20","Site 30","Site 45","Site 50")) +
  theme(axis.text.x = element_blank())+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="#BF0000",limits = c(1,10))+
  labs(x = "", y = "Depth (m)", fill="NTU")+
  geom_vline(xintercept = 153, lwd = 1.5, colour = "black", lty = 2)+
  geom_vline(xintercept = 320, lwd = 1.5, colour = "black", lty = 2)+
  geom_vline(xintercept = 510, lwd = 1.5, colour = "black", lty = 2)+
  geom_vline(xintercept = 725, lwd = 1.5, colour = "black", lty = 2)+
  geom_vline(xintercept = 820, lwd = 1.5, colour = "black", lty = 2)+
  #geom_text(aes(x = 144, y = 1.5, label = "BVR"), vjust = 0, colour = "black") +
  ggtitle("03Jun16")+
  mytheme

#p3

p4 = ggplot(data = plotdata2, aes(x = x, y = y))+
  geom_raster(aes(fill = z))+
  scale_y_reverse(expand = c(0,0))+
  scale_x_continuous(expand = c(0, 0), breaks = c(153,320,510,725,820), labels = c("Site 10","Site 20","Site 30","Site 45","Site 50")) +
  scale_fill_gradientn(colours = blue2green2red(60), na.value="#BF0000",limits = c(1,10))+
  labs(x = "Date", y = "Depth (m)", fill="NTU")+
  geom_vline(xintercept = 153, lwd = 1.5, colour = "black", lty = 2)+
  geom_vline(xintercept = 320, lwd = 1.5, colour = "black", lty = 2)+
  geom_vline(xintercept = 510, lwd = 1.5, colour = "black", lty = 2)+
  geom_vline(xintercept = 725, lwd = 1.5, colour = "black", lty = 2)+
  geom_vline(xintercept = 820, lwd = 1.5, colour = "black", lty = 2)+
  #geom_text(aes(x = 144, y = 1.5, label = "BVR"), vjust = 0, colour = "black") +
  ggtitle("06Jun16")+
  mytheme

#p4


#make final plot
final_plot1 <- plot_grid(p3, p4, ncol = 1,align = "hv",
                         labels=c('A','B'), label_size = 16) # rel_heights values control title margins
ggsave(plot=final_plot1, file = "plot.pdf", device = "pdf",
       h=6, w=10, units="in", dpi=300, scale = 1)


