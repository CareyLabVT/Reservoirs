# CCR Heatmaps
# Author: Ryan McClure
# Edited for Metals: Katie Krueger, Nick Hammond
# Date last updated: 12072022

# Makes heatmaps of the metals data in Carvins Cove reservoir

# load libraries
library(akima)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(gridExtra)
library(grid)
library(colorRamps)
library(RColorBrewer)
library(rLakeAnalyzer)
library(readxl)
library(lubridate)
library(zoo)


# Set the WD 
#setwd("C:/CCR_BVR Metals Data/EDI")

# load in metal data from the season (the excel sheet you worked on)
CCR <- read_csv("Metals_2022 - Sheet1.csv") %>% na.omit(CCR)
#convert characters to dates/numbers
CCR$TFe_mgL<-as.numeric(CCR$TFe_mgL)
#eliminate all rows where TFe is missing
CCR<-CCR[which(!is.na(CCR$TFe_mgL)),]
CCR$TMn_mgL<-as.numeric(CCR$TMn_mgL)
#eliminate all rows where TMn is missing
CCR<-CCR[which(!is.na(CCR$TMn_mgL)),]

CCR$SFe_mgL<-as.numeric(CCR$SFe_mgL)
CCR<-CCR[which(!is.na(CCR$SFe_mgL)),]

CCR$SMn_mgL<-as.numeric(CCR$SMn_mgL)
CCR<-CCR[which(!is.na(CCR$SMn_mgL)),]


# Filter to include desired reservoir (CCR), site (50), and year (2022)

#run this one once all entries have date and time
##CCR$DateTime <- mdy_hms(CCR$DateTime)

CCR$DateTime<-as.Date(CCR$DateTime, "%Y-%m-%d")

CCR <- CCR %>% filter(Reservoir=="CCR") %>%
  filter(Site==50) %>%
  filter(DateTime > "2022-01-01 00:00:00") %>%
  filter(DateTime < "2022-12-31 00:00:00")


# Select and make each variable a separate dataframe
# I have done this for the heatmap plotting purposes. 
TFe <- select(CCR, DateTime, Depth_m, TFe_mgL) 
TMn <- select(CCR, DateTime, Depth_m, TMn_mgL)
SFe <- select(CCR, DateTime, Depth_m, SFe_mgL)
SMn <- select(CCR, DateTime, Depth_m, SMn_mgL)
DateTime_u <- as.data.frame(unique(date(ymd(CCR$DateTime))))
colnames(DateTime_u) <- "DateTime"
DateTime_x <- date(ymd(CCR$DateTime))       

# Complete data interpolation for the heatmaps
# interative processes here

#Total Fe
interp_TFe <- interp(x=DateTime_x, y = TFe$Depth_m, z = TFe$TFe_mgL,
                     xo = seq(min(DateTime_x), max(DateTime_x), by = 0.5), 
                     yo = seq(0.1, max(TFe$Depth_m), by = 0.1),
                     extrap = F, linear = T, duplicate = "strip")
interp_TFe <- interp2xyz(interp_TFe, data.frame=T)
i_DateTime_TFe <- as.Date(interp_TFe$x)
interp_TFe_f <- interp_TFe %>% mutate(DateTime_i=i_DateTime_TFe)

#Total Mn
interp_TMn <- interp(x=DateTime_x, y = TMn$Depth_m, z = TMn$TMn_mgL,
                     xo = seq(min(DateTime_x), max(DateTime_x), by = 0.5), 
                     yo = seq(0.1, max(TFe$Depth_m), by = 0.1),
                     extrap = F, linear = T, duplicate = "strip")
interp_TMn <- interp2xyz(interp_TMn, data.frame=T)
i_DateTime_TMn <- as.Date(interp_TMn$x)
interp_TMn_f <- interp_TMn %>% mutate(DateTime_i=i_DateTime_TMn)

#Soluble Iron

interp_SFe <- interp(x=DateTime_x, y = SFe$Depth_m, z = SFe$SFe_mgL,
                     xo = seq(min(DateTime_x), max(DateTime_x), by = 0.5), 
                     yo = seq(0.1, max(TFe$Depth_m), by = 0.1),
                     extrap = F, linear = T, duplicate = "strip")
interp_SFe <- interp2xyz(interp_SFe, data.frame=T)
i_DateTime_SFe <- as.Date(interp_SFe$x)
interp_SFe_f <- interp_SFe %>% mutate(DateTime_i=i_DateTime_SFe)

#Soluble Mn
interp_SMn <- interp(x=DateTime_x, y = SMn$Depth_m, z = SMn$SMn_mgL,
                     xo = seq(min(DateTime_x), max(DateTime_x), by = 0.5), 
                     yo = seq(0.1, max(TFe$Depth_m), by = 0.1),
                     extrap = F, linear = T, duplicate = "strip")
interp_SMn <- interp2xyz(interp_SMn, data.frame=T)
i_DateTime_SMn <- as.Date(interp_SMn$x)
interp_SMn_f <- interp_SMn %>% mutate(DateTime_i=i_DateTime_SMn)

# Plotting #

# This a theme I have adapted from 
#https://gist.github.com/jslefche/eff85ef06b4705e6efbc
# I LIKE IT!
theme_black = function(base_size = 18, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_line(size = 1, colour = "white"),  
      axis.text.x = element_text(size = base_size*1.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*1.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  1.4),  
      axis.title.x = element_text(size = base_size*1.9, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size*1.9, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.5, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*1.8, color = "white"),  
      legend.title = element_text(size = base_size*1.9, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "black"),  
      panel.grid.major = element_line(color = "black"),  
      panel.grid.minor = element_line(color = "black"),  
      panel.spacing = unit(0, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*1.2, color = "white"),  
      strip.text.y = element_text(size = base_size*1.2, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*2.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}

theme_new = function(base_size = 18, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_line(size = 1, colour = "black"),  
      axis.text.x = element_text(size = base_size*1.8, color = "black", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*1.8, color = "black", lineheight = 0.9),  
      axis.ticks = element_line(color = "black", size  =  1.4),  
      axis.title.x = element_text(size = base_size*1.9, color = "black", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size*1.9, color = "black", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.5, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "white"),  
      legend.key = element_rect(color = "black",  fill = "white"),  
      legend.key.size = unit(2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*1.8, color = "black"),  
      legend.title = element_text(size = base_size*1.9, face = "bold", hjust = 0, color = "black"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "white", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "white"),  
      panel.grid.minor = element_line(color = "white"),  
      panel.spacing = unit(0, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*1.2, color = "white"),  
      strip.text.y = element_text(size = base_size*1.2, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "white", fill = "white"),  
      plot.title = element_text(size = base_size*2.2, color = "black"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}
#Vectors of dates for Hox#
#ON = as.data.frame(ymd(c("2019-06-03", "2019-07-08","2019-08-05","2019-09-02")))
#colnames(ON) = "Date"
#OFF = as.data.frame(ymd(c("2019-06-17", "2019-07-18","2019-08-19")))
#colnames(OFF) = "Date"

#Total Iron
png('CCR TFe Heatmap 2022_test.png', width = 15, height = 12, units = 'in', res = 300)
p1 <- ggplot(interp_TFe_f, aes(x=DateTime_i, y=y))+
  geom_raster(aes(fill=z), interpolate = TRUE)+
  scale_y_reverse()+
  geom_point(data = DateTime_u, aes(x=DateTime, y=0, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "CCR Total Iron (mg/L) 2022",fill=expression('(mg/L)'))+
  #geom_vline(data=ON, aes(xintercept=Date) , linetype="solid", color="black", size=1.6)+
  #geom_vline(data=OFF, aes(xintercept=Date) , linetype="dashed", color="black", size=1.6)+
  theme_black()
print(p1)
dev.off()

#Total Mn
png('CCR TMn Heatmap 2022.png', width = 15, height = 12, units = 'in', res = 300)
p2 <- ggplot(interp_TMn_f, aes(x=DateTime_i, y=y))+
  geom_raster(aes(fill=z), interpolate = TRUE)+
  scale_y_reverse()+
  geom_point(data = DateTime_u, aes(x=DateTime, y=0, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "CCR Total Manganese (mg/L) 2022",fill=expression('(mg/L)'))+
  theme_black()
print(p2)
dev.off()

#Soluble Fe
png('CCR SFe Heatmap 2022.png', width = 15, height = 12, units = 'in', res = 300)
p3 <- ggplot(interp_SFe_f, aes(x=DateTime_i, y=y))+
  geom_raster(aes(fill=z), interpolate = TRUE)+
  scale_y_reverse()+
  geom_point(data = DateTime_u, aes(x=DateTime, y=0, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "CCR Soluble Iron (mg/L) 2022",fill=expression('(mg/L)'))+
  theme_black()
print(p3)
dev.off()

#Soluble Mn
png('CCR SMn Heatmap 2022.png', width = 15, height = 12, units = 'in', res = 300)
p3 <- ggplot(interp_SMn_f, aes(x=DateTime_i, y=y))+
  geom_raster(aes(fill=z), interpolate = TRUE)+
  scale_y_reverse()+
  geom_point(data = DateTime_u, aes(x=DateTime, y=0, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "CCR Soluble Manganese (mg/L) 2022",fill=expression('(mg/L)'))+
  theme_black()
print(p3)
dev.off()





#Soluble Mn
png('CCR SMn Heatmap 2022 white.png', width = 15, height = 12, units = 'in', res = 300)
p4 <- ggplot(interp_SMn_f, aes(x=DateTime_i, y=y))+
  geom_raster(aes(fill=z), interpolate = TRUE)+
  scale_y_reverse()+
  geom_point(data = DateTime_u, aes(x=DateTime, y=0, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  ##added a vector of purple shades to create a continuous scale matching Gantzer et al. 2009's SMn figure
  scale_fill_gradientn(colors=c("white", "thistle", "medium purple", "purple", "purple4"), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "CCR Soluble Manganese (mg/L) 2021",fill=expression(''))+
  theme_new()
print(p4)

dev.off()

