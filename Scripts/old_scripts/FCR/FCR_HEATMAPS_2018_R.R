# FCR CTD Heatmaps
# Author: Ryan McClure
# Date last updated: 080718

# Makes heatmaps of the CTD data in Falling Crekk reservoir

# load libraries
library(akima)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(gridExtra)
library(grid)
library(colorRamps)
library(RColorBrewer)
library(rLakeAnalyzer)


# Set the WD (Probably should have made this a project but didnt think of it at the time)
setwd("C:/Users/Owner/Dropbox/2018_Data_RPM/CTD_2018/2018 CTD Files_csv/FCRsite50")

# load in appended CTD data from the season
# Appended means that each cast is stacked on top of one another
ctd <- read_csv("CTD_Appended_fcr50_2018.csv")

# filter out depths in the CTD cast that are closest to these specified values.
df.final<-data.frame()
ctd1<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
ctd2<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.4)))
ctd3<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.7)))
ctd4<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1)))
ctd5<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.3)))
ctd6<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.6)))
ctd7<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.9)))
ctd8<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.3)))
ctd9<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.6)))
ctd10<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.9)))
ctd11<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.2)))
ctd12<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.5)))
ctd13<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.8)))
ctd14<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.1)))
ctd15<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.4)))
ctd16<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.7)))
ctd17<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5)))
ctd18<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.3)))
ctd19<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.6)))
ctd20<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.9)))
ctd21<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.2)))
ctd22<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.5)))
ctd23<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.8)))
ctd24<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.1)))
ctd25<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.4)))
ctd26<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.7)))
ctd27<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8)))
ctd28<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.3)))
ctd29<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.7)))
ctd30<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9)))
ctd31<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.3)))
ctd32<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.6)))


# Bind each of the data layers together.
df.final = rbind(ctd1,ctd2,ctd3,ctd4,ctd5,ctd6,ctd7,ctd8,ctd9,ctd10,ctd11,ctd12,ctd13,ctd14,ctd15,ctd16,ctd17,ctd18,ctd19,
                 ctd20,ctd21,ctd22,ctd23,ctd24,ctd25,ctd26,ctd27,ctd28,ctd29,ctd30,ctd31, ctd32)

# Re-arrange the data frame by date
ctd <- arrange(df.final, Date)

# Round each extracted depth to the nearest 10th. 
ctd$Depth_m <- round(as.numeric(ctd$Depth_m), digits = 0.5)

# Select and make each CTD variable a separate dataframe
# I have done this for the heatmap plotting purposes. 
temp <- select(ctd, DOY, Depth_m, Temp_C)
chla <- select(ctd, DOY, Depth_m, Chla_ugL)
turb <- select(ctd, DOY, Depth_m, Turb_NTU)
cond <- select(ctd, DOY, Depth_m, Cond_uScm)
spccond <- select(ctd, DOY, Depth_m, Spec_Cond_uScm)
do <- select(ctd, DOY, Depth_m, DO_mgL)
psat <- select(ctd, DOY, Depth_m, DO_pSat)
ph <- select(ctd, DOY, Depth_m, pH)
orp <- select(ctd, DOY, Depth_m, ORP_mV)
par <- select(ctd, DOY, Depth_m, PAR)
sal <- select(ctd, DOY, Depth_m, Salinity)
desc <- select(ctd, DOY, Depth_m, Descent.Rate..m.s.)


# rLakeAnalyzer for Thermocline Depths

# Pulling just temp, depth and date and going from long to wide. 
temp_RLA <- temp %>%
  select(Date,Depth_m,Temp_C)%>%
  spread(Depth_m,Temp_C)

# renaming the column names to include wtr_ 
# Otherwise, rLakeAnaylzer will not run!
colnames(temp_RLA)[-1] = paste0('wtr_',colnames(temp_RLA)[-1])

# rename the first column to "datetime"
names(temp_RLA)[1] <- "datetime"

# Calculate thermocline depth
FCR_thermo_18 <- ts.thermo.depth(temp_RLA)

#rename the datetime name back to Date
names(FCR_thermo_18)[1] <- "Date"

# Using dplyr, rejoin the DOY column from the temp dataframe to the thermocline depth dataframe. 
# this is a bit more ambiguous than it needs to be, but it works. 
FCR_thermo_18 <- left_join(FCR_thermo_18, temp, by = "Date")


# Complete data interpolation for the heatmaps
# interative processes here

#temperature
interp_temp <- interp(x=temp$DOY, y = temp$Depth_m, z = temp$Temp_C,
                      xo = seq(min(temp$DOY), max(temp$DOY), by = .1), 
                      yo = seq(0.1, 9.6, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_temp <- interp2xyz(interp_temp, data.frame=T)

#chlorophyll a
interp_chla <- interp(x=chla$DOY, y = chla$Depth_m, z = chla$Chla_ugL,
                      xo = seq(min(chla$DOY), max(chla$DOY), by = .1), 
                      yo = seq(0.1, 9.6, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_chla <- interp2xyz(interp_chla, data.frame=T)

#turbidity
interp_turb <- interp(x=turb$DOY, y = turb$Depth_m, z = turb$Turb_NTU,
                      xo = seq(min(turb$DOY), max(turb$DOY), by = .1), 
                      yo = seq(0.1, 9.6, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_turb <- interp2xyz(interp_turb, data.frame=T)

#conductivity
interp_cond <- interp(x=cond$DOY, y = cond$Depth_m, z = cond$Cond_uScm,
                      xo = seq(min(cond$DOY), max(cond$DOY), by = .1), 
                      yo = seq(0.1, 9.6, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_cond <- interp2xyz(interp_cond, data.frame=T)

#specific conductivity
interp_spccond <- interp(x=spccond$DOY, y = spccond$Depth_m, z = spccond$Spec_Cond_uScm,
                      xo = seq(min(spccond$DOY), max(spccond$DOY), by = .1), 
                      yo = seq(0.1, 9.6, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_spccond <- interp2xyz(interp_spccond, data.frame=T)

#dissolved oxygen
interp_do <- interp(x=do$DOY, y = do$Depth_m, z = do$DO_mgL,
                      xo = seq(min(do$DOY), max(do$DOY), by = .1), 
                      yo = seq(0.1, 9.6, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_do <- interp2xyz(interp_do, data.frame=T)

#percent saturation
interp_psat <- interp(x=psat$DOY, y = psat$Depth_m, z = psat$DO_pSat,
                      xo = seq(min(psat$DOY), max(psat$DOY), by = .1), 
                      yo = seq(0.1, 9.6, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_psat <- interp2xyz(interp_psat, data.frame=T)

#pH
interp_ph <- interp(x=ph$DOY, y = ph$Depth_m, z = ph$pH,
                      xo = seq(min(ph$DOY), max(ph$DOY), by = .1), 
                      yo = seq(0.1, 9.6, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_ph <- interp2xyz(interp_ph, data.frame=T)

#Oxidation reduction pottential
interp_orp <- interp(x=orp$DOY, y = orp$Depth_m, z = orp$ORP_mV,
                      xo = seq(min(orp$DOY), max(orp$DOY), by = .1), 
                      yo = seq(0.1, 9.6, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_orp <- interp2xyz(interp_orp, data.frame=T)

#photosynthetic active radiation
interp_par <- interp(x=par$DOY, y = par$Depth_m, z = par$PAR,
                      xo = seq(min(par$DOY), max(par$DOY), by = .1), 
                      yo = seq(0.1, 9.6, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_par <- interp2xyz(interp_par, data.frame=T)

#salinity
interp_sal <- interp(x=sal$DOY, y = sal$Depth_m, z = sal$Salinity,
                      xo = seq(min(sal$DOY), max(sal$DOY), by = .1), 
                      yo = seq(0.1, 9.6, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_sal <- interp2xyz(interp_sal, data.frame=T)

#descent rate
interp_desc <- interp(x=desc$DOY, y = desc$Depth_m, z = desc$Descent.Rate..m.s.,
                      xo = seq(min(desc$DOY), max(desc$DOY), by = .1), 
                      yo = seq(0.1, 9.6, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_desc <- interp2xyz(interp_desc, data.frame=T)

# Plotting #

# This a theme I have adapted from 
#https://gist.github.com/jslefche/eff85ef06b4705e6efbc
# I LIKE IT!
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_line(size = 1, colour = "white"),  
      axis.text.x = element_text(size = base_size*1, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*1, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  1),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.5, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*1.5, face = "bold", hjust = 0, color = "white"),  
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
      panel.margin = unit(0, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.5, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}

# Create a pdf so the plots can all be saved in one giant bin!
pdf("FCR_CTD_2018.pdf", width=10, height=30)

#temperature
p1 <- ggplot(interp_temp, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = "FCR Temperature Heatmap",fill=expression(''*~degree*C*''))+
  theme_black()

#chlorophyll a
p2 <- ggplot(interp_chla, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = "FCR CHLA Heatmap", fill=expression(paste("", mu, "g/L")))+
  theme_black()

#turbidity
p3 <- ggplot(interp_turb, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = "FCR Turbidity Heatmap", fill="NTU")+
  theme_black()

#specific conductivity
p4 <- ggplot(interp_spccond, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = "FCR Specific Conductivity Heatmap",fill=expression(paste("", mu, "S/cm")))+
  theme_black()

#dissolved oxygen
p5 <- ggplot(interp_do, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = "FCR Dissolved Oxygen Heatmap", fill="mg/L")+
  theme_black()

#pH
p6 <- ggplot(interp_ph, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = "FCR pH Heatmap", fill="pH")+
  theme_black()

#oxidation reduction potential
p7 <- ggplot(interp_orp, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = "FCR ORP Heatmap", fill="mV")+
  theme_black()

#descent rate
p8 <- ggplot(interp_desc, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = "FCR Descent Rate Heatmap", fill = "m/s")+
  theme_black()

# create a grid that stacks all the heatmaps together. 
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3),
                ggplotGrob(p4), ggplotGrob(p5), ggplotGrob(p6),
                ggplotGrob(p7),ggplotGrob(p8),
                size = "first"))
# end the make-pdf function. 
dev.off()