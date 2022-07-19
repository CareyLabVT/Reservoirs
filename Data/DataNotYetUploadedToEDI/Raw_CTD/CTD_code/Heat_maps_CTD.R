# BVR CTD Heatmaps
# Author: Ryan McClure
# Edited: Adrienne Breef-Pilz
# Date last updated: 080718
##Modified 23 June 2022 by ABP for BVR01 plots 

# Makes heatmaps of the CTD data in Beaverdam reservoir

pacman::p_load('akima', 'tidyverse','dplyr','reshape2','gridExtra','grid','colorRamps',"RColorBrewer","rLakeAnalyzer","tidyr","lubridate","plotly")

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
library(lubridate) #for Julian date 
library(tidyr)


#### CCR heatmaps CTD data ####
### Getting data filtered selected for plots 


#need new way to read in CTD, to large for github 
CTD <- read_csv("Data/DataNotYetUploadedToEDI/Raw_CTD/CTD_season_csvs/CTD_notmatlab_ready_2021_fcr50.csv", col_types = cols(.default = "d", Date = "T"))

#CTD[!apply(CTD == "", 2, all),]

ccr <- CTD %>%
  filter(Date>"2021-12-31 00:00")%>% #subset year
  filter(Depth_m!="")%>%
  select(Date, Depth_m, Temp_C, DO_mgL, Spec_Cond_uScm, Chla_ugL, Turb_NTU, pH)%>% #select the variables we want
  mutate(Date = as.Date(Date),
         Year = format(as.Date(Date), "%Y"),
         DOY = yday(Date))
  #filter(Date!="2022-05-17")%>%
  #filter(Turb_NTU<30)


summary(ccr)

# ggplot(data = ccr, mapping = aes(x = DO_mgL, y = Depth_m))+
#   geom_line()+
#   facet_wrap(~Date)+
#   scale_y_continuous(trans = "reverse") #flips scale of yaxis (ie. 0 at top)


###getting heat maps 
#rename dataframe to ctd to fit code below 
ctd <- ccr

df.final<-data.frame()

ctd1<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.5)))
ctd2<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1)))
ctd3<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.5)))
ctd4<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2)))
ctd5<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.5)))
ctd6<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3)))
ctd7<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.5)))
ctd8<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4)))
ctd9<-ctd  %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.5)))
ctd10<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5)))
ctd11<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.5)))
ctd12<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6)))
ctd13<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.5)))
ctd14<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7)))
ctd15<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.5)))
ctd16<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8)))
ctd17<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.5)))
ctd18<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9)))
ctd19<-ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.5)))



df.final = rbind(ctd1,ctd2,ctd3,ctd4,ctd5,ctd6,ctd7,ctd8,ctd9,ctd10,ctd11,ctd12,ctd13,ctd14,ctd15,ctd16,ctd17,ctd18,ctd19,
                 deparse.level = 1)

ctd <- arrange(df.final, Date)
#ctd$Depth_m <- round(as.numeric(ctd$Depth_m), digits = 1)
ctd$Depth_m <-   round(ctd$Depth_m/0.5)*0.5  # new rounding function to ensure values get to nearest 0.5 


spcond<-select(ctd,Date, DOY,Depth_m,Spec_Cond_uScm)
temp <- select(ctd,Date, DOY, Depth_m, Temp_C) 
do <- select(ctd, Date, DOY, Depth_m, DO_mgL)
chla <- select(ctd, Date, DOY, Depth_m, Chla_ugL)
turb <- select(ctd, Date, DOY, Depth_m, Turb_NTU)
# cond <- select(ctd, DOY, Depth_m, Cond_uScm)
# spccond <- select(ctd, DOY, Depth_m, Spec_Cond_uScm)
# psat <- select(ctd, DOY, Depth_m, DO_pSat)
 ph <- select(ctd, Date, DOY, Depth_m, pH)
# orp <- select(ctd, DOY, Depth_m, ORP)
# par <- select(ctd, DOY, Depth_m, PAR)
# sal <- select(ctd, DOY, Depth_m, Salinity)
# desc <- select(ctd, DOY, Depth_m, `Descent Rate (m/s)`)

# rLakeAnalyzer for Thermocline Depths

# # Pulling just temp, depth and date and going from long to wide. 
# temp_RLA <- temp %>%
#   select(Date, Depth_m, Temp_C)%>%
#   #spread(Depth_m,Temp_C) #spread was retired and giving errors. Using suggested pivot wider now 
#   pivot_wider(names_from = Depth_m, values_from = Temp_C, values_fn = list(Temp_C = mean))
# 
# # renaming the column names to include wtr_ 
# # Otherwise, rLakeAnaylzer will not run!
# colnames(temp_RLA)[-1] = paste0('wtr_',colnames(temp_RLA)[-1])
# 
# # rename the first column to "datetime"
# names(temp_RLA)[1] <- "datetime"
# 
# # Calculate thermocline depth
# FCR_thermo_18 <- ts.thermo.depth(temp_RLA, na.rm = TRUE)
# 
# #rename the datetime name back to Date
# names(FCR_thermo_18)[1] <- "Date"
# 
# # Using dplyr, rejoin the DOY column from the temp dataframe to the thermocline depth dataframe. 
# # this is a bit more ambiguous than it needs to be, but it works. 
# FCR_thermo_18 <- left_join(FCR_thermo_18, temp, by = "Date")


# Complete data interpolation for the heatmaps
head(temp)
temp$Date <- as.POSIXct(temp$Date)
head(temp)

#temperature
interp_temp <- interp(x=temp$Date, y = temp$Depth_m, z = temp$Temp_C,  #x was DOY now by date
                      xo = seq(as.Date("2022-02-24"), as.Date("2022-06-20"), by = "day"), 
                      # xo = seq(min(temp$Date), max(temp$Date), by = "day"), 
                      yo = seq(0.1, 10, by = 0.01),                     #second seq value was 10.2. change to bottom depth
                      extrap = F, linear = T, duplicate = "strip")
interp_temp <- interp2xyz(interp_temp, data.frame=T)

#temperature DOY
#interp_temp <- interp(x=temp$DOY, y = temp$Depth_m, z = temp$Temp_C,  
#                      xo = seq(min(temp$DOY), max(temp$DOY), by = .1), 
#                      yo = seq(0.1, 4.5, by = 0.01),                     #second seq value was 10.2
#                      extrap = F, linear = T, duplicate = "strip")
#interp_temp <- interp2xyz(interp_temp, data.frame=T)


datetemp <- interp_temp %>% 
  mutate(Realdate = as.Date(x, origin = "1970-01-01"))


#dissolved oxygen
interp_do <- interp(x=do$Date, y = do$Depth_m, z = do$DO_mgL,        #ditto to lines for temp 
                    xo = seq(as.Date("2022-02-24"), as.Date("2022-06-20"), by = "day"), 
                    yo = seq(0.1, 10, by = 0.01),   
                    extrap = F, linear = T, duplicate = "strip")
interp_do <- interp2xyz(interp_do, data.frame=T)

#spconductivity
interp_spcond <- interp(x=spcond$Date, y = spcond$Depth_m, z = spcond$Spec_Cond_uScm,
                      xo = seq(as.Date("2022-02-24"), as.Date("2022-06-20"), by = "day"), 
                      # xo = seq(min(temp$Date), max(temp$Date), by = "day"), 
                      yo = seq(0.1, 10, by = 0.01), #this is the yaxis  
                      extrap = F, linear = T, duplicate = "strip")
interp_spcond <- interp2xyz(interp_spcond, data.frame=T)

#temperature
interp_cond <- interp(x=cond$DOY, y = cond$Depth_m, z = cond$Calc_SpCond_uScm,  
                      xo = seq(min(cond$DOY), max(cond$DOY), by = .1), 
                      yo = seq(0.1, 9.5, by = 0.01),                     #second seq value was 10.2
                      extrap = F, linear = T, duplicate = "strip")
interp_cond <- interp2xyz(interp_cond, data.frame=T)


#Other variables from CTD not using now 
{
  #chlorophyll a
  interp_chla <- interp(x=chla$Date, y = chla$Depth_m, z = chla$Chla_ugL,
                        xo = seq(as.Date("2022-02-24"), as.Date("2022-06-20"), by = "day"), #changed this for the xaxis
                        # xo = seq(min(temp$Date), max(temp$Date), by = "day"), 
                        yo = seq(0.1, 10, by = 0.01),
                        extrap = F, linear = T, duplicate = "strip")
  interp_chla <- interp2xyz(interp_chla, data.frame=T)
  
  #turbidity
  interp_turb <- interp(x=turb$Date, y = turb$Depth_m, z = turb$Turb_NTU,
                        xo = seq(as.Date("2022-02-24"), as.Date("2022-06-20"), by = "day"), 
                        # xo = seq(min(temp$Date), max(temp$Date), by = "day"), 
                        yo = seq(0.1, 10, by = 0.01),
                        extrap = F, linear = T, duplicate = "strip")
  interp_turb <- interp2xyz(interp_turb, data.frame=T)
  
  
  
  #specific conductivity
  interp_spccond <- interp(x=spccond$DOY, y = spccond$Depth_m, z = spccond$Spec_Cond_uScm,
                           xo = seq(min(spccond$DOY), max(spccond$DOY), by = .1), 
                           yo = seq(0.1, 10.2, by = 0.01),
                           extrap = F, linear = T, duplicate = "strip")
  interp_spccond <- interp2xyz(interp_spccond, data.frame=T)
  
  #percent saturation
  interp_psat <- interp(x=psat$DOY, y = psat$Depth_m, z = psat$DO_pSat,
                        xo = seq(min(psat$DOY), max(psat$DOY), by = .1), 
                        yo = seq(0.1, 10.2, by = 0.01),
                        extrap = F, linear = T, duplicate = "strip")
  interp_psat <- interp2xyz(interp_psat, data.frame=T)
  
  #pH
  interp_ph <- interp(x=ph$Date, y = ph$Depth_m, z = ph$pH,
                      xo = seq(min(ph$Date), max(ph$Date), by = .1), 
                      yo = seq(0.1, 10, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
  interp_ph <- interp2xyz(interp_ph, data.frame=T)
  
  #Oxidation reduction pottential
  interp_orp <- interp(x=orp$DOY, y = orp$Depth_m, z = orp$ORP,
                       xo = seq(min(orp$DOY), max(orp$DOY), by = .1), 
                       yo = seq(0.1, 10.2, by = 0.01),
                       extrap = F, linear = T, duplicate = "strip")
  interp_orp <- interp2xyz(interp_orp, data.frame=T)
  
  #photosynthetic active radiation
  interp_par <- interp(x=par$DOY, y = par$Depth_m, z = par$PAR,
                       xo = seq(min(par$DOY), max(par$DOY), by = .1), 
                       yo = seq(0.1, 10.2, by = 0.01),
                       extrap = F, linear = T, duplicate = "strip")
  interp_par <- interp2xyz(interp_par, data.frame=T)
  
  #salinity
  interp_sal <- interp(x=sal$DOY, y = sal$Depth_m, z = sal$Salinity,
                       xo = seq(min(sal$DOY), max(sal$DOY), by = .1), 
                       yo = seq(0.1, 10.2, by = 0.01),
                       extrap = F, linear = T, duplicate = "strip")
  interp_sal <- interp2xyz(interp_sal, data.frame=T)
  
  #descent rate
  interp_desc <- interp(x=desc$DOY, y = desc$Depth_m, z = desc$`Descent Rate (m/s)`,
                        xo = seq(min(desc$DOY), max(desc$DOY), by = .1), 
                        yo = seq(0.1, 10.2, by = 0.01),
                        extrap = F, linear = T, duplicate = "strip")
  interp_desc <- interp2xyz(interp_desc, data.frame=T)
}


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
      panel.spacing = unit(0, "lines"),   #chagned to panel.spacing from panel.margin in orginal code
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
jpeg("./CCR_plots/CCR_CTD_2013_2019_test.jpg", width=1440, height=480, quality = 150)  

#temperature using Date
p1 <- ggplot(interp_temp, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  #geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  geom_point(data = ctd, aes(x = Date, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  # geom_line(data = FCR_thermo_18, aes(x=Date, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  #geom_point(data = FCR_thermo_18, aes(x=Date, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "FCR50 Temperature Heatmap",fill=expression(''*~degree*C*''))+ #x was day of year
  theme_black()

#temperature using DOY
p1 <- ggplot(interp_temp, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  #geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  geom_point(data = ctd, aes(x = DOY, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  #geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  #geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "CCR 2020 Temperature Heatmap",fill=expression(''*~degree*C*''))+ #x was day of year
  theme_black()


#Specific Conductance
p2 <- ggplot(interp_cond, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  #geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  geom_point(data = ctd, aes(x = Date, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  #geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  #geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "FCR50 Specific Conductance",fill=expression(''*uScm*''))+ #x was day of year
  theme_black()

sp3=ggplot(EXO_2020, aes(x=DOY, y=EXOSpCond_uScm_1), color="black")+
  geom_point()+
  geom_point(data=cond, aes(x=DOY, y=Calc_SpCond_uScm, color=as.factor(Depth_m)))
scale_color_gradientn(colours = blue2green2red(60), na.value="gray")
theme_black()




#dissolved oxygen
p5 <- ggplot(interp_do, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_point(data = ctd, aes(x = Date, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  #geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  #geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  #geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "FCR50 Dissolved Oxygen Heatmap", fill="mg/L")+ #x was day of year 
  theme_black()



#other variables 
{
  #chlorophyll a
  p2 <- ggplot(interp_chla, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
    geom_raster(aes(fill=z))+
    scale_y_reverse()+
    geom_point(data = ctd, aes(x=Date, y=0.1, z=NULL), pch = 25, size = 3, color = "white", fill = "black")+
#    geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
#    geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
    scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
    labs(x = "Date", y = "Depth (m)", title = "FCR50 CHLA Heatmap", fill=expression(paste("", mu, "g/L")))+
    theme_black()
  
  #turbidity
  p3 <- ggplot(interp_turb, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
    geom_raster(aes(fill=z))+
    scale_y_reverse()+
    geom_point(data = ctd, aes(x=Date, y=0.1, z=NULL), pch = 25, size = 3, color = "white", fill = "black")+
    #geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
    #geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
    scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
    labs(x = "Date", y = "Depth (m)", title = "FCR50 Turbidity Heatmap", fill="NTU")+
    theme_black()
  
  #specific conductivity
  p4 <- ggplot(interp_cond, aes(x=x, y=y))+
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
  p6 <- ggplot(interp_ph, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
    geom_raster(aes(fill=z))+
    scale_y_reverse()+
    geom_point(data = ctd, aes(x=Date, y=0.1, z=NULL), pch = 25, size = 3, color = "white", fill = "black")+
    #geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
    #geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+  
    scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
    labs(x = "Day of year", y = "Depth (m)", title = "FCR50 pH Heatmap", fill="pH")+
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
    scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
    labs(x = "Day of year", y = "Depth (m)", title = "FCR Descent Rate Heatmap", fill = "m/s")+
    theme_black()
}

# create a grid that stacks all the heatmaps together. 
grid.newpage()
grid.draw(rbind(ggplotGrob(p4),  #would change to cbind for side to side  
                #ggplotGrob(p5),
                size = "first"))
#ggplotGrob(p2), ggplotGrob(p3), ggplotGrob(p4), ggplotGrob(p5), ggplotGrob(p6), ggplotGrob(p7),ggplotGrob(p8),
# size = "first"))
# end the make-pdf function. 
dev.off()





#### CCR heatmaps WVWA data ########################

#read in csv 
wvwa <- read_csv("C:/users/dwh18/Dropbox/WVWA_CCR_data/CCR_2007_2020_Compiled.csv")
wvwa$DOY <- strftime(wvwa$Date, format = "%j") #create DOY column 
wvwa$Date <- as.Date(wvwa$Date)
head(wvwa)


temp <- select(wvwa, Date, DOY, Depth_m, Elevation, Temp_C) %>% 
  filter(!is.na(Temp_C)) #remove NA's so interp_temp will run 

do <- select(wvwa, Date, DOY, Depth_m, Elevation, DO_mgL) %>% 
  filter(!is.na(DO_mgL)) #remove NA's so interp will run 

pH <- select(wvwa, Date, DOY, Depth_m, Elevation, pH) %>% 
  filter(!is.na(pH)) #remove NA's so interp will run 

algae <- select(wvwa, Date, DOY, Depth_m, Elevation, Algae_cells_mL) %>% 
  filter(!is.na(Algae_cells_mL)) #remove NA's so interp will run 

fe <- select(wvwa, Date, DOY, Depth_m, Elevation, Fe_mgL) %>% 
  mutate(Fe_mgL = ifelse(Fe_mgL == "ND", 0, Fe_mgL),
         Fe_mgL = ifelse(Fe_mgL == "<0.01", 0, Fe_mgL),
         Fe_mgL = as.double(Fe_mgL)) %>% #set ND (non-detectable) values to 0
  filter(!is.na(Fe_mgL))  #remove NA's so interp will run 
head(fe)

mn <- select(wvwa, Date, DOY, Depth_m, Elevation, Mn_mgL) %>% 
  mutate(Mn_mgL = ifelse(Mn_mgL == "ND", 0, Mn_mgL),
         Mn_mgL = ifelse(Mn_mgL == "<0.01", 0, Mn_mgL),
         Mn_mgL = as.double(Mn_mgL)) %>% #set ND (non-detectable) values to 0
  filter(!is.na(Mn_mgL)) #remove NA's so interp will run 


# Complete data interpolation for the heatmaps


#temperature
interp_temp <- interp(x=temp$Date, y = temp$Elevation, z = temp$Temp_C,  #x was DOY
                      xo = seq(as.Date("2007-01-10"), as.Date("2020-07-30"), by = "day"), 
                      # xo = seq(min(temp$Date), max(temp$Date), by = "day"), 
                      yo = seq(1120, 1160, by = 1),                     #was previously by 1100 to 1170
                      extrap = F, linear = T, duplicate = "strip")
interp_temp <- interp2xyz(interp_temp, data.frame=T)

interp_do <- interp(x=do$Date, y = do$Elevation, z = do$DO_mgL,  #x was DOY
                    xo = seq(as.Date("2007-01-10"), as.Date("2020-07-30"), by = "day"), 
                    yo = seq(1120, 1160, by = 1),                     #was previously by .01
                    extrap = F, linear = T, duplicate = "strip")
interp_do <- interp2xyz(interp_do, data.frame=T)

interp_pH <- interp(x=pH$Date, y = pH$Elevation, z = pH$pH,  #x was DOY
                    xo = seq(as.Date("2007-01-10"), as.Date("2020-07-30"), by = "day"), 
                    yo = seq(1120, 1160, by = 1),                     #was previously by .01
                    extrap = F, linear = T, duplicate = "strip")
interp_pH <- interp2xyz(interp_pH, data.frame=T)

interp_algae <- interp(x=algae$Date, y = algae$Elevation, z = algae$Algae_cells_mL,  #x was DOY
                       xo = seq(as.Date("2007-01-10"), as.Date("2020-07-30"), by = "day"), 
                       yo = seq(1120, 1160, by = 1),                     #was previously by .01
                       extrap = F, linear = T, duplicate = "strip")
interp_algae <- interp2xyz(interp_algae, data.frame=T)

interp_fe <- interp(x=fe$Date, y = fe$Elevation, z = fe$Fe_mgL,  #x was DOY
                    xo = seq(as.Date("2007-01-10"), as.Date("2020-07-30"), by = "day"), 
                    yo = seq(1120, 1160, by = 1),                     #was previously by .01
                    extrap = F, linear = T, duplicate = "strip")
interp_fe <- interp2xyz(interp_fe, data.frame=T)

interp_mn <- interp(x=mn$Date, y = mn$Elevation, z = mn$Mn_mgL,  #x was DOY
                    xo = seq(as.Date("2007-01-10"), as.Date("2020-07-30"), by = "day"), 
                    yo = seq(1120, 1160, by = 1),                     #was previously by .01
                    extrap = F, linear = T, duplicate = "strip")
interp_mn <- interp2xyz(interp_mn, data.frame=T)




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
      panel.spacing = unit(0, "lines"),   #chagned to panel.spacing from panel.margin in orginal code
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
pdf("./CCR_plots/Heatmaps/WVWA_CCR_2007_2020_truncated1120_to1160.pdf", width=10, height=30)  

#temperature
p1 <- ggplot(interp_temp, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
  geom_raster(aes(fill=z))+
  #scale_y_reverse()+ #this is used to reverse when using meters for depth plotting 
  #geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  geom_point(data = wvwa, aes(x = Date, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark sample dates 
  # geom_line(data = FCR_thermo_18, aes(x=Date, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  #geom_point(data = FCR_thermo_18, aes(x=Date, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  ylim(1100,1170)+
  labs(x = "Date", y = "Elevation (ft)", title = "CCR Temperature Heatmap",fill=expression(''*~degree*C*''))+ #x was day of year
  theme_black()

#temperature
p1 <- ggplot(interp_temp, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  #geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  geom_point(data = ctd, aes(x = DOY, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  #geom_line(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  #geom_point(data = FCR_thermo_18, aes(x=DOY, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "CCR 2020 Temperature Heatmap",fill=expression(''*~degree*C*''))+ #x was day of year
  theme_black()



#dissolved oxygen
p2 <- ggplot(interp_do, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
  geom_raster(aes(fill=z))+
  geom_point(data = wvwa, aes(x = Date, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  ylim(1100,1170)+
  labs(x = "Date", y = "Elevation (ft)", title = "CCR Dissolved Oxygen Heatmap", fill="mg/L")+ #x was day of year 
  theme_black()

#pH
p3 <- ggplot(interp_pH, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
  geom_raster(aes(fill=z))+
  geom_point(data = wvwa, aes(x = Date, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  ylim(1100,1170)+
  labs(x = "Date", y = "Elevation (ft)", title = "CCR pH Heatmap", fill="pH")+ #x was day of year 
  theme_black()

#alage
p4 <- ggplot(interp_algae, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
  geom_raster(aes(fill=z))+
  geom_point(data = wvwa, aes(x = Date, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  ylim(1100,1170)+
  labs(x = "Date", y = "Elevation (ft)", title = "CCR Algae Heatmap", fill="cells/mL")+ #x was day of year 
  theme_black()

#iron
p5 <- ggplot(interp_fe, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
  geom_raster(aes(fill=z))+
  geom_point(data = wvwa, aes(x = Date, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  ylim(1100,1170)+
  labs(x = "Date", y = "Elevation (ft)", title = "CCR Iron Heatmap", fill="mg/L")+ #x was day of year 
  theme_black()

#manganese 
p6 <- ggplot(interp_mn, aes(x= as.Date(x, origin = "1970-01-01"), y=y))+
  geom_raster(aes(fill=z))+
  geom_point(data = wvwa, aes(x = Date, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  ylim(1100,1170)+
  labs(x = "Date", y = "Elevation (ft)", title = "CCR Manganese Heatmap", fill="mg/L")+ #x was day of year 
  theme_black()

# create a grid that stacks all the heatmaps together. 
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3),  #would change to cbind for side to side  
                ggplotGrob(p4), ggplotGrob(p5), ggplotGrob(p6),
                size = "first"))
#ggplotGrob(p2), ggplotGrob(p3), ggplotGrob(p4), ggplotGrob(p5), ggplotGrob(p6), ggplotGrob(p7),ggplotGrob(p8),
# size = "first"))
# end the make-pdf function. 
dev.off()










