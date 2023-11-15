library(tidyverse)


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



Metals_2014_2022 <- read_csv("Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLmetals/Metals_2014_2022.csv")

FCR_met<-Metals_2014_2022%>%
  filter(Reservoir=="FCR")%>%
  filter(Site==50)%>%
  filter(Depth_m==9.0)

ggplot(FCR_met, aes(x=DateTime))+
  geom_point(aes(y=TMn_mgL), col="blue")+
    geom_point(aes(y=SMn_mgL), col="red")+
  theme_bw()


CCR<-Metals_2014_2022%>%
  filter(Reservoir=="CCR")%>%
  filter(Site==50)%>%
  dplyr::rename(Date="DateTime")%>%
  mutate(Depth_m=ifelse(Depth_m==21,20,Depth_m))
         #Depth_m=ifelse(Depth_m==18,20,Depth_m))


###getting heat maps 
#come back here after you do it for BVR and now do it for FCR 
ctd <- CCR



depths = seq(0, 20, by = 1)
df.final<-data.frame()

for (i in 1:length(depths)){
  
  fp_layer<-ctd %>% 
    mutate(Date_only = as.Date(Date)) %>% 
    group_by(Date_only) %>% 
    slice(which.min(abs(as.numeric(Depth_m) - depths[i])))
  
  # Bind each of the data layers together.
  df.final = bind_rows(df.final, fp_layer)
}
  
  
  

ctd <- arrange(df.final, Date)
#ctd$Depth_m <- round(as.numeric(ctd$Depth_m), digits = 1)
ctd$Depth_m <-   round(ctd$Depth_m/0.5)*0.5  # new rounding function to ensure values get to nearest 0.5 

CCR_TMn <-CCR%>%
  mutate(DOY=yday(Date))%>%
  select(DOY,Date, Depth_m, TMn_mgL)%>%
  drop_na()%>%
  unique()

# Go back to line 72 and switch to the reservoir you want
  CCR_SMn <- CCR%>%
    mutate(DOY=yday(Date))%>%
    select(DOY,Date, Depth_m, SMn_mgL)%>%
    drop_na()%>%
  unique()



# Complete data interpolation for the heatmaps
CCR_TMn_saved <- CCR_TMn
CCR_TMn=CCR_TMn %>%
  mutate(Rand = rnorm(nrow(CCR_TMn)))%>%
  arrange(Rand) %>%
  ungroup()
#specific conductivity
interp_spccond_BVR <- interp(x=CCR_TMn$DOY, y = CCR_TMn$Depth_m, z = CCR_TMn$TMn_mgL,
                             xo = seq(min(CCR_TMn$DOY), max(CCR_TMn$DOY), by = 0.1), 
                             #xo = seq(min(ymd_hms(CCR_TMn$Date)), max(ymd_hms(CCR_TMn$Date)), by = "day"),
                             yo = seq(0.1, 20, by = 1),
                             extrap = F, linear = T, duplicate = "error")
interp_spccond_BVR <- interp2xyz(interp_spccond_BVR, data.frame=T)

interp_spccond_BVR=interp_spccond_BVR%>%
  mutate(DateTime = as.POSIXct(x, origin = "1970-01-01")) 

#specific conductivity
interp_CCR_SMn <- interp(x=CCR_SMn$Date, y = CCR_SMn$Depth_m, z = CCR_SMn$SMn_mgL,
                             #xo = seq(min(CCR_TMn$DOY), max(CCR_TMn$DOY), by = 0.1), 
                             xo = seq(min(ymd_hms(CCR_SMn$Date)), max(ymd_hms(CCR_SMn$Date)), by = "day"),
                             yo = seq(0.1, 20, by = 0.01),
                             extrap = F, linear = T, duplicate = "strip")
interp_CCR_SMn <- interp2xyz(interp_CCR_SMn, data.frame=T)

interp_CCR_SMn=interp_CCR_SMn%>%
  mutate(DateTime = as.POSIXct(x, origin = "1970-01-01")) 



#datetemp <- interp_temp %>% 
#  mutate(Realdate = as.Date(x, origin = "1970-01-01"))






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

#temperature
p1 <- ggplot(interp_spccond_BVR, aes(x=x, y=y))+
  geom_raster(aes(fill=z),interpolate = TRUE)+
  scale_y_reverse()+
  ylim(20,0)+
  #geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  #geom_point(data = CCR_TMn, aes(x = Date, y = 0.1, z = NULL), pch = 25, size = 3, color = "white", fill = "black")+ #to mark cast dates 
  # geom_line(data = FCR_thermo_18, aes(x=Date, y=thermo.depth, z=NULL), color = "black", lwd = 1)+
  #geom_point(data = FCR_thermo_18, aes(x=Date, y=thermo.depth, z=NULL), pch = 21, size = 2, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "CCR Soluble Mn in 2022",fill= "mg/L")+ #x was day of year
  theme_black()
