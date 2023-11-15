pacman::p_load(tidyverse, lubridate, RColorBrewer, akima,colorRamps, scales, oogledrive, readxl, googlesheets4)


dtime<-read_sheet("https://docs.google.com/spreadsheets/d/1HoBeXWUm0_hjz2bmd-ZmS0yhgF1WvLenpvwEa8dL008/edit#gid=1256821207",
                  col_types = "cnTnnc")%>%
  mutate(Date=as.Date(DateTime))

ddtime<-dtime%>%
  filter(Date>as.Date("2023-03-01"))


GHG_file <- read.csv("./Data/DataNotYetUploadedToEDI/Raw_GHG/2023/GHG_Concentration_for_EDI.csv")%>%
  mutate(date.acquired=mdy_hm(date.acquired))%>%
  dplyr::rename("Vial Number"=Sample.Name)%>%
  select(date.acquired, "Vial Number", ANALYST.NOTES, CH4_umolL, Total.concentration.of.CO2_umolL)


# Uses the vial number and finds the closest date to each other. 
# Not perfect but a good attempt for now. 

by<-join_by("Vial Number", closest(date.acquired>= DateTime))
fg<-full_join(GHG_file, ddtime, by)

clean<-fg%>%
  mutate(Check=0)%>%
  mutate(Check=ifelse(Date+3>=date.acquired,DateTime, NA))%>%
  drop_na(Check)%>%
  mutate(Rep=NA)%>%
  dplyr::rename(CO2_umolL="Total.concentration.of.CO2_umolL")%>%
  select(Reservoir, Site, DateTime, Depth_m, Rep, CH4_umolL, CO2_umolL)%>%
  mutate(CH4_umolL=as.numeric(CH4_umolL),
         CO2_umolL=as.numeric(CO2_umolL))

# Read in EDI data 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/551/7/38d72673295864956cccd6bbba99a1a3" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read_csv(infile1)%>%
  select(-starts_with("Flag"))

all<-bind_rows(dt1,clean)

# create Heat map

FCR_CH4<-all%>%
  filter(DateTime>ymd_hms("2020-04-01 00:00:00"))%>%
  filter(Reservoir=="BVR")%>%
  filter(Site==50)%>%
  group_by(DateTime, Depth_m)%>%
  summarise(CH4_umolL=mean(CH4_umolL))%>%
  ungroup()%>%
  mutate(Date=as.Date(DateTime))%>%
  #mutate(Time="12:00:00")%>%
  #mutate(Date=ymd_hms(paste0(Date, Time)))%>%
  drop_na()%>%
  unique()

FCR_CO2<-all%>%
  filter(DateTime>ymd_hms("2020-04-01 00:00:00"))%>%
  filter(Reservoir=="BVR")%>%
  filter(Site==50)%>%
  group_by(DateTime, Depth_m)%>%
  summarise(CO2_umolL=mean(CO2_umolL))%>%
  ungroup()%>%
  mutate(Date=as.Date(DateTime))%>%
  #mutate(Time="12:00:00")%>%
  #mutate(Date=ymd_hms(paste0(Date, Time)))%>%
  drop_na()%>%
  unique()

#DO
interp_CH4_umolL <- interp(x=as.numeric(FCR_CH4$Date), y = FCR_CH4$Depth_m, z = FCR_CH4$CH4_umolL,
                        #xo = seq(min(FCR_DO$DOY), max(FCR_DO$DOY), by = 0.1), 
                        xo = seq(min(as.Date(FCR_CH4$Date)), max(as.Date(FCR_CH4$Date)), by = "day"),
                        yo = seq(0.1, 11, by = 0.1),
                        extrap = F, linear = T, duplicate = "error")
interp_CH4_umolL <- interp2xyz(interp_CH4_umolL, data.frame=T)

interp_CH4_umolL=interp_CH4_umolL%>%
  mutate(Date = as.Date(x, origin = "1970-01-01")) 

#temperature
interp_FCR_CO2 <- interp(x=as.numeric(FCR_CO2$Date), y = FCR_CO2$Depth_m, z = FCR_CO2$CO2_umolL,
                          #xo = seq(min(FCR_DO$DOY), max(FCR_DO$DOY), by = 0.1), 
                          xo = seq(min(as.Date(FCR_CO2$Date)), max(as.Date(FCR_CO2$Date)), by = "day"),
                          yo = seq(0.1, 11, by = 0.1),
                          extrap = F, linear = T, duplicate = "strip")
interp_FCR_CO2 <- interp2xyz(interp_FCR_CO2, data.frame=T)

interp_FCR_CO2=interp_FCR_CO2%>%
  mutate(Date = as.Date(x, origin = "1970-01-01")) 

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

#DO
p1<-interp_CH4_umolL%>%
  #filter(DateTime>ymd_hms("2023-01-01 00:00:00"))%>%
  ggplot(., aes(x=as.Date(Date), y=y))+
  geom_raster(aes(fill=z),interpolate = TRUE)+
  scale_y_reverse()+
  ylim(11,0)+
  #geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "FCR Discreet CH4 June 2020 - Sept. 2023",fill= "UmolL")+ #x was day of year
  scale_x_date(date_breaks = "4 months") +
  theme_black()

ggsave("FCR_CH4_20231008.jpg", width=13, height=5, units="in")

#temperature
p2<-interp_FCR_CO2%>%
  #filter(DateTime>ymd_hms("2023-01-01 00:00:00"))%>%
  ggplot(., aes(x=as.Date(Date), y=y))+
  geom_raster(aes(fill=z),interpolate = TRUE)+
  scale_y_reverse()+
  ylim(11,0)+
  #geom_point(data = ctd, aes(x=DOY, y=Flag, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "FCR Discreet CO2 concentrations 2020 - Sept. 2023",fill= "umolL")+ #x was day of year
  scale_x_date(date_breaks = "4 months") +
  theme_black()

ggsave("BVR_CO2_20231020.jpg", width=13, height=5, units="in")

