# Title: heatmap_EDI function
# Authors: Mary Lofton and Adrienne Breef-Pilz
# Created: 04 April 2024
# Edited: 

# Heatmap function for inspection plots. 
# Took Mary's code from Fluoroprobe



heatmap_EDI <- function(data, reservoir, site, z){
  
  # load packages
  pacman::p_load(tidyverse, lubridate, akima, reshape2, 
               gridExtra, grid, colorRamps, RColorBrewer, cowplot)
  #subset to relevant data
  fp <- data %>%
    filter(Reservoir %in% reservoir & Site %in% site) %>%
    select(DateTime, Depth_m, {{z}}) %>%
    mutate(Date = as.Date(DateTime))
  
  #slice by depth for each reservoir
  if (reservoir == "FCR"){
    
    depths = seq(0.1, 9.3, by = 0.3)
    df.final<-data.frame()
    
    for (i in 1:length(depths)){
      
      fp_layer <- fp %>% 
        group_by(Date) %>% 
        slice(which.min(abs(as.numeric(Depth_m) - depths[i])))
      
      # Bind each of the data layers together.
      df.final = bind_rows(df.final, fp_layer)%>%
       dplyr::distinct()
      
    }
    
    
  } else if (reservoir == "BVR"){
    
    depths = seq(0.1, 10, by = 0.3)
    df.final<-data.frame()
    
    for (i in 1:length(depths)){
      
      fp_layer<-fp %>% group_by(Date) %>% 
        slice(which.min(abs(as.numeric(Depth_m) - depths[i])))
      
      # Bind each of the data layers together.
      df.final = bind_rows(df.final, fp_layer)%>%
        dplyr::distinct()
      
    }
    
  } else if(reservoir == "CCR"){
    
    depths = seq(0.1, 20, by = 0.3)
    df.final<-data.frame()
    
    for (i in 1:length(depths)){
      
      fp_layer<-fp %>% group_by(Date) %>% 
        slice(which.min(abs(as.numeric(Depth_m) - depths[i])))
      
      # Bind each of the data layers together.
      df.final = bind_rows(df.final, fp_layer)%>%
        dplyr::distinct()
      
    }
  } else if(reservoir == "GWR"){
    
    depths = seq(0.1, 12, by = 0.3)
    df.final<-data.frame()
    
    for (i in 1:length(depths)){
      
      fp_layer<-fp %>% group_by(Date) %>% 
        slice(which.min(abs(as.numeric(Depth_m) - depths[i])))
      
      # Bind each of the data layers together.
      df.final = bind_rows(df.final, fp_layer)%>%
        dplyr::distinct()
      
    }
  } else if(reservoir == "SHR"){
    
    depths = seq(0.1, 30, by = 0.3)
    df.final<-data.frame()
    
    for (i in 1:length(depths)){
      
      fp_layer<-fp %>% group_by(Date) %>% 
        slice(which.min(abs(as.numeric(Depth_m) - depths[i])))
      
      # Bind each of the data layers together.
      df.final = bind_rows(df.final, fp_layer)%>%
        dplyr::distinct()
      
    } 
    
  }
  
  #wrangle final dataframe for plotting
  # Re-arrange the data frame by date
  fp_new <- arrange(df.final, DateTime)%>%
    drop_na(z)
  
  # Round each extracted depth to the nearest 10th. 
  fp_new$Depth_m <- round(as.numeric(fp_new$Depth_m), digits = 0.5)
  
  # Create title for plot
  fig_title <- paste(reservoir, "Site", site, z, sep = " ")
  
  interp <- interp(x=as.numeric(fp_new$Date), y = fp_new$Depth_m, z = unlist(fp_new[z]),
                   #xo = seq(min(fp_new$DOY), max(fp_new$DOY), by = .1), 
                   xo = seq(min(fp_new$Date), max(fp_new$Date), by = "day"),
                   yo = seq(min(fp_new$Depth_m), max(fp_new$Depth_m), by = 0.1),
                   extrap = T, linear = T, duplicate = "strip")
  interp <- interp2xyz(interp, data.frame=T)
  
  interp<-interp%>%
    mutate(Date=as.Date(x, origin = "1970-01-01"))
  
  
  p1 <- ggplot(interp, aes(x=Date, y=y))+
    geom_raster(aes(fill=z))+
    scale_y_reverse(expand = c(0,0))+
    scale_x_date(expand = c(0, 0)) +
    scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
    labs(x = "Date", y = "Depth (m)", title = fig_title,fill=("mg/L"))+
    theme_bw()
  
  return(p1)
  
}
