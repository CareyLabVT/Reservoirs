# Title: Functions for Sensors Visual Inspection 
# Use in the markdown to create plots and for plotly to work
# Author: Adrienne Breef-Pilz
# First developed: 28 December 24
# Last edited: 7 April 24 - add in heat map options

all_plot<-function(
    Var,
    data,
    raw_data=NULL, 
    reservoir,
    res_site,
    y_lab,  # This label can take an expression aka have the proper degrees C, 
    y_lab2, # This label is for the plotly function which can not handle expression argument. 
    Depth=F,  # Do you want depth as a factor
    Water=T, # Are these plots for inwater streaming sensors?
    Use_plotly = F, # Do you want to produce plotly interactive plots?
    Heatmap = F) # Do you want to make a heat maps?
{ 
  
  # data=current_df
  # reservoir = "FCR"
  # res_site=50
  # Var = "TFe_mgL"
  # y_lab = "mg/L"
  # y_lab2 = "mg/L"
  # Depth=T
  # Water=F
  # Use_plotly=T
  # Heatmap = T

   # source the heat map function
    source("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/Heatmap_EDI_function.R")

  # Make a list of the names of all the possible plots
  
  cur<-cur_grid<-cur_heat<-all<-all_grid<-all_heat<-den<-box<-com_curr<-com_all<-daily_plot <- NULL
  
  # Subset the data frames for what we need 
  # rename the current data and keep it to save at the end
  current_df <- data%>%
    filter(Reservoir %in% reservoir & Site %in% res_site) %>%
    #select(DateTime, Depth_m, Var) %>%
    mutate(Date = as.Date(DateTime),
           Year=year(DateTime))
  
  # just the current year
  current <- current_df%>%
    filter(DateTime>= current_time_start & DateTime<current_time_end)%>%
    select(-contains(c("Flag", "adjusted")))|>
    mutate(type = "qaqc")
  
  # daily average
  daily <- current_df%>% 
    group_by(Date = as.Date(DateTime), Reservoir, Site, Depth_m) %>% # change group by
    summarise_if(is.numeric, mean, na.rm=T)%>%
    mutate(Year = as.factor(year(Date)),
           Month = month(Date),
           Time = "12:00:00")%>%
    mutate(DateTime= paste0(Date, Time, sep=" "))%>%
    mutate(DateTime=ymd_hms(DateTime))
  
  # colors for the plots
  colors <- c("raw" = "red", "qaqc" = "black")
  # colors for comparing Thermistor, RDO and Pressure sensor
  colors2 <- c("Therm"="magenta","RDO"="dodgerblue2" ,"Pressure"="black")
  
  ## Make a list of depths and year to see if facet plots are needed
  all_depth <- current_df%>%
    select(Depth_m)%>%
    dplyr::distinct()%>%
    as.list()
  
  # Just the current year
  cur_depth <- current%>%
    select(Depth_m)%>%
    dplyr::distinct()%>%
    as.list()
  
  # If there is just one year then don't make the density and box plots
  all_year <- current_df%>%
    select(Year)%>%
    dplyr::distinct()%>%
    as.list()
  
  # Start off with the extra plot as false and it will change to true if there are extra plots
  Extra_plot=FALSE
  
  # Are there raw files. This is only for streaming sensors where we have access to the raw files before qaqc
  if(!is.null(raw_data)){
    switch_raw=T
    
  current_raw <- raw_data%>%
    filter(Reservoir %in% reservoir & Site %in% res_site) %>% 
  filter(DateTime>=current_time_start & DateTime<current_time_end)%>%
  mutate(type = "raw")


# Let's only keep values that are different instead of plotting the raw and the qaqc value
current_plot_df <- bind_rows(current, current_raw)%>%
  select(-contains(c("Flags", "RECORD")))
  dplyr::distinct(across(everything(), .keep_all = T))
    
    # If there are raw files to compare to then we only want to keep both observations if they are different.
    # It makes the file smaller. 
    # Let's just look at the current
    # only keep observations that are distinct for that variable
    
    df_unique <- current_plot_df[!duplicated(current_plot_df[c('DateTime', Var)]),]
    
    # create layers
    qaqc_current <- df_unique%>%filter(type=="qaqc")
    
    raw_current <- df_unique%>%filter(type=="raw")
    
  }else{
    switch_raw=F
    
    # rename the file
    qaqc_current <- current
  }
  
  
  
  if(Water==T){
    
    if(reservoir=="FCR" & Var %in% c("ThermistorTemp_C_5","ThermistorTemp_C_9")){
      Extra_plot = TRUE
      if (Var=="ThermistorTemp_C_5"){
        Var2="RDOTemp_C_5"
        Switch=F # Used to switch on and off the layer for the pressure sensor
      }else if (Var=="ThermistorTemp_C_9"){
        Var2="RDOTemp_C_9"
        Var3="LvlTemp_C_9"
        Switch=T # Used to switch on and off the layer for the pressure sensor
      } 
    }else if (reservoir=="BVR" & Var %in% c("ThermistorTemp_C_6","ThermistorTemp_C_13")){
      Extra_plot=TRUE
      if (Var=="ThermistorTemp_C_6"){
        Var2="RDOTemp_C_6"
        Switch=F # Used to switch on and off the layer for the pressure sensor
      }else if (Var=="ThermistorTemp_C_13"){
        Var2="RDOTemp_C_13"
        Var3="LvlTemp_C_13"
        Switch=T # Used to switch on and off the layer for the pressure sensor
      }
    }else if (reservoir=="CCR" & Var =="ThermistorTemp_C_13"){
      Extra_plot=TRUE
      Var2 = "LvlTemp_C_13"
      Switch=F # Used to switch on and off the layer for the pressure sensor
      
    }
    if(Extra_plot==TRUE){
      # Plot alltemp sensors at the same depth on top of each other
      # graph all
      com_all<-ggplot() +
        geom_scattermore(data=current_df, aes(x=DateTime, y=.data[[Var]], color="Therm"))+ 
        geom_scattermore(data=current_df, aes(x=DateTime, y=.data[[Var2]], color="RDO"))+
        {if(Switch)geom_scattermore(data=current, aes(x=DateTime, y=.data[[Var3]], color="Pressure"))}+ # print layer if Switch is TRUE
        ggtitle(paste0("All Water Temp from"," ",Var,", ",Var2," ",reservoir," ",res_site)) +
        labs(y = y_lab,
             color = "Legend") +
        scale_color_manual(values = colors2)+
        theme_bw()
      
      # Plots of just the current year with all temperatures reading from the same depth at different sensors
      
      com_curr <- ggplot() +
        geom_scattermore(data = qaqc_current, aes(x = DateTime, y=.data[[Var]], color="Therm"), pointsize = 3)+ 
        geom_scattermore(data = qaqc_current, aes(x = DateTime, y=.data[[Var2]], color="RDO"), pointsize = 3)+
        {if(Switch)geom_scattermore(data = qaqc_current, aes(x = DateTime, y=.data[[Var3]],
                                                             color="Pressure"), pointsize = 3)}+ # print layer if Switch is TRUE
        ggtitle(paste0("Current Water Temp from"," ",Var,", ",Var2," ",reservoir," ",res_site)) +
        labs(y = y_lab2,
             color = "Legend") +
        scale_color_manual(values = colors2)+
        theme_bw()
    }
    
  } 
  
  # Plot all of the observations
  if (Depth==T){
    
    all<- 
      ggplot() +
      geom_scattermore(data=current_df, aes(x=DateTime, y=.data[[Var]], color=as.factor(Depth_m)), pointsize = 3)+
      ggtitle(paste0("All QAQCd",Var," ",reservoir," ",res_site)) +
      labs(y = y_lab,
           color = "Legend") +
      #scale_color_manual(values = colors)+
      theme_bw()
    
    if(length(all_depth[["Depth_m"]])>1){
    
    all_grid <- current_df%>%
      drop_na(Var)%>%
      ggplot(.)+
      geom_scattermore(aes(x=DateTime, y=.data[[Var]], color=as.factor(Depth_m)), pointsize = 9)+
      labs(y = y_lab,
           color = "Legend") +
      ggtitle(paste0("All ",Var," by Depth"," ",reservoir," ",res_site)) +
      theme_bw()+
      facet_wrap(~ as.factor(Depth_m), scale="free")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
  }else {
    all <- geom_scattermore(data=current_df, aes(x=DateTime, y=.data[[Var]], color="qaqc"))+
      ggtitle(paste0("All QAQCd",Var," ",reservoir," ",res_site)) +
      labs(y = y_lab,
           color = "Legend") +
      scale_color_manual(values = colors)+
      theme_bw()
    
  }
  
  
  # Create the current plotly so we can get the date of points out of range 
  
  # Battery and cable power don't need to be interactive
if(length(qaqc_current$Reservoir)>0){
  if(grepl("Battery_V|power_V", Var)|Use_plotly==FALSE){
    
    if(Depth==T){
      cur <- 
        ggplot()+
        {if(switch_raw)geom_scattermore(data= raw_current, aes(x=DateTime, y=.data[[Var]], 
                                                               color=as.factor(Depth_m),shape=type), pointsize = 3)}+
        geom_scattermore(data= qaqc_current, aes(x=DateTime, y=.data[[Var]], 
                                                 color=as.factor(Depth_m), shape=type), pointsize = 3)+
        ggtitle(paste0("Current: ",Var," ",reservoir," ",res_site)) +
        labs(y = y_lab2,
             color = "Legend") +
        #scale_color_manual(values = colors)+
        theme_bw()
      
      # use facet wrap
      if(length(cur_depth[["Depth_m"]])>1){
        
        cur_grid <- qaqc_current%>%
          drop_na(Var)%>%
          ggplot(.)+
          geom_scattermore(aes(x=DateTime, y=.data[[Var]], color=as.factor(Depth_m)), pointsize = 9)+
          labs(y = y_lab,
               color = "Legend") +
          ggtitle(paste0("Current ",Var, " by Depth"," ",reservoir," ",res_site)) +
          theme_bw()+
          facet_wrap(~ as.factor(Depth_m), scale="free")
      }
    }else{
      cur <- ggplot()+
        {if(switch_raw)geom_scattermore(data= raw_current, aes(x=DateTime, y=.data[[Var]], 
                                                               color=type), pointsize = 3)}+
        geom_scattermore(data= qaqc_current, aes(x=DateTime, y=.data[[Var]], 
                                                 color=type), pointsize = 3)+
        ggtitle(paste0("Current: ",Var," ",reservoir," ",res_site)) +
        labs(y = y_lab2,
             color = "Legend") +
        scale_color_manual(values = colors)+
        theme_bw()
    }
  }else{
    
    if(Depth==T){
      cur <- {ggplot()+
          {if(switch_raw) geom_point(data= raw_current, aes(x=DateTime, y=.data[[Var]], 
                                                            color=as.factor(Depth_m),shape=type))}+
          geom_point(data= qaqc_current, aes(x=DateTime, y=.data[[Var]], 
                                             color=as.factor(Depth_m),shape=type))+
          ggtitle(paste0("Current: ",Var," ",reservoir," ",res_site)) +
          labs(y = y_lab2,
               color = "Legend") +
          #scale_color_manual(values = colors)+
          theme_bw()}%>% ggplotly%>% as_widget
      
      # use facet wrap
      if(length(cur_depth[["Depth_m"]])>1){
        
        cur_grid <- qaqc_current%>%
          drop_na(Var)%>%
          ggplot(.)+
          geom_scattermore(aes(x=DateTime, y=.data[[Var]], color=as.factor(Depth_m)), pointsize = 9)+
          labs(y = y_lab,
               color = "Legend") +
          ggtitle(paste0("Current ",Var, " by Depth"," ",reservoir," ",res_site)) +
          theme_bw()+
          facet_wrap(~ as.factor(Depth_m), scale="free")
      }
    }else{
      cur <- {ggplot()+
          {if(switch_raw) geom_point(data= raw_current, aes(x=DateTime, y=.data[[Var]], 
                                                            color=type))}+
          geom_point(data= qaqc_current, aes(x=DateTime, y=.data[[Var]], 
                                             color=type))+
          ggtitle(paste0("Current: ",Var," ",reservoir," ",res_site)) +
          labs(y = y_lab2,
               color = "Legend") +
          scale_color_manual(values = colors)+
          theme_bw()}%>% ggplotly%>% as_widget
      
    }
  }
} 
  # density plot
  
  if(length(all_year[["Year"]])>1){
  
  if(Depth==T){
    den <-daily%>%
      drop_na(Var)%>%
      ggplot(., aes(x = .data[[Var]], group = Year, fill = Year))+
      geom_density(alpha=0.5)+
      xlab("Daily avg.")+
      ggtitle(paste0("All",Var," ",reservoir," ",res_site)) +
      theme_bw()+
      facet_wrap(~ as.factor(Depth_m), scale="free")
    
    }else{
    den <-ggplot(data = daily, aes(x = .data[[Var]], group = Year, fill = Year))+
      geom_density(alpha=0.5)+
      xlab("Daily avg.")+
      ggtitle(paste0("All"," ",Var," ",reservoir," ",res_site)) +
      theme_bw()
    }
  } 
  
  # box plot
  if(length(all_year[["Year"]])>1){
    if(Depth==T){
    box <-daily%>%
      drop_na(Var)%>%
      ggplot(., aes(x = Year, y = .data[[Var]], group = Year, fill = Year))+
      geom_boxplot()+
      ylab(y_lab)+
      ggtitle(paste0("Boxplot",Var," ",reservoir," ",res_site)) +
      theme_bw()+
      facet_wrap(~ as.factor(Depth_m), scales="free")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
   }else{
    box <-ggplot(data = daily, aes(x = Year, y = .data[[Var]], group = Year, fill = Year))+
      geom_boxplot()+
      ylab(y_lab)+
      ggtitle(paste0("Boxplot",Var," ",reservoir," ",res_site)) +
      theme_bw()
    }
  } 
  # Create Heat maps
  
  if(Heatmap==T & length(all_depth[["Depth_m"]])>1){
    
    if(length(qaqc_current$Reservoir)>0){
    # Use this heatmap function 
    # create the heat map for the current year
    cur_heat <- heatmap_EDI(data= qaqc_current, reservoir=reservoir, site=res_site, z=Var)
    }
    
    # heat map for all the observations
    all_heat <- heatmap_EDI(data=current_df, reservoir = reservoir, site = res_site, z=Var)
    
  }
  # Add extra plots for chla and blue greens
  # Might need to Take out NA
  # data_comp <- daily[!is.na(daily$EXOChla_ugL_1), ] 
  
  if(grepl("Chla|BGAPC", Var)){
    
    daily_plot <- ggplot() +
      geom_point(data=current_df, aes(x=DateTime, y=.data[[Var]]))+
      {if(grepl("Chla", Var))geom_line(data=daily, aes(x=DateTime, y=.data[[Var]]), col="green")}+# print layer if Switch is TRUE
      {if(grepl("BGAPC", Var))geom_line(data=daily, aes(x=DateTime, y=.data[[Var]]), col="blue")}+
      ylab(y_lab)+
      ggtitle(paste0(Var, "Compare 10 minute observations to Daily mean"," ",reservoir," ",res_site))+
      theme_bw()
  }
  
  
  
  # Make a list of the plots. 
  
  # List of all possible plots
  alllist <- list(cur,cur_grid,cur_heat,all,all_grid,all_heat,den,box,com_curr,com_all,daily_plot) # have to list all outputs under one name
  
  # select only those plots that we made. Drop those that are NULL
  newlist <- alllist[!unlist(lapply(alllist, is.null))]
  
  
  # Prints the plots where they should be. 
  # Plotly would always print out of order. 
  # This is how I could get them working for each Variable
  
  
  for(j in 1:length(newlist)){
    x <- newlist[[j]]
    
    if (inherits(x, "plotly")) {
      # print the html piece of the htmlwidgets
      cat(renderTags(x)$html)
    } else {
      # print the ggplot graphs
      print(x)
      
    }
  }
  
  return(newlist)
}
