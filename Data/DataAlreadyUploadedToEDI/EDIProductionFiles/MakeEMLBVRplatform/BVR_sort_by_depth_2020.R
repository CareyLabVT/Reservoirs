pacman::p_load("tidyverse","lubridate", "plotly")

#read in data
bvr=read.csv("BVR_EDI_2020.csv")
depth=read.csv("BVR_Depth_offsets_2020.csv")

#take out EXO data so you can add it back in later
EXO=bvr%>%
  select(Reservoir, Site, DateTime, starts_with("EXO"), Flag_All, Flag_DO_1.5, Flag_Chla, Flag_Cond, Flag_Phyco,
         Flag_TDS, Flag_fDOM)

Flags=bvr%>%
  select(Reservoir,Site, DateTime, Depth_m_13, starts_with("Flag_Temp"), starts_with("Flag_DO"), Flag_Lvl_13, -Flag_DO_1.5)%>%
  pivot_longer(-c(Reservoir,Site,DateTime,Depth_m_13), names_to="Sensor", values_to="Flag", values_drop_na=FALSE)%>%
  separate(Sensor,c("Word","Sensor","Position"),"_")%>%
  select(Reservoir, Site, DateTime, Sensor, Position, Flag, Depth_m_13)%>%
  mutate(Position=as.numeric(Position))%>%
  merge(.,depth)%>%
  mutate(Sensor_depth=Depth_m_13-Offset)%>%
  select(Reservoir, Site, DateTime, Sensor, Flag, Sensor_depth)%>%
  mutate(Depth = 0)%>%
  mutate(Depth = ifelse(is.na(Sensor_depth), NA, Depth))%>%
  mutate(Depth = ifelse(Sensor_depth < 0, NA, Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 0 & Sensor_depth < 0.2, "0.1m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 0.2 & Sensor_depth < 0.5, "0.3m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 0.5 & Sensor_depth < 1.5, "1m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 1.5 & Sensor_depth < 2.5, "2m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 2.5 & Sensor_depth < 3.5, "3m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 3.5 & Sensor_depth < 4.5, "4m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 4.5 & Sensor_depth < 5.5, "5m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 5.5 & Sensor_depth < 6.5, "6m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 6.5 & Sensor_depth < 7.5, "7m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 7.5 & Sensor_depth < 8.5, "8m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 8.5 & Sensor_depth < 9.5, "9m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 9.5 & Sensor_depth < 10.5, "10m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 10.5 & Sensor_depth < 11.5, "11m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 11.5 & Sensor_depth < 12.5, "12m", Depth))%>%
  filter(!is.na(Depth))%>%
  mutate(Sensor=paste(Sensor,Depth, sep = "_"))%>%
  pivot_wider(
    id_cols=c(Reservoir, Site, DateTime),
    names_from = Sensor,
    names_sep = "_",
    values_from = c(Flag))

#take out the temp string data so we sort everything by depth
bvr_new=bvr%>%
  select(Reservoir,Site,DateTime, starts_with("Ther"), starts_with("RDO"), starts_with("Lvl"),Depth_m_13)%>%
  pivot_longer(-c(Reservoir,Site,DateTime,Depth_m_13), names_to="Sensor", values_to="Reading", values_drop_na=FALSE)%>%
  separate(Sensor,c("Sensor","Units","Position"),"_")%>%
  select(Reservoir, Site, DateTime, Sensor, Position, Reading, Depth_m_13)%>%
  #merge(.,Flags)%>%
  mutate(Position=as.numeric(Position))%>%
  merge(.,depth)%>%
  mutate(Sensor_depth=Depth_m_13-Offset)

bvr_new=bvr_new%>%
  select(Reservoir, Site, DateTime, Sensor, Reading, Sensor_depth)%>%
    mutate(Depth = 0)%>%
    mutate(Depth = ifelse(is.na(Sensor_depth), NA, Depth))%>%
    mutate(Depth = ifelse(Sensor_depth < 0, NA, Depth))%>%
    mutate(Depth = ifelse(Sensor_depth > 0 & Sensor_depth < 0.2, "0.1m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth > 0.2 & Sensor_depth < 0.5, "0.3m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth > 0.5 & Sensor_depth < 1.5, "1m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth > 1.5 & Sensor_depth < 2.5, "2m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth > 2.5 & Sensor_depth < 3.5, "3m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth > 3.5 & Sensor_depth < 4.5, "4m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth > 4.5 & Sensor_depth < 5.5, "5m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth > 5.5 & Sensor_depth < 6.5, "6m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth > 6.5 & Sensor_depth < 7.5, "7m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth > 7.5 & Sensor_depth < 8.5, "8m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth > 8.5 & Sensor_depth < 9.5, "9m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth > 9.5 & Sensor_depth < 10.5, "10m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth > 10.5 & Sensor_depth < 11.5, "11m", Depth))%>%
    mutate(Depth = ifelse(Sensor_depth > 11.5 & Sensor_depth < 12.5, "12m", Depth))

bvr_new=bvr_new%>%
  filter(!is.na(Depth))%>%
  mutate(Sensor=paste(Sensor,Depth, sep = "_"))%>%
  pivot_wider(
    id_cols=c(Reservoir, Site, DateTime),
    names_from = Sensor,
    names_sep = "_",
    values_from = c(Reading))%>%
  select(Reservoir, Site, DateTime, ThermistorTemp_0.1m, ThermistorTemp_0.3m, ThermistorTemp_1m,ThermistorTemp_2m,
         ThermistorTemp_3m, ThermistorTemp_4m, ThermistorTemp_5m, ThermistorTemp_6m, ThermistorTemp_7m, ThermistorTemp_8m,
         ThermistorTemp_9m, ThermistorTemp_10m, ThermistorTemp_11m, ThermistorTemp_12m, RDO_3m, RDO_4m, RDO_5m, 
         RDOsat_3m, RDOsat_4m, RDOsat_5m,RDOTemp_3m, RDOTemp_4m,RDOTemp_5m, RDO_11m, RDO_12m, 
         RDOsat_11m, RDOsat_12m, RDOTemp_11m, RDOTemp_12m, Lvl_11m, Lvl_12m,LvlTemp_11m,LvlTemp_12m)
  
  bvr_new_6=bvr_new%>%
    merge(.,EXO)

#subset and change to long format to graph
bvr_new_7=bvr_new_6%>%
  select(DateTime, starts_with("Ther"))%>%
  pivot_longer(-DateTime, names_to="Depth", values_to="Reading", values_drop_na=FALSE)

#level_order <- factor(iris$Species, level = c('virginica', 'versicolor', 'setosa'))
  bvr_new_7$Depth=factor(bvr_new_7$Depth, level=c("ThermistorTemp_0.1m", "ThermistorTemp_0.3m", "ThermistorTemp_1m","ThermistorTemp_2m",
          "ThermistorTemp_3m", "ThermistorTemp_4m", "ThermistorTemp_5m", "ThermistorTemp_6m", "ThermistorTemp_7m", "ThermistorTemp_8m",
          "ThermistorTemp_9m", "ThermistorTemp_10m", "ThermistorTemp_11m", "ThermistorTemp_12m"))

bvr_new_7$DateTime<-as.POSIXct(bvr_new_7$DateTime,format = "%Y-%m-%d %H:%M:%S")

Temp_depth <- ggplot(data = bvr_new_7, aes(x = DateTime, y = Reading, color=Depth)) +
 geom_line() +
labs(y = "Celsius")
  
Temp_depth

  bvr_new_2=bvr_new%>%
  mutate(Position=as.numeric(Position))%>%
  merge(.,depth)%>%
  mutate(Sensor_depth=Depth_m_13-Offset)%>%
  select(Reservoir, Site, DateTime, Sensor, Reading, Flag, Flag_All, Sensor_depth)

bvr_new_3=bvr_new_2%>%
  mutate(Depth = 0)%>%
  mutate(Depth = ifelse(is.na(Sensor_depth), NA, Depth))%>%
  mutate(Depth = ifelse(Sensor_depth < 0, NA, Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 0 & Sensor_depth < 0.2, "0.1m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 0.2 & Sensor_depth < 0.5, "0.3m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 0.5 & Sensor_depth < 1.5, "1m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 1.5 & Sensor_depth < 2.5, "2m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 2.5 & Sensor_depth < 3.5, "3m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 3.5 & Sensor_depth < 4.5, "4m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 4.5 & Sensor_depth < 5.5, "5m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 5.5 & Sensor_depth < 6.5, "6m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 6.5 & Sensor_depth < 7.5, "7m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 7.5 & Sensor_depth < 8.5, "8m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 8.5 & Sensor_depth < 9.5, "9m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 9.5 & Sensor_depth < 10.5, "10m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 10.5 & Sensor_depth < 11.5, "11m", Depth))%>%
  mutate(Depth = ifelse(Sensor_depth > 11.5 & Sensor_depth < 12.5, "12m", Depth))

bvr_new_4=bvr_new%>%
  filter(!is.na(Depth))%>%
  mutate(Sensor=paste(Sensor,Depth, sep = "_"))%>%
  pivot_wider(
    id_cols=c(Reservoir, Site, DateTime),
      names_from = Sensor,
      names_sep = "_",
      values_from = c(Reading)
  )

bvr_new_5=bvr_new_4%>%
  select(Reservoir, Site, DateTime, ThermistorTemp_0.1m, ThermistorTemp_0.3m, ThermistorTemp_1m,ThermistorTemp_2m,
         ThermistorTemp_3m, ThermistorTemp_4m, ThermistorTemp_5m, ThermistorTemp_6m, ThermistorTemp_7m, ThermistorTemp_8m,
         ThermistorTemp_9m, ThermistorTemp_10m, ThermistorTemp_11m, ThermistorTemp_12m, RDO_3m, RDO_4m, RDO_5m, 
         RDOsat_3m, RDOsat_4m, RDOsat_5m,RDOTemp_3m, RDOTemp_4m,RDOTemp_5m, RDO_11m, RDO_12m, 
         RDOsat_11m, RDOsat_12m, RDOTemp_11m, RDOTemp_12m, Lvl_11m, Lvl_12m,LvlTemp_11m,LvlTemp_12m)
  

str(bvr_new_4)  

  #rename flag columns
  
  
    rename(Flag_ThermistorTemp_1=Flag_Temp_1,Flag_ThermistorTemp_2=Flag_Temp_2,Flag_ThermistorTemp_3=Flag_Temp_3,Flag_ThermistorTemp_4=Flag_Temp_4,
           Flag_ThermistorTemp_5=Flag_Temp_5,Flag_ThermistorTemp_6=Flag_Temp_6,Flag_ThermistorTemp_7=Flag_Temp_7,Flag_ThermistorTemp_8=Flag_Temp_8,
           Flag_ThermistorTemp_9=Flag_Temp_9,Flag_ThermistorTemp_10=Flag_Temp_10,Flag_ThermistorTemp_11=Flag_Temp_11,Flag_ThermistorTemp_12=Flag_Temp_12,
           Flag_ThermistorTemp_13=Flag_Temp_13,Flag_RDO_6=Flag_DO_6, Flag_RDO_13=Flag_DO_13)%>%
    mutate(Flag_RDOsat_6=Flag_RDO_6)%>%
    mutate(Flag_RDOTemp_6=Flag_RDO_6)%>%
    mutate(Flag_RDOsat_13=Flag_RDO_13)%>%
    mutate(Flag_RDOTemp_13=Flag_RDO_13)%>%
    mutate(Flag_LvlTemp_13=Flag_Lvl_13)
  
