## Script to visualize seasonal UGGA data
## Abigail Lewis
## created: 20 December 2023
## last edit: 03 January 2024 (ADD)

## load historical data 
historical_data  <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1082/2/dd66453fae01815ee574bd69bb9fb213") 

#Load current data
current_data <- read_csv('https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/UGGA/UGGA_Raw/UGGA_L1.csv')

## combine all data
flux_all <- bind_rows(current_data, historical_data)

#Plot all CH4
flux_all%>%
  ggplot(aes(x = as.Date(Date), 
             color = as.factor(Site),
             y=ch4_flux_umolCm2s))+
  geom_smooth()+
  geom_point()+
  ylab("CH4 flux (µumol/m2/s)")+
  facet_wrap(~Reservoir)

#Plot all CO2
flux_all%>%
  ggplot(aes(x = as.Date(Date), 
             color = as.factor(Site),
             y=co2_flux_umolCm2s))+
  geom_smooth()+
  geom_point()+
  ylab("CO2 flux (µumol/m2/s)")+
  facet_wrap(~Reservoir)

#Compare FCR and BVR
flux_all%>%
  filter(Site==50)%>%
  ggplot(aes(x = as.Date(Date), 
             y = ch4_flux_umolCm2s, 
             color = Reservoir))+
  geom_smooth()+
  geom_point()+
  geom_vline(xintercept = as.Date("2022-05-29"))+
  ylab("CH4 flux (µumol/m2/s)")+
  xlab("Date")+
  ggtitle("Site 50")

#Compare FCR and BVR
flux_all%>%
  filter(Site==50)%>%
  ggplot(aes(x = as.Date(Date), 
             y = co2_flux_umolCm2s, 
             color = Reservoir))+
  geom_smooth()+
  geom_point()+
  geom_vline(xintercept = as.Date("2022-05-29"))+
  ylab("CO2 flux (µumol/m2/s)")+
  xlab("Date")+
  ggtitle("Site 50")