## Script to visualize seasonal UGGA data
## Abigail Lewis
## 20 December 2023

#Load data
flux_all <- read.csv("2023_season_Flux_Output.csv")

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