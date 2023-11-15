#check out to see how IR down is related to surface water temp

library(tidyverse)

download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/Catwalk.csv","Catwalk.csv")

fcrdata=read.csv("Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_FCRcatwalk/2021/FCR_Catwalk_2018_2021.csv")

Met=read.csv("Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_FCRMetData/2021/FCR_Met_final_2015_2021.csv")

fcrdata$DateTime<-as.POSIXct(fcrdata$DateTime, "%Y-%m-%d %H:%M:%S", tz = "EST")

Met$DateTime<-as.POSIXct(Met$DateTime, "%Y-%m-%d %H:%M:%S", tz = "EST")

Met_IR=Met%>%
  select(DateTime, InfraredRadiationDown_Average_W_m2)
  filter(DateTime>"2021-12-01 00:00" & DateTime<"2021-12-31 23:59")


fcrdata_surface=fcrdata%>%
  select(DateTime,ThermistorTemp_C_surface)
  filter(DateTime>"2021-12-01 00:00" & DateTime<"2021-12-31 23:59")



Met_sur=merge(fcrdata_surface,Met_IR, all.x=T)

compare=Met_sur%>%
  filter(DateTime>"2021-12-01 00:00"& DateTime<"2021-12-31 23:59")
  

Twenty_raw=Met_raw%>%
  filter(DateTime>"2020-12-31 23:59"& DateTime<"2021-12-31 23:59")

Twenty=Met%>%
  filter(DateTime>"2021-12-01 00:00"& DateTime<"2021-12-31 23:59")

Twenty_old=Met_old%>%
  filter(DateTime>"2021-12-01 00:00"& DateTime<"2021-12-31 23:59")

Tweenty_Met_NA=Met_no_NAreplace%>%
  filter(DateTime>"2021-12-01 00:00"& DateTime<"2021-12-31 23:59")

FCR_dec_2021=fcrdata%>%
  filter(TIMESTAMP>"2021-12-01 00:00"& TIMESTAMP<"2021-12-31 23:59")

x11(); par(mfrow=c(2,2))
plot(Twenty_raw$DateTime, Twenty_raw$InfraredRadiationDown_Average_W_m2, col="red", type='l', ylim=c(300,550))
plot(Twenty$DateTime, Twenty$InfraredRadiationDown_Average_W_m2, type = 'l')
plot(FCR_dec_2021$TIMESTAMP, FCR_dec_2021$wtr_surface, type='l', col="blue")
plot(Tweenty_Met_NA$DateTime, Tweenty_Met_NA$InfraredRadiationDown_Average_W_m2, type="l", col="green")
plot(Twenty$DateTime, Twenty$AirTemp_Average_C, type="l", col="magenta")
plot(Twenty$DateTime, Twenty$InfraredRadiationDown_Average_W_m2, type='l')
points(Twenty_old$DateTime, Twenty_old$InfraredRadiationDown_Average_W_m2, type='l', col="blue")


ggplot(NULL)+
  geom_line(data=Twenty, aes(x=DateTime,y=CR3000Panel_temp_C), col="black")+
  geom_line(data=FCR_dec_2021, aes(x=TIMESTAMP, y=PTemp_C), col="red") 


#Two-axis graph

coeff <- 70
  
  ggplot(NULL)+
  geom_line(data=Twenty, aes(x=DateTime,y=InfraredRadiationDown_Average_W_m2/coeff), col="black")+
    geom_line(data=FCR_dec_2021, aes(x=TIMESTAMP, y=wtr_surface), col="blue") +
    scale_y_continuous(
      name="Celcius",
      sec.axis = sec_axis(~.*coeff, name="W_m2"))
    )

a=Met_sur%>%
  filter(DateTime>"2019-12-01 00:00"& DateTime<"2019-12-31 23:59")%>%
  ggplot(., aes(x=ThermistorTemp_C_surface, y=InfraredRadiationDown_Average_W_m2))+
  geom_point()

Twenty$InfraredRadiationDown_Average_W_m2

met=read.csv("Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_FCRMetData/2021/FCR_Met_final_2015_2021.csv")




IR_help=Met_raw%>%
  select(DateTime,AirTemp_Average_C, ShortwaveRadiationUp_Average_W_m2,ShortwaveRadiationDown_Average_W_m2, InfraredRadiationUp_Average_W_m2,InfraredRadiationDown_Average_W_m2)

Met_sur=merge(IR_help,fcrdata_surface, all.x=T)

#Tsurface = (LWout /5.67.10-8)1/4

LWin = (Upyrgeo, up / Epyrgeo, up) + 5.67.10-8 (Tpyrgeo)
4



Mur=Met_sur%>%
  #mutate(Tsurface=((InfraredRadiationDown_Average_W_m2/(5.67*10^(-8)))^(0.25))-273.15)%>%
  #mutate(Tair=((InfraredRadiationUp_Average_W_m2/(5.67*10^(-8)))^(0.25))-273.15)%>%
  mutate(IRdncalc=((5.67*10^-8)*(ThermistorTemp_C_surface+273.15)^4))%>%
  mutate(IRupcalc=((5.67*10^-8)*(AirTemp_Average_C+273.15)^4))%>%
  mutate(LWin=ShortwaveRadiationUp_Average_W_m2 + (5.67*10^-8*(AirTemp_Average_C+273.15)^4))%>%
  mutate(LWout=ShortwaveRadiationDown_Average_W_m2 + (5.67*10^-8*(AirTemp_Average_C+273.15)^4))
  
Dec=Met%>%
  filter(DateTime>"2021-10-01 00:00")
  
#look at IR vs. calc IR
b=Mur%>%
  filter(DateTime>"2021-10-01 00:00")%>%
ggplot(., aes(x=DateTime))+
  #geom_line(aes(y=InfraredRadiationDown_Average_W_m2), col="blue") +
  geom_point(aes(y=IRdncalc), col="black")+
  geom_line(data=Dec, aes(x=DateTime, y=InfraredRadiationDown_Average_W_m2), col="red")
  





