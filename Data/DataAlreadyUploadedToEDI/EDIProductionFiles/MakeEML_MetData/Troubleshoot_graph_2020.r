#troubleshooting the met data making graphs to look more closely at the values
#ABP 14 Jan 2020

#adding a daily average column 


#seperate low dates for BP and pair with precipitation 

summer_15=subset(Met_agg, DateTime >= as.POSIXct('2015-05-01 00:00', tz="Etc/GMT+5") & 
                  DateTime <= as.POSIXct('2015-08-31 23:59', tz='Etc/GMT+5'))
summer_16=subset(Met_agg, DateTime >= as.POSIXct('2016-05-01 00:00', tz="Etc/GMT+5") & 
                 DateTime <= as.POSIXct('2016-08-31 23:59', tz='Etc/GMT+5'))
summer_17=subset(Met_agg, DateTime >= as.POSIXct('2017-05-01 00:00', tz="Etc/GMT+5") & 
                 DateTime <= as.POSIXct('2017-08-31 23:59', tz='Etc/GMT+5'))
summer_18=subset(Met_agg, DateTime >= as.POSIXct('2018-05-01 00:00', tz="Etc/GMT+5") & 
                 DateTime <= as.POSIXct('2018-08-31 23:59', tz='Etc/GMT+5')) 
summer_19=subset(Met_agg, DateTime >= as.POSIXct('2019-05-01 00:00', tz="Etc/GMT+5") & 
                   DateTime <= as.POSIXct('2019-08-31 23:59', tz='Etc/GMT+5'))
summer_20=subset(Met_agg, DateTime >= as.POSIXct('2020-05-01 00:00', tz="Etc/GMT+5") & 
                   DateTime <= as.POSIXct('2020-08-31 23:59', tz='Etc/GMT+5'))
July_15=subset(Met_agg, DateTime >= as.POSIXct('2015-07-01 00:00', tz="Etc/GMT+5") & 
                   DateTime <= as.POSIXct('2015-07-31 23:59', tz='Etc/GMT+5'))
#subset all years
all_15=subset(Met_agg, DateTime >= as.POSIXct('2015-01-01 00:00', tz="Etc/GMT+5") & 
                   DateTime <= as.POSIXct('2015-12-31 23:59', tz='Etc/GMT+5'))
all_16=subset(Met_agg, DateTime >= as.POSIXct('2016-01-01 00:00', tz="Etc/GMT+5") & 
                   DateTime <= as.POSIXct('2016-12-31 23:59', tz='Etc/GMT+5'))
all_17=subset(Met_agg, DateTime >= as.POSIXct('2017-01-01 00:00', tz="Etc/GMT+5") & 
                   DateTime <= as.POSIXct('2017-12-31 23:59', tz='Etc/GMT+5'))
all_18=subset(Met_agg, DateTime >= as.POSIXct('2018-01-01 00:00', tz="Etc/GMT+5") & 
                   DateTime <= as.POSIXct('2018-12-31 23:59', tz='Etc/GMT+5')) 
all_19=subset(Met_agg, DateTime >= as.POSIXct('2019-01-01 00:00', tz="Etc/GMT+5") & 
                   DateTime <= as.POSIXct('2019-12-31 23:59', tz='Etc/GMT+5'))
all_20=subset(Met_agg, DateTime >= as.POSIXct('2020-01-01 00:00', tz="Etc/GMT+5") & 
                   DateTime <= as.POSIXct('2020-12-31 23:59', tz='Etc/GMT+5'))
July_15=subset(Met_agg, DateTime >= as.POSIXct('2015-07-12 00:00', tz="Etc/GMT+5") & 
                 DateTime <= as.POSIXct('2015-07-14 23:59', tz='Etc/GMT+5'))

#adding a daily average column 
daily_all_16=all_16%>%
  group_by(DOY) %>%
  summarize(Daily_mean_par = mean(PAR_Average_umol_s_m2))
daily_all_17=all_17%>%
  group_by(DOY) %>%
  summarize(Daily_mean_par = mean(PAR_Average_umol_s_m2))
daily_all_18=all_18%>%
  group_by(DOY) %>%
  summarize(Daily_mean_par = mean(PAR_Average_umol_s_m2))
daily_all_19=all_19%>%
  group_by(DOY) %>%
  summarize(Daily_mean_par = mean(PAR_Average_umol_s_m2))
daily_all_20=all_20%>%
  group_by(DOY) %>%
  summarize(Daily_mean_par = mean(PAR_Average_umol_s_m2))

 

summer=rbind(summer_19,summer_20)

plot(summer_16$DOY, summer_16$ShortwaveRadiationUp_Average_W_m2, type= 'l', col="green", cex=1.5, main= "Shortwave Radiation UP")
points(summer_17$DOY, summer_17$ShortwaveRadiationUp_Average_W_m2, type = 'l', col="blue", cex=1.5)
points(summer_18$DOY, summer_18$ShortwaveRadiationUp_Average_W_m2, type = 'l', col = "purple", cex=1.5)
points(summer_19$DOY, summer_19$ShortwaveRadiationUp_Average_W_m2, type = 'l', col = "black", cex=1.5)
points(summer_20$DOY, summer_20$ShortwaveRadiationUp_Average_W_m2, type = 'l', col="red", cex=1.5)
legend("topright",c("2016", "2017", "2018", "2019", "2020"), text.col = c("green", "blue", "purple", "black", "red"), cex = 0.75 )

plot(summer_20$DOY, summer_20$InfaredRadiationDown_Average_W_m2, type= 'l', main= "Infared Radiation Down 2020")
points(summer_19$DOY, summer_19$InfaredRadiationUp_Average_W_m2, type= 'l', col="red")
legend("topright",c("2019", "2020"), text.col = c("red","black"), cex = 0.75 )

plot(all_16$DOY, all_16$ShortwaveRadiationDown_Average_W_m2, type= 'l', col="green", cex=1.5, main= "Shortwave Radiation Down")
points(all_17$DOY, all_17$ShortwaveRadiationDown_Average_W_m2, type = 'l', col="blue", cex=1.5)
points(all_18$DOY, all_18$ShortwaveRadiationDown_Average_W_m2, type = 'l', col = "purple", cex=1.5)
points(all_19$DOY, all_19$ShortwaveRadiationDown_Average_W_m2, type = 'l', col = "black", cex=1.5)
points(all_20$DOY, all_20$ShortwaveRadiationDown_Average_W_m2, type = 'l', col="red", cex=1.5)
legend("topright",c("2016", "2017", "2018", "2019", "2020"), text.col = c("green", "blue", "purple", "black", "red"), cex = 0.75 )

plot(summer_16$DOY, summer_16$PAR_Average_umol_s_m2, type= 'l', col="green", cex=1.5, main= "PAR Summer Average")
points(summer_17$DOY, summer_17$PAR_Average_umol_s_m2, type = 'l', col="blue", cex=1.5)
points(summer_18$DOY, summer_18$PAR_Average_umol_s_m2, type = 'l', col = "purple", cex=1.5)
points(summer_19$DOY, summer_19$PAR_Average_umol_s_m2, type = 'l', col = "black", cex=1.5)
points(summer_20$DOY, summer_20$PAR_Average_umol_s_m2, type = 'l', col="red", cex=1.5)
legend("topright",c("2016", "2017", "2018", "2019", "2020"), text.col = c("green", "blue", "purple", "black", "red"), cex = 0.75 )

plot(daily_all_16$DOY, daily_all_16$Daily_mean_par, type= 'l', col="green", cex=1.5, main= "PAR daily_all Average")
points(daily_all_17$DOY, daily_all_17$Daily_mean_par, type = 'l', col="blue", cex=1.5)
points(daily_all_18$DOY, daily_all_18$Daily_mean_par, type = 'l', col = "purple", cex=1.5)
points(daily_all_19$DOY, daily_all_19$Daily_mean_par, type = 'l', col = "black", cex=1.5)
points(daily_all_20$DOY, daily_all_20$Daily_mean_par, type = 'l', col="red", cex=1.5)
legend("topright",c("2016", "2017", "2018", "2019", "2020"), text.col = c("green", "blue", "purple", "black", "red"), cex = 0.75 )

plot(summer_16$DOY, summer_16$InfaredRadiationDown_Average_W_m2, type= 'l', col="green", cex=1.5, main= "Shortwave Radiation Down")
points(summer_17$DOY, summer_17$InfaredRadiationDown_Average_W_m2, type = 'l', col="blue", cex=1.5)
points(summer_18$DOY, summer_18$InfaredRadiationDown_Average_W_m2, type = 'l', col = "purple", cex=1.5)
points(summer_19$DOY, summer_19$InfaredRadiationDown_Average_W_m2, type = 'l', col = "black", cex=1.5)
points(summer_20$DOY, summer_20$InfaredRadiationDown_Average_W_m2, type = 'l', col="red", cex=1.5)
legend("topright",c("2016", "2017", "2018", "2019", "2020"), text.col = c("green", "blue", "purple", "black", "red"), cex = 0.75 )

summer_20$Flag_short=100*summer_20$Flag_ShortwaveRadiationUp_Average_W_m2
legend("topright",c("2016", "2017", "2018", "2019", "2020"), text.col = c("green", "blue", "purple", "black", "red"), cex = 0.75 )
 


fall_15=subset(Met_agg, DateTime >= as.POSIXct('2015-09-01 00:00', tz="Etc/GMT+5") & 
                   DateTime <= as.POSIXct('2015-10-31 23:59', tz='Etc/GMT+5'))
fall_16=subset(Met_agg, DateTime >= as.POSIXct('2016-09-01 00:00', tz="Etc/GMT+5") & 
                   DateTime <= as.POSIXct('2016-10-31 23:59', tz='Etc/GMT+5'))
fall_17=subset(Met_agg, DateTime >= as.POSIXct('2017-09-01 00:00', tz="Etc/GMT+5") & 
                   DateTime <= as.POSIXct('2017-10-31 23:59', tz='Etc/GMT+5'))
fall_18=subset(Met_agg, DateTime >= as.POSIXct('2018-09-01 00:00', tz="Etc/GMT+5") & 
                   DateTime <= as.POSIXct('2018-10-31 23:59', tz='Etc/GMT+5')) 
fall_19=subset(Met_agg, DateTime >= as.POSIXct('2019-09-01 00:00', tz="Etc/GMT+5") & 
                   DateTime <= as.POSIXct('2019-10-31 23:59', tz='Etc/GMT+5'))
fall_20=subset(Met_agg, DateTime >= as.POSIXct('2020-09-01 00:00', tz="Etc/GMT+5") & 
                   DateTime <= as.POSIXct('2020-10-31 23:59', tz='Etc/GMT+5'))
jan_18=subset(Met_agg, DateTime >= as.POSIXct('2018-01-01 00:00', tz="Etc/GMT+5") & 
                 DateTime <= as.POSIXct('2018-01-31 23:59', tz='Etc/GMT+5')) 
sub_jan_18=subset(Met_agg, DateTime >= as.POSIXct('2018-01-16 00:00', tz="Etc/GMT+5") & 
                    DateTime <= as.POSIXct('2018-01-24 23:59', tz='Etc/GMT+5')) 

jan_16=subset(Met_agg, DateTime >= as.POSIXct('2016-01-15 00:00', tz="Etc/GMT+5") & 
                DateTime <= as.POSIXct('2016-02-15 23:59', tz='Etc/GMT+5')) 
jan_17=subset(Met_agg, DateTime >= as.POSIXct('2017-01-15 00:00', tz="Etc/GMT+5") & 
                DateTime <= as.POSIXct('2017-02-15 23:59', tz='Etc/GMT+5')) 
sub_jan_18=subset(Met_agg, DateTime >= as.POSIXct('2018-01-16 00:00', tz="Etc/GMT+5") & 
                    DateTime <= as.POSIXct('2018-01-24 23:59', tz='Etc/GMT+5')) 
jul_16=subset(Met_agg, DateTime >= as.POSIXct('2016-07-01 00:00', tz="Etc/GMT+5") & 
                    DateTime <= as.POSIXct('2016-07-31 23:59', tz='Etc/GMT+5')) 
sub_dec_20=subset(Met_agg, DateTime >= as.POSIXct('2020-11-01 00:00', tz="Etc/GMT+5") & 
                    DateTime <= as.POSIXct('2020-12-31 23:59', tz='Etc/GMT+5')) 

plot(all_16$DOY, all_16$ShortwaveRadiationUp_Average_W_m2, type= 'l', col="green", cex=1.5)
points(all_17$DOY, all_17$ShortwaveRadiationUp_Average_W_m2, type = 'l', col="blue", cex=1.5)
points(all_18$DOY, all_18$ShortwaveRadiationUp_Average_W_m2, type = 'l', col = "purple", cex=1.5)
points(all_19$DOY, all_19$ShortwaveRadiationUp_Average_W_m2, type = 'l', col = "black", cex=1.5)
points(all_20$DOY, all_20$ShortwaveRadiationUp_Average_W_m2, type = 'l', col="red", cex=1.5)
legend("topright",c("2016", "2017", "2018", "2019", "2020"), text.col = c("green", "blue", "purple", "black", "red"), cex = 0.75 )

plot(all_16$DOY, all_16$ShortwaveRadiationDown_Average_W_m2, type= 'l', col="green", cex=1.5)
points(all_17$DOY, all_17$ShortwaveRadiationDown_Average_W_m2, type = 'l', col="blue", cex=1.5)
points(all_18$DOY, all_18$ShortwaveRadiationDown_Average_W_m2, type = 'l', col = "purple", cex=1.5)
points(all_19$DOY, all_19$ShortwaveRadiationDown_Average_W_m2, type = 'l', col = "black", cex=1.5)
points(all_20$DOY, all_20$ShortwaveRadiationDown_Average_W_m2, type = 'l', col="red", cex=1.5)
legend("topright",c("2016", "2017", "2018", "2019", "2020"), text.col = c("green", "blue", "purple", "black", "red"), cex = 0.75 )

plot(all_18$DateTime, all_18$ShortwaveRadiationUp_Average_W_m2, type = 'l', col = "black", cex=1.5)


plot(July_15$DateTime, July_15$Rain_Total_mm, col="black", type='l')
plot(Met$DateTime, Met$AirTemp_Average_C, type = 'l')
plot(all_20$DateTime, all_20$PAR_Average_umol_s_m2, type="l", main="PAR Avg. 2020")

#try QAQC for shortwaveradiation_up just as the same as shortwaveradiation_down
Met_agg3$DOY=yday(Met_agg3$DateTime)
Met_infrad2=Met_agg3[year(Met_agg3$DateTime)<2018,]
Met_infrad2$infradavg=ave(Met_infrad2$InfaredRadiationUp_Average_W_m2, Met_infrad2$DOY) #creating column with mean of infraddown by day of year
Met_infrad2$infradsd=ave(Met_infrad2$InfaredRadiationUp_Average_W_m2, Met_infrad2$DOY, FUN = sd) #creating column with sd of infraddown by day of year
Met_infrad2=unique(Met_infrad2[,c(46,47,48)])

Met_agg3=merge(Met_agg3, Met_infrad2, by = "DOY") #putting in columns for infrared mean and sd by DOY into main data set
Met=Met[order(Met$DateTime),] #ordering table after merging and removing unnecessary columns

Met_agg3$Flag_InfaredRadiationUp_Average_W_m2=ifelse((Met_agg3$InfaredRadiationUp_Average_W_m2-Met_agg3$infradavg)<(-3*Met_agg3$infradsd),5,Met_agg3$Flag_InfaredRadiationUp_Average_W_m2)
Met_agg3$Note_InfaredRadiationUp_Average_W_m2=ifelse((Met_agg3$InfaredRadiationUp_Average_W_m2-Met_agg3$infradavg)<(-3*Met_agg3$infradsd),"Value_corrected_from_mean_InfRadUp_before_fouling_as_described_in_metadata",Met_agg3$Note_InfaredRadiationUp_Average_W_m2)
Met_agg3$InfaredRadiationUp_Average_W_m2=ifelse((Met_agg3$InfaredRadiationUp_Average_W_m2-Met_agg3$infradavg)<(-3*Met_agg3$infradsd),Met_agg3$infradavg,Met_agg3$InfaredRadiationUp_Average_W_m2)

Met_agg3=Met_agg3[,-c(1,47,48)]

all_18_editUP=subset(Met_agg3, DateTime >= as.POSIXct('2018-01-01 00:00', tz="Etc/GMT+5") & 
                DateTime <= as.POSIXct('2018-12-31 23:59', tz='Etc/GMT+5')) 

plot(all_18$DateTime, all_18$ShortwaveRadiationUp_Average_W_m2, type = 'l', col = "black", cex=1.5)
abline(v=(DateTime= as.POSIXct('2018-09-10 12:0', tz='Etc/GMT+5')), col="red", lwd=3, lty=2)

plot(all_18$DateTime, all_18$ShortwaveRadiationDown_Average_W_m2, type = 'l', col = "black", cex=1.5)

plot(jan_18$DateTime, jan_18$ShortwaveRadiationDown_Average_W_m2, type = 'p', col = "black", cex=0.5)

plot(jan_18$DateTime,jan_18$ShortwaveRadiationDown_Average_W_m2, type = 'l', col = "black", cex=1.5, ylim= c(0, 300) )
points(jan_18$DateTime,jan_18$PAR_Total_mmol_m2, type = 'l', col = "red", cex=1.5)
points(jan_18$DateTime,jan_18$Rain_Total_mm, type = 'l', col = "purple", cex=1.5)

plot(all_16$DateTime, all_16$ShortwaveRadiationDown_Average_W_m2, type = 'l', col = "black", cex=1.5)
plot(summer_20$DateTime, summer_20$ShortwaveRadiationUp_Average_W_m2, type = 'p', col = "black", cex=0.5)
points(summer_19$DateTime, summer_19$ShortwaveRadiationUp_Average_W_m2, type = 'p', col = "red", cex=0.5)

plot(summer_20$DOY, summer_20$ShortwaveRadiationUp_Average_W_m2, type = 'p', col = "black", cex=0.5)
points(summer_19$DOY, summer_19$ShortwaveRadiationUp_Average_W_m2, type = 'p', col = "red", cex=0.5)

plot(all_20$DateTime, all_20$ShortwaveRadiationDown_Average_W_m2, type = 'l', col = "black", cex=1.5)
plot(jan_16$DateTime, jan_16$ShortwaveRadiationDown_Average_W_m2, type = 'p', col = "black", cex=0.5)
plot(jan_17$DateTime, jan_17$ShortwaveRadiationDown_Average_W_m2, type = 'p', col = "black", cex=0.5)
plot(sub_dec_20$DateTime, sub_dec_20$ShortwaveRadiationDown_Average_W_m2, type = 'p', col = "black", cex=0.5)
