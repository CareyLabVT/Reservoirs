#MET QAQC Bonus Script

#Contains code comparing fixes for air temperature and infrared radiation down

#BONUS CODE#
#justification for using panel temp lm from 2015 to substitue erroneous data
########### AirTemp lm ###########

# ####Air Temp vs. Panel Temp
# #air temp red, p temp black
# # 1. PTemp vs. Air Temp whole time series
plot(Met$TIMESTAMP, Met$AirTC_Avg, type = 'l', col='red')
lines(Met$TIMESTAMP, Met$PTemp_C, type = 'l')
#
# # 2. 1:1 Ptemp vs. Air temp whole time series
lm_Met=lm(Met$PTemp_C ~ Met$AirTC_Avg)
plot(Met$PTemp_C, Met$AirTC_Avg)
abline(lm_Met, col = "blue")
print(lm_Met$coefficients)
#
# # 3. Ptemp vs. Air Temp 2016
Met_16=Met[year(Met$TIMESTAMP) == 2016,]
plot(Met_16$TIMESTAMP, Met_16$AirTC_Avg, type = 'l', col='red')
lines(Met_16$TIMESTAMP, Met_16$PTemp_C, type = 'l')
#
# # 4. 1:1 Ptemp vs. AirTemp 2016
lm_Met16=lm(Met_16$PTemp_C ~ Met_16$AirTC_Avg)
plot(Met_16$PTemp_C, Met_16$AirTC_Avg)
abline(lm_Met16, col = "blue")
print(lm_Met16$coefficients)
#
# # 5. Ptemp vs. Air Temp after 2017
Met_hot=Met[Met$TIMESTAMP > "2016-12-31 23:59:00",]
plot(Met_hot$TIMESTAMP, Met_hot$AirTC_Avg, type = 'l', col='red')
lines(Met_hot$TIMESTAMP, Met_hot$PTemp_C, type = 'l')
#
# # 6. 1:1 Ptemp vs. AirTemp after 2017
lm_Methot=lm(Met_hot$PTemp_C ~ Met_hot$AirTC_Avg)
plot(Met_hot$PTemp_C, Met_hot$AirTC_Avg)
abline(lm_Methot, col = "blue")
print(lm_Methot$coefficients)
#
# # 7. Ptemp vs. Air Temp 2018 plus line noting cleaning time
Met_18=Met[year(Met$TIMESTAMP) == 2018,]
plot(Met_18$TIMESTAMP, Met_18$AirTC_Avg, type = 'l', col='red')
lines(Met_18$TIMESTAMP, Met_18$PTemp_C, type = 'l')
abline(v=ymd_hms("2018-09-03 11:40:00"), col="blue", lwd = 2)
#
lm_Met18=lm(Met_18$PTemp_C ~ Met_18$AirTC_Avg)
plot(Met_18$PTemp_C, Met_18$AirTC_Avg)
abline(lm_Met18, col = "blue")
print(lm_Met18$coefficients)
#
# # 8. 1:1 Ptemp vs. AirTemp Jan 2018 - Sep 2 2018
Met_early18=Met[Met$TIMESTAMP > "2017-12-31 23:59:00"& Met$TIMESTAMP < "2018-09-03 00:00:00",]
lm_Metearly18=lm(Met_early18$PTemp_C ~ Met_early18$AirTC_Avg)
plot(Met_early18$PTemp_C, Met_early18$AirTC_Avg)
abline(lm_Metearly18, col = "blue")
print(lm_Metearly18$coefficients)
#
# 9. 1:1 Ptemp vs. AirTemp Sep 3 2018 - Dec 2018
Met_late18=Met[Met$TIMESTAMP > "2018-09-03 11:40:00"& Met$TIMESTAMP < "2019-01-01 00:00:00",]
lm_Metlate18=lm(Met_late18$PTemp_C ~ Met_late18$AirTC_Avg)
plot(Met_late18$PTemp_C, Met_late18$AirTC_Avg)
abline(lm_Metlate18, col = "blue")
print(lm_Metlate18$coefficients)

####NDLAS comparison####
NLDAS=read.csv("https://raw.githubusercontent.com/CareyLabVT/FCR-GLM/master/NLDASData/FCR_GLM_met_NLDAS2_Dec14_Dec18.csv", header = T)
N_AirTemp=NLDAS[,c(6,3)]
N_AirTemp$time=ymd_hms(N_AirTemp$time)
Met_air=Met[,c(1,4,8)]
names(Met_air)<- c("time", "Panel_temp","AirTemp_Average_C")
# #does NLDAS use GMT -4? or EST? What time zone is it??????????
# #for now, assuming GMT -4 for simplicity's sake
compare<-merge(N_AirTemp, Met_air, by="time")
#
# #Met Air vs. NLDAS
x11()
par(mfrow=c(1,2))
plot(compare$time, compare$AirTemp_Average_C, ylim=c(-15,60))
points(compare$time, compare$AirTemp, col="blue")
legend("topleft", legend=c("MetStation","NLDAS"), col=c("black", "blue"), pch=1)
plot(compare$AirTemp_Average_C,compare$AirTemp)
abline(0,1, col="red")
lm_NLDAS=lm(compare$AirTemp_Average_C ~ compare$AirTemp)
abline(lm_NLDAS, col="blue")
legend("bottomright", legend=c("1:1","lm"), col=c("red", "blue"), lty = 1)
mean(lm_NLDAS$residuals)
range(lm_NLDAS$residuals)
sd(lm_NLDAS$residuals)

#Panel vs. Air
x11()
par(mfrow=c(1,2))
plot(compare$time, compare$AirTemp_Average_C, ylim=c(-15,60))
points(compare$time, compare$Panel_temp, col="green")
legend("topleft", legend=c("MetStation AirTemp","MetStation Panel"), col=c("black", "green"), pch=1)
plot(compare$Panel_temp,compare$AirTemp)
abline(0,1, col="red")
lm_Panel=lm(compare$AirTemp_Average_C ~ compare$Panel_temp)
abline(lm_Panel, col="green")
legend("bottomright", legend=c("1:1","lm"), col=c("red", "green"), lty = 1)
mean(lm_Panel$residuals)
range(lm_Panel$residuals)
sd(lm_Panel$residuals)

#NLDAS vs. Panel??
legend("topleft", legend=c("MetStation Panel","NLDAS"), col=c("green", "blue"), pch=1)
plot(compare$Panel_temp,compare$AirTemp)
abline(0,1, col="red")
lm_Panel=lm(compare$AirTemp_Average_C ~ compare$Panel_temp)
abline(lm_Panel, col="blue")

compare_2015=compare[compare$time<"2016-01-01 00:00:00",]

#Met Air vs. NLDAS 2015
x11()
par(mfrow=c(1,2))
plot(compare_2015$time, compare_2015$AirTemp_Average_C, ylim=c(-15,60))
points(compare_2015$time, compare_2015$AirTemp, col="blue")
legend("topleft", legend=c("MetStation","NLDAS"), col=c("black", "blue"), pch=1)
plot(compare_2015$AirTemp_Average_C,compare_2015$AirTemp)
abline(0,1, col="red")
lm_NLDAS=lm(compare_2015$AirTemp_Average_C ~ compare_2015$AirTemp)
abline(lm_NLDAS, col="blue")
legend("bottomright", legend=c("1:1","lm"), col=c("red", "blue"), lty = 1)
mean(lm_NLDAS$residuals)
range(lm_NLDAS$residuals)
sd(lm_NLDAS$residuals)

#Panel vs. Air 2015
x11()
par(mfrow=c(1,2))
plot(compare_2015$time, compare_2015$AirTemp_Average_C, ylim=c(-15,60))
points(compare_2015$time, compare_2015$Panel_temp, col="green")
legend("topleft", legend=c("MetStation AirTemp","MetStation Panel"), col=c("black", "green"), pch=1)
plot(compare_2015$Panel_temp,compare_2015$AirTemp)
abline(0,1, col="red")
lm_Panel2015=lm(compare_2015$AirTemp_Average_C ~ compare_2015$Panel_temp)
abline(lm_Panel, col="green")
legend("bottomright", legend=c("1:1","lm"), col=c("red", "green"), lty = 1)
mean(lm_Panel$residuals)
range(lm_Panel$residuals)
sd(lm_Panel$residuals)

#inf rad correction cleanup
#1. Post volt correction raw, only inf rad +time
#replace mean, 3sd
Met$DOY=yday(Met$DateTime)
Met_infrad=Met[, c(1,16,46)]
Met_infradraw=Met[, c(1,16,46)]
#2. Abs value, all data mean
Met_infrad$AllDataavg=ave(Met_infrad$InfaredRadiationDown_Average_W_m2, Met_infrad$DOY) #creating column with mean of infraddown by day of year
Met_infrad$AllDatasd=ave(Met_infrad$InfaredRadiationDown_Average_W_m2, Met_infrad$DOY, FUN = sd) #creating column with sd of infraddown by day of year

Met_infrad$AllData_abs=ifelse((abs(Met_infrad$InfaredRadiationDown_Average_W_m2-Met_infrad$AllDataavg))>(3*Met_infrad$AllDatasd),Met_infrad$AllDataavg,Met_infrad$InfaredRadiationDown_Average_W_m2)

#3. -sd, all data mean
Met_infrad$AllData_negsd=ifelse((Met_infrad$InfaredRadiationDown_Average_W_m2-Met_infrad$AllDataavg)<(-3*Met_infrad$AllDatasd),Met_infrad$AllDataavg,Met_infrad$InfaredRadiationDown_Average_W_m2)

#4. Abs value, <2017
Met_infrad17=Met_infrad[year(Met_infrad$DateTime)<2017,]
Met_infrad17$avg17=ave(Met_infrad17$InfaredRadiationDown_Average_W_m2, Met_infrad17$DOY) #creating column with mean of infraddown by day of year
Met_infrad17$sd17=ave(Met_infrad17$InfaredRadiationDown_Average_W_m2, Met_infrad17$DOY, FUN = sd) #creating column with sd of infraddown by day of year
Met_infrad17=unique(Met_infrad17[,c(3,6,7)])

Met_infrad=merge(Met_infrad, Met_infrad17, by = "DOY")

Met_infrad$Data17_abs=ifelse((abs(Met_infrad$InfaredRadiationDown_Average_W_m2-Met_infrad$avg17))>(3*Met_infrad$sd17),Met_infrad$avg17,Met_infrad$InfaredRadiationDown_Average_W_m2)
#5. -sd, <2017
Met_infrad$negsd17=ifelse((Met_infrad$InfaredRadiationDown_Average_W_m2-Met_infrad$avg17)<(-3*Met_infrad$sd17),Met_infrad$avg17,Met_infrad$InfaredRadiationDown_Average_W_m2)

#6. Abs value, <2018
Met_infrad18=Met_infrad[year(Met_infrad$DateTime)<2018,]
Met_infrad18$avg18=ave(Met_infrad18$InfaredRadiationDown_Average_W_m2, Met_infrad18$DOY) #creating column with mean of infraddown by day of year
Met_infrad18$sd18=ave(Met_infrad18$InfaredRadiationDown_Average_W_m2, Met_infrad18$DOY, FUN = sd) #creating column with sd of infraddown by day of year
Met_infrad18=unique(Met_infrad18[,c(1,12,13)])

Met_infrad=merge(Met_infrad, Met_infrad18, by = "DOY")

Met_infrad$Data18_abs=ifelse((abs(Met_infrad$InfaredRadiationDown_Average_W_m2-Met_infrad$avg18))>(3*Met_infrad$sd18),Met_infrad$avg18,Met_infrad$InfaredRadiationDown_Average_W_m2)

#7. -sd, <2018
Met_infrad$negsd18=ifelse((Met_infrad$InfaredRadiationDown_Average_W_m2-Met_infrad$avg18)<(-3*Met_infrad$sd18),Met_infrad$avg18,Met_infrad$InfaredRadiationDown_Average_W_m2)
Met_infrad$negsd182=ifelse((Met_infrad$InfaredRadiationDown_Average_W_m2-Met_infrad$avg18)<(-2*Met_infrad$sd18),Met_infrad$avg18,Met_infrad$InfaredRadiationDown_Average_W_m2)


#8. abs, year = 2018
Met_infrad18only=Met_infrad[year(Met_infrad$DateTime)==2018,]
Met_infrad18only$avg18only=ave(Met_infrad18only$InfaredRadiationDown_Average_W_m2, Met_infrad18only$DOY) #creating column with mean of infraddown by day of year
Met_infrad18only$sd18only=ave(Met_infrad18only$InfaredRadiationDown_Average_W_m2, Met_infrad18only$DOY, FUN = sd) #creating column with sd of infraddown by day of year
Met_infrad18only=unique(Met_infrad18only[,c(1,17,18)])

Met_infrad=merge(Met_infrad, Met_infrad18only, by = "DOY")

Met_infrad$Data18only_abs=ifelse((abs(Met_infrad$InfaredRadiationDown_Average_W_m2-Met_infrad$avg18only))>(3*Met_infrad$sd18only),Met_infrad$avg18only,Met_infrad$InfaredRadiationDown_Average_W_m2)

#9. -sd, year = 2018
Met_infrad$negsd18only=ifelse((Met_infrad$InfaredRadiationDown_Average_W_m2-Met_infrad$avg18only)<(-3*Met_infrad$sd18only),Met_infrad$avg18only,Met_infrad$InfaredRadiationDown_Average_W_m2)
Met_infrad$negsd18only2=ifelse((Met_infrad$InfaredRadiationDown_Average_W_m2-Met_infrad$avg18only)<(-2*Met_infrad$sd18only),Met_infrad$avg18only,Met_infrad$InfaredRadiationDown_Average_W_m2)
Met_infrad$negsd18only1=ifelse((Met_infrad$InfaredRadiationDown_Average_W_m2-Met_infrad$avg18only)<(-Met_infrad$sd18only),Met_infrad$avg18only,Met_infrad$InfaredRadiationDown_Average_W_m2)

#10. Plot comparison
Met_infrad=Met_infrad[order(Met_infrad$DateTime),]
x11(); par(mfrow=c(2,3))
plot(Met_infrad$DateTime, Met_infrad$InfaredRadiationDown_Average_W_m2, type = 'l')
points(Met_infrad$DateTime,Met_infrad$AllData_abs, type = 'l', col = 'deeppink')

plot(Met_infrad$DateTime, Met_infrad$InfaredRadiationDown_Average_W_m2, type = 'l')
points(Met_infrad$DateTime,Met_infrad$AllData_negsd, type = 'l', col = 'green')

plot(Met_infrad$DateTime, Met_infrad$InfaredRadiationDown_Average_W_m2, type = 'l')
points(Met_infrad$DateTime,Met_infrad$Data17_abs, type = 'l', col = 'blue')

plot(Met_infrad$DateTime, Met_infrad$InfaredRadiationDown_Average_W_m2, type = 'l')
points(Met_infrad$DateTime,Met_infrad$negsd18, type = 'l', col = 'orange')

plot(Met_infrad$DateTime, Met_infrad$InfaredRadiationDown_Average_W_m2, type = 'l')
points(Met_infrad$DateTime,Met_infrad$Data18_abs, type = 'l', col = 'red')

plot(Met_infrad$DateTime, Met_infrad$InfaredRadiationDown_Average_W_m2, type = 'l')
points(Met_infrad$DateTime,Met_infrad$negsd18, type = 'l', col = 'yellow')
points(Met_infrad$DateTime,Met_infrad$negsd182, type = 'l', col = 'purple')

plot(Met_infrad$DateTime, Met_infrad$InfaredRadiationDown_Average_W_m2, type = 'l')
points(Met_infrad$DateTime,Met_infrad$Data18only_abs, type = 'l', col = 'brown')

plot(Met_infrad$DateTime, Met_infrad$InfaredRadiationDown_Average_W_m2, type = 'l')
points(Met_infrad$DateTime,Met_infrad$negsd18only, type = 'l', col = 'cyan3')
points(Met_infrad$DateTime,Met_infrad$negsd18only2, type = 'l', col = 'cyan4')
points(Met_infrad$DateTime,Met_infrad$negsd18only1, type = 'l', col = 'aquamarine')

x11()
plot(Met_infrad$DOY, Met_infrad$InfaredRadiationDown_Average_W_m2, type = 'l', col='red')
points(Met_infrad$DOY,Met_infrad$AllDataavg, pch=19)
arrows(Met_infrad$DOY, Met_infrad$AllDataavg-(3*Met_infrad$AllDatasd), Met_infrad$DOY, Met_infrad$AllDataavg+(3*Met_infrad$AllDatasd), length=0.05, angle=90, code=3)

x11()
plot(Met_infrad$DOY, Met_infrad$InfaredRadiationDown_Average_W_m2, type = 'l', col='red')
points(Met_infrad$DOY,Met_infrad$AllDataavg, pch=19)
arrows(Met_infrad$DOY, Met_infrad$AllDataavg-(2*Met_infrad$AllDatasd), Met_infrad$DOY, Met_infrad$AllDataavg+(2*Met_infrad$AllDatasd), length=0.05, angle=90, code=3)

######MORE SIN CURVE MAYBE I CAN MAKE THIS WORK########
#1. Basic sin curve

xc=cos(2*pi*Met_infrad$DOY/366)
xs=cos(2*pi*Met_infrad$DOY/366)

IRlm <- lm(Met_infrad$InfaredRadiationDown_Average_W_m2~xc+xs)
summary(IRlm)

# access the fitted series (for plotting)
fit <- fitted(IRlm)  
pred <- predict(IRlm, newdata=data.frame(Time=Met_infrad$DOY))  

x11()
plot(Met_infrad$InfaredRadiationDown_Average_W_m2 ~ Met_infrad$DOY, xlim=c(1, 900))
lines(fit, col="red")
lines(Met_infrad$DOY, pred, col="blue")

InfRadlm <- lm(InfaredRadiationDown_Average_W_m2 ~ sin(2*pi*DOY/366)+cos(2*pi*DOY/366),data=Met_infrad)
summary(InfRadlm)

x11()
plot(InfaredRadiationDown_Average_W_m2~DOY,data=Met_infrad)
lines(Met_infrad$DOY,InfRadlm$fitted,col=2)
Met_infrad=cbind(Met_infrad, lmfitData)
summary(InfRadlm$fitted.values)

Met_infrad$sinlm=ifelse((Met_infrad$InfaredRadiationDown_Average_W_m2-Met_infrad$lmfit)<(-sd(InfRadlm$residuals)),Met_infrad$lmfit,Met_infrad$InfaredRadiationDown_Average_W_m2)
x11()
plot(Met_infrad$DateTime, Met_infrad$InfaredRadiationDown_Average_W_m2, type = 'l')
points(Met_infrad$DateTime,Met_infrad$sinlm, type = 'l', col = 'red')
points(Met_infrad$DateTime, Met_infrad$lmfit, type='l')
abline(v=ymd_hms('2018-09-10 12:32:00'), col='red')
abline(v=ymd_hms('2018-08-02 02:32:00'), col='red')
sd(InfRadlm$residuals)

#2018 sin wave
#this does not work..
Met_infrad18sin=Met_infrad[year(Met_infrad$DateTime)==2018,]
xc18=cos(2*pi*Met_infrad18sin$DOY/366)
xs18=cos(2*pi*Met_infrad18sin$DOY/366)

IRlm18 <- lm(Met_infrad18sin$InfaredRadiationDown_Average_W_m2~xc18+xs18)
summary(IRlm18)

# access the fitted series (for plotting)
fit <- fitted(IRlm18)  
pred <- predict(IRlm18, newdata=data.frame(Time=Met_infrad18sin$DOY))  

x11()
plot(Met_infrad18sin$InfaredRadiationDown_Average_W_m2 ~ Met_infrad18sin$DOY, xlim=c(1, 900))
lines(fit, col="red")
lines(Met_infrad18sin$DOY, pred, col="blue")

InfRadlm <- lm(InfaredRadiationDown_Average_W_m2 ~ sin(2*pi*DOY/366)+cos(2*pi*DOY/366),data=Met_infrad)
summary(InfRadlm)

