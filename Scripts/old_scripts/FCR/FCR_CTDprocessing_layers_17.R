### CTD CLLEAN UP FILES ###

setwd("~/Dropbox/2017/CTD_2017")

site50 = read.table("070717_bvr.asc", header = T, row.names = NULL)

names(site50)[1] <- "depth_m"
names(site50)[2] <- "do_mgL"
names(site50)[3] <- "do_sat"
names(site50)[4] <- "pH"
names(site50)[5] <- "ORP_mV"
names(site50)[6] <- "cond_uScm"
names(site50)[7] <- "temp_C"
names(site50)[8] <- "press"
names(site50)[9] <- "press_db"
names(site50)[10] <- "salinity"
names(site50)[11] <- "density"

for(i in 1:length(site50$depth_m)){
    if(site50$depth_m[i]<0){
    site50$temp_C[i] = NA
    }
    if(site50$do_mgL[i]<0){
      site50$temp_C[i] = NA
    }
  if(site50$ORP_mV[i]<0){
    site50$temp_C[i] = NA
  }
  if(site50$cond_uScm[i]<0){
    site50$temp_C[i] = NA
  }
}

site = na.omit(site50)

site = site[,c(1,2,3,4,5,6,7,10,11)]

layer = data.frame(site, row.names = NULL)

#layer = layer[,-1]

df.final<-data.frame()

### Run the for loop across the depths ###

for(i in 1:length(layer[,1])){
  layer1<-layer[order(abs(layer[,1] - 0.1)), ][1,]
  layer2<-layer[order(abs(layer[,1] - 0.4)), ][1,]
  layer3<-layer[order(abs(layer[,1] - 0.7)), ][1,]
  layer4<-layer[order(abs(layer[,1] - 1)), ][1,]
  layer5<-layer[order(abs(layer[,1] - 1.3)), ][1,]
  layer6<-layer[order(abs(layer[,1] - 1.6)), ][1,]
  layer7<-layer[order(abs(layer[,1] - 1.9)), ][1,]
  layer8<-layer[order(abs(layer[,1] - 2.3)), ][1,]
  layer9<-layer[order(abs(layer[,1] - 2.6)), ][1,]
  layer10<-layer[order(abs(layer[,1] - 2.9)), ][1,]
  layer11<-layer[order(abs(layer[,1] - 3.2)), ][1,]
  layer12<-layer[order(abs(layer[,1] - 3.5)), ][1,]
  layer13<-layer[order(abs(layer[,1] - 3.8)), ][1,]
  layer14<-layer[order(abs(layer[,1] - 4.1)), ][1,]
  layer15<-layer[order(abs(layer[,1] - 4.4)), ][1,]
  layer16<-layer[order(abs(layer[,1] - 4.7)), ][1,]
  layer17<-layer[order(abs(layer[,1] - 5)), ][1,]
  layer18<-layer[order(abs(layer[,1] - 5.3)), ][1,]
  layer19<-layer[order(abs(layer[,1] - 5.6)), ][1,]
  layer20<-layer[order(abs(layer[,1] - 5.9)), ][1,]
  layer21<-layer[order(abs(layer[,1] - 6.2)), ][1,]
  layer22<-layer[order(abs(layer[,1] - 6.5)), ][1,]
  layer23<-layer[order(abs(layer[,1] - 6.8)), ][1,]
  layer24<-layer[order(abs(layer[,1] - 7.1)), ][1,]
  layer25<-layer[order(abs(layer[,1] - 7.4)), ][1,]
  layer26<-layer[order(abs(layer[,1] - 7.7)), ][1,]
  layer27<-layer[order(abs(layer[,1] - 8)), ][1,]
  layer28<-layer[order(abs(layer[,1] - 8.3)), ][1,]
  layer29<-layer[order(abs(layer[,1] - 8.7)), ][1,]
  layer30<-layer[order(abs(layer[,1] - 9)), ][1,]
  layer31<-layer[order(abs(layer[,1] - 9.3)), ][1,]
}

df.final = rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,layer11,layer12,layer13,layer14,layer15,layer16,layer17,layer18,layer19,
                 layer20,layer21,layer22,layer23,layer24,layer25,layer26,layer27,layer28,layer29,layer30,layer31)

write.table(df.final, "bvrlayer_070717.csv", row.names = F, col.names = TRUE, sep = ',')
# 
par(mfrow = c(1,1))
plot(df.final$do_mgL, -(df.final$depth_m), type = "l", lwd = 4,lty = "solid", main = "", xlab = "DO (ppm)", ylab = "Depth (m)",col.lab = 'white', col = "red")
axis(1, c(0,2,4,6,8,10,12,14), col = 'white', col.axis = 'white', col.ticks = 'white', cex.axis = 1.5)
axis(2, c(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0), col = 'white', col.axis = 'white', col.ticks = 'white', cex.axis = 1.5)
par(new = T)

abline(h = -1.5, lty = "dashed", lwd = 3, col = "blue")
abline(h = -5.2, lty = "dashed", lwd = 3, col = "darkblue")
abline(h = 0, lty = "dashed", lwd = 3, col = "lightblue")

mtext(side = 3, adj = 0.1, "Epilimnion", line = -1.5, cex = 0.75, col = "white")
mtext(side = 3, adj = 0.1, "Metalimnion", line = -4.5, cex = 0.75, col = "white")
mtext(side = 3, adj = 0.1, "Hypolimnion", line = -12, cex = 0.75, col = "white")


par(mfrow = c(1,1))
plot(df.final$temp_C, -(df.final$depth_m), type = "l", lwd = 4,lty = "solid", main = "", xlab = "Temperature (C)", ylab = "Depth (m)",col.lab = 'white', col = "turquoise2")
axis(1, c(12,14,16,18,20,22,24,26,28), col = 'white', col.axis = 'white', col.ticks = 'white', cex.axis = 1.5)
axis(2, c(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0), col = 'white', col.axis = 'white', col.ticks = 'white', cex.axis = 1.5)

abline(h = -1.5, lty = "dashed", lwd = 3, col = "blue")
abline(h = -5.2, lty = "dashed", lwd = 3, col = "darkblue")
abline(h = 0, lty = "dashed", lwd = 3, col = "lightblue")

mtext(side = 3, adj = 0.1, "Epilimnion", line = -1.5, cex = 0.75, col = "white")
mtext(side = 3, adj = 0.1, "Metalimnion", line = -4.5, cex = 0.75, col = "white")
mtext(side = 3, adj = 0.1, "Hypolimnion", line = -12, cex = 0.75, col = "white")


plot(df.final$temp_C, -(df.final$depth_m), type = "l", lwd = 3, xaxt = "n", xlab = "", ylab = "", col = "goldenrod")
axis(3, col = 'white', col.axis = 'white', col.ticks = 'white', cex.axis = 1.5)

legend("topleft", c("Temp C", "DO mg/L"), lwd = c(3,3), lty = c("solid", "dotted"), col = c("darkred", "darkblue"))

par(bg = 'black')
par(mfrow = c(1,1))
plot(df.final$pH, -(df.final$depth_m), type = "l", lwd = 3,lty = "solid", main = "", xlab = "pH", ylab = "Depth (m)", col = "green")
par(new = T)
plot(df.final$temp_C, -(df.final$depth_m), type = "l", lwd = 3, xaxt = "n", xlab = "", ylab = "", col = "orange")
axis(3)
mtext(side = 3, adj = 0.5, "Conductivity (uS/cm)", line = 2)

legend("topleft", c("pH", "Conductivity (uS/cm)"), lwd = c(3,3), lty = c("solid", "solid"), col = c("green", "orange"), bty = "n")




