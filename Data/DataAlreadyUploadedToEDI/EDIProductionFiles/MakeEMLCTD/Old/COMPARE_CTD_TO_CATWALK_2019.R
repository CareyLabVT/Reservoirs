# Compare the CTD and catwalk data. 
# RPM 18June2019

pacman::p_load(tidyverse, rLakeAnalyzer, httr)

cat <- read.csv("Catwalk.csv", skip = 1)

cat_sum_19 <- cat %>% filter(TIMESTAMP >= "2019-06-01 12:00:00") %>%
  select(TIMESTAMP, doobs_1, doobs_5, doobs_9) %>%
  filter(doobs_1 != "NAN") %>%
  filter(doobs_5 != "NAN") %>%
  filter(doobs_9 != "NAN") %>%
  filter(TIMESTAMP != "NAN") %>%
  filter(TIMESTAMP != "YYYY_MM_DD_HH_MM_SS")
  
ctd_1.0 <- ctd_new %>% filter(Depth_m == 1.6)

ctd_5.0 <- ctd_new %>% filter(Depth_m == 5.0)

ctd_9.0 <- ctd_new %>% filter(Depth_m == 9.0)

pdf("SEASONAL_CATWALK_CTD_COMPARE_DO_2019.pdf", width=14, height=8)
plot(as.POSIXct(cat_sum_19$TIMESTAMP), cat_sum_19$doobs_1, type = "l", ylim = c(0,14), xlab = "", ylab = "DO (mg/L)")
lines(as.POSIXct(cat_sum_19$TIMESTAMP), cat_sum_19$doobs_5, type = "l", ylim = c(0,14), col = "blue")
lines(as.POSIXct(cat_sum_19$TIMESTAMP), cat_sum_19$doobs_9, type = "l", ylim = c(0,14), col = "magenta")
points(ctd_1.0$Date, ctd_1.0$DO_mgL, type = "p", pch = 21, col = "black", bg = "black", cex = 2, lwd = 2)
points(ctd_5.0$Date, ctd_5.0$DO_mgL, type = "p", pch = 21, col = "black", bg = "blue", cex = 2, lwd = 2)
points(ctd_9.0$Date, ctd_9.0$DO_mgL, type = "p", pch = 21, col = "black", bg = "magenta", cex = 2, lwd = 2)
dev.off()

### THERE IS NO dotemp_1 ???
cat_sum_19_temp <- cat %>% filter(TIMESTAMP >= "2019-06-01 12:00:00") %>%
  select(TIMESTAMP, wtr_1, dotemp_5, dotemp_9) %>%
  filter(wtr_1 != "NAN") %>%
  filter(dotemp_5 != "NAN") %>%
  filter(dotemp_9 != "NAN") %>%
  filter(TIMESTAMP != "NAN") %>%
  filter(TIMESTAMP != "YYYY_MM_DD_HH_MM_SS")


pdf("SEASONAL_CATWALK_CTD_COMPARE_TEMP_2019.pdf", width=14, height=8)
plot(as.POSIXct(cat_sum_19_temp$TIMESTAMP), cat_sum_19_temp$wtr_1, type = "l", ylim = c(4,33), xlab = "", ylab = "Temp (C)")
lines(as.POSIXct(cat_sum_19_temp$TIMESTAMP), cat_sum_19_temp$dotemp_5, type = "l", ylim = c(4,33), col = "blue")
lines(as.POSIXct(cat_sum_19_temp$TIMESTAMP), cat_sum_19_temp$dotemp_9, type = "l", ylim = c(4,33), col = "magenta")
points(ctd_1.0$Date, ctd_1.0$Temp_C, type = "p", pch = 21, col = "black", bg = "black", cex = 2, lwd = 2)
points(ctd_5.0$Date, ctd_5.0$Temp_C, type = "p", pch = 21, col = "black", bg = "blue", cex = 2, lwd = 2)
points(ctd_9.0$Date, ctd_9.0$Temp_C, type = "p", pch = 21, col = "black", bg = "magenta", cex = 2, lwd = 2)
dev.off()


cat_sum_19_chla <- cat %>% filter(TIMESTAMP >= "2019-06-01 12:00:00") %>%
  select(TIMESTAMP, Chla_1 ) %>%
  filter(Chla_1 != "NAN") %>%
  filter(TIMESTAMP != "NAN") %>%
  filter(TIMESTAMP != "YYYY_MM_DD_HH_MM_SS")

pdf("SEASONAL_CATWALK_CTD_COMPARE_CHLA_2019.pdf", width=14, height=8)
plot(as.POSIXct(cat_sum_19_chla$TIMESTAMP), cat_sum_19_chla$Chla_1, type = "l", ylim = c(0,50), xlab = "", ylab = "chla (ug/L)")
points(ctd_1.0$Date, ctd_1.0$Chla_ugL, type = "p", pch = 21, col = "black", bg = "green", cex = 2, lwd = 2)
dev.off()



