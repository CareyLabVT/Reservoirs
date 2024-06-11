#FP vs. EXO sonde temp compare
#Author: Mary Lofton
#Date: 01JUN20

#load packages
#install.packages('pacman')
pacman::p_load(tidyverse, lubridate)

#load FCR EXO sonde data and limit to sampling datetime
exo <- read_csv("C:/Users/Mary Lofton/Desktop/Catwalk.csv", skip = 1)
exo <- exo[-c(1:2),]
exo <- exo %>%
  filter(date(TIMESTAMP) == "2020-05-25") %>%
  select(TIMESTAMP, wtr_1:wtr_9)

#1st cast
exo1 <- exo[c(53,54),] %>%
  gather(wtr_1:wtr_9, key = "Depth",value = "temp_C") %>%
  mutate(Depth = sapply(strsplit(Depth,"_"),"[", 2 )) %>%
  group_by(Depth) %>%
  summarize(Temp_C_exo = mean(as.numeric(temp_C), na.rm = TRUE)) %>%
  mutate(Depth = as.numeric(Depth))

#2nd cast
exo2 <- exo[89,] %>%
  gather(wtr_1:wtr_9, key = "Depth",value = "temp_C") %>%
  mutate(Depth = sapply(strsplit(Depth,"_"),"[", 2 )) %>%
  group_by(Depth) %>%
  summarize(Temp_C_exo = mean(as.numeric(temp_C), na.rm = TRUE)) %>%
  mutate(Depth = as.numeric(Depth))

#load FP profiles and limit to relevant depths

#1st cast
fp1 <- read_tsv("./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/20200525_FCR_50_1.txt")
fp1 <- fp1[-1,]
fp1 <- fp1[,c(1,12,13)]
fp1 <- fp1 %>%
  mutate(Depth = as.numeric(Depth)) %>%
  filter((Depth >= 0.9 & Depth <= 1.1) |
           (Depth >= 1.9 & Depth <= 2.1) |
           (Depth >= 2.9 & Depth <= 3.1) |
           (Depth >= 3.9 & Depth <= 4.1) |
           (Depth >= 4.9 & Depth <= 5.1) |
           (Depth >= 5.9 & Depth <= 6.1) |
           (Depth >= 6.9 & Depth <= 7.1) |
           (Depth >= 7.9 & Depth <= 8.1) |
           (Depth >= 8.9 & Depth <= 9.1)) %>%
  mutate(Depth = round(Depth, digits = 0)) %>%
  group_by(Depth) %>%
  summarize(Temp_C_fp = mean(as.numeric(`Temp. Sample`),na.rm = TRUE))

#2nd cast
fp2 <- read_tsv("./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/20200525_FCR_50_2.txt")
fp2 <- fp2[-1,]
fp2 <- fp2[,c(1,12,13)]
fp2 <- fp2 %>%
  mutate(Depth = as.numeric(Depth)) %>%
  filter((Depth >= 0.9 & Depth <= 1.1) |
           (Depth >= 1.9 & Depth <= 2.1) |
           (Depth >= 2.9 & Depth <= 3.1) |
           (Depth >= 3.9 & Depth <= 4.1) |
           (Depth >= 4.9 & Depth <= 5.1) |
           (Depth >= 5.9 & Depth <= 6.1) |
           (Depth >= 6.9 & Depth <= 7.1) |
           (Depth >= 7.9 & Depth <= 8.1) |
           (Depth >= 8.9 & Depth <= 9.1)) %>%
  mutate(Depth = round(Depth, digits = 0)) %>%
  group_by(Depth) %>%
  summarize(Temp_C_fp = mean(as.numeric(`Temp. Sample`),na.rm = TRUE))

#join exo and fp data by cast
temp1 <- left_join(exo1, fp1, by = "Depth")
write.csv(temp1, file = "C:/Users/Mary Lofton/Desktop/FP_SCC_temp_compare/cast1_compare.csv",row.names = FALSE)
temp2 <- left_join(exo2, fp2, by = "Depth")
write.csv(temp2, file = "C:/Users/Mary Lofton/Desktop/FP_SCC_temp_compare/cast2_compare.csv",row.names = FALSE)


#plot them
tplot1 <- ggplot(data = temp1, aes(x = Temp_C_exo, y = Temp_C_fp))+
  geom_point(size = 3)+
  geom_abline(slope = 1, intercept = 0)+
  ggtitle("Cast 1: 2020-05-25 9:45 a.m.")+
  xlab("SCC sensor temp degrees C")+
  ylab("FP sensor temp degrees C")+
  theme_classic()
tplot1
ggsave(tplot1, device = "png", filename = "C:/Users/Mary Lofton/Desktop/FP_SCC_temp_compare/cast1_compare.png",
       height = 4, width = 4, units = "in", dpi=300)

tplot2 <- ggplot(data = temp2, aes(x = Temp_C_exo, y = Temp_C_fp))+
  geom_point(size = 3)+
  geom_abline(slope = 1, intercept = 0)+
  ggtitle("Cast 2: 2020-05-25 3:40 p.m")+
  xlab("SCC sensor temp degrees C")+
  ylab("FP sensor temp degrees C")+
  theme_classic()
tplot2
ggsave(tplot2, device = "png", filename = "C:/Users/Mary Lofton/Desktop/FP_SCC_temp_compare/cast2_compare.png",
       height = 4, width = 4, units = "in", dpi=300)

#calculate RMSE
RMSE = function(m, o){
  sqrt(mean((m - o)^2, na.rm = TRUE))
}

cast1 <- read_csv("C:/Users/Mary Lofton/Desktop/FP_SCC_temp_compare/cast1_compare.csv") %>%

cast2 <- read_csv("C:/Users/Mary Lofton/Desktop/FP_SCC_temp_compare/cast2_compare.csv")

rmse1 <- RMSE(cast1$Temp_C_fp, cast1$Temp_C_exo)
rmse2 <- RMSE(cast2$Temp_C_fp, cast2$Temp_C_exo)

rmse_all <- RMSE(c(cast1$Temp_C_fp,cast2$Temp_C_fp),c(cast1$Temp_C_exo,cast2$Temp_C_exo))
