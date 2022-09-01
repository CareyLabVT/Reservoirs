# Master QAQC script in prep for publishing catwalk sensor string data to EDI
# this script combines code from other QAQC scripts found in misc_QAQC_scipts folder
# as well as other data files from misc_data_files
# final EDI-ready file outputs directly to MakeEMLCatwalk/2020 folder
# Set up ----
pacman::p_load("RCurl","tidyverse","lubridate", "plotly", "magrittr")
folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_FCRcatwalk/2022/"
source(paste0(folder, "FCR_catwalk_QAQC_function_2018_2022.R"))


# Create a misc_data_files folder if one doesn't already exist

misc_folder <-paste0(folder, "misc_data_files")

if (file.exists(misc_folder)) {
  cat("The folder already exists")
} else {
  dir.create(misc_folder)
}

# download most up to date catwalk data and maintenance log
download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/CAT_MaintenanceLog.txt",paste0(folder, "misc_data_files/FCR_CAT_MaintenanceLog_2018_2022.txt"))
download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/fcre-waterquality.csv",paste0(folder, "misc_data_files/fcre-waterquality.csv"))
download.file('https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/CR6_Files/FCRcatwalk_manual_2022.csv', paste0(folder, "misc_data_files/CAT_2.csv"))

# run standard qaqc function from FCR_catwalk_QAQC_function_2021.R
data_file <- paste0(folder, 'misc_data_files/fcre-waterquality.csv')#current file from the data logger
data2_file <- paste0(folder, 'misc_data_files/CAT_2.csv')#manual downloads to add missing data 
maintenance_file <- paste0(folder, "misc_data_files/FCR_CAT_MaintenanceLog_2018_2022.txt")#maintenance file
output_file <- paste0(folder, "misc_data_files/Catwalk_first_QAQC_2018_2022.csv")#name of the output file
qaqc(data_file,data2_file, maintenance_file, output_file)#function to do the main qaqc


# read in qaqc function output

catdata <- read.csv(output_file)

#create depth column
catdata=catdata%>%mutate(Depth_m_9=Lvl_psi_9*0.70455)#1psi=2.31ft, 1ft=0.305m

depth=catdata%>%
  select(DateTime, ThermistorTemp_C_surface, ThermistorTemp_C_1, Lvl_psi_9,Depth_m_9)

#current time of QAQC for graphing
start_time="2022-01-01 00:00"
end_time="2022-05-31 23:59"


# subset file to only unpublished data

catdata_flag =catdata%>%
  filter(DateTime<"2022-06-01 00:00")

#make sure no NAS in the Flag columns
Flags=catdata_flag%>%
  select(DateTime, starts_with("Flag"))

RowsNA=Flags[!complete.cases(Flags), ] # Keep only the complete rows

# Change time to make graphing easier

catdata_flag$DateTime <- ymd_hms(catdata_flag$DateTime)

# Flag values
# 0: no flag
# 1: value removed due to maintenance and set to NA
# 2: negative or outlier value removed and set to NA, see Methods section for more detail on QAQC process
# 3: negative values set to 0
# 4: value removed due to fouling and set to NA
# 5: questionable value due to potential fouling
# 6: very questionable value due to potential fouling. Values adjusted using a linear or square root function to match high-resolution CTD profiles are given in RDO_mgL_5 and RDO_sat_percent_5
# 7: missing data
# 8: Value corrected using a constant offset due to two thermistor malfunctions in Fall 2020



################################################################################################################
# #graphing temperature
# 
# 
# #Surface Temp
# #From 2018-current
 surf <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_surface)) +
   geom_point()
 surf
# #Plotly so can pick out questionable values
# ggplotly(surf)
# 
# #Just the current year
 surf21=catdata_flag%>%
   filter(DateTime>start_time & DateTime<end_time)%>%
   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_surface)) +
   geom_point()
 surf21
# #Plotly so can pick out questionable values
 ggplotly(surf21)
# 
# # check 1m temp data
# #From 2018-current
 m_1 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_1)) +
  geom_point()
 m_1
# #Plotly so can pick out questionable values
 ggplotly(m_1)
# 
# #Just the current year
 m_1_21=catdata_flag%>%
   filter(DateTime>start_time & DateTime<end_time)%>%
   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_1)) +
   geom_point()
 m_1_21
# #Plotly so can pick out questionable values
# ggplotly(m_1_21)
# 
# # check 2m temp data
# #Plot 2018-current
m_2 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_2)) +
 geom_point()
m_2
# #Plotly so can pick out questionable values
# ggplotly(m_2)
# 
# #Just the current year
m_2_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_2)) +
  geom_point()
m_2_21
# #Plotly so can pick out questionable values
 ggplotly(m_2_21)
# 
# # check 3m temp data
# #Plot From 2018-current
m_3 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_3)) +
 geom_point()
m_3
# #Plotly so can pick out questionable values
 ggplotly(m_3)
# 
# #Just the current year
m_3_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_3)) +
  geom_point()
m_3_21
# #Plotly so can pick out questionable values
 ggplotly(m_3_21)
# 
# # check 4m temp data
# #Plot from 2018-current
m_4 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_4)) +
 geom_point()
m_4
# #Plotly so can pick out questionable values
# ggplotly(m_4)
# 
# # Just from current year
m_4_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_4)) +
  geom_point()
m_4_21
# # Plotly so can pick out questionable values
 ggplotly(m_4_21)
# 
# # check 5m temp data
# # Plot from 2018-current
m_5 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_5)) +
  geom_point()
m_5
# # Plotly so can pick out questionable values
# ggplotly(m_5)
# 
# # Just current year
m_5_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_5)) +
  geom_point()
m_5_21
# # Plotly so can pick out questionable values
 ggplotly(m_5_21)
# 
# # check 6m temp data
# # Plot from 2018-current
m_6 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_6)) +
 geom_point()
m_6
# # Plotly so can pick out questionable values
# ggplotly(m_6)
# 
# # Just the current year
m_6_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_6)) +
  geom_point()
m_6_21
# # Plotly so can pick out questionable values
 ggplotly(m_6_21)
# 
# # check 7m temp data
# # all the temp 2018-current
m_7 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_7)) +
 geom_point()
m_7
# # Plotly so can pick out questionable values
# ggplotly(m_7)
# 
# #filter for the current year
m_7_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_7)) +
  geom_point()
m_7_21
# # plotly so you can pick out the questionable values
 ggplotly(m_7_21)
# 
# # check 8m temp data
# # Plot 2018-current
m_8 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_8)) +
 geom_point()
m_8
# # Plotly so can pick out questionable values
# ggplotly(m_8)
# 
# # Plot just the current year
m_8_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_8)) +
  geom_point()
m_8_21
# # Plotly so can pick out questionable values
 ggplotly(m_8_21)
# 
# 
# # check 9m temp data
# # Plot 2018-current
m_9 <- ggplot(data = catdata_flag, aes(x = DateTime, y = ThermistorTemp_C_9)) +
 geom_point()
m_9
# # Plotly so can pick out questionable values
# ggplotly(m_9)
# 
# # Just the current year
m_9_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_9)) +
  geom_point()
m_9_21
# # Plotly so can pick out questionable values
 ggplotly(m_9_21)
# 
# 
# 
# #graph all the temps in 2021on the same graph so use base R
# #create a new data frame for 2021
t2021=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)
# 
# #this part taken from the daily email script
par(mfrow=c(1,1))
par(oma=c(1,1,1,4))
plot(t2021$DateTime,t2021$ThermistorTemp_C_surface, main="Water Temp", xlab="Time", ylab="degrees C", type='l', col="firebrick4", lwd=1.5, ylim=c(0,35))
points(t2021$DateTime, t2021$ThermistorTemp_C_1, col="firebrick1", type='l', lwd=1.5)
points(t2021$DateTime, t2021$ThermistorTemp_C_2, col="DarkOrange1", type='l', lwd=1.5)
points(t2021$DateTime, t2021$ThermistorTemp_C_3, col="gold", type='l', lwd=1.5)
points(t2021$DateTime, t2021$ThermistorTemp_C_4, col="greenyellow", type='l', lwd=1.5)
points(t2021$DateTime, t2021$ThermistorTemp_C_5, col="medium sea green", type='l', lwd=1.5)
points(t2021$DateTime, t2021$ThermistorTemp_C_6, col="sea green", type='l', lwd=1.5)
points(t2021$DateTime, t2021$ThermistorTemp_C_7, col="DeepSkyBlue4", type='l', lwd=1.5)
points(t2021$DateTime, t2021$ThermistorTemp_C_8, col="blue2", type='l', lwd=1.5)
points(t2021$DateTime, t2021$ThermistorTemp_C_9, col="blue4", type='l', lwd=1.5)
par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right",c("0.1m","1m", "2m", "3m", "4m", "5m", "6m", "7m","8m", "9m"),
       text.col=c("firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                  "DeepSkyBlue4", "blue2", "blue4"), cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
# 
# #check the DO temp compared to temp string in 2021
# 
plot(t2021$DateTime,t2021$ThermistorTemp_C_1, main="EXO vs. Temp String", xlab="Time", ylab="degrees C", type='l', col="firebrick4", lwd=1.5, ylim=c(0,35))
points(t2021$DateTime, t2021$ThermistorTemp_C_2, col="firebrick1", type='l', lwd=1.5)
points(t2021$DateTime, t2021$EXOTemp_C_1, col="black", type='l', lwd=1.5)

plot(t2021$DateTime,t2021$RDOTemp_C_5, main="RDO 5m vs. Temp String", xlab="Time", ylab="degrees C", type='l', col="black", lwd=1.5, ylim=c(0,15))
points(t2021$DateTime, t2021$ThermistorTemp_C_5, col="medium sea green", type='l', lwd=1.5)

plot(t2021$DateTime,t2021$RDOTemp_C_9, main="RDO 9m vs. Temp String", xlab="Time", ylab="degrees C", type='l', col="black", lwd=1.5, ylim=c(0,10))
points(t2021$DateTime, t2021$ThermistorTemp_C_9, col="blue4", type='l', lwd=1.5)






###########################################################################################################################################################################


#Creating a new flag "6" started in 2019: "very questionable value due to potential fouling. Values adjusted using a linear or square root function to match high-resolution CTD profiles are given in RDO_mgL_5 and RDO_sat_percent_5"
#Creating a new flag "5" which means that the values are very questionable due to fouling but not adjusted (starting in 2019)

#check the DO with graphs
  
#convert DateTime to make the graphs nice
  #Convert the time and put it in current time zone so the times line up when changing NAs.
  #+5 is during EDT and +4 is during EST(make sure to check this in December)
  #Have to do strpttime or you get some NAs in the DateTime column
  #Do this so the graphing is nice
  #catdata_flag$DateTime<-as.POSIXct(strptime(catdata_flag$DateTime, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5")

#Check 1.6m EXO DO data
#Plot 2018-current
EXODO <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXODO_mgL_1)) +
  geom_point()
EXODO
# 
# # Plotly so can pick out questionable values
# ggplotly(EXODO)
# 
# #Plot Just the current year
 EXODO_21=catdata_flag%>%
   filter(DateTime>start_time & DateTime<end_time)%>%
   ggplot(.,aes(x = DateTime, y = EXODO_mgL_1)) +
   geom_point()
 EXODO_21
# # Plotly so can pick out questionable values
# ggplotly(EXODO_21)
# 
# # Check the 5m RDO
# # plot of 5m DO from 2018 to present
RDO5 <- ggplot(data = catdata_flag, aes(x = DateTime, y = RDO_mgL_5)) +
  geom_point()
RDO5
# # Plotly so can pick out questionable values
# ggplotly(RDO5)
# 
# # Plot the current year
RDO5_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = RDO_mgL_5)) +
  geom_point()
RDO5_21
# # Plotly so can pick out questionable values
# ggplotly(RDO5_21)
# 
# 
# # Plot the 9m Do
# # From 2018-current 
RDO9=ggplot(catdata_flag, aes(x = DateTime, y = RDO_mgL_9)) +
  geom_point()
RDO9
# # Plotly so can pick out questionable values
# ggplotly(RDO9)
# 
# #Just the current year
 RDO9_21=catdata_flag%>%
   #filter(DateTime>start_time & DateTime<end_time)%>%
   filter(DateTime>start_time & DateTime<end_time)%>%
   ggplot(.,aes(x = DateTime, y = RDO_mgL_9)) +
   geom_point()
 RDO9_21
# # Plotly so can pick out questionable values
 ggplotly(RDO9_21)
# 


###########################################################################################################################################################################
# chl and phyco visual qaqc-plot to see if everything looks right

# Chla
# Plot for 2018-current
chl_ugl <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOChla_ugL_1)) +
  geom_point()
chl_ugl
# # Plotly so can pick out questionable values
# ggplotly(chl_ugl)
# 
# # Plot just the current year
chl_ugl_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = EXOChla_ugL_1)) +
  geom_point()
chl_ugl_21
# 
# 
# # plot the daily mean
# # calculate the daily mean
chl_mean <- catdata_flag %>%
 select(DateTime, EXOChla_ugL_1) %>%
 mutate(day = date(DateTime)) %>%
 group_by(day) %>%
 mutate(daily_mean = mean(EXOChla_ugL_1, na.rm = TRUE)) %>%
 distinct(day, .keep_all = TRUE)
# 
# # plot the daily mean
chl_mean_plot <- ggplot(data = chl_mean, aes(x = day, y = daily_mean)) +
 geom_point()
chl_mean_plot
# # Plotly so can pick out questionable values
# ggplotly(chl_mean)
# 
# # Plot the chla and the daily mean on the same graph
plot(catdata_flag$DateTime, catdata_flag$EXOChla_ugL_1)
points(chl_mean$DateTime, chl_mean$daily_mean, type="l", col="green")
# 
# 
# # Chla-RFU
# # Plot 2018-current
chl_rfu <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOChla_RFU_1)) +
  geom_point()
chl_rfu
# # Plotly so can pick out questionable values
# ggplotly(chl_rfu)
# 
# 
# # Just the current year
chl_rfu_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = EXOChla_RFU_1)) +
  geom_point()
chl_rfu_21
# 
# # Phyco-RFU
# # Plot 2018-current
phyco_rfu <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOBGAPC_RFU_1)) +
  geom_point()
phyco_rfu
# # Plotly so can pick out questionable values
# ggplotly(phyco_rfu)
# 
# # Just the current year
phyco_rfu_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = EXOBGAPC_RFU_1)) +
  geom_point()
# # Plotly so can pick out questionable values
# ggplotly(phyco_rfu_21)
# 
# #################################################################################################
# 
# # fDOM-RFU
# # Plot 2018-current
fDOM_rfu <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOfDOM_RFU_1)) +
  geom_point()
fDOM_rfu
# # Plotly so can pick out questionable values
# ggplotly(fDOM_rfu)
# 
# # Just the current year
fDOM_rfu_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = EXOfDOM_RFU_1)) +
  geom_point()
 fDOM_rfu_21
# # Plotly so can pick out questionable values
# ggplotly(fDOM_rfu_21)
# 
# # fDOM-QSU
# # Plot 2018-current
fDOM_qsu <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOfDOM_QSU_1)) +
  geom_point()
fDOM_qsu
# # Plotly so can pick out questionable values
# ggplotly(fDOM_qsu)
# 
# # Just the current year
fDOM_qsu_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = EXOfDOM_QSU_1)) +
  geom_point()
 fDOM_qsu_21
# # Plotly so can pick out questionable values
# ggplotly(fDOM_qsu_21)
# 


###########################################################################################################################################################################
# conductivity visual QAQC

#convert DateTime to make the graphs nice
#Convert the time and put it in current time zone so the times line up when changing NAs.
#+5 is during EDT and +4 is during EST(make sure to check this in December)
#Have to do strpttime or you get some NAs in the DateTime column
#Do this so the graphing is nice
 #catdata_flag$DateTime<-as.POSIXct(strptime(catdata_flag$DateTime, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5")
# 
# #Plot from 2018-current
Cond <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOCond_uScm_1)) +
  geom_point()
Cond
# # Plotly so can pick out questionable values
# ggplotly(Cond)
 
 #Look at the Flag 5 values
 co=catdata_flag%>%filter(Flag_Cond<5)
 
 plot(catdata_flag$DateTime,catdata_flag$EXOCond_uScm_1, col="red")
 points(co$DateTime,co$EXOCond_uScm_1, col="black")
# 
# 
Con=catdata_flag%>%
  select(c(DateTime,EXOTemp_C_1,EXOCond_uScm_1,EXOSpCond_uScm_1,EXO_wiper,EXO_pressure))
# #Just the current year
# 
Cond_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = EXOCond_uScm_1)) +
  geom_point()
Cond_21
# # Plotly so can pick out questionable values
# ggplotly(Cond_21)
# 
# #Specific Conductivity
# 
# #Plot from 2018-current
SpCond <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOSpCond_uScm_1)) +
  geom_point()
SpCond
# # Plotly so can pick out questionable values
# ggplotly(SpCond)
# 
# #Just the current year
SpCond_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = EXOSpCond_uScm_1)) +
  geom_point()
SpCond_21
# # Plotly so can pick out questionable values
# ggplotly(SpCond_21)
# 
# #Total Dissolved Solids
TDS <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXOTDS_mgL_1)) +
  geom_point()
TDS
# # Plotly so can pick out questionable values
# ggplotly(TDS)
# 
# #Just the current year
TDS_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = EXOTDS_mgL_1)) +
  geom_point()
TDS_21
# # Plotly so can pick out questionable values
# ggplotly(TDS_21)
# 
# #Depth
Depth <- ggplot(data = catdata_flag, aes(x = DateTime, y = EXO_depth_m)) +
  geom_point()
Depth
# # Plotly so can pick out questionable values
# ggplotly(Depth)
# 
# #Just the current year
Depth_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = EXO_depth_m)) +
  geom_point()
Depth_21
# # Plotly so can pick out questionable values
# ggplotly(Depth_21)

# #Depth 9m
Depth <- ggplot(data = catdata_flag, aes(x = DateTime, y = Depth_m_13)) +
  geom_point()
Depth
# # Plotly so can pick out questionable values
# ggplotly(Depth)
# 
# #Just the current year
Depth_9_21=catdata_flag%>%
  filter(DateTime>start_time & DateTime<end_time)%>%
  ggplot(.,aes(x = DateTime, y = Depth_m_13)) +
  geom_point()
Depth_9_21
# # Plotly so can pick out questionable values
# ggplotly(Depth_21)

###########################################################################################################################################################################
# write final csv ----
# some final checking of flags
# for (i in 1:nrow(catdata_all)) {
#   if(is.na(catdata_all$Flag_fDOM[i])){
#     catdata_all$Flag_fDOM[i] <- 0
#   }
# }

#check the flag column
Flags=catdata_flag%>%
  select(starts_with("Flag"))

for(b in 1:nrow(Flags)){
  print(colnames(Flags[b]))
  print(table(Flags[,b], useNA = "always"))
}

#Order by date and time

catdata_flag <- catdata_flag[order(catdata_flag$DateTime),]


# for(b in 1:nrow())
# str(catdata_flag)

#rearrange the cols
catdata_flag <- catdata_flag %>%
  select(Reservoir, Site, DateTime, ThermistorTemp_C_surface:ThermistorTemp_C_9,
         RDO_mgL_5, RDOsat_percent_5, RDO_mgL_5_adjusted, RDOsat_percent_5_adjusted,
         RDOTemp_C_5, RDO_mgL_9, RDOsat_percent_9, RDO_mgL_9_adjusted, RDOsat_percent_9_adjusted,
         RDOTemp_C_9,EXOTemp_C_1, EXOCond_uScm_1, EXOSpCond_uScm_1, EXOTDS_mgL_1, EXODOsat_percent_1,
         EXODO_mgL_1, EXOChla_RFU_1, EXOChla_ugL_1, EXOBGAPC_RFU_1, EXOBGAPC_ugL_1,
         EXOfDOM_RFU_1, EXOfDOM_QSU_1,EXOTurbidity_FNU_1, EXO_pressure_psi, EXO_depth_m, EXO_battery_V, EXO_cablepower_V,
         EXO_wiper_V, Lvl_psi_9, LvlTemp_C_9, Depth_m_9, RECORD, CR6_Batt_V, CR6Panel_Temp_C,
         Flag_ThermistorTemp_C_surface:Flag_ThermistorTemp_C_9,Flag_RDO_mgL_5, Flag_RDOsat_percent_5, Flag_RDOTemp_C_5,
         Flag_RDO_mgL_9, Flag_RDOsat_percent_9, Flag_RDOTemp_C_9,Flag_EXOTemp_C_1, Flag_EXOCond_uScm_1, Flag_EXOSpCond_uScm_1,Flag_EXOTDS_mgL_1,
         Flag_EXODOsat_percent_1, Flag_EXODO_mgL_1, Flag_EXOChla_RFU_1,Flag_EXOChla_ugL_1, Flag_EXOBGAPC_RFU_1,Flag_EXOBGAPC_ugL_1,
         Flag_EXOfDOM_RFU_1,Flag_EXOfDOM_QSU_1,Flag_EXOTurbidity_FNU_1, Flag_EXO_pressure_psi, Flag_EXO_depth_m, Flag_EXO_battery_V, Flag_EXO_cablepower_V,
         Flag_EXO_wiper_V, Flag_Lvl_psi_9, Flag_LvlTemp_C_9)

# convert datetimes to characters so that they are properly formatted in the output file
catdata_flag$DateTime <- as.character(catdata_flag$DateTime)
  
  
write.csv(catdata_flag, paste0(folder, '/FCR_Catwalk_2018_2022.csv'), row.names = FALSE)


