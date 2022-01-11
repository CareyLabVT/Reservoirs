# Master QAQC script in prep for publishing catwalk sensor string data to EDI
# this script combines code from other QAQC scripts found in misc_QAQC_scipts folder
# as well as other data files from misc_data_files
# final EDI-ready file outputs directly to MakeEML_CCRcatwalk/2020 folder
# Set up ----
pacman::p_load("RCurl","tidyverse","lubridate", "plotly", "magrittr")
folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCRcatwalk/2021/"
source(paste0(folder, "CCR_catwalk_QAQC_function_2021.R"))

# download most up to date catwalk data and maintenance log for input into the function
download.file("https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/ccre-dam-data/CCRW_maintenance_log.txt",paste0(folder, "CCRW_maintenance_log_2021.txt"))
download.file("https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/ccre-dam-data/ccre-waterquality.csv",paste0(folder, "ccre-waterquality.csv"))
#download.file('https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/CR6_Files/FCRcatwalk_manual_2021.csv', paste0(folder, "CAT_2.csv"))

# run standard qaqc function from FCR_catwalk_QAQC_function_2021.R
data_file <- paste0(folder, "ccre-waterquality.csv")#current file from the data logger
#data2_file <- paste0(folder, 'CAT_2.csv')#manual downloads to add missing data 
maintenance_file <- paste0(folder, "CCRW_maintenance_log_2021.txt")#maintenance file
output_file <- paste0(folder, "CCRwaterquality_first_QAQC_2021.csv")#name of the output file
qaqc(data_file, maintenance_file, output_file)#function to do the main qaqc


# read in qaqc function output

ccrwater <- read.csv(output_file)

#current time of QAQC for graphing
current_time_start="2021-01-01 00:00"
current_time_end="2021-12-31 23:59"


# subset file to only unpublished data
ccrwater <- ccrwater[ccrwater$DateTime<"2021-12-31 23:59",]


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


###########################################################################################################################################################################
# temp qaqc ----

# #graph to figure out off set
 # depth=ccrwater%>%
 #   select(DateTime,LvlDepth_m_13, Lvl_psi_13, ThermistorTemp_C_1,ThermistorTemp_C_2,ThermistorTemp_C_3, EXO_depth_m_9)%>%
 #   drop_na(ThermistorTemp_C_1)
 # 
 # depth$DateTime<-as.POSIXct(strptime(depth$DateTime, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5")
 # #look at the top 10 depths to make sure they make sense
 # ccrwater2=depth%>%
 #   mutate(depth_1=LvlDepth_m_13-18.92)%>%
 #   mutate(depth_2=LvlDepth_m_13-18.065)%>%
 #    mutate(depth_3=LvlDepth_m_13-17.07)%>%
 #    mutate(depth_4=LvlDepth_m_13-16.075)%>%
 #    mutate(depth_5=LvlDepth_m_13-15.08)%>%
 #    mutate(depth_6=LvlDepth_m_13-14.085)%>%
 #    mutate(depth_7=LvlDepth_m_13-13.09)%>%
 #    mutate(depth_8=LvlDepth_m_13-12.095)%>%
 #    mutate(depth_9=LvlDepth_m_13-11.1)%>%
 #    mutate(depth_10=LvlDepth_m_13-9.11)
 # #reorder to put the EXO depth in between Thermistor 9 and 10 to see if they consistent
 # ccrwater2=ccrwater2%>%
 #    select(DateTime,depth_1,depth_2,depth_3, depth_4,depth_5,depth_6, depth_7,depth_8,depth_9, EXO_depth_m_9, depth_10, everything())
 # #Use to figure out when the thermistor is out of the water-larger variability when out of the water
 # a=depth%>%
 #   filter(DateTime>"2021-11-29 00:00" & DateTime<"2021-12-02 00:00")%>%
 #   ggplot(., aes(x=DateTime))+
 #   geom_line(aes(y=ThermistorTemp_C_1, color="red"))+
 #   geom_line(aes(y=ThermistorTemp_C_2, color="blue"))+
 #   geom_line(aes(y=ThermistorTemp_C_3, color="green"))+
 #   geom_line(aes(y=LvlDepth_m_13, color="purple"))

#
#Setting the temperature to NA when the thermistors are out of the water
#add a depth column which we set NAs for when the thermistor is out of the water. Flag 2
#change the temp to NA when the thermistor is clearly out of the water which we used to determine the depth of the temp string
#negative depths are changed to NA

ccrwater=ccrwater%>%
  mutate(depth_1=LvlDepth_m_13-18.92)%>%
  mutate(depth_2=LvlDepth_m_13-18.065)%>%
   mutate(depth_3=LvlDepth_m_13-17.07)%>%
  mutate(Flag_Temp_1= ifelse(!is.na(depth_1) & depth_1<0 ,2,Flag_Temp_1))%>%
  mutate(ThermistorTemp_C_1=ifelse(!is.na(depth_1) & depth_1<0,NA,ThermistorTemp_C_1))%>%
  mutate(Flag_Temp_2= ifelse(!is.na(depth_2) & depth_2<0 ,2,Flag_Temp_2))%>%
  mutate(ThermistorTemp_C_2=ifelse(!is.na(depth_2) & depth_2<0,NA,ThermistorTemp_C_2))%>%
   mutate(Flag_Temp_3= ifelse(!is.na(depth_3) & depth_3<0 ,2,Flag_Temp_3))%>%
   mutate(ThermistorTemp_C_3=ifelse(!is.na(depth_3) & depth_3<0,NA,ThermistorTemp_C_3))%>%
  select(-depth_1,-depth_2, -depth_3)
  

################################################################################################################
# #graphing temperature
  #convert DateTime to make the graphs nice
  #Convert the time and put it in current time zone so the times line up when changing NAs.
  #+5 is during EDT and +4 is during EST(make sure to check this in December)
  #Have to do strpttime or you get some NAs in the DateTime column
  #Do this so the graphing is nice
  #  ccrwater$DateTime<-as.POSIXct(strptime(ccrwater$DateTime, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5")
# 
# 
# # check 1 temp data
# #From 2021-current
 m_1 <- ggplot(data = ccrwater, aes(x = DateTime, y = ThermistorTemp_C_1)) +
  geom_point()
 m_1
#Only use plotly to find DateTime of questionable values
# ggplotly(m_1)

# #Just the current year
 m_1_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_1)) +
   geom_point()
 m_1_21
#Only use plotly to find DateTime of questionable values
 #ggplotly(m_1_21)

# check 2 temp data
#Plot 2021-current
 m_2 <- ggplot(data = ccrwater, aes(x = DateTime, y = ThermistorTemp_C_2)) +
  geom_point()
 m_2
#Only use plotly to find DateTime of questionable values
# ggplotly(m_2)

 #Just the current year
 m_2_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_2)) +
   geom_point()
 m_2_21
#Only use plotly to find DateTime of questionable values
 #ggplotly(m_2_21)

# check 3 temp data
#Plot From 2021-current
 m_3 <- ggplot(data = ccrwater, aes(x = DateTime, y = ThermistorTemp_C_3)) +
  geom_point()
 m_3
#Only use plotly to find DateTime of questionable values
# ggplotly(m_3)

#Just the current year
 m_3_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_3)) +
   geom_point()
 m_3_21
#Only use plotly to find DateTime of questionable values
# ggplotly(m_3_21)

# check 4 temp data
#Plot from 2021-current
 m_4 <- ggplot(data = ccrwater, aes(x = DateTime, y = ThermistorTemp_C_4)) +
  geom_point()
 m_4
#Only use plotly to find DateTime of questionable values
# ggplotly(m_4)

# Just from current year
 m_4_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_4)) +
   geom_point()
 m_4_21
#Only use plotly to find DateTime of questionable values
# ggplotly(m_4_21)

# check 5 temp data
# Plot from 2021-current
 m_5 <- ggplot(data = ccrwater, aes(x = DateTime, y = ThermistorTemp_C_5)) +
   geom_point()
 m_5
#Only use plotly to find DateTime of questionable values
# ggplotly(m_5)

# Just current year
 m_5_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_5)) +
   geom_point()
 m_5_21
#Only use plotly to find DateTime of questionable values
 #ggplotly(m_5_21)

# check 6 temp data
# Plot from 2021-current
   m_6 <- ggplot(data = ccrwater, aes(x = DateTime, y = ThermistorTemp_C_6)) +
  geom_point()
 m_6
#Only use plotly to find DateTime of questionable values
# ggplotly(m_6)

# Just the current year
 m_6_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_6)) +
   geom_point()
m_6_21
#Only use plotly to find DateTime of questionable values
 #ggplotly(m_6_21)

# check 7 temp data
# all the temp 2021-current
 m_7 <- ggplot(data = ccrwater, aes(x = DateTime, y = ThermistorTemp_C_7)) +
  geom_point()
 m_7
#Only use plotly to find DateTime of questionable values
# ggplotly(m_7)

#filter for the current year
 m_7_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_7)) +
   geom_point()
m_7_21
# plotly so you can pick out the questionable values
 #ggplotly(m_7_21)

# check 8 temp data
# Plot 2021-current
 m_8 <- ggplot(data = ccrwater, aes(x = DateTime, y = ThermistorTemp_C_8)) +
  geom_point()
 m_8
#Only use plotly to find DateTime of questionable values
# ggplotly(m_8)

# # Plot just the current year
 m_8_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_8)) +
   geom_point()
 m_8_21
#Only use plotly to find DateTime of questionable values
# ggplotly(m_8_21)


# check 9 temp data
  # Plot 2021-current
 m_9 <- ggplot(data = ccrwater, aes(x = DateTime, y = ThermistorTemp_C_9)) +
  geom_point()
 m_9
#Only use plotly to find DateTime of questionable values
# ggplotly(m_9)

# Just the current year
 m_9_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_9)) +
   geom_point()
 m_9_21
#Only use plotly to find DateTime of questionable values
# ggplotly(m_9_21)


  # check 10 temp data
   #From 2021-current
   m_10 <- ggplot(data = ccrwater, aes(x = DateTime, y = ThermistorTemp_C_10)) +
    geom_point()
   m_10
  #Only use plotly to find DateTime of questionable values
   ggplotly(m_10)
  
   #Just the current year
   m_10_21=ccrwater%>%
    filter(DateTime>current_time_start & DateTime<current_time_end)%>%
     ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_10)) +
     geom_point()
   m_10_21
  #Only use plotly to find DateTime of questionable values
  # ggplotly(m_10_21)


  # check 11 temp data
  #From 2021-current
   m_11 <- ggplot(data = ccrwater, aes(x = DateTime, y = ThermistorTemp_C_11)) +
    geom_point()
   m_11
  #Only use plotly to find DateTime of questionable values
   #ggplotly(m_11)
  
  #Just the current year
   m_11_21=ccrwater%>%
    filter(DateTime>current_time_start & DateTime<current_time_end)%>%
     ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_11)) +
     geom_point()
   m_11_21
  #Only use plotly to find DateTime of questionable values
  # ggplotly(m_11_21)

  # check 12 temp data
  #From 2021-current
   m_12 <- ggplot(data = ccrwater, aes(x = DateTime, y = ThermistorTemp_C_12)) +
    geom_point()
   m_12
  #Only use plotly to find DateTime of questionable values
  # ggplotly(m_12)
  
  # #Just the current year
   m_12_21=ccrwater%>%
    filter(DateTime>current_time_start & DateTime<current_time_end)%>%
     ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_12)) +
     geom_point()
   m_12_21
 #Only use plotly to find DateTime of questionable values
  # ggplotly(m_12_21)

  # check 13 temp data
  #From 2021-current
   m_13 <- ggplot(data = ccrwater, aes(x = DateTime, y = ThermistorTemp_C_13)) +
    geom_point()
   m_13
  #Only use plotly to find DateTime of questionable values
   ggplotly(m_13)
  
  #Just the current year
   m_13_21=ccrwater%>%
    filter(DateTime>current_time_start & DateTime<current_time_end)%>%
     ggplot(.,aes(x = DateTime, y = ThermistorTemp_C_13)) +
     geom_point()
   m_13_21
   #Only use plotly to find DateTime of questionable values
   #ggplotly(m_13_21)
  
 #graph all the temps in 2021on the same graph so use base R
 #create a new data frame for 2021
 t2021=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)

#this part taken from the daily email script
#Just checking all depths over time for something funky
   par(mfrow=c(1,1))
   par(oma=c(1,1,1,4))
   plot(t2021$DateTime,t2021$ThermistorTemp_C_1, main="Water Temp", xlab="Time", ylab="degrees C", type='l', col="firebrick4", lwd=1.5, ylim=c(0, 35))
   points(t2021$DateTime, t2021$ThermistorTemp_C_2, col="firebrick1", type='l', lwd=1.5)
   points(t2021$DateTime, t2021$ThermistorTemp_C_3, col="DarkOrange1", type='l', lwd=1.5)
   points(t2021$DateTime, t2021$ThermistorTemp_C_4, col="gold", type='l', lwd=1.5)
   points(t2021$DateTime, t2021$ThermistorTemp_C_5, col="greenyellow", type='l', lwd=1.5)
   points(t2021$DateTime, t2021$ThermistorTemp_C_6, col="medium sea green", type='l', lwd=1.5)
   points(t2021$DateTime, t2021$ThermistorTemp_C_7, col="sea green", type='l', lwd=1.5)
   points(t2021$DateTime, t2021$ThermistorTemp_C_8, col="DeepSkyBlue4", type='l', lwd=1.5)
   points(t2021$DateTime, t2021$ThermistorTemp_C_9, col="blue2", type='l', lwd=1.5)
   points(t2021$DateTime, t2021$EXOTemp_C_9, col="blue4", type='l', lwd=1.5)
   points(t2021$DateTime, t2021$ThermistorTemp_C_10, col="darkslateblue", type='l', lwd=1.5)
   points(t2021$DateTime, t2021$ThermistorTemp_C_11, col="magenta2", type='l', lwd=1.5)
   points(t2021$DateTime, t2021$ThermistorTemp_C_12, col="darkmagenta", type='l', lwd=1.5)
   points(t2021$DateTime, t2021$ThermistorTemp_C_13, col="black", type='l', lwd=1.5)
   par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
   plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
   legend("right",c("0.1m","1m", "2m", "3m", "4m", "5m", "6m", "7m", "8m","EXO_9m","10m","11m","15m","19m"),
          text.col=c("firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                     "DeepSkyBlue4", "blue2", "blue4", "darkslateblue", "magenta2", "darkmagenta", "black"), cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')

#check the EXO temp compared to temp string in 2021

 plot(t2021$DateTime,t2021$ThermistorTemp_C_1, main="EXO vs. Temp String", xlab="Time", ylab="degrees C", type='l', col="firebrick4", lwd=1.5, ylim=c(0,35))
 points(t2021$DateTime, t2021$ThermistorTemp_C_2, col="firebrick1", type='l', lwd=1.5)
 points(t2021$DateTime, t2021$EXOTemp_C_1, col="black", type='l', lwd=1.5)
 points(t2021$TIMESTAMP, t2021$wtr_3, col="DarkOrange1", type='l', lwd=1.5)
 points(t2021$TIMESTAMP, t2021$wtr_4, col="gold", type='l', lwd=1.5)

#check the pressure sensor temp compared to temp string in 2021
  
   plot(t2021$DateTime,t2021$ThermistorTemp_C_13, main="Pressure Sensor vs. Temp String", xlab="Time", ylab="degrees C", type='l', col="firebrick4", lwd=1.5, ylim=c(0,35))
   points(t2021$DateTime, t2021$LvlTemp_C_13, col="black", type='l', lwd=1.5)




###########################################################################################################################################################################


#Creating a new flag "6" started in 2019: "very questionable value due to potential fouling. Values adjusted using a linear or square root function to match high-resolution CTD profiles are given in RDO_mgL_5 and RDO_sat_percent_5"
#Creating a new flag "5" which means that the values are very questionable due to fouling but not adjusted (starting in 2019)
################
#Correcting some DO values 

#check the DO with graphs
  
#convert DateTime to make the graphs nice
  #Convert the time and put it in current time zone so the times line up when changing NAs.
  #+5 is during EDT and +4 is during EST(make sure to check this in December)
  #Have to do strpttime or you get some NAs in the DateTime column
  #Do this so the graphing is nice
#  ccrwater$DateTime<-as.POSIXct(strptime(ccrwater$DateTime, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+4")

#Check 1.5m EXO DO data
#Plot 2021-current
 EXODO_1 <- ggplot(data = ccrwater, aes(x = DateTime, y = EXODO_mgL_1)) +
   geom_point()
 EXODO_1
 #Only use plotly to find DateTime of questionable values
 #ggplotly(EXODO_1)
 
#Plot Just the current year
 EXODO_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = EXODO_mgL_1)) +
   geom_point()
 EXODO_21
#Only use plotly to find DateTime of questionable values
 #ggplotly(EXODO_21)
 

 
# Plot the 9m Do from EXO
# From 2021-current 
 EXO9=ggplot(ccrwater, aes(x = DateTime, y = EXODO_mgL_9)) +
   geom_point()
 EXO9
#Only use plotly to find DateTime of questionable values
 #ggplotly(EXO9)
 
 #Just the current year
 EXO9_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = EXODO_mgL_9)) +
   geom_point()
 EXO9_21
#Only use plotly to find DateTime of questionable values
 #ggplotly(EXO9_21)
 


###########################################################################################################################################################################
# chl and phyco visual qaqc-plot to see if everything looks right

# Chla 1m EXO
# Plot for 2021-current
 chl_ugl_1 <- ggplot(data = ccrwater, aes(x = DateTime, y = EXOChla_ugL_1)) +
   geom_point() 
 chl_ugl_1
#Only use plotly to find DateTime of questionable values
 #ggplotly(chl_ugl_1)
 
# Plot just the current year
 chl_ugl_1_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = EXOChla_ugL_1)) +
   geom_point()
 chl_ugl_1_21
 #Only use plotly to find DateTime of questionable values
 #ggplotly(chl_ugl_1_21)
 
# plot the daily mean
# calculate the daily mean
 chl_mean_1 <- ccrwater %>%
  select(DateTime, EXOChla_ugL_1) %>%
  mutate(day = date(DateTime)) %>%
  group_by(day) %>%
  mutate(daily_mean = mean(EXOChla_ugL_1, na.rm = TRUE)) %>%
  distinct(day, .keep_all = TRUE)
# 
# plot the daily mean
 chl_mean_plot <- ggplot(data = chl_mean_1, aes(x = day, y = daily_mean)) +
  geom_point()
 chl_mean_plot
#Only use plotly to find DateTime of questionable values
 #ggplotly(chl_mean)

# Plot the chla and the daily mean on the same graph
 plot(ccrwater$DateTime, ccrwater$EXOChla_ugL_1)
 points(chl_mean$DateTime, chl_mean$daily_mean, type="l", col="green")

 
# Chla-RFU
# Plot 2021-current
 chl_rfu <- ggplot(data = ccrwater, aes(x = DateTime, y = EXOChla_RFU_1)) +
   geom_point()
 chl_rfu
#Only use plotly to find DateTime of questionable values
 #ggplotly(chl_rfu)

# Just the current year
 chl_rfu_21=ccrwater%>%
   filter(DateTime>current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = EXOChla_RFU_1)) +
   geom_point()
 chl_rfu_21
 
# Phyco-RFU
# Plot 2021-current
 phyco_rfu <- ggplot(data = ccrwater, aes(x = DateTime, y = EXOBGAPC_RFU_1)) +
   geom_point() 
 phyco_rfu
#Only use plotly to find DateTime of questionable values
 #ggplotly(phyco_rfu)
 
# Just the current year
 phyco_rfu_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
 
   ggplot(.,aes(x = DateTime, y = EXOBGAPC_RFU_1)) +
   geom_point()
#Only use plotly to find DateTime of questionable values
 #ggplotly(phyco_rfu_21)
 
# #################################################################################################
# 
# fDOM-RFU 1m EXO
# Plot 2021-current
 fDOM_rfu_1 <- ggplot(data = ccrwater, aes(x = DateTime, y = EXOfDOM_RFU_1)) +
   geom_point() 
 fDOM_rfu_1
#Only use plotly to find DateTime of questionable values
 #ggplotly(fDOM_rfu_1)
 
# Just the current year
 fDOM_rfu_1_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = EXOfDOM_RFU_1)) +
   geom_point()
#Only use plotly to find DateTime of questionable values
 #ggplotly(fDOM_rfu_1_21)
  
# fDOM-RFU 9m EXO
# Plot 2021-current
   fDOM_rfu_9 <- ggplot(data = ccrwater, aes(x = DateTime, y = EXOfDOM_RFU_9)) +
     geom_point() 
   fDOM_rfu_9
#Only use plotly to find DateTime of questionable values
   #ggplotly(fDOM_rfu_9)
   
# Just the current year
   fDOM_rfu_9_21=ccrwater%>%
    filter(DateTime>current_time_start & DateTime<current_time_end)%>%
     ggplot(.,aes(x = DateTime, y = EXOfDOM_RFU_9)) +
     geom_point()
#Only use plotly to find DateTime of questionable values
   #ggplotly(fDOM_rfu_9_21)
   
 # fDOM-QSU 1m EXO
# Plot 2021-current
 fDOM_qsu_1 <- ggplot(data = ccrwater, aes(x = DateTime, y = EXOfDOM_QSU_1)) +
   geom_point() 
 fDOM_qsu_1
 #Only use plotly to find DateTime of questionable values
 #ggplotly(fDOM_qsu_1)
 
# Just the current year
 fDOM_qsu_1_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = EXOfDOM_QSU_1)) +
   geom_point()
#Only use plotly to find DateTime of questionable values
 #ggplotly(fDOM_qsu_1_21)
 
# fDOM-QSU 9m EXO
# Plot 2021-current
   fDOM_qsu_9 <- ggplot(data = ccrwater, aes(x = DateTime, y = EXOfDOM_QSU_9)) +
     geom_point() 
   fDOM_qsu_9
#Only use plotly to find DateTime of questionable values
   #ggplotly(fDOM_qsu_9)
   
# Just the current year
   fDOM_qsu_9_21=ccrwater%>%
    filter(DateTime>current_time_start & DateTime<current_time_end)%>%
     ggplot(.,aes(x = DateTime, y = EXOfDOM_QSU_9)) +
     geom_point()
#Only use plotly to find DateTime of questionable values
 #ggplotly(fDOM_qsu_9_21)

###########################################################################################################################################################################
# conductivity visual QAQC


#convert DateTime to make the graphs nice
#Convert the time and put it in current time zone so the times line up when changing NAs.
#+5 is during EDT and +4 is during EST(make sure to check this in December)
#Have to do strpttime or you get some NAs in the DateTime column
#Do this so the graphing is nice
# ccrwater$DateTime<-as.POSIXct(strptime(ccrwater$DateTime, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5")

#Cond from 1m EXO 
#Plot from 2021-current
 Cond_1 <- ggplot(data = ccrwater, aes(x = DateTime, y = EXOCond_uScm_1)) +
   geom_point()
 Cond_1
#Only use plotly to find DateTime of questionable values
 #ggplotly(Cond_1)

 
#Just the current year
 
 Cond_1_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = EXOCond_uScm_1)) +
   geom_point()
 Cond_1_21
#Only use plotly to find DateTime of questionable values
 #ggplotly(Cond_1_21)
 
 #Cond from 9m EXO 
 #Plot from 2021-current
 Cond_9 <- ggplot(data = ccrwater, aes(x = DateTime, y = EXOCond_uScm_9)) +
   geom_point()
 Cond_9
 #Only use plotly to find DateTime of questionable values
 #ggplotly(Cond_9)
 
 
 #Just the current year
 
 Cond_9_21=ccrwater%>%
   filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = EXOCond_uScm_9)) +
   geom_point()
 Cond_9_21
 #Only use plotly to find DateTime of questionable values
 #ggplotly(Cond_9_21)

###########################################################################################################  
#Specific Conductivity from 1m EXO
 
#Plot from 2021-current
 SpCond_1 <- ggplot(data = ccrwater, aes(x = DateTime, y = EXOSpCond_uScm_1)) +
   geom_point()
 SpCond_1
#Only use plotly to find DateTime of questionable values
 #ggplotly(SpCond_1)
 
#Just the current year
 SpCond_1_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = EXOSpCond_uScm_1)) +
   geom_point()
 SpCond_1_21
#Only use plotly to find DateTime of questionable values
# ggplotly(SpCond_1_21)
 
 #Specific Conductivity from 9m EXO
 
 #Plot from 2021-current
 SpCond_9 <- ggplot(data = ccrwater, aes(x = DateTime, y = EXOSpCond_uScm_9)) +
   geom_point()
 SpCond_9
 #Only use plotly to find DateTime of questionable values
 #ggplotly(SpCond_9)
 
 #Just the current year
 SpCond_9_21=ccrwater%>%
   filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = EXOSpCond_uScm_9)) +
   geom_point()
 SpCond_9_21
 #Only use plotly to find DateTime of questionable values
 # ggplotly(SpCond_9_21)
 
###########################################################################################################
# #Total Dissolved Solids
#TDS for 1m EXO
 TDS_1 <- ggplot(data = ccrwater, aes(x = DateTime, y = EXOTDS_mgL_1)) +
   geom_point()
 TDS_1
#Only use plotly to find DateTime of questionable values
# ggplotly(TDS_1)
 
#Just the current year
 TDS_1_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = EXOTDS_mgL_1)) +
   geom_point()
 TDS_1_21
 #Only use plotly to find DateTime of questionable values
# ggplotly(TDS_1_21)
 
 #TDS for 9m EXO
 TDS_9 <- ggplot(data = ccrwater, aes(x = DateTime, y = EXOTDS_mgL_9)) +
   geom_point()
 TDS_9
 #Only use plotly to find DateTime of questionable values
 # ggplotly(TDS_9)
 
 #Just the current year
 TDS_9_21=ccrwater%>%
   filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = EXOTDS_mgL_9)) +
   geom_point()
 TDS_9_21
 #Only use plotly to find DateTime of questionable values
 # ggplotly(TDS_9_21)
 
 
############################################################################################################# 
#Depth
#Making sure I didn't miss any times when the EXO was out of place according to the depth sensor

#Depth from the 1m EXO
 Depth_1 <- ggplot(data = ccrwater, aes(x = DateTime, y = EXO_depth_m_1)) +
   geom_point()
 Depth_1
#Only use plotly to find DateTime of questionable values
# ggplotly(Depth_1)
 
#Just the current year
 Depth_1_21=ccrwater%>%
  filter(DateTime>current_time_start & DateTime<current_time_end)%>%
  ggplot(.,aes(x = DateTime, y = EXO_depth_m_1)) +
   geom_point()
 Depth_1_21
#Only use plotly to find DateTime of questionable values
# ggplotly(Depth_1_21)
 
 #Depth from the 9m EXO
 Depth_9 <- ggplot(data = ccrwater, aes(x = DateTime, y = EXO_depth_m_9)) +
   geom_point()
 Depth_9
 #Only use plotly to find DateTime of questionable values
 # ggplotly(Depth_9)
 
 #Just the current year
 Depth_9_21=ccrwater%>%
   filter(DateTime>current_time_start & DateTime<current_time_end)%>%
   ggplot(.,aes(x = DateTime, y = EXO_depth_m_9)) +
   geom_point()
 Depth_9_21
 #Only use plotly to find DateTime of questionable values
 # ggplotly(Depth_9_21)

###########################################################################################################################################################################
# write final csv ----
# some final checking of flags

#Order by date and time

ccrwater <- ccrwater[order(ccrwater$DateTime),]


# for(b in 1:nrow())
# str(ccrwater)

#rearrange the cols
 ccrwater <- ccrwater %>% select(Reservoir, Site, DateTime,  
                                 ThermistorTemp_C_1, ThermistorTemp_C_2, ThermistorTemp_C_3, ThermistorTemp_C_4,
                                 ThermistorTemp_C_5, ThermistorTemp_C_6, ThermistorTemp_C_7, ThermistorTemp_C_8,
                                 ThermistorTemp_C_9,ThermistorTemp_C_10,ThermistorTemp_C_11, ThermistorTemp_C_12,
                                 ThermistorTemp_C_13, EXOTemp_C_1, EXOCond_uScm_1,
                                 EXOSpCond_uScm_1, EXOTDS_mgL_1, EXODOsat_percent_1, EXODO_mgL_1, EXOChla_RFU_1,
                                 EXOChla_ugL_1, EXOBGAPC_RFU_1, EXOBGAPC_ugL_1, EXOfDOM_RFU_1, EXOfDOM_QSU_1,
                                 EXO_pressure_psi_1, EXO_depth_m_1, EXO_battery_V_1, EXO_cablepower_V_1, EXO_wiper_V_1,
                                 EXOTemp_C_9, EXOCond_uScm_9,
                                 EXOSpCond_uScm_9, EXOTDS_mgL_9, EXODOsat_percent_9, EXODO_mgL_9, 
                                 EXOfDOM_RFU_9, EXOfDOM_QSU_9,EXO_pressure_psi_9, EXO_depth_m_9, EXO_battery_V_9,
                                 EXO_cablepower_V_9, EXO_wiper_V_9,Lvl_psi_13,LvlDepth_m_13, LvlTemp_C_13, 
                                 RECORD, CR3000_Batt_V, CR3000Panel_Temp_C,everything())

# convert datetimes to characters so that they are properly formatted in the output file
ccrwater$DateTime <- as.character(ccrwater$DateTime)
  
  
write.csv(ccrwater, paste0(folder, 'CCR_Catwalk_EDI_2021.csv'), row.names = FALSE)


