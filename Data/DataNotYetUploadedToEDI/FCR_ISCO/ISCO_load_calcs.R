#### ISCO Load Calculations ####
# Purpose:
# 1. Convert ISCO water level measurements to discharge (Q)
# 2. Two methods: a.) v-notch weir equation only, b.) v-notch weir equation + rectangular weir equation for water levels above the v-notch
      # Method b is still a rough approximation, since the flow over the weir likely does not conform to the same 
      # geometry as a rectangular weir, but this approximation may be more accurate than simply using the v-notch weir equation for all water levels
# 3. Compare ISCO discharge measurements to Carey Lab discharge measurements (from pressure transducer also at the weir)
# 4. Calculate loads of total Fe and Mn using the ISCO flow-weighted composite sampling and compare to loads calculated from weekly grab samples

### NOTES: This script is just working with data from 2019, but it can be adapted for subsequent years (once the ISCO metals samples have been analyzed)

### Created by Nick Hammond ###
## Last updated: 09 Aug 2022 ##


#clear environment
rm(list=ls(all=TRUE))
#load packages
library(lubridate)
library(dplyr)
library(magrittr)
library(readxl)
library(plyr)
library(ggplot2)
library(patchwork) # To display 2 charts together
library(hrbrthemes)


# set working directory to location of ISCO data
setwd("~/Reservoirs/Data/DataNotYetUploadedToEDI/FCR_ISCO")

#Load data
Discharge <- read.csv("190601_191201_ISCO_FCRWeir_Level.csv", skip=6)
colnames(Discharge) <- c("Date_Time","head")
Discharge <- as.data.frame(Discharge)

#make Head values numeric
Discharge=Discharge%>%
  mutate(head=as.numeric(head))


#Calculate Q from head

# V-notch weir equation used for all flow levels #      (120 deg V-notch weir, discharge equation: Q = 2.391 x H^2.5)
Discharge_v <- Discharge %>% mutate(Flow_cms = 2.391*(head^2.5)) %>% 
 select(Date_Time, head, Flow_cms)


# V-notch weir equation + rectangular weir equation for flow above 0.275 m #    (flow over-topped the weir at 27.5 cm)
B = 0.953 + 1.99 + 1.59 # channel width in meters (see 'WeirMeasurements.jpg')
Discharge_r <- Discharge %>% mutate(Flow_cms = ifelse(head > 0.275, 
2.391*(0.275^2.5) + (1.84*B*((head-0.275)^1.5)), 2.391*(head^2.5))) %>% 
select(Date_Time, head, Flow_cms)

# Plot to check
par(mfrow=c(1,2))
plot(Discharge_v$Date_Time,Discharge_v$Flow_cms) 
plot(Discharge_r$Date_Time,Discharge_r$Flow_cms) 
dev.off()

plot(Discharge_r$Date_Time,Discharge_r$Flow_cms)
points(Discharge_v$Date_Time,Discharge_v$Flow_cms,col="red")

# Read in Carey lab pressure transducer discharge data for comparison (from EDI)
Carey_Q = read.csv("inflow_for_EDI_2013_06Mar2020.csv")
Carey_Q2 = Carey_Q[,c("Reservoir", "Site", "DateTime", "WVWA_Flow_cms", "VT_Flow_cms")]
colnames(Carey_Q2) <- c("Reservoir", "Site", "Date_Time", "WVWA_Flow_cms", "VT_Flow_cms")
Carey_Q2$Date_Time = ymd_hms(Carey_Q2$Date_Time)

# Merge ISCO Q with Carey Q
All_Q_v = merge(Discharge_v,Carey_Q2, by = "Date_Time", all.x = TRUE)
All_Q_r = merge(Discharge_r,Carey_Q2, by = "Date_Time", all.x = TRUE)

# Plot ISCO Q vs. Carey Q

png('ISCO_Q_Carey_Q_comp.png', width = 15, height = 12, 
    units = 'in', res = 300)
par(mfrow=c(1,2), cex=2)
plot(All_Q_v$Flow_cms,All_Q_v$VT_Flow_cms,xlab = "ISCO Q (m3/s)",
     ylab = "CareyLab PT Q (m3/s)", main= "V-notch weir eqn only")
abline(h=0.09482248, lwd=2, lty="dashed") # level where head > 0.275 m
abline(v=0.09482248, lwd=2, lty="dashed") # level where head > 0.275 m
plot(All_Q_r$Flow_cms,All_Q_r$VT_Flow_cms, xlab = "ISCO Q (m3/s)", col="orange",
     ylab = "CareyLab PT Q (m3/s)", main= "V-notch weir eqn + rect weir eqn" )
abline(h=0.09482248, lwd=2, lty="dashed") # level where head > 0.275 m
abline(v=0.09482248, lwd=2, lty="dashed")
dev.off()

dev.off()
plot(All_Q_r$Date_Time,All_Q_r$Flow_cms, type="l")
points(All_Q_r$Date_Time,All_Q_r$VT_Flow_cms, col="red")

# Regression between ISCO Q and Carey lab Q
reg_isco = lm(All_Q_v$Flow_cms ~ All_Q_v$VT_Flow_cms)
summary(reg_isco) # R^2 = 0.8927 slope = 9.579e-01
reg_wvwwa = lm(All_Q_r$Flow_cms ~ All_Q_r$VT_Flow_cms)
summary(reg_wvwwa) # R^2 = 0.8602 slope = 9.695e-01

#Read in ISCO + Inflow Data
IN <- read_xlsx("INFLOW 2019.xlsx")
ISCO <- read_xlsx("ISCO 2019.xlsx")

# Separate by rep (for ISCO samples)
Rep_one <- ISCO[-which(ISCO$Rep==2),]
Rep_two <- ISCO[-which(ISCO$Rep==1),]

# Average two replicates
Rep_one$Avg_TFe <- (Rep_one$TFe_mgl + Rep_two$TFe_mgl)/2
Rep_one$Avg_TMn <- (Rep_one$TMn_mgl + Rep_two$TMn_mgl)/2
Rep_one[2,5] = 	3.434  # Fill in average for sample with only 1 rep
Rep_one[2,6] = 	0.662
metals = Rep_one # renamed because 'rep_one' seemed misleading
metals = metals[,-c(2,3,4)]

##### Load calculations w/ v-notch weir equation only #### 

#Subset Q based on sampling times
#int = data.frame(metals$Date)
#colnames(int) = c("interval")
time = data.frame(metals$Date, metals$Date)
colnames(time) = c("time1", "time2")
time[1,1] <- Discharge_v$Date_Time[1]
time[1,2] <- metals$Date[1]
time$int = interval(time$time1, time$time2)
#int[1,1] <- interval(time[1,1], time[1,2])
sample = list()
sample[[1]] = Discharge_v[Discharge_v$Date_Time %within% time[1,3],]

  for(i in 2:nrow(metals)){
  time[i,1] <- metals$Date[i-1]
  time[i,2] <- metals$Date[i]
  time[i,3] <- interval(time[i,1], time[i,2])
  sample[[i]] = Discharge_v[Discharge_v$Date_Time %within% time[i,3],]}

# Multiply each minute Q by 60 s to get a cumulative volume of flow per minute
for(i in 1:length(sample)){
  sample[[i]]$cum_flow = sample[[i]]$Flow_cms*60
}

# Sum up all cum_flow for each sampling period to get a total cumulative flow
for(i in 1:nrow(metals)){
  metals$cum_v[i] = sum(sample[[i]]$cum_flow)
}

#quick plot
par(mfrow=c(1,2))
plot(Discharge_v$Date_Time,Discharge_v$Flow_cms)
plot(metals$Date,metals$cum_v, col="red")
dev.off()

#convert m3 to L
metals$cum_v_L = metals$cum_v*1000

#cumulative volume (L) * metals concentration (mg/L) = load (mg)
metals$Fe_load = metals$Avg_TFe*metals$cum_v_L
metals$Mn_load = metals$Avg_TMn*metals$cum_v_L

#convert load to kg
metals$Fe_load_kg = metals$Fe_load/1000000
metals$Mn_load_kg = metals$Mn_load/1000000

#divide load (kg) by the number of sampling points per period (minutes)
#to get a load/min, then multiply by 1440 to get kg/day
for(i in 1:nrow(metals)){
metals$Fe_load_kg_d[i] = (metals$Fe_load_kg[i]/nrow(sample[[i]]))*1440
}
for(i in 1:nrow(metals)){
  metals$Mn_load_kg_d[i] = (metals$Mn_load_kg[i]/nrow(sample[[i]]))*1440
}

#Find periods where discharge data is missing
na = Discharge_v$head[is.na(Discharge_v$head)]

#Get rid of data from 2019-07-08 because flow data is missing
metals = metals[-3,]

par(mfrow=c(1,2))
plot(metals$cum_v_L,metals$Fe_load_kg_d)
plot(metals$Avg_TFe,metals$Fe_load_kg_d)

summary(lm(metals$cum_v_L ~ metals$Fe_load_kg)) # Adjusted R-squared = 0.5283
summary(lm(metals$Avg_TFe ~ metals$Fe_load_kg)) # Adjusted R-squared = 0.36

summary(lm(metals$cum_v ~ metals$Avg_TFe)) # Adjusted R-squared = -0.06224
# negative R-squared?!?


#removed first row which had a very high cumulative volume
metals_sansO = metals[-1,]
summary(lm(metals_sansO$cum_v ~ metals_sansO$Avg_TFe)) # Adjusted R-squared = 0.09534
# R-squared is still really bad haha


##### Load calculations w/ v-notch weir equation plus rect. weir equation #### 
# Create new metals df
metals_r = Rep_one 

#Subset Q based on sampling times
time_r = data.frame(metals_r$Date, metals_r$Date)
colnames(time_r) = c("time1", "time2")
time_r[1,1] <- Discharge_r$Date_Time[1]
time_r[1,2] <- metals_r$Date[1]
time_r$int = interval(time_r$time1, time_r$time2)
sample_r = list()
sample_r[[1]] = Discharge_r[Discharge_r$Date_Time %within% time_r[1,3],]

for(i in 2:nrow(metals_r)){
  time_r[i,1] <- metals_r$Date[i-1]
  time_r[i,2] <- metals_r$Date[i]
  time_r[i,3] <- interval(time_r[i,1], time_r[i,2])
  sample_r[[i]] = Discharge_r[Discharge_r$Date_Time %within% time_r[i,3],]}

# Multiply each minute Q by 60 s to get a cumulative volume of flow per minute
for(i in 1:length(sample_r)){
  sample_r[[i]]$cum_flow = sample_r[[i]]$Flow_cms*60
}

# Sum up all cum_flow for each sampling period to get a total cumulative flow
for(i in 1:nrow(metals_r)){
  metals_r$cum_v[i] = sum(sample_r[[i]]$cum_flow)
}

#quick plot
par(mfrow=c(1,2))
plot(Discharge_r$Date_Time,Discharge_r$Flow_cms)
plot(metals_r$Date,metals_r$cum_v, col="red")

#convert m3 to L
metals_r$cum_v_L = metals_r$cum_v*1000

#cumulative volume (L) * metals concentration (mg/L) = load (mg)
metals_r$Fe_load = metals_r$Avg_TFe*metals_r$cum_v_L
metals_r$Mn_load = metals_r$Avg_TMn*metals_r$cum_v_L

#convert load to kg
metals_r$Fe_load_kg = metals_r$Fe_load/1000000
metals_r$Mn_load_kg = metals_r$Mn_load/1000000

#divide load (kg) by the number of sampling points per period (minutes)
#to get a load/min, then multiply by 1440 to get kg/day
for(i in 1:nrow(metals_r)){
  metals_r$Fe_load_kg_d[i] = (metals_r$Fe_load_kg[i]/nrow(sample_r[[i]]))*1440
}
for(i in 1:nrow(metals_r)){
  metals_r$Mn_load_kg_d[i] = (metals_r$Mn_load_kg[i]/nrow(sample_r[[i]]))*1440
}

#Get rid of data from 2019-07-08 because flow data is missing
metals_r = metals_r[-3,]

#Find periods where discharge data is missing
na_r = Discharge_r$head[is.na(Discharge_r$head)]

par(mfrow=c(1,2))
plot(metals_r$cum_v_L,metals_r$Fe_load_kg_d)
plot(metals_r$Avg_TFe,metals_r$Fe_load_kg_d)

#Linear regressions
summary(lm(metals_r$cum_v_L ~ metals_r$Fe_load_kg)) # Adjusted R-squared = 0.5284
summary(lm(metals_r$Avg_TFe ~ metals_r$Fe_load_kg)) # Adjusted R-squred = 0.3635

summary(lm(metals_r$cum_v ~ metals_r$Avg_TFe)) # Adjusted R-squared = -0.06196 
# negative R-squared?!?

#removed first row which had a very high cumulative volume
metals_r_sansO = metals_r[-1,]
summary(lm(metals_r_sansO$cum_v ~ metals_r_sansO$Avg_TFe)) # Adjusted R-squared = 0.09635
# R-squared is still really bad haha

# Plot to compare loads for different Q calculation methods
dev.off()
plot(metals_r$Date,metals_r$Fe_load_kg_d, col="red")
points(metals$Date,metals$Fe_load_kg_d, col="black")

# Calculate difference between Fe loads for the two methods
metals_r$diff_Fe = metals_r$Fe_load_kg - metals$Fe_load_kg
metals_r$diff_V = metals_r$cum_v_L - metals$cum_v_L


#### Load calculations using INFLOW grab sampling data ####

#Create new dataframe with INFLOW samples that match the dates of ISCO samples
ISCO_dates = metals[,1]
IN_match = merge(IN,ISCO_dates, by="Date",all.y = TRUE)

#Add in cumulative volumes that were previously calculated (no need to reinvent the wheel)
IN_match$cum_v_v = metals$cum_v
IN_match$cum_v_L_v = metals$cum_v_L
IN_match$cum_v_r = metals_r$cum_v
IN_match$cum_v_L_r = metals_r$cum_v_L

#cumulative volume (L) * metals concentration (mg/L) = load (mg)
IN_match$Fe_load_v = IN_match$TFe_mgl*IN_match$cum_v_L_v
IN_match$Fe_load_r = IN_match$TFe_mgl*IN_match$cum_v_L_r
IN_match$Mn_load_v = IN_match$TMn_mgl*IN_match$cum_v_L_v
IN_match$Mn_load_r = IN_match$TMn_mgl*IN_match$cum_v_L_r

#convert load to kg
IN_match$Fe_load_kg_v = IN_match$Fe_load_v/1000000
IN_match$Fe_load_kg_r = IN_match$Fe_load_r/1000000
IN_match$Mn_load_kg_v = IN_match$Mn_load_v/1000000
IN_match$Mn_load_kg_r = IN_match$Mn_load_r/1000000

#divide load (kg) by the number of sampling points per period (minutes)
#to get a load/min, then multiply by 1440 to get kg/day
for(i in 1:nrow(IN_match)){
  IN_match$Fe_load_kg_d_v[i] = (IN_match$Fe_load_kg_v[i]/nrow(sample[[i]]))*1440
}
for(i in 1:nrow(IN_match)){
  IN_match$Fe_load_kg_d_r[i] = (IN_match$Fe_load_kg_r[i]/nrow(sample[[i]]))*1440
}
for(i in 1:nrow(IN_match)){
  IN_match$Mn_load_kg_d_v[i] = (IN_match$Mn_load_kg_v[i]/nrow(sample[[i]]))*1440
}
for(i in 1:nrow(IN_match)){
  IN_match$Mn_load_kg_d_r[i] = (IN_match$Mn_load_kg_r[i]/nrow(sample[[i]]))*1440
}

#Get rid of data from 2019-07-08 because flow data is missing
#IN_match = IN_match[-3,] #Since I merged with the metals dataset, this has
#already been done

#Find periods where discharge data is missing
#na_r = Discharge_r$head[is.na(Discharge_r$head)]

par(mfrow=c(1,2))
plot(IN_match$cum_v_L_v,IN_match$Fe_load_kg_d_v)
plot(IN_match$TFe_mgl,IN_match$Fe_load_kg_d_v)

#Linear regressions
summary(lm(IN_match$cum_v_L_v ~ IN_match$Fe_load_kg_v)) # Adjusted R-squared = 0.1446
summary(lm(IN_match$cum_v_L_r ~ IN_match$Fe_load_kg_r)) # Adjusted R-squared = 0.1446
summary(lm(IN_match$TFe_mgl ~ IN_match$Fe_load_kg_v)) # Adjusted R-squred = 0.7209

summary(lm(IN_match$cum_v_v ~ IN_match$TFe_mgl)) # Adjusted R-squared = -0.06196 
# negative R-squared?!?

#removed first row which had a very high cumulative volume
#IN_match_sansO = IN_match[-1,]
#summary(lm(IN_match_sansO$cum_v ~ IN_match_sansO$Avg_TFe)) # Adjusted R-squared = 0.09635
# R-squared is still really bad haha

# Plot to compare loads for different Q calculation methods
#dev.off()
#plot(IN_match$Date,IN_match$Fe_load_kg_d, col="red")
#points(metals$Date,metals$Fe_load_kg_d, col="black")

# Calculate difference between Fe loads for the two methods
#IN_match$diff_Fe = IN_match$Fe_load_kg - metals$Fe_load_kg
#IN_match$diff_V = IN_match$cum_v_L - metals$cum_v_L



#### Make some plots! ####



#Plot Discharge for both methods of calculation
png('ISCO_Q_calc_comparison.png', width = 15, height = 12, units = 'in', res = 300)
par(mar = c(5,5,2,5))
plot(Discharge_r$Date_Time , Discharge_r$Flow_cms , type="p", pch = 20, col="orange", cex=2, cex.axis=2, cex.lab=2.5,
     ylab= "Discharge (m3/s)", xlab = "Date")
lines(Discharge_v$Date_Time , Discharge_v$Flow_cms , type="p", pch = 20, col="black", cex=2, cex.axis=2, cex.lab=2.5,
       ylab= "Discharge (m3/s)", xlab = "Date")
abline(h=0.09482248, lwd=2) # level where head > 0.275 m
legend("topright",
       legend=c("V-notch + rect. weir eqn's","V-notch weir eqn only"),
       pch=c(20,20), cex= 2.5, col=c("orange", "black"))
dev.off()

#Plot cumulative volume for each sampling period
png('ISCO_cumulative_vol.png', width = 15, height = 12, units = 'in', res = 300)
par(mar = c(5,5,2,5))
plot(metals_r$Date,metals_r$cum_v, type="p", pch = 19, col="black", cex=3, cex.axis=2, cex.lab=2.5,
     ylab= "Cumulative Volume (m3)", xlab = "Date")
points(metals$Date,metals$cum_v, type="p", pch = 19, col="red", cex=3, cex.axis=2, cex.lab=2.5,
      ylab= "Cumulative Volume (m3)", xlab = "Date")
legend("topright",
       legend=c("V-notch + rect. weir eqn's","V-notch weir eqn only"),
       pch=c(19,19), col=c("black", "red"), cex= 3)
dev.off()

#Plot Fe/Mn loads in kg/d
png('ISCO_Fe_load_kg_d.png', width = 15, height = 12, units = 'in', res = 300)
par(mar = c(5,5,2,5))
plot(metals_r$Date_Time,metals_r$Fe_load_kg_d, type="p", pch = 19, col="orange", cex=3, cex.axis=2, cex.lab=2.5,
     ylab= "Fe Load (kg/d)", xlab = "Date")
points(metals$Date_Time,metals$Fe_load_kg_d, type="p", pch = 19, col="black", cex=3, cex.axis=2, cex.lab=2.5,
       ylab= "Fe Load (kg/d)", xlab = "Date")
legend("topright",
       legend=c("V-notch + rect. weir eqn's","V-notch weir eqn only"),
       pch=c(19,19), col=c("orange","black"), cex= 3)
dev.off()

png('ISCO_Mn_load_kg_d.png', width = 15, height = 12, units = 'in', res = 300)
par(mar = c(5,5,2,5))
plot(metals_r$Date,metals_r$Mn_load_kg_d, type="p", pch = 19, col="black", cex=3, cex.axis=2, cex.lab=2.5,
     ylab= "Mn Load (kg/d)", xlab = "Date")
points(metals$Date,metals$Mn_load_kg_d, type="p", pch = 19, col="red", cex=3, cex.axis=2, cex.lab=2.5,
       ylab= "Mn Load (kg/d)", xlab = "Date")
legend("topright",
       legend=c("V-notch + rect. weir eqn's","V-notch weir eqn only"),
       pch=c(19,19), col=c("black", "red"), cex= 3)
dev.off()


# Dates are not lining up when plotting... need to fix!!!!
# It appears that the dates for the load data are stretched to fit the entire plot window

#Plot Minute Data - Fe
png('Fe_loads_ISCO_2018.png', width = 15, height = 12, units = 'in', res = 300)
par(mar = c(5,5,2,5))
plot(Discharge_v$Date_Time , Discharge_v$Flow_cms , type="l", lty = 1, col="black", lwd=2, cex.axis=2, cex.lab=2.5,
     ylab= "Discharge (m3/s)", xlab = "Date",
     ylim=c(0,0.3))
par(new = T)
plot(metals$Date, metals$Fe_load_kg, pch=16, axes=F, xlab=NA, ylab=NA, cex=1.6, col= "magenta")
#points(IN$Date, IN$TFe_mgl , pch=16, axes=F, xlab=NA, ylab=NA, cex=1.6, col = "blue")
axis(side = 4, cex.axis=2, cex.lab=2.5 )
mtext(side = 4, line = 3, 'Fe Load (kg)', cex = 2.5)
legend("topright",
       legend=c("Discharge (m3/s)", "ISCO load (kg Fe)"),
       lty=c(1,0,0), lwd= c(2,0,0), pch=c(NA, 16,16), col=c("black", "magenta", "blue"), cex= 2)
dev.off()


###Plot Discharge Data and Load Data together - v-notch only method ###

#merge discharge_v and metals_v and _IN_match_v
colnames(metals)[1] = c("Date_Time")
colnames(IN_match)[1] = c("Date_Time")
IN_metals = merge(IN_match,metals, by="Date_Time", all.x = TRUE)
Q_load = merge(Discharge_v, IN_metals, by="Date_Time", all.x = TRUE)
# Value used to transform the data
coeff <- 39

# A few constants
QColor <- "#69b3a2"
loadColor <- rgb(0.2, 0.6, 0.9, 1)

png('ISCO_Q_plus_Fe.png', width = 15, height = 12, units = 'in', res = 300)
ggplot(Q_load, aes(x=Date_Time)) +
  
  geom_point( aes(y=Flow_cms), size=2, color=QColor) + 
  geom_point( aes(y=Fe_load_kg_d / coeff), size=5, color=loadColor) +
  geom_point( aes(y=Fe_load_kg_d_v / coeff), size=5, shape=18, color=loadColor) +
  
  xlab("Date") +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Discharge (m3/s)",
  
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Fe Load (kg/day)")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.text.x = element_text(size= 22),
    axis.text.y.left = element_text(size= 22),
    axis.text.y.right = element_text(size= 22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = QColor, size=25),
    axis.title.y.right = element_text(color = loadColor, size=25)
  ) 
  
  #ggtitle(" ISCO Discharge and Fe Loads 2018")

dev.off()

###Plot Discharge Data and Load Data together - 
### v-notch plus rect. method ###

#merge discharge_r and metals_r and _IN_match_r
colnames(metals_r)[1] = c("Date_Time")
colnames(IN_match)[1] = c("Date_Time")
IN_metals_r = merge(IN_match,metals_r, by="Date_Time", all.x = TRUE)
Q_load_r = merge(Discharge_r, IN_metals_r, by="Date_Time", all.x = TRUE)
# Value used to transform the data
coeff_r <- 21.6336

# A few constants
QColor <- "#69b3a2"
loadColor <- rgb(0.2, 0.6, 0.9, 1)

png('ISCO_Q_plus_Fe_r.png', width = 15, height = 12, units = 'in', res = 300)
ggplot(Q_load_r, aes(x=Date_Time)) +
  
  geom_point( aes(y=Flow_cms), size=2, color=QColor) + 
  geom_point( aes(y=Fe_load_kg_d / coeff_r), size=5, color=loadColor) +
  geom_point( aes(y=Fe_load_kg_d_v / coeff_r), size=5, shape=18, color=loadColor) +
  
  xlab("Date") +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Discharge (m3/s)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff_r, name="Fe Load (kg/day)")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.text.x = element_text(size= 22),
    axis.text.y.left = element_text(size= 22),
    axis.text.y.right = element_text(size= 22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = QColor, size=25),
    axis.title.y.right = element_text(color = loadColor, size=25)
  ) 

#ggtitle(" ISCO Discharge and Fe Loads 2018")

dev.off()

#### Plot the difference in calculated load (based on method) over discharge
# Value used to transform the data
coeff_r <- 21.6336

# A few constants
QColor <- "#69b3a2"
loadColor <- rgb(0.2, 0.6, 0.9, 1)

png('ISCO_Q_plus_Fe_r.png', width = 15, height = 12, units = 'in', res = 300)
ggplot(Q_load_r, aes(x=Date_Time)) +
  
  geom_point( aes(y=Flow_cms), size=2, color=QColor) + 
  geom_point( aes(y=Fe_load_kg_d / coeff_r), size=5, color=loadColor) +
  geom_point( aes(y=Fe_load_kg_d_v / coeff_r), size=5, shape=18, color=loadColor) +
  
  xlab("Date") +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Discharge (m3/s)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff_r, name="Fe Load (kg/day)")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.text.x = element_text(size= 22),
    axis.text.y.left = element_text(size= 22),
    axis.text.y.right = element_text(size= 22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = QColor, size=25),
    axis.title.y.right = element_text(color = loadColor, size=25)
  ) 

#ggtitle(" ISCO Discharge and Fe Loads 2018")

dev.off()



#### Old code for averaging Q ####

#Calculate Daily Mean Flows
daily_mean = ddply(Discharge, .(Date_Time), summarise, mean_flow = mean(Flow_cms))

#Multiply each Q by 60 s to get a Q/min
Discharge <- Discharge %>% mutate(MinuteQ = Flow_cms*60) %>% 
  select(Date_Time, head, Flow_cms, MinuteQ)

#Sum up Cumulative Q for each day
DailyQ = ddply(Discharge, .(Date_Time), summarise, DailyQ = sum(MinuteQ))

#creat column for 'weeks'
DailyQ$Week <- week(DailyQ$Date_Time)

#Delete incomplete weeks
DailyQ <- DailyQ[-c(1:5),]
DailyQ <- DailyQ[-c(156,157),]

#Sum up cumulative Q for each week
WeeklyQ = ddply(DailyQ, .(Week), summarise, WeeklyQ = sum(DailyQ))
WeeklyQ$Week_new <- 86400*seq(1,23*7,7)
WeeklyQ$Date <- date(as.POSIXct(WeeklyQ$Week_new, origin = "2019-06-17"))
