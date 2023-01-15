###QA/QC script for FCR meteorological data Environmental Data Initiative publishing
###Script written by Bethany Bookout & Cayelan Carey & Adrienne Breef-Pilz
###Last modified 20 Jan 2021 to publish 2015-2020 met data to EDI
###Contact info: Cayelan Carey, Virginia Tech, cayelan@vt.edu

###1) Install and load packages needed

pacman::p_load("RCurl","tidyverse","lubridate", "plotly", "magrittr", "suncalc")

#rm(list=ls()) #let's start with a blank slate

folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCRMetData/2022/"

# Create a misc_data_files folder if one doesn't already exist

misc_folder <-paste0(folder, "misc_data_files")

if (file.exists(misc_folder)) {
  cat("The folder already exists")
} else {
  dir.create(misc_folder)
}

###2) Download the "raw" meteorological FCR datasets from GitHub and aggregate into 1 file: 
# Set the timeout option to 100 seconds instead of 60
options(timeout=10000)
#a. Past Met data, manual downloads
#download current met data from GitHub
download.file("https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/ccre-dam-data/ccre-met.csv", paste0(folder, "misc_data_files/ccre-met.csv"))

#download maintenance file. 
download.file("https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/ccre-dam-data/CCRM_Maintenancelog.txt", paste0(folder, "misc_data_files/CCR_Met_Maintenance_2021_2022.txt"))


# download legacy files
download.file("https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/ccre-dam-data/ccre-met_legacy_2021.csv", paste0(folder, "misc_data_files/rawccre-met_2021.csv"))
#original raw files from 



#merge previous metdata

 mydir = "Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCRMetData/2022/misc_data_files"
 myfiles = list.files(path=mydir, pattern=".csv", full.names=TRUE)#list the files from BVR platform
# 
# 
# #create dataframe for the for loop
out.file<-""
 
# #combine all of the files into one data sheet, have to come back and fix this loop
 for(k in 1:length(myfiles)){
   files<-read.csv(myfiles[k],skip= 4, header=F) #get header minus wonky Campbell rows
   if(length(names(files))==17){
     names(files) = c("DateTime","Record", "CR3000_Batt_V", "CR3000Panel_temp_C", 
                      "PAR_Average_umol_s_m2", "PAR_Total_mmol_m2", "BP_Average_kPa", "AirTemp_Average_C", 
                    "RH_percent", "Rain_Total_mm", "WindSpeed_Average_m_s", "WindDir_degrees", "ShortwaveRadiationUp_W_m2_Average",
                    "ShortwaveRadiationDown_W_m2_Average", "InfraredRadiationUp_Average_W_m2",
                     "InfraredRadiationDown_Average_W_m2", "Albedo_Average_W_m2")#rename headers
  }
  if(length(names(files))>17){ #removes NR01TK_Avg column, which was downloaded on some but not all days
    files$V17<-NULL #remove column
    names(files) = c("DateTime","Record", "CR3000_Batt_V", "CR3000Panel_temp_C", 
                     "PAR_Average_umol_s_m2", "PAR_Total_mmol_m2", "BP_Average_kPa", "AirTemp_Average_C", 
                     "RH_percent", "Rain_Total_mm", "WindSpeed_Average_m_s", "WindDir_degrees", "ShortwaveRadiationUp_W_m2_Average",                      "ShortwaveRadiationDown_W_m2_Average", "InfraredRadiationUp_Average_W_m2",
                      "InfraredRadiationDown_Average_W_m2", "Albedo_Average_W_m2")#rename headers
   }
   out.file=rbind(out.file, files)
 }
 
 out.file=out.file%>%
   filter(Record!="")%>%
   filter(!is.na(DateTime))
   
 

#put datetime in a useable form
out.file$DateTime<-as.POSIXct(strptime(out.file$DateTime, "%Y-%m-%d %H:%M"), tz = "EST")


####Add current file from Github ####
# Met_now=read.csv(paste0(folder, "ccre-met.csv"), skip = 4, header = F) 
# #loads in data from SCC_data repository for latest push
# if(length(names(Met_now))>17){ #removes NR01TK_Avg column, which was downloaded on some but not all days
#   Met_now$V17<-NULL #remove extra column
# }
# 
# #Because using the finalized version of past data must QAQC the current the metdata to add flags and then add the past file
# #renames and reformats dataset for easy bind
# names(Met_now) = c("DateTime","Record", "CR3000_Batt_V", "CR3000Panel_temp_C", 
#                    "PAR_Average_umol_s_m2", "PAR_Total_mmol_m2", "BP_Average_kPa", "AirTemp_Average_C", 
#                    "RH_percent", "Rain_Total_mm", "WindSpeed_Average_m_s", "WindDir_degrees", "ShortwaveRadiationUp_W_m2_Average",
#                    "ShortwaveRadiationDown_W_m2_Average", "InfraredRadiationUp_Average_W_m2",
#                    "InfraredRadiationDown_Average_W_m2", "Albedo_Average_W_m2")
# 
# 

####3) Aggregate data set for QA/QC ####
#add the past and now together
 #Met=rbind(out.file,Met_now)
Met=out.file

# take out duplicates
Met=Met%>%distinct()

#take out the data when still setting up and testing the station
# added an hour because having issues getting 19:00:00

Met=Met%>%filter(DateTime>"2021-03-29 19:59:00")

#Set end date to years working with 
Met=Met%>%filter(DateTime<"2023-01-01 00:00:00")


#change the columns from as.character to as.numeric after the merge
Met[, c(2:17)] <- sapply(Met[, c(2:17)], as.numeric)

Met$DateTime<-as.POSIXct(Met$DateTime, "%Y-%m-%d %H:%M", tz = "EST")

######################################################################
#Check for record gaps and day gpas

#order data by timestamp
Met=Met[order(Met$DateTime),]
Met$DOY=yday(Met$DateTime)


#check record for gaps
#daily record gaps by day of year
for(i in 2:nrow(Met)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(Met$DOY[i]-Met$DOY[i-1]>1){
    print(c(Met$DateTime[i-1],Met$DateTime[i]))
  }
}
# #sub-daily record gaps by record number
for(i in 2:length(Met$Record)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(abs(Met$Record[i]-Met$Record[i-1])>1){
    print(c(Met$DateTime[i-1],Met$DateTime[i]))
  }
}


#EDI Column names
names(Met) = c("DateTime","Record", "CR3000Battery_V", "CR3000Panel_Temp_C", 
               "PAR_umolm2s_Average", "PAR_Total_mmol_m2", "BP_Average_kPa", "AirTemp_C_Average", 
               "RH_percent", "Rain_Total_mm", "WindSpeed_Average_m_s", "WindDir_degrees", "ShortwaveRadiationUp_Average_W_m2",
               "ShortwaveRadiationDown_Average_W_m2", "InfraredRadiationUp_Average_W_m2",
               "InfraredRadiationDown_Average_W_m2", "Albedo_Average_W_m2", "DOY") #finalized column names
Met$Reservoir="CCR" #add reservoir name for EDI archiving
Met$Site=51 #add site column for EDI archiving




Met_raw=Met #Met=Met_raw; reset your data, compare QAQC
Met=Met_raw

####4) Load in maintenance txt file #### 
# the maintenance file tracks when sensors were repaired or offline due to maintenance
RemoveMet=read.csv(paste0(folder, "misc_data_files/CCR_Met_Maintenance_2021_2022.txt"), header = T)
#str(RemoveMet)
RemoveMet$TIMESTAMP_start=ymd_hms(RemoveMet$TIMESTAMP_start, tz="EST")#setting time zone
RemoveMet$TIMESTAMP_end=ymd_hms(RemoveMet$TIMESTAMP_end, tz="EST") #setting time zone
RemoveMet$notes=as.character(RemoveMet$notes)

####5) Create data flags for publishing ####
#get rid of NaNs
#create flag + notes columns for data columns c(5:17)
#set flag 2
for(i in 5:17) { #for loop to create new columns in data frame
  Met[,paste0("Flag_",colnames(Met[i]))] <- 0 #creates flag column + name of variable
  Met[,paste0("Note_",colnames(Met[i]))] <- NA #creates note column + names of variable
  Met[which(is.nan(Met[,i])),i] <- NA
  Met[c(which(is.na(Met[,i]))),paste0("Flag_",colnames(Met[i]))] <-2 #puts in flag 2
  Met[c(which(is.na(Met[,i]))),paste0("Note_",colnames(Met[i]))] <- "Sample not collected" #note for flag 2
}

#create loop putting in maintenance flags 1 + 4 (these are flags for values removed due
# to maintenance and also flags potentially questionable values)
for(j in 1:nrow(RemoveMet)){
  #print(j) # #if statement to only write in flag 4 if there are no other flags
  if(RemoveMet$flag[j]==4){
    print(j) # #if statement to only write in flag 4 if there are no other flags
    Met[c(which(Met[,1]>=RemoveMet[j,2] & Met[,1]<=RemoveMet[j,3] & (Met[,paste0("Flag_",colnames(Met[RemoveMet$colnumber[j]]))]==0))), paste0("Note_",colnames(Met[RemoveMet$colnumber[j]]))]=RemoveMet$notes[j]#same as above, but for notes
    Met[c(which(Met[,1]>=RemoveMet[j,2] & Met[,1]<=RemoveMet[j,3] & (Met[,paste0("Flag_",colnames(Met[RemoveMet$colnumber[j]]))]==0))), paste0("Flag_",colnames(Met[RemoveMet$colnumber[j]]))]=RemoveMet$flag[j]#when met timestamp is between remove timestamp
    #print(j)#and met column derived from remove column
    #matching time frame, inserting flag
    Met[Met[,1]>=RemoveMet[j,2] & Met[,1]<=RemoveMet[j,3], RemoveMet$colnumber[j]] = NA
  }
  #if flag == 1, set parameter to NA, overwrites any other flag
  
  if(RemoveMet$flag[j]==1){
    print(j)
    Met[c(which((Met[,1]>=RemoveMet[j,2]) & (Met[,1]<=RemoveMet[j,3]))),paste0("Flag_",colnames(Met[RemoveMet$colnumber[j]]))] = RemoveMet$flag[j] #when met timestamp is between remove timestamp
    #and met column derived from remove column
    #matching time frame, inserting flag
    Met[Met[,1]>=RemoveMet[j,2] & Met[,1]<=RemoveMet[j,3], paste0("Note_",colnames(Met[RemoveMet$colnumber[j]]))]=RemoveMet$notes[j]#same as above, but for notes
    
    Met[Met[,1]>=RemoveMet[j,2] & Met[,1]<=RemoveMet[j,3], RemoveMet$colnumber[j]] = NA
  } #replaces value of var with NA
  
  if(RemoveMet$flag[j]==5){
    print(j)
    Met[c(which((Met[,1]>=RemoveMet[j,2]) & (Met[,1]<=RemoveMet[j,3]))),paste0("Flag_",colnames(Met[RemoveMet$colnumber[j]]))] = RemoveMet$flag[j] #when met timestamp is between remove timestamp
    #and met column derived from remove column
    #matching time frame, inserting flag
    Met[Met[,1]>=RemoveMet[j,2] & Met[,1]<=RemoveMet[j,3], paste0("Note_",colnames(Met[RemoveMet$colnumber[j]]))]=RemoveMet$notes[j]#same as above, but for notes
  }
}


# # #Filter for just flags
# Flags=Met%>%
#   select(starts_with("Flag"))
# for(b in 1:nrow(Flags)){
#   print(colnames(Flags[b]))
#   print(table(Flags[,b],useNA="always"))
# }


#No pre and post filter but run all through the QAQC 
MetAir_2021=Met[Met$DateTime<"2021-12-31 19:00:00",c(1,4,8)]
lm_Panel2021=lm(MetAir_2021$AirTemp_C_Average ~ MetAir_2021$CR3000Panel_Temp_C)
summary(lm_Panel2021)#gives data on linear model parameters


# Replace missing values with the panel temp and linear equation
 Met=Met%>%
   mutate(
     AirTemp_C_Average=ifelse(Flag_AirTemp_C_Average==2,(-3.5604+(0.9289*CR3000Panel_Temp_C)), AirTemp_C_Average),
     Note_AirTemp_C_Average=ifelse(Flag_AirTemp_C_Average==2,"Substituted_value_calculated_from_Panel_Temp_and_linear_model", Note_AirTemp_C_Average),
     Flag_AirTemp_C_Average=ifelse(Flag_AirTemp_C_Average==2, 4, Flag_AirTemp_C_Average))
 
 
Met=Met%>%
  mutate(
    Note_AirTemp_C_Average=ifelse(abs(AirTemp_C_Average - (-3.5604+(0.9289*CR3000Panel_Temp_C))>(4*sd(lm_Panel2021$residuals))) & !is.na(AirTemp_C_Average),"Substituted_value_calculated_from_Panel_Temp_and_linear_model", Note_AirTemp_C_Average),
    Flag_AirTemp_C_Average=ifelse(abs(AirTemp_C_Average - (-3.5604+(0.9289*CR3000Panel_Temp_C))>(4*sd(lm_Panel2021$residuals))) & !is.na(AirTemp_C_Average), 4, Flag_AirTemp_C_Average),
    AirTemp_C_Average=ifelse(abs(AirTemp_C_Average - (-3.5604+(0.9289*CR3000Panel_Temp_C))>(4*sd(lm_Panel2021$residuals))) & !is.na(AirTemp_C_Average),(-3.5604+(0.9289*CR3000Panel_Temp_C)), AirTemp_C_Average))


#put all airtemp through the panel QAQC check
#plot(Met_raw$DateTime, Met_raw$AirTemp_C_Average, type= 'l')
#points(Met$DateTime, Met$AirTemp_C_Average, type = "l", col="red")

#Air temp maximum set
Met=Met%>%
  mutate(
    Flag_AirTemp_C_Average=ifelse(AirTemp_C_Average>40.6 & !is.na(AirTemp_C_Average),4,Flag_AirTemp_C_Average),
    Note_AirTemp_C_Average=ifelse(AirTemp_C_Average>40.6 & !is.na(AirTemp_C_Average),"Outlier_set_to_NA",Note_AirTemp_C_Average),
    AirTemp_C_Average=ifelse(AirTemp_C_Average>40.6 & !is.na(AirTemp_C_Average),NA,AirTemp_C_Average))


#Infared radiation cleaning

#Mean correction for InfRadDown (needs to be after voltage correction)
#Using 2018 data, taking the mean and sd of values on DOY to correct to

#Need to figure out what to do about IR
# the equation is 5.67*10^-8*(IR$AirTemp_C_Average+273.15)^4 which is pretty close to the 
# IRdown value up not the IR up. It is all based on Temperature. 

#look at IRdown and IR up equations

# IR=Met%>%
#   select(DateTime,CR3000Panel_Temp_C,AirTemp_C_Average,ShortwaveRadiationUp_Average_W_m2, ShortwaveRadiationDown_Average_W_m2,
#          InfraredRadiationUp_Average_W_m2, InfraredRadiationDown_Average_W_m2)
# 
# IR$IRdn_calc=IR$ShortwaveRadiationDown_Average_W_m2+(5.67*10^-8*(IR$AirTemp_C_Average+273.15)^4)
# IR$IRup_calc=IR$ShortwaveRadiationUp_Average_W_m2+ (5.67*10^-8*(IR$AirTemp_C_Average+273.15)^4)
# Met$DOY=yday(Met$DateTime)
# 
# 
# Met_infrad=Met[year(Met$DateTime)<2022,]
# Met_infrad$infradavg=ave(Met_infrad$InfraredRadiationDown_Average_W_m2, Met_infrad$DOY) #creating column with mean of infraddown by day of year
# Met_infrad$infradsd=ave(Met_infrad$InfraredRadiationDown_Average_W_m2, Met_infrad$DOY, FUN = sd) #creating column with sd of infraddown by day of year
# Met_infrad=unique(Met_infrad[,c(18,47,48)])
# 
# Met=merge(Met, Met_infrad, by = "DOY") #putting in columns for infrared mean and sd by DOY into main data set
# Met=Met[order(Met$DateTime),] #ordering table after merging and removing unnecessary columns





#If the IR Down is greater than 3SD by DOY and replace with the equation from the manual(5.67*10-8(AirTemp_C_Average+273.15)^4)
# Met=Met%>%
#   mutate(
#     Flag_InfraredRadiationDown_Average_W_m2=ifelse((abs(InfraredRadiationDown_Average_W_m2-infradavg)>(2*infradsd)) & !is.na(InfraredRadiationDown_Average_W_m2),4,Flag_InfraredRadiationDown_Average_W_m2),
#     Note_InfraredRadiationDown_Average_W_m2=ifelse((abs(InfraredRadiationDown_Average_W_m2-infradavg)>(2*infradsd)) & !is.na(InfraredRadiationDown_Average_W_m2),"Greater_than_2SD_Value_corrected_with_InfRadDn_equation_as_described_in_metadata",Note_InfraredRadiationDown_Average_W_m2),
#     InfraredRadiationDown_Average_W_m2=ifelse((abs(InfraredRadiationDown_Average_W_m2-infradavg)>(2*infradsd)) & !is.na(InfraredRadiationDown_Average_W_m2),5.67*10^-8*(AirTemp_C_Average+273.15)^4,InfraredRadiationDown_Average_W_m2))
    

# Met=Met[,-c(1,47,48)]


#Inf outliers, must go after corrections
Met=Met%>%
  mutate(
    Flag_InfraredRadiationUp_Average_W_m2=ifelse(InfraredRadiationUp_Average_W_m2<150 & !is.na(InfraredRadiationUp_Average_W_m2) ,4,Flag_InfraredRadiationUp_Average_W_m2),
    Note_InfraredRadiationUp_Average_W_m2=ifelse(InfraredRadiationUp_Average_W_m2<150 & !is.na(InfraredRadiationUp_Average_W_m2),"Outlier_set_to_NA",Note_InfraredRadiationUp_Average_W_m2),
    InfraredRadiationUp_Average_W_m2=ifelse(InfraredRadiationUp_Average_W_m2<150 & !is.na(InfraredRadiationUp_Average_W_m2),NA,InfraredRadiationUp_Average_W_m2))

Met=Met%>%
  mutate(
    Flag_InfraredRadiationDown_Average_W_m2=ifelse(InfraredRadiationDown_Average_W_m2>540 & !is.na(InfraredRadiationDown_Average_W_m2)|InfraredRadiationDown_Average_W_m2<200 & !is.na(InfraredRadiationDown_Average_W_m2),4,Flag_InfraredRadiationDown_Average_W_m2),
    Note_InfraredRadiationDown_Average_W_m2=ifelse(InfraredRadiationDown_Average_W_m2>540 & !is.na(InfraredRadiationDown_Average_W_m2)|InfraredRadiationDown_Average_W_m2<200 & !is.na(InfraredRadiationDown_Average_W_m2),"outlier_set_to_NA_due_to_probable_sensor_failure",Note_InfraredRadiationDown_Average_W_m2),
    InfraredRadiationDown_Average_W_m2=ifelse(InfraredRadiationDown_Average_W_m2>540  & !is.na(InfraredRadiationDown_Average_W_m2) |InfraredRadiationDown_Average_W_m2<200 & !is.na(InfraredRadiationDown_Average_W_m2),NA,InfraredRadiationDown_Average_W_m2)
  )

#Replace missing values with estimations from the equation-add back in later
#Need to check the IR up equation of InfraredRadiationUp_Average_W_m2+5.67*10^-8*(AirTemp_C_Average+273.15)^4
# Met=Met%>%
#   mutate( 
#     InfraredRadiationDown_Average_W_m2=ifelse(Flag_InfraredRadiationDown_Average_W_m2==2,5.67*10^-8*(AirTemp_C_Average+273.15)^4,InfraredRadiationDown_Average_W_m2),
#     Note_InfraredRadiationDown_Average_W_m2=ifelse(Flag_InfraredRadiationDown_Average_W_m2==2,"Value_corrected_with_InfRadDn_equation_as_described_in_metadata",Note_InfraredRadiationDown_Average_W_m2),
#     Flag_InfraredRadiationDown_Average_W_m2=ifelse(Flag_InfraredRadiationDown_Average_W_m2==2,4,Flag_InfraredRadiationDown_Average_W_m2),
#     Note_InfraredRadiationUp_Average_W_m2=ifelse(Flag_InfraredRadiationUp_Average_W_m2==2,"Value_corrected_with_InfRadUp_equation_as_described_in_metadata",Note_InfraredRadiationUp_Average_W_m2),
#     InfraredRadiationUp_Average_W_m2=ifelse(Flag_InfraredRadiationUp_Average_W_m2==2,InfraredRadiationUp_Average_W_m2+5.67*10^-8*(AirTemp_C_Average+273.15)^4,InfraredRadiationUp_Average_W_m2),
#     Flag_InfraredRadiationUp_Average_W_m2=ifelse(Flag_InfraredRadiationUp_Average_W_m2==2,4,Flag_InfraredRadiationUp_Average_W_m2))

##### Rain Guage####
# The rain guage was clogged from 2022-07-19 to 2022-09-12 11:04 when it was unclogged. 
# Used the rain totals from FCR(2021-03-29 19:00-2022-07-18 00:00:00) to estimate rain totals during this time. 
# ccr=0.110725(fcr)+ 0.001494

# Read in the FCR met file 
#inUrl1  <- "https://pasta-s.lternet.edu/package/data/eml/edi/143/8/a5524c686e2154ec0fd0459d46a7d1eb" 
#infile1 <- paste0(getwd(),"/Data/Met_final_2015_2022.csv")
#download.file(inUrl1,infile1,method="curl")


##fc <- read_csv("./Data/Met_final_2015_2022.csv", header=T) %>%
fc <-read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_FCRMetData/2022/FCR_Met_final_2015_2022.csv")%>%  
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d %H:%M:%S", tz="EST"))) %>% 
  filter(DateTime>as.POSIXct("2021-03-29 00:00:00"))%>%
  select(DateTime,Rain_Total_mm)

Met<-merge(Met, fc, by="DateTime")

Met=Met%>%
  mutate(
    Note_Rain_Total_mm=ifelse(DateTime>"2022-07-19 00:00:00" & DateTime<"2022-09-12 12:09:00","Substituted value calculated from FCR rain guage and linear_model", Note_Rain_Total_mm),
    Flag_Rain_Total_mm=ifelse(DateTime>"2022-07-19 00:00:00" & DateTime<"2022-09-12 12:09:00", 4, Flag_Rain_Total_mm),
    Rain_Total_mm.x=ifelse(DateTime>"2022-07-19 00:00:00" & DateTime<"2022-09-12 12:09:00",(0.001494+(0.110725*Rain_Total_mm.y)), Rain_Total_mm.x))

# Change the values that should be 0 but are 0.001494 because of the linear model

# bf<-Met%>%
#   filter(DateTime>"2022-05-01 00:00:00"&DateTime<"2022-10-01 00:00:00")%>%
#   ggplot(., aes(x=DateTime))+
#   geom_point(aes(y=Rain_Total_mm.x),color="blue")+
#   geom_line(aes(y=Rain_Total_mm.y),color="red")

Met$Rain_Total_mm.x=ifelse(Met$Rain_Total_mm.x==0.001494,0,Met$Rain_Total_mm.x)

#Take out FCR rain 
Met$Rain_Total_mm.y<-NULL

Met<-Met%>%dplyr::rename("Rain_Total_mm"="Rain_Total_mm.x")

# Fine estimated total rain over that time and compare to actual 

a<-Met_raw%>%
  filter(DateTime>"2022-07-19 00:00:00" & DateTime<"2022-09-12 12:09:00")%>%
  select(DateTime, Rain_Total_mm)

sum(a$Rain_Total_mm)
  

# Take out Rain Totals above 5mm in 1 minute
Met=Met%>%
  mutate(
    Note_Rain_Total_mm=ifelse(Rain_Total_mm>5,"Rain total over 5 mm in one minute so removed as outlier", Note_Rain_Total_mm),
    Flag_Rain_Total_mm=ifelse(Rain_Total_mm>5, 4, Flag_Rain_Total_mm),
    Rain_Total_mm=ifelse(Rain_Total_mm>5,NA, Rain_Total_mm))



### Take Out Outliers###
#take out impossible outliers and Infinite values before the other outliers are removed
#set flag 3 (see metadata: this corrects for impossible outliers)
for(i in 5:17) { #for loop to create new columns in data frame
  Met[c(which(is.infinite(Met[,i]))),paste0("Flag_",colnames(Met[i]))] <-3 #puts in flag 3
  Met[c(which(is.infinite(Met[,i]))),paste0("Note_",colnames(Met[i]))] <- "Infinite_value_set_to_NA" #note for flag 3
  Met[c(which(is.infinite(Met[,i]))),i] <- NA #set infinite vals to NA
  
  if(i!=8) { #flag 3 for negative values for everything except air temp
    Met[c(which((Met[,i]<0))),paste0("Flag_",colnames(Met[i]))] <- 3
    Met[c(which((Met[,i]<0))),paste0("Note_",colnames(Met[i]))] <- "Negative_value_set_to_0"
    Met[c(which((Met[,i]<0))),i] <- 0 #replaces value with 0
  }
  if(i==9) { #flag for RH over 100
    Met[c(which((Met[,i]>100))),paste0("Flag_",colnames(Met[i]))] <- 3
    Met[c(which((Met[,i]>100))),paste0("Note_",colnames(Met[i]))] <- "Value_set_to_100"
    Met[c(which((Met[,i]>100))),i] <- 100 #replaces value with 100
  }
}

###Wind Outliers####
# Take out if wind direction is not between 0 and 360 and if wind speed is above 50 m/s

Met=Met%>%
  mutate(
    Note_WindDir_degrees=ifelse(WindDir_degrees<0 | WindDir_degrees>360 & !is.na(WindDir_degrees),"Wind direction is below 0 or above 360 degrees", Note_WindDir_degrees),
    Flag_WindDir_degrees=ifelse(WindDir_degrees<0 | WindDir_degrees>360 & !is.na(WindDir_degrees), 4, Flag_WindDir_degrees),
    WindDir_degrees=ifelse(WindDir_degrees<0 | WindDir_degrees>360 & !is.na(WindDir_degrees),NA, WindDir_degrees))

Met=Met%>%
  mutate(
    Note_WindSpeed_Average_m_s=ifelse(WindSpeed_Average_m_s>50 & !is.na(WindSpeed_Average_m_s),"Wind Speed is above 50 m/s", Note_WindSpeed_Average_m_s),
    Flag_WindSpeed_Average_m_s=ifelse(WindSpeed_Average_m_s>50 & !is.na(WindSpeed_Average_m_s), 4, Flag_WindSpeed_Average_m_s),
    WindSpeed_Average_m_s=ifelse(WindSpeed_Average_m_s>50 & !is.na(WindSpeed_Average_m_s),NA, WindSpeed_Average_m_s))


# #Filter for just flags
# Flags=Met%>%
#   select(starts_with("Flag"))
# for(b in 1:nrow(Flags)){
#   print(colnames(Flags[b]))
#   print(table(Flags[,b],useNA="always"))
# }



#full data set QAQC


###BP outliers####

#Remove barometric pressure outliers. Checked weather Underground and lines up well. Might have to adjust next year
Met=Met%>%
  mutate(
    Flag_BP_Average_kPa=ifelse(BP_Average_kPa<95.5 & !is.na(BP_Average_kPa), 4,Flag_BP_Average_kPa),
    Note_BP_Average_kPa=ifelse(BP_Average_kPa<95.5 & !is.na(BP_Average_kPa),"Outlier_set_to_NA",Note_BP_Average_kPa),
    BP_Average_kPa=ifelse(BP_Average_kPa<95.5 & !is.na(BP_Average_kPa),NA,BP_Average_kPa))

### Take out Night high PAR values####
#remove high PAR values at night
#get sunrise and sunset times
suntimes=getSunlightTimes(date = seq.Date(as.Date("2021-03-29"), Sys.Date(), by = 1),
                          keep = c("sunrise",  "sunset"),
                          lat = 37.37, lon = -79.96, tz = "EST")

#create date column
Met$date <- as.Date(Met$DateTime)


#create subset to join


#now merge the datasets to get daylight time
Met <- left_join(Met, suntimes, by = "date") %>%
  mutate(daylight_intvl = interval(sunrise, sunset)) %>%
  mutate(during_day = DateTime %within% daylight_intvl)

#check that times line up
PAR=Met%>%
  select(DateTime, sunrise, sunset,daylight_intvl,during_day)

#Remove PAR Tot
Met=Met%>%
  mutate(
    Flag_PAR_mmol_m2_Average=ifelse(during_day==FALSE & PAR_mmol_m2_Average>1 & !is.na(PAR_mmol_m2_Average) , 4, Flag_PAR_mmol_m2_Average),
    Note_PAR_mmol_m2_Average=ifelse(during_day==FALSE & PAR_mmol_m2_Average>1 & !is.na(PAR_mmol_m2_Average), "Outlier_set_to_NA", Note_PAR_mmol_m2_Average),
    PAR_mmol_m2_Average=ifelse(during_day==FALSE & PAR_mmol_m2_Average>1 & !is.na(PAR_mmol_m2_Average), NA, PAR_mmol_m2_Average))

Met=Met%>%
  mutate(
    Flag_PAR_umolm2s_Average=ifelse(during_day==FALSE & PAR_umolm2s_Average> 12 & !is.na(PAR_umolm2s_Average), 4, Flag_PAR_umolm2s_Average),
    Note_PAR_umolm2s_Average=ifelse(during_day==FALSE & PAR_umolm2s_Average>12 & !is.na(PAR_umolm2s_Average), "Outlier_set_to_NA", Note_PAR_umolm2s_Average),
    PAR_umolm2s_Average=ifelse(during_day==FALSE & PAR_umolm2s_Average>12 & !is.na(PAR_umolm2s_Average), NA, PAR_umolm2s_Average))

#Remove total PAR (PAR_Tot) outliers
Met=Met%>%
  mutate(
    Flag_PAR_mmol_m2_Average=ifelse(PAR_mmol_m2_Average>200& !is.na(PAR_mmol_m2_Average), 4, Flag_PAR_mmol_m2_Average),
    Flag_PAR_umolm2s_Average=ifelse(PAR_umolm2s_Average>3000 & !is.na(PAR_umolm2s_Average), 4, Flag_PAR_umolm2s_Average),
    Note_PAR_mmol_m2_Average=ifelse(PAR_mmol_m2_Average>200 & !is.na(PAR_mmol_m2_Average), "Outlier_set_to_NA", Note_PAR_mmol_m2_Average),
    Note_PAR_umolm2s_Average=ifelse(PAR_umolm2s_Average>3000 & !is.na(PAR_umolm2s_Average), "Outlier_set_to_NA", Note_PAR_umolm2s_Average),
    PAR_mmol_m2_Average=ifelse(PAR_mmol_m2_Average>200 & !is.na(PAR_mmol_m2_Average), NA, PAR_mmol_m2_Average),
    PAR_umolm2s_Average=ifelse(PAR_umolm2s_Average>3000 & !is.na(PAR_umolm2s_Average), NA, PAR_umolm2s_Average))



#####Remove shortwave radiation outliers########
#first shortwave upwelling
Met=Met%>%
  mutate(
    Flag_ShortwaveRadiationUp_Average_W_m2=ifelse(ShortwaveRadiationUp_Average_W_m2>1500 & !is.na(ShortwaveRadiationUp_Average_W_m2), 4, Flag_ShortwaveRadiationUp_Average_W_m2),
    Note_ShortwaveRadiationUp_Average_W_m2=ifelse(ShortwaveRadiationUp_Average_W_m2>1500 & !is.na(ShortwaveRadiationUp_Average_W_m2), "Outlier_set_to_NA", Note_ShortwaveRadiationUp_Average_W_m2),
    ShortwaveRadiationUp_Average_W_m2=ifelse(ShortwaveRadiationUp_Average_W_m2>1500 & !is.na(ShortwaveRadiationUp_Average_W_m2), NA, ShortwaveRadiationUp_Average_W_m2))


#and then shortwave downwelling (what goes up must come down)
Met=Met%>%
  mutate(
    Flag_ShortwaveRadiationDown_Average_W_m2=ifelse(ShortwaveRadiationDown_Average_W_m2>300 & !is.na(ShortwaveRadiationDown_Average_W_m2), 4, Flag_ShortwaveRadiationDown_Average_W_m2),
    Note_ShortwaveRadiationDown_Average_W_m2=ifelse(ShortwaveRadiationDown_Average_W_m2>300 & !is.na(ShortwaveRadiationDown_Average_W_m2), "Outlier_set_to_NA", Note_ShortwaveRadiationDown_Average_W_m2),
    ShortwaveRadiationDown_Average_W_m2=ifelse(ShortwaveRadiationDown_Average_W_m2>300 & !is.na(ShortwaveRadiationDown_Average_W_m2), NA, ShortwaveRadiationDown_Average_W_m2))

# High questionable values when no one was at the reservoir on April 12- April 15th

Met=Met%>%
  mutate(
    Flag_ShortwaveRadiationDown_Average_W_m2=ifelse(DateTime>"2021-04-12 00:00" & DateTime<"2021-04-15 23:59" & ShortwaveRadiationDown_Average_W_m2>80 & !is.na(ShortwaveRadiationDown_Average_W_m2), 5, Flag_ShortwaveRadiationDown_Average_W_m2),
    Note_ShortwaveRadiationDown_Average_W_m2=ifelse(DateTime>"2021-04-12 00:00" & DateTime<"2021-04-15 23:59" & ShortwaveRadiationDown_Average_W_m2>80 & !is.na(ShortwaveRadiationDown_Average_W_m2), "Questionable_value_left_in_dataset", Note_ShortwaveRadiationDown_Average_W_m2))


####Remove albedo outliers####
#over 1000
Met=Met%>%
  mutate(
    Flag_Albedo_Average_W_m2=ifelse(Albedo_Average_W_m2>1000& !is.na(Albedo_Average_W_m2), 4, Flag_Albedo_Average_W_m2),
    Note_Albedo_Average_W_m2=ifelse(Albedo_Average_W_m2>1000& !is.na(Albedo_Average_W_m2), "Outlier_set_to_NA", Note_Albedo_Average_W_m2),
    Albedo_Average_W_m2=ifelse(Albedo_Average_W_m2>1000& !is.na(Albedo_Average_W_m2), NA, Albedo_Average_W_m2))

#set to NA when shortwave radiation up is equal to NA
Met=Met%>%
  mutate(
    Flag_Albedo_Average_W_m2=ifelse(is.na(ShortwaveRadiationUp_Average_W_m2)|is.na(ShortwaveRadiationDown_Average_W_m2), 4, Flag_Albedo_Average_W_m2),
    Note_Albedo_Average_W_m2=ifelse(is.na(ShortwaveRadiationUp_Average_W_m2)|is.na(ShortwaveRadiationDown_Average_W_m2), "Set_to_NA_because_Shortwave_equals_NA", Note_Albedo_Average_W_m2),
    Albedo_Average_W_m2=ifelse(is.na(ShortwaveRadiationUp_Average_W_m2)|is.na(ShortwaveRadiationDown_Average_W_m2), NA, Albedo_Average_W_m2))

###Reorder the coulmns####
#Reorder so the flags are all next to each other
Met=Met%>%
  select(-c(date, lat, lon, sunrise, sunset,daylight_intvl,during_day))%>%
  select(c("Reservoir","Site","DateTime","Record","CR3000Battery_V","CR3000Panel_Temp_C","PAR_umolm2s_Average","PAR_mmol_m2_Average","BP_Average_kPa",                          
           "AirTemp_C_Average","RH_percent","Rain_Total_mm","WindSpeed_Average_m_s","WindDir_degrees","ShortwaveRadiationUp_Average_W_m2",       
           "ShortwaveRadiationDown_Average_W_m2","InfraredRadiationUp_Average_W_m2","InfraredRadiationDown_Average_W_m2","Albedo_Average_W_m2",
           "Flag_PAR_umolm2s_Average","Flag_PAR_mmol_m2_Average","Flag_BP_Average_kPa","Flag_AirTemp_C_Average","Flag_Rain_Total_mm","Flag_RH_percent",
           "Flag_WindSpeed_Average_m_s","Flag_WindDir_degrees","Flag_ShortwaveRadiationUp_Average_W_m2","Flag_ShortwaveRadiationDown_Average_W_m2",
           "Flag_InfraredRadiationUp_Average_W_m2","Flag_InfraredRadiationDown_Average_W_m2","Flag_Albedo_Average_W_m2","Note_PAR_umolm2s_Average",              
           "Note_PAR_mmol_m2_Average","Note_BP_Average_kPa","Note_AirTemp_C_Average","Note_RH_percent","Note_Rain_Total_mm","Note_WindSpeed_Average_m_s",              
           "Note_WindDir_degrees","Note_ShortwaveRadiationUp_Average_W_m2","Note_ShortwaveRadiationDown_Average_W_m2","Note_InfraredRadiationUp_Average_W_m2",       
           "Note_InfraredRadiationDown_Average_W_m2","Note_Albedo_Average_W_m2"))

###Check out the flag frequency####
#prints table of flag frequency and make sure no NAs in Flag column

for(b in 20:32){
  print(colnames(Met[b]))
  print(table(Met[,b],useNA="always"))
}


####6) Make plots to view data #####
#plots to check for any wonkiness
x11(); par(mfrow=c(2,2))
plot(Met$DateTime, Met$CR3000BBattery_V, type = 'l')
plot(Met$DateTime, Met$CR3000Panel_Temp_C, type = 'l')
#Albedo
plot(Met_raw$DateTime, Met_raw$Albedo_Average_W_m2, col="red", type='l')
plot(Met$DateTime, Met$Albedo_Average_W_m2, type = 'l')
#PAR
plot(Met_raw$DateTime, Met_raw$PAR_umolm2s_Average, col="red", type='l')
plot(Met$DateTime, Met$PAR_umolm2s_Average, type = 'l')
plot(Met_raw$DateTime, Met_raw$PAR_mmol_m2_Average, col="red", type='l')
plot(Met$DateTime, Met$PAR_mmol_m2_Average, type = 'l')
#Air Temp
plot(Met_raw$DateTime, Met_raw$AirTemp_C_Average, col="red", type='l')
points(Met$DateTime, Met$AirTemp_C_Average, type = 'l')
#RH
plot(Met_raw$DateTime, Met_raw$RH_percent, col="red", type='l')
points(Met$DateTime, Met$RH_percent, type = 'l')
#BP
plot(Met_raw$DateTime, Met_raw$BP_Average_kPa, col="red", type='l')
plot(Met$DateTime, Met$BP_Average_kPa, type = 'l')
#Rain
plot(Met_raw$DateTime, Met_raw$Rain_Total_mm, col="red", type='h')
plot(Met$DateTime, Met$Rain_Total_mm, type = 'h')
#Wind
plot(Met$DateTime, Met$WindSpeed_Average_m_s, type = 'l')
hist(Met$WindDir_degrees)

#SW Radiation
plot(Met_raw$DateTime, Met_raw$ShortwaveRadiationUp_Average_W_m2, col="red", type='l')
plot(Met$DateTime, Met$ShortwaveRadiationUp_Average_W_m2, type = 'l')
plot(Met_raw$DateTime, Met_raw$ShortwaveRadiationDown_Average_W_m2, col="red", type='l')
plot(Met$DateTime, Met$ShortwaveRadiationDown_Average_W_m2, type = 'l')

#InfRad
plot(Met_raw$DateTime, Met_raw$InfraredRadiationUp_Average_W_m2, col="red", type='l')
plot(Met$DateTime, Met$InfraredRadiationUp_Average_W_m2, type = 'l')
plot(Met_raw$DateTime, Met_raw$InfraredRadiationDown_Average_W_m2, col="red", type='l')
plot(Met$DateTime, Met$InfraredRadiationDown_Average_W_m2, type = 'l')


#####Just for the current year#####
Twenty_raw=Met_raw%>%
  filter(DateTime>"2021-12-31 23:59"& DateTime<"2023-01-01 00:00")

Twenty=Met%>%
  filter(DateTime>"2021-12-31 23:59"& DateTime<"2023-01-01 00:00")

x11(); par(mfrow=c(2,2))
plot(Twenty$DateTime, Twenty$CR3000Battery_V, type = 'l')
plot(Twenty$DateTime, Twenty$CR3000Panel_Temp_C, type = 'l')
#PAR
plot(Twenty_raw$DateTime, Twenty_raw$PAR_umolm2s_Average, col="red", type='l')
plot(Twenty$DateTime, Twenty$PAR_umolm2s_Average, type = 'l')
plot(Twenty_raw$DateTime, Twenty_raw$PAR_mmol_m2_Average, col="red", type='l')
plot(Twenty$DateTime, Twenty$PAR_mmol_m2_Average, type = 'l')
#BP
plot(Twenty_raw$DateTime, Twenty_raw$BP_Average_kPa, col="red", type='l')
plot(Twenty$DateTime, Twenty$BP_Average_kPa, type = 'l')
#Air Temp
plot(Twenty_raw$DateTime, Twenty_raw$AirTemp_C_Average, col="red", type='l')
plot(Twenty$DateTime, Twenty$AirTemp_C_Average, type = 'l')
#RH
plot(Twenty_raw$DateTime, Twenty_raw$RH_percent, col="red", type='l')
points(Twenty$DateTime, Twenty$RH_percent, type = 'l')
#Rain
plot(Twenty_raw$DateTime, Twenty_raw$Rain_Total_mm, col="red", type='h')
plot(Twenty$DateTime, Twenty$Rain_Total_mm, type = 'h')
#Wind
plot(Twenty$DateTime, Twenty$WindSpeed_Average_m_s, type = 'l')
hist(Twenty$WindDir_degrees)
#SW Radiation
plot(Twenty_raw$DateTime, Twenty_raw$ShortwaveRadiationUp_Average_W_m2, col="red", type='l')
plot(Twenty$DateTime, Twenty$ShortwaveRadiationUp_Average_W_m2, type = 'l')
plot(Twenty_raw$DateTime, Twenty_raw$ShortwaveRadiationDown_Average_W_m2, col="red", type='l')
plot(Twenty$DateTime, Twenty$ShortwaveRadiationDown_Average_W_m2, type = 'l')
#Albedo
plot(Twenty_raw$DateTime, Twenty_raw$Albedo_Average_W_m2, col="red", type='l')
plot(Twenty$DateTime, Twenty$Albedo_Average_W_m2, type = 'l')
#InfRad
plot(Twenty_raw$DateTime, Twenty_raw$InfraredRadiationUp_Average_W_m2, col="red", type='l')
plot(Twenty$DateTime, Twenty$InfraredRadiationUp_Average_W_m2, type = 'l')
plot(Twenty_raw$DateTime, Twenty_raw$InfraredRadiationDown_Average_W_m2, col="red", type='l')
plot(Twenty$DateTime, Twenty$InfraredRadiationDown_Average_W_m2, type = 'p')



#Met unique values for notes
for (u in 33:45) {
  print(colnames(Met[u]))
  print(unique(Met[,u]))
}



###7) Write file with final cleaned dataset! ###
Met_final=Met%>%
  select(c("Reservoir","Site","DateTime","Record","CR3000Battery_V","CR3000Panel_Temp_C","PAR_umolm2s_Average","PAR_mmol_m2_Average","BP_Average_kPa",                          
           "AirTemp_C_Average","RH_percent","Rain_Total_mm","WindSpeed_Average_m_s","WindDir_degrees","ShortwaveRadiationUp_Average_W_m2",       
           "ShortwaveRadiationDown_Average_W_m2","InfraredRadiationUp_Average_W_m2","InfraredRadiationDown_Average_W_m2","Albedo_Average_W_m2",
           "Flag_PAR_umolm2s_Average","Note_PAR_umolm2s_Average","Flag_PAR_mmol_m2_Average","Note_PAR_mmol_m2_Average","Flag_BP_Average_kPa",
           "Note_BP_Average_kPa","Flag_AirTemp_C_Average","Note_AirTemp_C_Average","Flag_RH_percent","Note_RH_percent","Flag_Rain_Total_mm",
           "Note_Rain_Total_mm","Flag_WindSpeed_Average_m_s","Note_WindSpeed_Average_m_s",
           "Flag_WindDir_degrees","Note_WindDir_degrees","Flag_ShortwaveRadiationUp_Average_W_m2","Note_ShortwaveRadiationUp_Average_W_m2",
           "Flag_ShortwaveRadiationDown_Average_W_m2","Note_ShortwaveRadiationDown_Average_W_m2",
           "Flag_InfraredRadiationUp_Average_W_m2","Note_InfraredRadiationUp_Average_W_m2",
           "Flag_InfraredRadiationDown_Average_W_m2","Note_InfraredRadiationDown_Average_W_m2","Flag_Albedo_Average_W_m2","Note_Albedo_Average_W_m2"))

###8) Make the maintenance data table and save as csv#####

###Prep RemoveMet for final file version
RemoveMet_Final=RemoveMet
names(RemoveMet_Final) = c("Station", "DateTime_start","DateTime_end", "Parameter", "ColumnNumber", "Flag", "Notes") #finalized column names
RemoveMet_Final$Reservoir= "CCR"#add reservoir name for EDI archiving
RemoveMet_Final$Site=51 #add site column for EDI archiving

RemoveMet_Final=RemoveMet_Final[,c(8:9,1:7)]

# reorder columns 
RemoveMet_Final<-RemoveMet%>%
  select(Reservoir, Site,DataStream,TIMESTAMP_start, TIMESTAMP_end, parameter, colnumber, flag, notes)

#setwd('./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCRMetData/2021')
write.csv(Met_final, paste0(folder,"CCR_Met_final_2021_2022.csv"), row.names=F, quote=F)
write.csv(RemoveMet_Final, paste0(folder,"CCR_Met_MaintenanceLog_2021_2022.csv"), row.names=F, quote=F)




