# Collate and QAQC manual Chla samples
# By A. Breef-Pilz
# 01 Feb 2023



#### Install packages ####
pacman::p_load(tidyverse, lubridate, magrittr, ggplot2, dplyr, readxl)

### Set up where data is ####

folder<- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFilteredChlorophyll"

### Set to current date ###
end_date <- "2023-01-01 00:00:00, tz=UTC"

#### Read in data files ###

# Read in compiled files from 2014-2021. This is the most raw files I can find

Old_14_21<- read_csv(paste0(folder, "/data/manual_chlorophyll_2014_2021.csv"))

#### Read in and compile raw data from 2022 onward #####

mydir = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFilteredChlorophyll/data/2022_data/chl_calculations/"
myfiles = list.files(path=mydir, patter=".xlsx", full.names=TRUE)#list the files from BVR platform


# take out files for scotty
 myfileschla <- myfiles[ !grepl("forScotty*", myfiles) ]#exclude the Temp test data

#create dataframe for the for loop
out.file<-""

#combine all of the files and make sure all times are in EST
for(k in 1:length(myfileschla)){
  files<-read_excel(myfileschla[k], sheet=1) #get header minus wonky Campbell rows
  
  # select the rows you want 
  fil<- files%>%
    select(`Sample ID`, 
           `absorbance before acidification...10`,
           `Chlorophyll a in extract (ug/L from Arar)`,
            `Pheopigment in extract (ug/L from Arar)`,
           `Chlorophyll a Conc of original water sample in ug/L (or mg/m3-same thing- APHA)`,
           `Pheopigment Conc of original water sample in ug/L (or mg/m3-same thing- APHA)`)%>%
    mutate(Flag_Chla_ugL=0,
           Flag_Pheo_ugL=0)
  
  # rename the columns
  names(fil)<-c("Sample_ID","Check_Absorb", "Check_chla", "Check_pheo", "Chla_ugL", "Pheo_ugL", "Flag_Chla_ugL", "Flag_Pheo_ugL")
  
  # only include the rows with complete observations
  # Select F for FCR, B for BVR, and C for CCR, S for SUNP
  fil<-fil%>%filter(grepl("^F|^B|^C|^S", Sample_ID)) #keep only the right TIMESTAMP rows
  
  # Now bind the rows together after they are all in EST
  out.file=rbind(out.file, fil)
}

# Now that files are stitched together let's make it usable

# Make the columns numbers nice
out.file[,-1] <- sapply(out.file[, -1], as.numeric)

chla_new<-out.file%>%
  filter(Sample_ID!="")%>%
  separate(., col = Sample_ID, into = c("Reservoir", "Date", "Depth_m", "Dup"), sep = "_")%>%
  separate(.,col = Reservoir, into = c("Reservoir", "Site"), sep = 1)%>%
  mutate(Reservoir=ifelse(Reservoir=="B","BVR", Reservoir),
         Reservoir=ifelse(Reservoir=="F","FCR", Reservoir),
         Reservoir=ifelse(Reservoir=="S","SNP", Reservoir))%>%
  # Add flags for low absorbance and pigment below detection
  mutate(Flag_Chla_ugL=ifelse(Check_Absorb<0.03,1, Flag_Chla_ugL),
         Flag_Pheo_ugL=ifelse(Check_Absorb<0.03,1, Flag_Pheo_ugL),
         Flag_Chla_ugL=ifelse(Check_chla<34,4,Flag_Chla_ugL),
         Flag_Pheo_ugL=ifelse(Check_pheo<34,4,Flag_Pheo_ugL))%>%
  # Average the dups
  group_by(Reservoir, Site, Date, Depth_m)%>%
  mutate(count = n())%>%
  mutate(Chla_ugL = mean(Chla_ugL)) %>%
  mutate(Pheo_ugL = mean(Pheo_ugL))%>%
  ungroup()%>%
  mutate(Flag_Chla_ugL=ifelse(Flag_Chla_ugL==0 & count==2,5,Flag_Chla_ugL),
         Flag_Chla_ugL=ifelse(Flag_Chla_ugL==4 & count==2,45,Flag_Chla_ugL), 
         Flag_Pheo_ugL=ifelse(Flag_Pheo_ugL==0 & count==2,5,Flag_Pheo_ugL),
         Flag_Pheo_ugL=ifelse(Flag_Pheo_ugL==4 & count==2,45,Flag_Pheo_ugL))%>%
  distinct(Reservoir, Site, Date, Depth_m, .keep_all = T)%>%
  # convert date and add time
    mutate(Date=format(strptime(Date, format = "%d%b%Y"), "%Y-%m-%d"))%>%
    mutate(Time="12:00:00")%>%
    mutate(DateTime=paste(Date,Time))%>%
    mutate(DateTime=ymd_hms(DateTime))%>%
    mutate(Depth_m=as.numeric(Depth_m))%>%
    mutate(Site=as.numeric(Site))%>%
    select(Reservoir,Site,DateTime, Depth_m, Chla_ugL,Pheo_ugL,Flag_Chla_ugL,Flag_Pheo_ugL)%>%
    mutate(Flag_Chla_ugL=ifelse(is.na(Chla_ugL), 2, Flag_Chla_ugL))%>% #Add a 2 flag if an observation is missing
    mutate(Flag_Pheo_ugL=ifelse(is.na(Pheo_ugL),2,Flag_Pheo_ugL))
  
### Let's bind 2014-2021 with the other ####
  
  # Clean up Old 2014_2021
  old<-Old_14_21%>%
    dplyr::rename(Flag_Chla_ugL=Flag_Chla,
                  Flag_Pheo_ugL=Flag_Pheo)%>%
    separate(., col = DateTime, into = c("Date", "Time"), sep = -8)%>%
    mutate(Time="12:00:00")%>% # had to fix because there were some midnight times
    mutate(DateTime=paste(Date,Time))%>%
    mutate(DateTime=ymd_hms(DateTime))%>%
    select(Reservoir,Site,DateTime, Depth_m, Chla_ugL,Pheo_ugL,Flag_Chla_ugL,Flag_Pheo_ugL)
  
  # bind the old and the new  
  all_chla<-rbind(old,chla_new)
  
  # put in order
  all_chla=all_chla[order(all_chla$DateTime),]
  
  all_chla <- all_chla[all_chla$DateTime<ymd_hms(end_date),]
  
### Save csv ####

  write.csv(all_chla, paste0(folder,"/2022/manual_chlorophyll_2014_2022.csv"), row.names=F, quote=F)

#### Graph ####
  # Facetted plot of the datapoints within each reservoir over the entire dataset 
  
  ggplot(subset(all_chla), aes(x = DateTime, y = Chla_ugL, col = Reservoir)) +
    geom_point(size = 1) + 
    facet_grid(Reservoir ~., scales = "free_y") + 
    ggtitle("Entire Dataset Timeseries")
  
  
  # Altering dataset in order to plot stats 
  chla_long_year <- all_chla %>% 
    ungroup(.) %>% 
    select(-(Flag_Chla_ugL)) %>% 
    gather(metric, value, Chla_ugL) %>% 
    mutate(year = year(DateTime)) %>% 
    mutate(month = month(DateTime))
  
  
  # Facetted plot of the range of each reservoir for each year and the mean of the range
  ggplot(subset(chla_long_year), aes(x = year, y = value, col = Reservoir))+ 
    geom_point(size = 1) + 
    stat_summary( fun.y = "mean", geom = "point", pch = 21, size = 3, fill = 'black') + 
    facet_grid(metric ~ Reservoir, scales = 'free_y') + 
    scale_x_continuous("DateTime", breaks = seq(2014, 2022, 1)) + 
    scale_y_continuous("Concentration (ugL)") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') + 
    ggtitle("Range and Mean of Range of Each Reservoir Over Time")
  
  
  # Facetted plot of the range of each reservoir for each year and the median of the range
  ggplot(subset(chla_long_year, Site == 50), aes(x = year, y = value, col = Reservoir))+ 
    geom_point(size = 1) + 
    stat_summary( fun.y = "median", geom = "point", pch = 21, size = 3, fill = 'black') + 
    facet_grid(metric ~ Reservoir, scales = 'free_y') + 
    scale_x_continuous("DateTime", breaks = seq(2014,2022, 1)) + 
    scale_y_continuous("Concentration (ugL)") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') + 
    ggtitle("Range and Median of Range of Each Reservoir Over Time")
  
  
  jet.colors <- c("#00007F", "#00007F", "blue", "blue", "#007FFF", "cyan", "#7FFF7F", "#7FFF7F",
                  "yellow","yellow", "#FF7F00", "#FF7F00", "red", "#7F0000")
  
  
  ggplot(subset(chla_long_year, Reservoir == 'FCR' & Site == 50), aes(x = month, y = value, col = as.factor(Depth_m))) + 
    geom_point(cex = 2) + 
    facet_grid(year ~., scales = 'free_y') +
    scale_x_continuous("Month") + 
    scale_y_continuous("Concentration (ugL)") + 
    ggtitle("FCR Timeseries")
  
  ggplot(subset(chla_long_year, Reservoir == 'BVR' & Site == 50), aes(x = month, y = value, col = as.factor(Depth_m))) + 
    geom_point(cex = 2) + 
    facet_grid(year ~., scales = 'free_y') +
    scale_x_continuous("Month") + 
    scale_y_continuous("Concentration (ugL)") + 
    ggtitle ("BVR Timeseries")  
  
  ggplot(subset(chla_long_year, Reservoir == 'CCR'), aes(x = month, y = value, col = as.factor(Depth_m))) + 
    geom_point(cex = 2) + 
    facet_grid(year ~., scales = 'free_y') +
    scale_x_continuous("Month") + 
    scale_y_continuous("Concentration (ugL)") + 
    ggtitle ("CCR Timeseries")  
  
  ggplot(subset(chla_long_year, Reservoir == 'SNP'), aes(x = month, y = value, col = as.factor(Depth_m))) + 
    geom_point(cex = 2) + 
    facet_grid(year ~.) +
    scale_x_continuous("Month") + 
    scale_y_continuous("Concentration (ugL)") + 
    ggtitle ("SNP Timeseries")  
  
  
#### Make Site Description csv ####
  
  # These lines of code make the csv of the site descriptions with lat and long

  #Install the required googlesheets4 package
  install.packages('googlesheets4')
  #Load the library 
  library(googlesheets4)
  sites <- read_sheet('https://docs.google.com/spreadsheets/d/1TlQRdjmi_lzwFfQ6Ovv1CAozmCEkHumDmbg_L4A2e-8/edit#gid=124442383')
  #data<- read_csv("YOUR DATA.csv")# Use this if you read in a csv
  data <- all_chla #This is the line you need to modify!
  trim_sites = function(data,sites){
    data_res_site=data%>% #Create a Reservoir/Site combo column
      mutate(res_site = trimws(paste0(Reservoir,Site)))
    sites_merged = sites%>% #Filter to Sites that are in the dataframe
      mutate(res_site = trimws(paste0(Reservoir,Site)))%>%
      filter(res_site%in%data_res_site$res_site)%>%
      select(-res_site)
  }
  sites_trimmed = trim_sites(data,sites) 
  write.csv(sites_trimmed,paste0(folder,"/2022/site_descriptions.csv"), row.names=F)# Write to file
  
  
  
