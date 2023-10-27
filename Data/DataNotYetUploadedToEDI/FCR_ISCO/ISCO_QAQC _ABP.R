#### ISCO QAQC Script ####

### Purpose: read in ISCO water level files from Github, combine into single dataset for each year, 
### plot to visualize, and check for missing data
### Then, combine all years into a single dataframe and write to csv.

## Created by: Nick Hammond
## Last updated: 12 August 2022
## ABP edits

### NOTES: raw ISCO water level files are located on GitHub -> CareyLabVT -> Reservoirs -> Data -> DataNotYetUploadedToEDI -> FCR_ISCO

#### Load Packages ####

pacman::p_load("tidyverse","lubridate", "scattermore")

### compile large files from 2019-2021 and 2021-2022 ####

waterlevel <- dir(path = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/Data/WaterLevel", pattern = "_full_")%>%  
  map_df(~ read_csv(file.path(path = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/Data/WaterLevel", .),
                    col_names = c("DateTime", "WaterLevel_m"), col_types = cols(.default = "c"), skip = 6))

waterlevel2<-waterlevel%>%
  mutate(DateTime=mdy_hms(DateTime),
         WaterLevel_m=as.numeric(WaterLevel_m))%>%
  filter(DateTime>ymd_hms("2019-06-06 14:51:00"))%>%
  filter(WaterLevel_m<2)%>%
  mutate(Date = date(DateTime),
         Year=year(DateTime),
         DOY=yday(DateTime))%>%
  filter(DOY>50)%>%
  drop_na()

waterlevel3<-waterlevel2%>%
  select(DateTime, WaterLevel_m)
  
# Need to take out 2020-07-20 10:00:00 EST, 2020-08-24 14:49:00 EST for when the weir was blown out

# Ensure Dataset completeness (check for gaps in data)
# and add NAs for missing observations

DateTime = seq(from = min(waterlevel$DateTime,na.rm = T),to = max(waterlevel$DateTime,na.rm = T), by = "min")
df<-data.frame(DateTime) 

all<-merge(DateTime,waterlevel,by.x="DateTime",by.y="DateTime", all.x=TRUE)

# Identify dates with missing data

date_range = seq.Date(from = min(ISCO_19$Date,na.rm = T),to = max(ISCO_19$Date,na.rm = T), by = "day") #create vector of dates for entire range
missing=date_range[!date_range %in% ISCO_19$Date] # print out dates that are not present


# Plot to visualize
ISCO_19 %>% group_by(Date) %>% summarize(daily_flow = mean(WaterLevel_m)) %>% 
  ggplot() +
  geom_point(aes(x =Date, y = daily_flow))


ggplot(ISCO_19, aes(x=DOY))+
         geom_scattermore(aes(y=WaterLevel_m, color=as.factor(Year)), pointsize = 2)+
  facet_wrap(.~factor(Year))

ggplot(waterlevel, aes(x=DateTime,y=WaterLevel_m ))+
  geom_scattermore()

# Save as a csv
waterlevel2 %>% select(DateTime,WaterLevel_m) %>% 
  write_csv("2019_2022_ISCO_FCRWEIR_WaterLevel.csv")

#### Combine Sample Events #####

sample <- dir(path = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/Data/Sample_events", pattern = "")%>%  
  map_df(~ read_csv(file.path(path = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/Data/Sample_events", .),
                    col_names = c("DateTime", "Sample"), col_types = cols(.default = "c"), skip = 6))

sample2<-sample%>%
  mutate(DateTime=mdy_hms(DateTime),
         Sample=as.numeric(Sample))%>%
  filter(DateTime>ymd_hms("2019-06-06 14:51:00"))%>%
  drop_na()%>%
  mutate(Sample=ifelse(DateTime<ymd_hms("2022-06-15 10:00:00") & Sample==0, 100, Sample),
         Sample=ifelse(DateTime>ymd_hms("2022-06-15 10:00:00") & DateTime<ymd_hms("2022-07-05 10:00:00") & Sample==0, 50, Sample),
         Sample=ifelse(DateTime>ymd_hms("2022-07-05 10:00:00") & DateTime<ymd_hms("2022-09-26 14:05:00") & Sample==0, 100, Sample),
         Sample=ifelse(DateTime>ymd_hms("2022-09-26 14:09:00") & Sample==0, 200, Sample))

# add if statements when collection amount changed
# Changed to 50mL on 2022-06-15
# Changed back to 100mL on 2022-07-05
# Changed to 200 mL on  9/26/2022 2:09:44 PM

ggplot(sample2, aes(x=DateTime, y=Sample))+
  geom_point()

bn<-merge(waterlevel3, sample2, all=T)

# Start df at 2019-06-06 11:12:00


bv<-bn%>%
  filter(DateTime>ymd_hms("2022-05-20 00:00:00")& DateTime<ymd_hms("2022-06-10 00:00:00"))


ggplot()+
  geom_vline(data=sample2, aes(xintercept = DateTime), color="red")+
  geom_scattermore(data=waterlevel2, aes(x=DateTime, y=WaterLevel_m), pointsize = 1)

#### Read in times the ISCO was downloaded ####
down <- dir(path = "./Data/DataNotYetUploadedToEDI/FCR_ISCO", pattern = "ISCO_log")%>%  
  map_df(~ read.table(file.path(path = "./Data/DataNotYetUploadedToEDI/FCR_ISCO",.), sep = '\t',header = F))
      
      
colnames(down)<-c("DateTime","DB", "Notes","NA") 

Date<-down%>%
  select(DateTime)%>%
  unique()%>%
  filter(DateTime!="")%>%
  mutate(DateTime=mdy_hms(DateTime),
         Date=as.Date(DateTime),
  Download="Download")%>%
  filter(Date!="2021-02-12")%>% # These are times when the ISCO was not in the field
  filter(Date!="2021-03-19")

Date<-Date[!duplicated(Date$Date),]



# merge the data frame

all<- merge(bn,Date, all=T)

alls<-all[!duplicated(all$DateTime),]


