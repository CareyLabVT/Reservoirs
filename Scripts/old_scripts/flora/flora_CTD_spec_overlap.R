##Title: CTD/FP/Chl-a overlap
#Author: Mary Lofton
#Date: 20NOV18

#load packages
install.packages("pacman")
pacman::p_load(tidyverse, lubridate)
source("C:/Users/Mary Lofton/Documents/RScripts/ggplot_smooth_func.R")

#read in CTD data - change this to the appropriate working directory
files_ctd <- list.files(path = "C:/Users/Mary Lofton/Dropbox/2018_Data_RPM/CTD_2018",
                        pattern = "50_PAR.csv")

#change this to the appropriate filename on your computer
filename_ctd = paste("C:/Users/Mary Lofton/Dropbox/2018_Data_RPM/CTD_2018",files_ctd[1],sep = "/")

#read in files and create datetime column
CTD <- read_csv(filename_ctd)
CTD$Date <- as_datetime(CTD$Date) #note that this is now in wrong time zone

#adding site and reservoir columns to CTD files
place <- unlist(strsplit(files_ctd[1],"_"))
reservoir <- substr(place[2], start = 1, stop = 3)
site <- as.double(substr(place[2], start = 4, stop = 5))
CTD$Reservoir <- reservoir
CTD$Site <- site

#you will need to change the filepath to be appropriate for your computer
#data wrangling to get a beautiful csv
for (i in 2:length(files_ctd)){
  filename_ctd = paste("C:/Users/Mary Lofton/Dropbox/2018_Data_RPM/CTD_2018",files_ctd[i],sep = "/")
  cast <- read_csv(filename_ctd, skip = 1, col_names = FALSE)
  place <- unlist(strsplit(files_ctd[i],"_"))
  reservoir <- substr(place[2], start = 1, stop = 3)
  site <- as.double(substr(place[2], start = 4, stop = 5))
  cast[,5] <- reservoir
  cast[,6] <- site
  colnames(cast) <- c("Date","Depth_m","PAR","Chla_ugL","Reservoir","Site")
  cast$Date <- as_datetime(cast$Date)
  CTD <- bind_rows(CTD, cast)
}

#read in flora data - change this to the appropriate working directory
files_flora <- list.files(path = "C:/Users/Mary Lofton/Documents/Github/Reservoirs/Data/DataAlreadyUploadedToEDI/CollatedDataForEDI/FluoroProbeData/",
                    pattern = glob2rx("*2018*_50*"))

#ditto for filepath - edit for your computer
filename_flora = paste("C:/Users/Mary Lofton/Documents/Github/Reservoirs/Data/DataAlreadyUploadedToEDI/CollatedDataForEDI/FluoroProbeData/",
                       files_flora[1],sep = "/")

col_names <- names(read_tsv(filename_flora, n_max = 0))
units <- read_tsv(filename_flora,n_max = 1)
new_col_names <- paste(col_names,units, sep = "_")
new_col_names

flora <- read_tsv(filename_flora,skip = 2,col_names = FALSE,
                  cols(X1 = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
                       X2 = col_double(),
                       X3 = col_double(),
                       X4 = col_double(),
                       X5 = col_double(),
                       X6 = col_double(),
                       X7 = col_double(),
                       X8 = col_double(),
                       X9 = col_double(),
                       X10 = col_double(),
                       X11 = col_double(),
                       X12 = col_double(),
                       X13 = col_double(),
                       X14 = col_integer(),
                       X15 = col_integer(),
                       X16 = col_integer(),
                       X17 = col_integer(),
                       X18 = col_integer(),
                       X19 = col_integer(),
                       X20 = col_integer(),
                       X21 = col_integer(),
                       X22 = col_integer(),
                       X23 = col_double(),
                       X24 = col_double(),
                       X25 = col_double(),
                       X26 = col_double(),
                       X27 = col_double(),
                       X28 = col_double(),
                       X29 = col_double(),
                       X30 = col_double(),
                       X31 = col_double(),
                       X32 = col_double()
                  ))
colnames(flora) <- new_col_names
place <- unlist(strsplit(files_flora[1],"_"))
reservoir <- substr(place[2], start = 1, stop = 3)
site <- as.double(substr(place[3], start = 1, stop = 2))
flora$Reservoir <- reservoir
flora$Site <- site

#you need to change filepath!
for (i in 2:length(files_flora)){
  filename_flora = paste("C:/Users/Mary Lofton/Documents/Github/Reservoirs/Data/DataAlreadyUploadedToEDI/CollatedDataForEDI/FluoroProbeData/",
                         files_flora[i],sep = "/")  
  cast <- read_tsv(filename_flora,skip = 2,col_names = FALSE,
                    cols(X1 = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
                         X2 = col_double(),
                         X3 = col_double(),
                         X4 = col_double(),
                         X5 = col_double(),
                         X6 = col_double(),
                         X7 = col_double(),
                         X8 = col_double(),
                         X9 = col_double(),
                         X10 = col_double(),
                         X11 = col_double(),
                         X12 = col_double(),
                         X13 = col_double(),
                         X14 = col_integer(),
                         X15 = col_integer(),
                         X16 = col_integer(),
                         X17 = col_integer(),
                         X18 = col_integer(),
                         X19 = col_integer(),
                         X20 = col_integer(),
                         X21 = col_integer(),
                         X22 = col_integer(),
                         X23 = col_double(),
                         X24 = col_double(),
                         X25 = col_double(),
                         X26 = col_double(),
                         X27 = col_double(),
                         X28 = col_double(),
                         X29 = col_double(),
                         X30 = col_double(),
                         X31 = col_double(),
                         X32 = col_double()
                    ))
  colnames(cast) <- new_col_names
  place <- unlist(strsplit(files_flora[i],"_"))
  reservoir <- substr(place[2], start = 1, stop = 3)
  site <- as.double(substr(place[3], start = 1, stop = 2))
  cast$Reservoir <- reservoir
  cast$Site <- site
  flora <- bind_rows(flora, cast)
}

#read in spec data - IGNORE THIS FOR NOW
spec <- read_csv("Chla_running_2018.csv")
spec$Site <- 50

#create tibble with all 3 forms of chla data
ctd_overlap <- CTD %>%
  mutate_if(is.character, str_replace_all, pattern = 'fcr', replacement = 'FCR') %>%
  mutate_if(is.character, str_replace_all, pattern = 'bvr', replacement = 'BVR') %>%
  mutate_if(is.character, str_replace_all, pattern = 'ccr', replacement = 'CCR') %>%
  mutate(Date = date(Date),
         Site = 50) %>%
  rename(Chla_CTD_ugL = Chla_ugL) %>%
  mutate(Depth_m = round(Depth_m, 1)) %>%
  select(Reservoir, Site, Date, Depth_m, Chla_CTD_ugL)



flora_overlap <- flora %>%
  select(c(1:5,10,12,33,34))

colnames(flora_overlap) <- c("Date","greenalgae_ugL", "bluegreens_ugL","diatoms_ugL",
                             "cryptophytes_ugL","totalconc_ugL","Depth_m","Reservoir","Site")

flora_overlap <- flora_overlap %>%
         mutate(Depth_m = ifelse(Depth_m <=0.4, 0.1, Depth_m),
                Depth_m = ifelse(Depth_m >=1.4 & Depth_m <= 1.8, 1.6, Depth_m),
                Depth_m = ifelse(Depth_m >=3.6 & Depth_m <= 4.0, 3.8, Depth_m),
                Depth_m = ifelse(Depth_m >=4.8 & Depth_m <= 5.2, 5.0, Depth_m),
                Depth_m = ifelse(Depth_m >=2.8 & Depth_m <= 3.2, 3.0, Depth_m),
                Depth_m = ifelse(Depth_m >=5.8 & Depth_m <= 6.2, 6.0, Depth_m),
                Depth_m = ifelse(Depth_m >=11.8 & Depth_m <= 12.2, 12.0, Depth_m))

flora_overlap <- flora_overlap %>%
  mutate(Date = date(Date),
         Depth_m = round(Depth_m, 1),
         Site = 50)

#just checking to make sure data wrangling worked
test <- flora_overlap %>%
  filter(Reservoir == "BVR", Date == "2018-05-24", Depth_m == 3)
  
#AGAIN, MORE TO DO WITH SPEC DATA = IGNORE THIS
colnames(spec)[c(1,5:6)] <- c("Sample_ID","Chla_ugL_spec","Pheophytin_ugL_spec")
spec_overlap <- spec %>%
  mutate(Date = date(Date),
         Site = 50)%>%
  rename(Depth_m = Depth) %>%
  select(Reservoir, Site, Date, Depth_m, Sample_ID, Chla_ugL_spec, Pheophytin_ugL_spec) %>%
  filter(Chla_ugL_spec >= 0 & Pheophytin_ugL_spec >= 0)

overlap <- flora_overlap %>%
  left_join(ctd_overlap, by = c("Reservoir","Date","Depth_m","Site"))

overlap <- overlap %>%
  left_join(spec_overlap, by = c("Reservoir","Date","Depth_m","Site")) 

overlap <- overlap[,c(8,9,7,1,10,2,3,4,5,6,12,13,11)]

#calculate averages for each depth/date combination
dates <- unique(overlap$Date)
depths <- unique(overlap$Depth_m)
real_points <- unique(overlap[,c('Reservoir','Date','Depth_m')])
final <- overlap[1,]
final[1,] <- NA

for (i in 1:(length(real_points$Date))){
  point <- overlap %>%
    filter(Reservoir == real_points$Reservoir[i], 
           Date == real_points$Date[i], 
           Depth_m == real_points$Depth_m[i])
  chla_avg <- colMeans(point[,c(3,5:10)], na.rm = TRUE)
  point[1,c(3,5:10)] <- chla_avg
  final = bind_rows(final,point[1,])
}

final <- final[-1,]

#edit this statement depending on your purpose
final = final[!is.na(final$Chla_ugL_spec),]

#plot overlap with 1:1 line, best fit line, and best fit function
mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 legend.key = element_blank(),legend.background = element_blank(),
                 text = element_text(size=16))


#set x and y vars
xvar = c(5,11)
yvar = c(6:10)

#generate plots
for (j in 1:2){
  for (i in 1:5){
  
    data = final %>% filter(Reservoir == "BVR")
    
    colnames(data)[xvar[j]] <- "x"
    colnames(data)[yvar[i]] <- "y"
    
compare <- ggplot(data = data, mapping = aes(x = x, y = y))+
  geom_point(size = 2)+
  mytheme+
  xlab(colnames(final)[xvar[j]])+
  ylab(colnames(final)[yvar[i]])+
  geom_smooth(method=lm)+
  geom_abline(slope = 1, intercept = 0)+
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)+
  geom_text(aes(label=ifelse((x>4*IQR(x, na.rm = TRUE)|y>4*IQR(y, na.rm = TRUE)),Sample_ID,"")), hjust=1.1)

compare

filename = paste0(colnames(final)[xvar[j]],"_vs_",colnames(final)[yvar[i]],"_BVR.tif")

ggsave(filename = filename,plot=compare,device = "tiff")
  }
}


#spec vs. CTD
compare2 <- ggplot(data = final[which(final$Date != "2018-06-28"),], mapping = aes(x = Chla_ugL_spec, y = Chla_CTD_ugL))+
  geom_point(size=2)+
  #geom_text(aes(label = Date))+
  mytheme+
  geom_smooth(method=lm)+
  geom_abline(slope = 1, intercept = 0)+
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)
compare2
ggsave("spec_vs_CTD_wo_20180628.tif",plot=compare2,device = "tiff")

#spec + pheophytin vs. FP
compare3 <- ggplot(data = final, mapping = aes(x = Chla_ugL_spec + Pheophytin_ugL_spec, y = totalconc_ugL))+
  geom_point(size=2)+
  #geom_text(aes(label = Date))+
  mytheme+
  geom_smooth(method=lm)+
  geom_abline(slope = 1, intercept = 0)+
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)
compare3
ggsave("spec_chla_and_pheo_vs_totalconc_ugL.tif",plot=compare3,device = "tiff")

#spec vs. FP - cyanos
compare4 <- ggplot(data = final[which(final$Date != "2018-06-28"),], mapping = aes(x = Chla_ugL_spec, y = totalconc_ugL - bluegreens_ugL))+
  geom_point(size=2)+
  #geom_text(aes(label = Date))+
  mytheme+
  geom_smooth(method=lm)+
  geom_abline(slope = 1, intercept = 0)+
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)
compare4
ggsave("spec_chla_vs_totalconc_wo_bluegreens_wo_20180628.tif",plot=compare4,device = "tiff")


