library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)



setwd("~/chla_pub/CSV")


#################################################################
###################### 2014 data below ##########################
#################################################################

chla_2014 <- read_csv("chla_running_2014.csv")

colnames(chla_2014) <- c('Reservoir', 'Date', 'Depth_m', 'Chla_ugL')

chla_2014$Site <- 50
chla_2014$Flag_Chla <- 0
chla_2014$Pheo_ugL <- NA
chla_2014$Flag_Pheo <- 2

chla_2014 <- select(chla_2014, 'Reservoir', 'Site', 'Date', 'Depth_m', 'Chla_ugL', 'Pheo_ugL', 'Flag_Chla', 'Flag_Pheo')




#################################################################
###################### 2015 data below ##########################
#################################################################


chla_2015 <- read_csv("Chla_running_2015.csv")
colnames(chla_2015) <- c("Reservoir", "Site", "Date", "Depth_m", "Sample_ID", "chla_ugL", "pheo_ugL", "Flag_Chla", "Flag_Pheo")
str(chla_2015)

chla_2015 <- filter(chla_2015, is.na(chla_2015$chla_ugL) == FALSE)


# No data
#flag_2_15 <- c('B2Jy150m', 'B2Jy153m', 'B2Jy156m', 'B2Jy159m', 'B2Jy1512m', 'B9Jy150m', 'B9Jy153m', 'B9Jy156m', 'B9Jy159m', 'B9Jy1512m', 
#            'B25Ju150m', 'B25Ju153m', 'B25Ju156m', 'B25Ju159m', 'B25Ju1511m', 'B30Jy150m', 'B30Jy153m', 'B30Jy156m', 'B30Jy159m', 'B30Jy1512m', 
#            'F6Jy151.6m', 'F6Jy153.8m', 'F6Jy155.0m', 'F6Jy156.2m', 'F6Jy9.0m', 'F13Jy151.6m', 'F13Jy153.8m', 'F13Jy155m', 'F13Jy156.2m', 
#            'F13Jy159m', 'F29Ju151.6m', 'F29Ju153.8m', 'F29Ju155.0m', 'F29Ju156.2m', 'F29Ju159m', 'GR20May15 0m', 'CC12Ju15 31m')
#for(value in flag_2_15){
#  chla_2015$Flag_Pheo[chla_2015$Sample_ID == value] <- 2
#  chla_2015$Flag_Chla[chla_2015$Sample_ID == value] <- 2
#}

# Pigment in extract below detection (<34 ugL)
 

# for chla
flag_6_15_chla <- c('CC12Jun15 0m', 'CC26Jun15 0m', 'CC26Aug15 0m', 'CC26Aug15 20m', 'CC23Sep15 19m', 'CC28Oct15 20m', 
               'F28Sep15 3.8m', 'GR20May15 9m', 'GR09May15 14m', 'GR20May15 13m', 'GR30May15 13m', 'GR20Jul15 14m',
               'G03Aug15 13m', 'SH08May15 0m', 'SH29May15 31m', 'SH26Jun15 0m', 'SH10Jul15 0m', 'SH10Jul15 31m', 
               'SH24Jul15 0m', 'SH24Jy15 8m', 'SH24Jul15 31m', 'SH07Aug15 0m', 'SH07Aug15 28m', 'SH26Aug15 31m',
               'SH26Aug15 8m', 'SH9Sep15 0m', 'SH9Sep15 31m', 'SH28Oct15 0m', 'SH28Oct15 10m', 'SH28Oct15 31m'
               )
for(value in flag_6_15_chla){
  chla_2015$Flag_Chla[chla_2015$Sample_ID == value] <- 4
}

# for pheo
flag_6_15_pheo <- c('B13Aug15 12.0m', 'B4Sep15 9.0m', 'B2Oct15 6m', 'B2Oct15 12m', 'B23Oct15 0m', 'B23Oct15 3m', 'B23Oct15 6m', 
                    'B23Oct15 9.0m', 'B23Oct15 12m', 'CC08May15 0m', 'CC08May15 21m', 'CC29May15 0m', 'CC29May15 6m', 'CC12Jun15 0m', 
                    'CC10Jul15 0m', 'CC24Jy15 0m', 'CC24Jul15 20m', 'CC07Aug15 0m', 'CC07Aug15 6m', 'CC07Aug15 20m', 'CC9Sep15 0m', 'CC23Sep15 0m', 
                    'CC23Sep15 6m', 'CC23Sep15 19m', 'CC28Oct15 0m', 'CC28Oct15 20m', 'F3Aug15 9.0m', 'F10Aug15 1.6m', 'F10Aug15 9.0m', 
                    'F14Sep15 3.8m', 'F14Sep15 9.0m', 'F28Sep15 6.2m', 'F19Oct15 1.6m', 'F19Oct15 3.8m', 'F19Oct15 5.0m', 'F19Oct15 6.2m', 
                    'F19Oct15 9.0m', 'F9Nov15 1.6m' , 'F9Nov15 3.8m' , 'F9Nov15 5.0m', 'F9Nov15 6.2m', 'F9Nov15 9m', 'GR20May15 13m', 
                    'GR10Jun15 0m', 'GR10Ju15 14m', 'GR22Jun15 0m', 'GR22Jun15 14m', 'G20Jul15 0m', 'G03Aug15 9m', 'G15Aug15 13.5m', 'G17Sep15 0m', 
                    'SH08May15 0m', 'SH08May15 8m', 'SH08May15 28m', 'SH22May15 0m', 'SH22May15 8m', 'SH22May15 28m', 'SH29May15 0m', 'SH29May15 8m', 'SH29May15 31m', 
                    'SH12Ju15 0m', 'SH12Jun15 8m', 'SH12Ju15 31m', 'SH26Jun15 0m', 'SH10Jul15 0m', 'SH10Jy15 8m', 'SH10Jul15 31m', 'SH24Jul15 0m', 
                    'SH24Jy15 8m', 'SH24Jul15 31m'
)

for(value in flag_6_15_pheo){
  chla_2015$Flag_Pheo[chla_2015$Sample_ID == value] <- 4
}

# Pigment in extract below detection, no data

# for chla
#flag_2_15_chla_bd <- c('SH07Aug15 0m', 'SH07Aug15 8m', 'SH07Aug15 28m', 'SH26Aug15 31m', 'SH26Aug15 0m', 'SH26Aug15 8m', 
#                        'SH9Sep15 0m', 'SH9Sep15 8m', 'SH9Sep15 31m', 'SH28Oct15 0m', 'SH28Oct15 10m', 'SH28Oct15 31m')
#for(value in flag_2_15_chla_bd){
#  chla_2015$Flag_Chla[chla_2015$Sample_ID == value] <- 2
#}

#for pheo 
flag_2_15_pheo_bd <- c('SH07Aug15 0m', 'SH07Aug15 8m', 'SH07Aug15 28m', 'SH26Aug15 31m', 'SH26Aug15 0m', 'SH26Aug15 8m', 
                        'SH9Sep15 0m', 'SH9Sep15 8m', 'SH9Sep15 31m', 'SH28Oct15 0m', 'SH28Oct15 10m', 'SH28Oct15 31m', '9Nov15T1S1', 
                       '9Nov15T1S2', '9Nov15T1S3', '9Nov15T2S1', '9Nov15T2S2', '9Nov15T2S3')
for(value in flag_2_15_pheo_bd){
  chla_2015$Flag_Pheo[chla_2015$Sample_ID == value] <- 2
  chla_2015$Flag_Chla[chla_2015$Sample_ID == value] <- 2
  
  
}

# changing reservoir names to proper codes
res_list <- c()
for (value in chla_2015$Reservoir){
  if (value == 'Beaver Dam'){
    res_list <- c(res_list, 'BVR')
  }
  if (value == 'Carvins Cove'){
    res_list <- c(res_list, 'CCR')
  }
  if (value == 'Falling Creek'){
    res_list <- c(res_list, 'FCR')
  }
  if (value == 'Gatewood'){
    res_list <- c(res_list, 'GWR')
  }
  if (value == 'Spring Hollow'){
    res_list <- c(res_list, 'SHR')
  }
  if (value == 'Unknown'){
    res_list <- c(res_list, 'UNK')
  }
}
res_df <- as.data.frame(res_list)
chla_2015 <- cbind(res_df, chla_2015)

chla_2015$Site <- 50


# Pull out columns 
colnames(chla_2015) <- c('Reservoir', 'remove_1', 'Site', 'DateTime', 'Depth_m', 'remove_2', 'Chla_ugL', 'Pheo_ugL', 'Flag_Chla', 'Flag_Pheo') 
chla_2015 <- select(chla_2015, 'Reservoir', 'Site', 'DateTime', 'Depth_m', 'Chla_ugL', 'Pheo_ugL', 'Flag_Chla', 'Flag_Pheo')
str(chla_2015)


to_num_15 <- c('Site', 'Depth_m', 'Chla_ugL', 'Flag_Chla')
for (value in to_num_15)chla_2015[ ,value] <- as.numeric(chla_2015[,value])
str(chla_2015)

chla_2015$Date <- as.Date(chla_2015$DateTime)
chla_2015 <- select(chla_2015, 'Reservoir', 'Site', 'Date', 'Depth_m', 'Chla_ugL', 'Pheo_ugL', 'Flag_Chla', 'Flag_Pheo')


chla_2015 <- filter(chla_2015, is.na(chla_2015$Chla_ugL) == FALSE)
chla_2015 <- filter(chla_2015, is.na(chla_2015$Depth_m) == FALSE)

chla_2015$Pheo_ugL <- as.numeric(chla_2015$Pheo_ugL)



#################################################################
###################### 2016 data below ##########################
#################################################################

chla_2016 <- read_csv("Chla_running_2016.csv")

names(chla_2016) <- gsub(" ", "_", names(chla_2016))


# dup check failed

#  # according to 102216Chlacalcs_JF
#chla_2016$Flag_Pheo[chla_2016$Sample_ID == "B11AUGC7r2"] <- 3
#
#chla_2016$Flag_Pheo[chla_2016$Sample_ID == "F27JULC7r2V2"] <- 3
#
#  # According to 072616
#chla_2016$Flag_Pheo[chla_2016$Sample_ID == "B14JulC4r1"] <- 3
#
#chla_2016$Flag_Pheo[chla_2016$Sample_ID == "B14JulC4r1"] <- 3
#
#chla_2016$Flag_Pheo[chla_2016$Sample_ID == "B23JunC5r2"] <- 3


dup_failed_lst <- c('F8AUGC5r2', 'F25JULC6r1', 'F25JULC6r1', 'B21JULC5r1', 'F27JULC7r1V2', 'B28JULC7r2', 'B24JULC3.8', 'F8AUGC5r1', 'B11AUGC3.5', 'B18AUGC6.0r1', 
                    'B21JULC5r2', 'B21JULC7', 'B04AUGC7r1', 'B24JULC6.8r2', 'B24JULC6.8r1', 'F01AUGC4.5', 'F01AUGC2.8', 'F01AUGC6.2r1', 'B18AUGC4.0', 'F26AUGC5', 
                    'B28JULC4', 'F15AUGC3.8r2', 'B23AUGC6.5r1', 'B11AUGC7r1', 'F01AUGC6.2r2', 'B28JULC7r1', 'B23AUGC4', 'F25JULC3.8', 'B11AUGC7r2', 'F15AUGC5',
                    'F25JULC6r2', 'B04AUGC7r2', 'F15AUGC3.8r1', 'F27JULC7r2V2', 'B23AUGC6.5r2', 'F26AUGC3.8r1', 'B18AUGC6.0r2', 'B04AUGC3', 'F26AUGC3.8r2', 
                    'F27JULC4V2'
                    )

for(value in dup_failed_lst){
  chla_2016$Flag_Chla[chla_2016$Sample_ID == value] <- 3
  chla_2016$Flag_Pheo[chla_2016$Sample_ID == value] <- 3
}



# Absorbance < 0.030

  # from 072616_Chlacalcs
chla_2016$Flag_Pheo[chla_2016$Sample_ID == "B7JulC8"] <- 1
chla_2016$Flag_Chla[chla_2016$Sample_ID == "B7JulC8"] <- 1

chla_2016$Flag_Pheo[chla_2016$Sample_ID == "B30JunC9"] <- 1
chla_2016$Flag_Chla[chla_2016$Sample_ID == "B30JunC9"] <- 1

chla_2016$Flag_Pheo[chla_2016$Sample_ID == "F25APRC3.8"] <- 1
chla_2016$Flag_Chla[chla_2016$Sample_ID == "F25APRC3.8"] <- 1



# Overacidified
  # According to 072616_Chlacalcs

# overacidified
#chla_2016$Flag_Pheo[chla_2016$Sample_ID == "B14JulC4r2"] <- 5

#chla_2016$Flag_Pheo[chla_2016$Sample_ID == "B11AUGC3.5"] <- 5

#user error
#chla_2016$Flag_Pheo[chla_2016$Sample_ID == "F27JULC4V2"] <- 5
#chla_2016$Flag_Chla[chla_2016$Sample_ID == "F27JULC4V2"] <- 5


# Removing flags that were previously placed on 

remove_flags_2015 <- c('F30SepC2r2', 'F30SepC6r2', 'F7OctC1.6r1', 'F7OctC5', 'B11AUGC3.5', 'B23AUGC4', 'B23AUGC6.5r1', 
                       'B6SepC3r1', 'B6SepC7', 'B17SepC3', 'F25SepC3.8r2', 'B30JunC9', 'B7JulC8')

for(value in remove_flags_2015){
  chla_2016$Flag_Chla[chla_2016$Sample_ID == value] <- 0
  chla_2016$Flag_Pheo[chla_2016$Sample_ID == value] <- 0
  
}

chla_2016$Flag_Chla[chla_2016$Sample_ID == "B17SepC6r1"] <- 0


# Temporary renaming of columns to make them easier to work with 
colnames(chla_2016) <- c("Sample_ID", "Reservoir", "Site", "Year", "Date", "Depth", "Chla", "Pheo", "Flag_Chla", "Flag_Pheo", "Notes")

#chla_2016_plot <- ggplot(data = chla_2016, aes(x = Date, y = Chla)) + geom_point()
# +geom_text(aes(label = Sample_ID), hjust=0, vjust=0)
#chla_2016_plot


chla_2016 <- select(chla_2016, 'Reservoir', 'Site', 'Date', 'Depth', 'Chla', 'Pheo','Flag_Chla', 'Flag_Pheo')
colnames(chla_2016) <- c('Reservoir', 'Site', 'DateTime', 'Depth_m', 'Chla_ugL', 'Pheo_ugL', 'Flag_Chla', 'Flag_Pheo')

chla_2016$Date <- as.Date(chla_2016$DateTime)

chla_2016 <- select(chla_2016, 'Reservoir', 'Site', 'Date', 'Depth_m', 'Chla_ugL', 'Pheo_ugL', 'Flag_Chla', 'Flag_Pheo')

chla_2016 <- filter(chla_2016, is.na(chla_2016$Chla_ugL) == FALSE)



#################################################################
###################### 2018 data below ##########################
#################################################################


chla_2018 <- read_csv("Chla_running_2018.csv")
names(chla_2018) <- gsub(" ", "_", names(chla_2018))
colnames(chla_2018) <- c("Sample_ID", "Reservoir", "Site", "Date", "Depth", "Chla", "Pheo", "Flag_Chla", "Flag_Pheo")
chla_2018
str(chla_2018)

# Pigment in extract below detection (< 34 ugL) 

# Based on 051618_Chlacalcs
chla_2018$Flag_Chla[chla_2018$Sample_ID == "B4May18C6"] <- 4
chla_2018$Flag_Pheo[chla_2018$Sample_ID == "B4May18C6"] <- 4


chla_2018_plot <- ggplot(data = chla_2018, aes(x = Date, y = Chla)) + geom_point()
# +geom_text(aes(label = Sample_ID), hjust=0, vjust=0)
chla_2018_plot


chla_2018 <- select(chla_2018, 'Reservoir', 'Site', 'Date', 'Depth', 'Chla', 'Pheo','Flag_Chla', 'Flag_Pheo')
colnames(chla_2018) <- c('Reservoir', 'Site', 'Date', 'Depth_m',  'Chla_ugL', 'Pheo_ugL', 'Flag_Chla', 'Flag_Pheo')
str(chla_2018)

chla_2018 <- filter(chla_2018, is.na(chla_2018$Chla_ugL) == FALSE)



#################################################################
###################### 2019 data below ##########################
#################################################################

chla_2019 <- read_csv("Chla_running_2019new.csv")
names(chla_2019) <- gsub(" ", "_", names(chla_2019))
chla_2019

# Assigning duplicates and removing all duplicate data (dups, checks, etc)
chla_2019$Dup[chla_2019$Date == '2019-04-15' & chla_2019$Reservoir == 'FCR' & chla_2019$Site == 50 & chla_2019$Dup == 'r2'] <- NA

chla_2019$Dup[chla_2019$Date == '2019-05-27' & chla_2019$Reservoir == 'FCR' & chla_2019$Site == 50 & chla_2019$Depth == 5.0 & chla_2019$Chla_ugL == 8.48] <- NA
chla_2019$Dup[chla_2019$Date == '2019-06-24' & chla_2019$Reservoir == 'FCR' & chla_2019$Site == 50 & chla_2019$Depth == 1.6 & chla_2019$Chla_ugL == 6.82] <- NA
chla_2019$Dup[chla_2019$Date == '2019-04-01' & chla_2019$Reservoir == 'FCR' & chla_2019$Site == 50 & chla_2019$Depth == 5.0 & chla_2019$Chla_ugL == 3.21] <- NA
chla_2019$Dup[chla_2019$Date == '2019-07-29' & chla_2019$Reservoir == 'FCR' & chla_2019$Site == 50 & chla_2019$Depth == 1.6 & chla_2019$Chla_ugL == 19.31] <- NA



chla_2019$Dup[chla_2019$Date == '2019-04-22' & chla_2019$Reservoir == 'FCR' & chla_2019$Site == 50 & chla_2019$Depth == 1.6 & chla_2019$Chla_ugL == 3.85] <- 'DUP'
chla_2019$Dup[chla_2019$Date == '2019-04-29' & chla_2019$Reservoir == 'FCR' & chla_2019$Site == 50 & chla_2019$Depth == 1.6 & chla_2019$Chla_ugL == 1.66] <- 'DUP'

chla_2019 <- filter(chla_2019, is.na(chla_2019$Dup) == TRUE)


# Pigment in extract below detection (<34 ugL)

# Based on 18sep19_FCR_BVR_Depths_WWedits

chla_2019$Flag_Pheo[chla_2019$Date == '2019-04-01' & chla_2019$Reservoir == 'FCR' & chla_2019$Site == 50 & chla_2019$Depth == 3.8 & chla_2019$Chla_ugL == 0.80] <- 4
chla_2019$Flag_Chla[chla_2019$Date == '2019-04-01' & chla_2019$Reservoir == 'FCR' & chla_2019$Site == 50 & chla_2019$Depth == 3.8 & chla_2019$Chla_ugL == 0.80] <- 4


chla_2019_plot <- ggplot(data = chla_2019, aes(x = Date, y = Chla_ugL)) + geom_point()
# +geom_text(aes(label = Sample_ID), hjust=0, vjust=0)
chla_2019_plot

str(chla_2019)

chla_2019 <- select(chla_2019, 'Reservoir', 'Site', 'Date', 'Depth', 'Chla_ugL', 'Pheo_ugL', 'Flag_Chla', 'Flag_Pheo')
colnames(chla_2019) <- c('Reservoir', 'Site', 'Date', 'Depth_m', 'Chla_ugL', 'Pheo_ugL', 'Flag_Chla', 'Flag_Pheo')

chla_2019 <- filter(chla_2019, is.na(chla_2019$Chla_ugL) == FALSE)



# upload continuum day data 
chla_2019_rc <- read_csv('chla_RCD_data.csv')
chla_2019_rc


# Adding pheo cols
chla_2019_rc$Pheo_ugL <- NA
chla_2019_rc$Flag_Pheo <- 2


res_list_rc <- c()
for (value in chla_2019_rc$Reservoir){
  if (value == 'B'){
    res_list_rc <- c(res_list_rc, 'BVR')
  }
  if (value == 'F'){
    res_list_rc <- c(res_list_rc, 'FCR')
  }
}

res_rc_df <- as.data.frame(res_list_rc)
chla_2019_rc <- cbind(res_list_rc, chla_2019_rc)


site_list_rc <- c()
for (value in chla_2019_rc$Site){
  if (value == 'B01'){
    site_list_rc <- c(site_list_rc, 01)
  }
  if (value == 'B100'){
    site_list_rc <- c(site_list_rc, 100)
  }
  if (value == 'B20'){
    site_list_rc <- c(site_list_rc, 20)
  }
  if (value == 'B200'){
    site_list_rc <- c(site_list_rc, 200)
  }
  if (value == 'B30'){
    site_list_rc <- c(site_list_rc, 30)
  }
  if (value == 'B45'){
    site_list_rc <- c(site_list_rc, 45)
  }
  if (value == 'B50'){
    site_list_rc <- c(site_list_rc, 50)
  }
  if (value == 'F01'){
    site_list_rc <- c(site_list_rc, 01)
  }
  if (value == 'F100'){
    site_list_rc <- c(site_list_rc, 100)
  }
  if (value == 'F101'){
    site_list_rc <- c(site_list_rc, 101)
  }
  if (value == 'F102'){
    site_list_rc <- c(site_list_rc, 102)
  }
  if (value == 'F20'){
    site_list_rc <- c(site_list_rc, 20)
  }
  if (value == 'F200'){
    site_list_rc <- c(site_list_rc, 200)
  }
  if (value == 'F30'){
    site_list_rc <- c(site_list_rc, 30)
  }
  if (value == 'F45'){
    site_list_rc <- c(site_list_rc, 45)
  }
  if (value == 'F50'){
    site_list_rc <- c(site_list_rc, 50)
  }
  if (value == 'F99'){
    site_list_rc <- c(site_list_rc, 99)
  }
}

site_rc_df <- as.data.frame(site_list_rc)
chla_2019_rc <- cbind(site_rc_df, chla_2019_rc)

chla_2019_rc$Depth_m <- 0.1



str(chla_2019_rc)

chla_2019_rc <- filter(chla_2019_rc, is.na(chla_2019_rc$Dup) == TRUE)

chla_2019_rc <- select(chla_2019_rc, 'res_list_rc', 'site_list_rc', 'Date', 'Depth_m', 'Chla_ugL', 'Pheo_ugL', 'Flag_BDL', 'Flag_Pheo')
colnames(chla_2019_rc) <- c('Reservoir', 'Site', 'Date', 'Depth_m', 'Chla_ugL', 'Pheo_ugL', 'Flag_Chla', 'Flag_Pheo')

chla_2019_rc <- filter(chla_2019_rc, is.na(chla_2019_rc$Chla_ugL) == FALSE)



# upload gps data 
chla_gps_2019 <- read_csv('chla_gps_2019.csv')

chla_gps_2019$Flag_Chla <- 0
chla_gps_2019$Flag_Pheo <- 0

# Pigment in extract below detection (<34 ugL)

  # Based on Chla calculations GPS Data 

chla_gps_2019$Flag_Pheo[chla_gps_2019$Date == '2019-06-13' & chla_gps_2019$Reservoir == 'BVR' & chla_gps_2019$Site == 50 & chla_gps_2019$Depth == 6.0 & chla_gps_2019$Chla_ugL == 27.08] <- 4

str(chla_gps_2019)

chla_gps_2019 <- select(chla_gps_2019, 'Reservoir', 'Site', 'Date', 'Depth', 'Chla_ugL', 'Pheo_ugL', 'Flag_Chla', 'Flag_Pheo')
colnames(chla_gps_2019) <- c('Reservoir', 'Site', 'Date', 'Depth_m', 'Chla_ugL', 'Pheo_ugL', 'Flag_Chla', 'Flag_Pheo')

chla_gps_2019 <- filter(chla_gps_2019, is.na(chla_gps_2019$Chla_ugL) == FALSE)



#################################################################
###################### Master DataFrame #########################
#################################################################

chla_master_df <- bind_rows(chla_2014, chla_2015, chla_2016, chla_2018, chla_2019, chla_2019_rc, chla_gps_2019)
chla_master_df <-chla_master_df %>% distinct(Reservoir, Site, Date, Depth_m, Chla_ugL, Pheo_ugL, Flag_Chla, Flag_Pheo)

chla_master_df$chla_new <- round(chla_master_df$Chla_ugL, digits = 1)
chla_master_df$pheo_new <- round(chla_master_df$Pheo_ugL, digits = 1)

chla_master_df <- select(chla_master_df, 'Reservoir', 'Site', 'Date', 'Depth_m', 'chla_new', 'pheo_new', 'Flag_Chla', 'Flag_Pheo')
colnames(chla_master_df) <- c('Reservoir', 'Site', 'Date', 'Depth_m', 'Chla_ugL', 'Pheo_ugL', 'Flag_Chla', 'Flag_Pheo')

# making 0.0 depth 0.1 

chla_master_df[chla_master_df$Depth_m == 0.0, 'Depth_m'] <- 0.1

master_plot <- ggplot(data = chla_master_df, aes(x = Date, y = Chla_ugL))+ geom_point()
#+geom_text(aes(label = Date), hjust=0, vjust=0)
master_plot


write_csv(chla_master_df, 'chla_master_df.csv')


date_test <- as.data.frame(as.POSIXct(chla_master_df$Date), format = 12:00:00)




