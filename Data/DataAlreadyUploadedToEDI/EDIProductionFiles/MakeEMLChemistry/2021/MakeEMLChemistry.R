##MakeEMLChemistry
##Author: Mary Lofton
##Modified by Whitney Woelmer
##Slight modification by Jacob Wynne
##5Jun2022 - 2021 nutrient aggregation
#NOTE - 9 samples need to be rerun with 2022 samples - will need to average/add these data when we publish 2022 field season samples (HLW)

#good site for step-by-step instructions
#https://ediorg.github.io/EMLassemblyline/articles/overview.html
#and links therein

#append this year's chemistry to last year's published data
library(tidyverse)
library(viridis)
library(plotly)

old <- read.csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2020/chemistry_2013_2020.csv")
new <- read.csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2021/2021_chemistry_collation_final_nocommas.csv")
new <- new %>% select(-X)

#get cols in same order
new <- new[names(old)]

#removing the 9 samples that will be rerun with 2022 field samples
#B09Aug21 10m DOC, B22Nov21 9m NO3, B12Oct21 9m SRP, B08Mar21 11m TP, B23Aug21 11m TP,
#C19Aug21 21m NH4, NO3, and SRP, C23Sep21 20m TN, C19Nov21 0.1m TN, F12Jul21 3.8m TP
new$DOC_mgL[new$DateTime=="2021-08-09 11:10:00" & new$Reservoir=="BVR" & new$Depth_m==10] <- NA
new$NO3NO2_ugL[new$DateTime=="2021-11-22 10:18:00" & new$Reservoir=="BVR" & new$Depth_m==9] <- NA
new$SRP_ugL[new$DateTime=="2021-10-12 11:53:00" & new$Reservoir=="BVR" & new$Depth_m==9] <- NA
new$TP_ugL[new$DateTime=="2021-03-08 10:58:00" & new$Reservoir=="BVR" & new$Depth_m==11] <- NA
new$TP_ugL[new$DateTime=="2021-08-23 10:39:00" & new$Reservoir=="BVR" & new$Depth_m==11] <- NA
new$NO3NO2_ugL[new$DateTime=="2021-08-19 12:00:00" & new$Reservoir=="CCR" & new$Depth_m==21 & new$Site==50] <- NA
new$NH4_ugL[new$DateTime=="2021-08-19 12:00:00" & new$Reservoir=="CCR" & new$Depth_m==21 & new$Site==50] <- NA
new$SRP_ugL[new$DateTime=="2021-08-19 12:00:00" & new$Reservoir=="CCR" & new$Depth_m==21 & new$Site==50] <- NA
new$TN_ugL[new$DateTime=="2021-09-23 13:55:00" & new$Reservoir=="CCR" & new$Depth_m==20] <- NA
new$TN_ugL[new$DateTime=="2021-11-19 12:11:00" & new$Reservoir=="CCR" & new$Depth_m==0.1 & new$Site==50] <- NA
new$TP_ugL[new$DateTime=="2021-07-12 09:40:00" & new$Reservoir=="FCR" & new$Depth_m==3.8] <- NA


#merge old and new dfs
chem <- rbind(old, new) 

#replace NA flags with 0
chem$Flag_DateTime[is.na(chem$Flag_DateTime)] <- 0
chem$Flag_TN[is.na(chem$Flag_TN)] <- 0
chem$Flag_TP[is.na(chem$Flag_TP)] <- 0
chem$Flag_NH4[is.na(chem$Flag_NH4)] <- 0
chem$Flag_NO3NO2[is.na(chem$Flag_NO3NO2)] <- 0
chem$Flag_SRP[is.na(chem$Flag_SRP)] <- 0
chem$Flag_DOC[is.na(chem$Flag_DOC)] <- 0
chem$Flag_DIC[is.na(chem$Flag_DIC)] <- 0
chem$Flag_DC[is.na(chem$Flag_DC)] <- 0
chem$Flag_DN[is.na(chem$Flag_DN)] <- 0

write.csv(chem, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2021/chemistry_2013_2021.csv",row.names = FALSE)

#select columns for plotting
raw_chem <- chem [,(names(chem) %in% c("Reservoir","Site","DateTime",
                                               "Depth_m","Rep","TP_ugL","TN_ugL","NH4_ugL","SRP_ugL","NO3NO2_ugL","DOC_mgL"))]


#### Chemistry diagnostic plots ####
chemistry_long <- raw_chem %>% 
  gather(metric, value, TN_ugL:DOC_mgL) %>% 
  mutate(month = strftime(DateTime, "%b")) %>%
  mutate(DateTime = as.Date(DateTime))

# FCR deep hole data time series plot
ggplot(subset(chemistry_long, metric=="TN_ugL" & Depth_m==0.1 & Reservoir=="FCR"), aes(x=DateTime, y=value )) +
geom_point(cex=2) + theme_bw()


#plots for each variable by reservoir 
fcrplots <- chemistry_long %>% 
  filter(Reservoir == 'FCR') %>% 
  #filter(Site == 50) %>% 
  #filter(Depth_m %in% c(0.1, 1.6, 3.8, 5, 6.2, 8, 9)) %>%  #If you want to just see the routine depths 
  ggplot(aes(x = DateTime, y = value, color = as.factor(Depth_m)))+
  geom_point()+
  ggtitle("FCR chem")+
  theme_bw()+
  facet_wrap(~metric, scales = 'free_y')

fcrplots
#ggplotly(fcrplots) #interactive plot 

bvrplots <- chemistry_long %>% 
  filter(Reservoir == 'BVR') %>% 
  # filter(Site == 50) %>% 
  ggplot(aes(x = DateTime, y = value, color = as.factor(Depth_m)))+
  geom_point()+
  ggtitle("BVR chem")+
  theme_bw()+
  facet_wrap(~metric, scales = 'free_y')

bvrplots

ccrplots <- chemistry_long %>% 
  filter(Reservoir == 'CCR') %>% 
  # filter(Site == 50) %>% 
  ggplot(aes(x = DateTime, y = value, color = as.factor(Depth_m)))+
  geom_point()+
  ggtitle("CCR chem")+
  theme_bw()+
  facet_wrap(~metric, scales = 'free_y')

ccrplots


# (install and) Load EMLassemblyline #####
# install.packages('devtools')

#devtools::install_github("EDIorg/EMLassemblyline")
#note that EMLassemblyline has an absurd number of dependencies and you
#may exceed your API rate limit; if this happens, you will have to wait an
#hour and try again or get a personal authentification token (?? I think)
#for github which allows you to submit more than 60 API requests in an hour
library(EMLassemblyline)


#Step 1: Create a directory for your dataset
#in this case, our directory is Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2018

#Step 2: Move your dataset to the directory

#Step 3: Identify an intellectual rights license
#ours is CCBY

#Step 4: Identify the types of data in your dataset
#right now the only supported option is "table"; happily, this is what 
#we have!

#Step 5: Import the core metadata templates

#for our application, we will need to generate all types of metadata
#files except for taxonomic coverage, as we have both continuous and
#categorical variables and want to report our geographic location

# View documentation for these functions
?template_core_metadata
?template_table_attributes
?template_categorical_variables #don't run this till later
?template_geographic_coverage

# Import templates for our dataset licensed under CCBY, with 1 table.
template_core_metadata(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2021",
                 license = "CCBY",
                 file.type = ".txt",
                 write.file = TRUE)

template_table_attributes(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2021",
                       data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2021",
                       data.table = "chemistry_2013_2021.csv",
                       write.file = TRUE)


#we want empty to be true for this because we don't include lat/long
#as columns within our dataset but would like to provide them
template_geographic_coverage(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2021",
                          data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2021",
                          data.table = "chemistry_2013_2021.csv",
                          empty = TRUE,
                          write.file = TRUE)

#Step 6: Script your workflow
#that's what this is, silly!

#Step 7: Abstract
#copy-paste the abstract from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics
# updated for 2019 to include info on RC sites

#Step 8: Methods
#copy-paste the methods from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 9: Additional information
#I put the notes about FCR manipulations and pubs documenting it in this file

#Step 10: Keywords
#DO NOT EDIT KEYWORDS FILE USING A TEXT EDITOR!! USE EXCEL!!
#not sure if this is still true...let's find out! :-)
#see the LabKeywords.txt file for keywords that are mandatory for all Carey Lab data products

#Step 11: Personnel
#copy-paste this information in from your metadata document
#Cayelan needs to be listed several times; she has to be listed separately for her roles as
#PI, creator, and contact, and also separately for each separate funding source (!!)
# Updated this for 2019 to include HLW and WMW

#Step 12: Attributes
#grab attribute names and definitions from your metadata word document
#for units....
# View and search the standard units dictionary
#view_unit_dictionary()
#put flag codes and site codes in the definitions cell
#force reservoir to categorical

#Step 13: Close files
#if all your files aren't closed, sometimes functions don't work

#Step 14: Categorical variables
# Run this function for your dataset
#THIS WILL ONLY WORK once you have filled out the attributes_chemistry.txt and
#identified which variables are categorical
template_categorical_variables(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2021",
                               data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2021",
                               write.file = TRUE)

#open the created value IN A SPREADSHEET EDITOR and add a definition for each category

#Step 15: Geographic coverage
#copy-paste the bounding_boxes.txt file (or geographic_coverage.txt file) that is Carey Lab specific into your working directory

## Step 16: Obtain a package.id. ####
# Go to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using one of the Carey Lab usernames and passwords.

# Select Tools --> Data Package Identifier Reservations and click 
# "Reserve Next Available Identifier"
# A new value will appear in the "Current data package identifier reservations" 
# table (e.g., edi.123)
# Make note of this value, as it will be your package.id below

#Step 17: Make EML
# View documentation for this function
?make_eml

# Run this function
make_eml(
  path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2021",
  data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2021",
  eml.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2021",
  dataset.title = "Water chemistry time series for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2021",
  temporal.coverage = c("2013-04-04", "2022-04-19"),
  maintenance.description = 'ongoing',
  data.table = "chemistry_2013_2021.csv",
  data.table.description = "Reservoir water chemistry dataset",
  other.entity = "2021_chemistry_collation.R",
  other.entity.description = "Nutrient QAQC script",
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.890.3') #reserve new staging environment package id each year

## Step 8: Check your data product! ####
# Return to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using one of the Carey Lab usernames and passwords. 

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File", 
# choose your metadata (.xml) file (e.g., edi.270.1.xml), check "I want to 
# manually upload the data by selecting files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder), 
# then click Upload. Files will upload and your EML metadata will be checked 
# for errors. If there are no errors, your data product is now published! 
# If there were errors, click the link to see what they were, then fix errors 
# in the xml file. 
# Note that each revision results in the xml file increasing one value 
# (e.g., edi.270.1, edi.270.2, etc). Re-upload your fixed files to complete the 
# evaluation check again, until you receive a message with no errors.

## Step 9: PUBLISH YOUR DATA! ####
#  #DO NOT REQUEST A NEW PACKAGE ID FOR UPDATING THE CHEMISTRY DATASET
#  SIMPLY INCREASE THE LAST DIGIT OF THE PREVIOUS PACKAGE ID BY 1 TO UPDATE THE CURRENT PUBLICATION
# DIRECTIONS ON HOW TO GET A NEW ID ARE HERE, BUT DO NOT USE THEM FOR ALREADY PUBLISHED DATASETS BEING UPDATED (E.G. CHEMISTRY, CATWALK, CTD, ETC.)
# NEVER ASSIGN this identifier to a staging environment package.
# Go to the EDI Production environment (https://portal.edirepository.org/nis/home.jsp)
# and login using the ccarey (permanent) credentials. 

# Select Tools --> Data Package Identifier Reservations and click "Reserve Next 
# Available Identifier". A new value will appear in the "Current data package 
# identifier reservations" table (e.g., edi.518)
# This will be your PUBLISHED package.id

# In the make_eml command below, change the package.id to match your 
# PUBLISHED package id. This id should end in .1 (e.g., edi.518.1)

# ALL OTHER entries in the make_eml() command should match what you ran above,
# in step 7

make_eml(
  path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2021",
  data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2021",
  eml.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2021",
  dataset.title = "Water chemistry time series for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2021",
  temporal.coverage = c("2013-04-04", "2022-04-19"),
  maintenance.description = 'ongoing',
  data.table = "chemistry_2013_2021.csv",
  data.table.description = "Reservoir water chemistry dataset",
  other.entity = "2021_chemistry_collation.R",
  other.entity.description = "Nutrient QAQC script",
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.199.9') #DO NOT REQUEST A NEW PACKAGE ID, SIMPLY INCREASE THE LAST DIGIT HERE BY 1 TO UPDATE THE CURRENT PUBLICATION

# Once your xml file with your PUBLISHED package.id is Done, return to the 
# EDI Production environment (https://portal.edirepository.org/nis/home.jsp)

# Select Tools --> Preview Your Metadata, then upload your metadata (.xml) file 
# associated with your PUBLISHED package.id. Look through the rendered 
# metadata one more time to check for mistakes (author order, bounding box, etc.)

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File", 
# choose your metadata (.xml) file associated with your PUBLISHED package.id 
# (e.g., edi.518.1.xml), check "I want to manually upload the data by selecting 
# files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder), 
# then click Upload. Files will upload and your EML metadata will be checked for 
# errors. Since you checked for and fixed errors in the staging environment, this 
# should run without errors, and your data product is now published! 

# Click the package.id hyperlink to view your final product! HOORAY!