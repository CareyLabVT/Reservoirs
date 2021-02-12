##MakeEML_BVRplatformData
##Author: Cayelan Carey
##Date: 21 July 2019
## Updated: 10 Feb 2021, A. Breef-Pilz

setwd("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform")
# # Install devtools
install.packages("devtools")
# 
# # Load devtools
library(devtools)
# 
devtools::install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

#Step 1: Create a directory for your dataset
#in this case, our directory is Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform

#Step 2: Move your dataset to the directory

#Step 3: Create an intellectual rights license
#ours is CCBY

#Step 4: Identify the types of data in your dataset
#right now the only supported option is "table"

#Step 5: Import the core metadata templates
#Prepare metadata file templates using the 
?template_table_attributes
template_table_attributes(
 path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform",
 data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform",
 data.table = 'BVR_EDI_2020.csv',
 write.file=TRUE)
  
# command. **Note:** 'import_templates' command currently (Dec. 2018) only works 
# for data products that include table-based data files (e.g., .csv). To 
# prepare module metadata files, manually copy the following metadata file 
# templates from a previous module directory (e.g., the Module 1 EDI folder):
# - start with the metadata template word doc and then populate for all of the text files, which include:
# - abstract.txt
# - intellectual_rights.txt (we use CCBY); won't be altered
# - keywords.txt (EDIT THIS FILE IN EXCEL; see LabKeywords.txt for Carey 
# Lab-specific keywords) and also http://vocab.lternet.edu/vocab/vocab/index.php
# https://environmentaldatainitiative.org/resources/five-phases-of-data-publishing/phase-3/controlled-vocabularies/
# if there is not a word in the existing vocabularies, make it:
# "carey lab controlled vocabulary"
# - methods.txt
# - personnel.txt (EDIT THIS FILE IN EXCEL) Author order in the citation is in the order that 'creators' are listed in this file
# Edit each of these files for your current module upload, by copying and 
# pasting the relevant information from the EDI_metadata_template you prepared

# Important! Before saving, check that the contents of each .txt file do not include any 
# non-allowed characters by going to: https://pteo.paranoiaworks.mobi/diacriticsremover/, 
# pasting your text, and clicking remove diacritics. copy and paste that text back into the .txt file.

# After saving each file, make sure it is closed.

#Step 6: Script your workflow
#that's what this is, silly!

#Step 7: Abstract
#copy-paste the abstract from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 8: Methods
#copy-paste the methods from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 9: Additional information
#nothing mandatory for Carey Lab in this section

#Step 10: Keywords
#DO NOT EDIT KEYWORDS FILE USING A TEXT EDITOR!! USE EXCEL!!
#see the LabKeywords.txt file for keywords that are mandatory for all Carey Lab data products

#Step 11: Personnel
#copy-paste this information in from your metadata document
#Cayelan needs to be listed several times; she has to be listed separately for her roles as
#PI, creator, and contact, and also separately for each separate funding source (!!)

#Step 12: Attributes
#grab attribute names and definitions from your metadata word document
#for units....
# View and search the standard units dictionary
view_unit_dictionary()
#put flag codes and site codes in the definitions cell
#force reservoir to categorical

#Step 13: Close files
#if all your files aren't closed, sometimes functions don't work

#Step 14: Categorical variables
# View documentation for this function

?template_categorical_variables

# Run this function for your dataset

template_categorical_variables(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform",
                               data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform",
                               write.file = TRUE)

#open the created value IN A SPREADSHEET EDITOR and add a definition for each category

#Step 15: Geographic coverage
#copy-paste the bounding_boxes.txt file that is Carey Lab specific into your working directory
template_geographic_coverage(path = "C:/R/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform",
                             data.path = "C:/R/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform",
                             data.table = 'BVR_EDI_2020.csv',
                             empty = TRUE,
                             write.file = TRUE)

#Step 16: Make EML
# View documentation for this function
?make_eml

## Step XXX: Obtain a package.id. ####
# Go to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using one of the Carey Lab usernames and passwords. 

# Select Tools --> Data Package Identifier Reservations and click 
# "Reserve Next Available Identifier"
# A new value will appear in the "Current data package identifier reservations" 
# table (e.g., edi.123)
# Make note of this value, as it will be your package.id below

## Step XXX: Make EML metadata file using the EMLassemblyline::make_eml() command ####
# For modules that contain only zip folders, modify and run the following 
# ** double-check that all files are closed before running this command! **

# You will need to modify the following lines to match your current module: 
# path: set to your computer's FULL file path!
# dataset.title: Update to current module
# zip.dir: Change the name of the module files zip folder
# temporal.coverage: Update the dates
# package.id: enter the ID you obtained in Step 6
make_eml(path = "C:/R/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_BVRplatform",
         dataset.title = "Time series of high-frequency sensor data measuring water temperature, dissolved oxygen, pressure, conductivity, 
         specific conductance, total dissolved solids, chlorophyll a, phycocyanin, and fluorescent dissolved organic matter at discrete depths 
         in Beaverdam Reservoir, Virginia, USA in 2020",
         data.table = c('BVR_EDI_2020.csv', 'BVR_Maintenance_2020.csv', 'BVR_Depth_offsets_2020.csv'),
         data.table.name = c('BVR_EDI_2020', 'BVR_Maintenance_2020', 'BVR_Depth_offsets_2020'),
         data.table.description = c('All water quality parameters measured at Beaverdam during 2020',
                                    'The log of all maintenance applied to the sensors',
                                    'The position, offset, and distance above the sediment for the sensors'),
         other.entity = c('MET_QAQC_2020.R', 'BVR_sort_by_depth_2020.R'),
         other.entity.name = c('MET_QAQC_2020', 'BVR_sort_by_depth'),
         other.entity.description = c('Data aggregation and QA/QC R script','Applying the depth offset and sorting by sensor depth'),
         temporal.coverage = c("2020-06-18", "2020-12-31"),
         #geographic.description = c("Beaverdam Reservoir, Vinton, Virginia, USA"), #have it in a .txt file
         #geographic.coordinates = c('37.309589', '-79.836009', '37.302660', '-79.839249'), #N, E, S, W
         maintenance.description = "ongoing", 
         user.id = "ccarey",
         user.domain = 'EDI',
         package.id = "edi.143.4") # Put your package.id here, followed by .1 (for 1st version)


#PROBLEMS WITH MAKING METATDATA! SO, COLIN SUGGESTED THAT THE FALLING CREEK SPACE IN THE PATH NAME WAS
#  PROBLEMATIC, SO I COPIED AND PASTED THE ENTIRE DIRECTORY TO MY DESKTOP AND RAN THE MAKE_EML PATH THERE. THAT SEEMED TO WORK
# ??!!! SO AM COPYING & PASTING THE .XML FILE BACK INTO THE GITHUB DIRECTORY. WORTH A TRY TO RUN IT OUT OF THERE
# NEXT TIME WE UPDATE THE MET DATA IN THE FUTURE. I ALSO DELETED THE ZIP FILES 
setwd("/Users/cayelan/Desktop/MakeEML_MetData")
make_eml(path = "/Users/cayelan/Desktop/MakeEML_MetData",
         dataset.title = "Time series of high-frequency meteorological data at Falling Creek Reservoir, Virginia, USA 2015-2018",
         data.table = c('Met_final_2015_2018.csv', 'Met_Maintenance_2015_2018.csv'),
         data.table.description = c('All meteorological parameters measured at Falling Creek Reservoir during 2015-2018',
                                    'The log of all maintenance applied to the meteorological station'),
         other.entity = 'Met_QAQC.R',
         other.entity.description = 'Data aggregation and QA/QC R script',
         temporal.coverage = c("2015-07-07", "2018-12-31"),
         #geographic.description = c("Falling Creek Reservoir, Vinton, Virginia, USA"),#have it in a .txt file
         #geographic.coordinates = c('37.309589', '-79.836009', '37.302660', '-79.839249'), #N, E, S, W
         maintenance.description = "ongoing", 
         user.id = "ccarey",
         user.domain = 'EDI',
         package.id = "edi.389.2") # Put your package.id here, followed by .1 (for 1st version)


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
