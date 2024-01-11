##MakeEML_MetData
##Author: Cayelan Carey
##Date: 21 July 2019
## Updated: 07 Feb 2020, A. Hounshell


# # Install devtools


library(devtools)
install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_FCRMetData/2023"


#Step 1: Create a directory for your dataset
#in this case, our directory is Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_MetData

#Step 2: Move your dataset to the directory

#Step 3: Create an intellectual rights license
#ours is CCBY

#Step 4: Identify the types of data in your dataset
#right now the only supported option is "table"

#Step 5: Import the core metadata templates
#Prepare metadata file templates using the 
template_table_attributes(
 path = folder,
 data.path = folder,
 data.table = c('FCRMet_2015_2023.csv', "FCRMet_MaintenanceLog_2015_2023.csv", "FCRMet_Infrad_DOY_Avg_2018.csv"))
  
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
#This is where the authorship contribution statement goes

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
#view_unit_dictionary()
#put flag codes and site codes in the definitions cell
#force reservoir to categorical

#Step 13: Close files
#if all your files aren't closed, sometimes functions don't work

#Step 14: Categorical variables
# View documentation for this function

?template_categorical_variables

# Run this function for your dataset

template_categorical_variables(path = folder,
                               data.path = folder,
                               write.file = TRUE)

#open the created value IN A SPREADSHEET EDITOR and add a definition for each category

#Step 15: Geographic coverage
#copy-paste the bounding_boxes.txt file that is Carey Lab specific into your working directory
template_geographic_coverage(path = folder,
                             data.path = folder,
                             data.table = 'FCR_Met_final_2015_2022.csv',
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
make_eml(path = folder,
         data.path = folder,
         eml.path = folder,
         dataset.title = "Time series of high-frequency meteorological data at Falling Creek Reservoir, Virginia, USA 2015-2023",
         data.table = c('FCRMet_2015_2023.csv','FCRMet_MaintenanceLog_2015_2023.csv', 'FCRMet_Infrad_DOY_Avg_2018.csv'),
         data.table.name = c('FCRMet_2015_2023','FCRMet_MaintenanceLog_2015_2023', 'FCRMet_Infrad_DOY_Avg_2018'),
         data.table.description = c('All meteorological parameters measured at Falling Creek Reservoir during 2015-2023',
                            'The log for all maintenance and outliers for the Met station that go into the QAQC script for 2015-2023',
                            'Infrared average and standard deviation for day of year in 2018. Used for QAQC in the script.'),
         other.entity = c('FCRMet_qaqc_2015_2023.R', 'FCRMet_inspection_2015_2023.Rmd', 'Plotting_function.R'),
         other.entity.name = c('FCRMet_qaqc_2015_2023', 'FCRMet_inspection_2015_2023', 'Plotting_function'),
         other.entity.description = c('Script used to take out observations from the dataset based on the maintenance log and other outliers. Also known as the L1 function.',
                                      'Inspection script creates QAQC plots and downloads the necessary files for publication.',
                                      'A function used to create the QAQC plots in the inspection script'),
         temporal.coverage = c("2015-07-07", "2023-12-31"),
         #geographic.description = c("Falling Creek Reservoir, Vinton, Virginia, USA"), #have it in a .txt file
         #geographic.coordinates = c('37.309589', '-79.836009', '37.302660', '-79.839249'), #N, E, S, W
         maintenance.description = "ongoing", 
         user.id = "ccarey",
         user.domain = 'EDI',
         package.id = "edi.143.24") # Put your package.id here, for staging use "edi.143.#",for the final version package id for 2021 was "edi.389.6"


#PROBLEMS WITH MAKING METATDATA! SO, COLIN SUGGESTED THAT THE FALLING CREEK SPACE IN THE PATH NAME WAS
#  PROBLEMATIC, SO I COPIED AND PASTED THE ENTIRE DIRECTORY TO MY DESKTOP AND RAN THE MAKE_EML PATH THERE. THAT SEEMED TO WORK
# ??!!! SO AM COPYING & PASTING THE .XML FILE BACK INTO THE GITHUB DIRECTORY. WORTH A TRY TO RUN IT OUT OF THERE
# NEXT TIME WE UPDATE THE MET DATA IN THE FUTURE. I ALSO DELETED THE ZIP FILES 
setwd("/Users/cayelan/Desktop/MakeEML_MetData")
make_eml(path = folder,
         data.path = folder,
         eml.path = folder,
         dataset.title = "Time series of high-frequency meteorological data at Falling Creek Reservoir, Virginia, USA 2015-2021",
         data.table = 'FCR_Met_final_2015_2021.csv',
         data.table.name = 'FCR_Met_final_2015_2021',
         data.table.description = 'All meteorological parameters measured at Falling Creek Reservoir during 2015-2021',
         other.entity = c('FCR_MET_QAQC_2021.R','FCR_Met_Maintenance_2015_2021.csv'),
         other.entity.name = c('FCR_MET_QAQC_2021',"FCR_Met_Maintenance_2015_2021"),
         other.entity.description = c('Data aggregation and QA/QC R script','The log of all maintenance applied to the meteorological station'),
         temporal.coverage = c("2015-07-07", "2021-12-31"),
         #geographic.description = c("Falling Creek Reservoir, Vinton, Virginia, USA"), #have it in a .txt file
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
