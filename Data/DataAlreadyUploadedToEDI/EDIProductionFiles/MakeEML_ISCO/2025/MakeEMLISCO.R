##MakeEML_ISCO
##Author: Mary Lofton
##Date: 07SEP19
##Edited for ISCO by: CEB 07May25


#good site for step-by-step instructions
#https://ediorg.github.io/EMLassemblyline/articles/overview.html
#and links therein

library(tidyverse)

# (install and) Load EMLassemblyline #####

install.packages('devtools')
library(devtools)
install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

#note that EMLassemblyline has an absurd number of dependencies and you
#may exceed your API rate limit; if this happens, you will have to wait an
#hour and try again or get a personal authentification token (?? I think)
#for github which allows you to submit more than 60 API requests in an hour


#Step 1: Create a directory for your dataset
#in this case, our directory is C:/FCR_BVR Metals Data/EDI

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
??template_core_metadata
??template_table_attributes
??template_categorical_variables #don't run this till later
??template_geographic_coverage

# Import templates for our dataset licensed under CCBY, with 1 table.
template_core_metadata(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ISCO/2025",
                       license = "CCBY",
                       file.type = ".txt",
                       write.file = TRUE)

template_table_attributes(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ISCO/2025",
                          data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ISCO/2025",
                          data.table = c("isco_2019_2024.csv", 'site_descriptions.csv', 'isco_maintenancelog_2019_2024.csv'),
                          write.file = TRUE)



#we want empty to be true for this because we don't include lat/long
#as columns within our dataset but would like to provide them
template_geographic_coverage(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ISCO/2025",
                             data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ISCO/2025",
                             data.table = c("isco_2019_2024.csv", 'site_descriptions.csv'),
                             empty = TRUE,
                             write.file = TRUE)

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
#I put the notes about FCR manipulations and pubs documenting it in this file

#Step 10: Keywords
#DO NOT EDIT KEYWORDS FILE USING A TEXT EDITOR!! USE EXCEL!!
#not sure if this is still true...let's find out! :-)
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
# Run this function for your dataset
#THIS WILL ONLY WORK once you have filled out the attributes_chemistry.txt and
#identified which variables are categorical
template_categorical_variables(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ISCO/2025",
                               data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ISCO/2025",
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
  path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ISCO/2025",
  data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ISCO/2025",
  eml.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ISCO/2025",
  dataset.title = "Time series of total metal and nutrient loads into Falling Creek Reservoir in southwestern Virginia, USA from 2019-2024",
  temporal.coverage = c("2019-06-06", "2024-11-19"),
  maintenance.description = 'ongoing',
  data.table = c("isco_2019_2024.csv", "isco_maintenancelog_2019_2024.csv"),
  data.table.description = c("Cumulative flow and total metal and nutrient loads", 'Log describing dataset maintenance'),
  other.entity = c('isco_qaqc_2019_2024.R', 'isco_inspection_2019_2024.Rmd'),
  other.entity.description = c("QAQC script which takes the output from sample collection and analysis, cleans data, and flags errant data", 'Script that uses isco_qaqc_2019_2024.R script to create the final dataframe and plots the data'),
  user.id = 'mschreib',
  user.domain = 'EDI',
  package.id = 'edi.1308.10') # This package identifier is only for the staging environment

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
# Reserve a package.id for your error-free data package. 
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
  path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ISCO/2025",
  data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ISCO/2025",
  eml.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ISCO/2025",
  dataset.title = "Time series of total metal and nutrient loads into Falling Creek Reservoir in southwestern Virginia, USA from 2019-2024",
  temporal.coverage = c("2019-06-06", "2024-11-19"),
  maintenance.description = 'ongoing',
  data.table = c("isco_2019_2024.csv", "isco_maintenancelog_2019_2024.csv"),
  data.table.description = c("Cumulative flow and total metal and nutrient loads", 'Log describing dataset maintenance'),
  other.entity = c('isco_qaqc_2019_2024.R', 'isco_inspection_2019_2024.Rmd'),
  other.entity.description = c("QAQC script which takes the output from sample collection and analysis, cleans data, and flags errant data", 'Script that uses isco_qaqc_2019_2024.R script to create the final dataframe and plots the data'),
  user.id = 'mschreib',
  user.domain = 'EDI',
  package.id = 'edi.2035.1') # This is the package identifer for the production environment

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