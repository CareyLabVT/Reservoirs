# Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools,EMLassemblyline,tidyverse)

library(devtools)
devtools::install_github("ropensci/bold")
devtools::install_github("EDIorg/taxonomyCleanr")
remotes::install_github("ropensci/taxize", dependencies = TRUE)
remotes::install_github("EDIorg/EMLassemblyline")


#note that EMLassemblyline has an absurd number of dependencies and you
#may exceed your API rate limit; if this happens, you will have to wait an
#hour and try again or get a personal authentification token (?? I think)
#for github which allows you to submit more than 60 API requests in an hour
library(EMLassemblyline)

# All the files you need are found in this folder
folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_UGGA/EDI 2024/"

#Step 1: Create a directory for your dataset


#Step 2: Move your dataset to the directory or save it directly into directory, most inspection scripts should do this

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
# template_core_metadata(path = folder,
#                        license = "CCBY",
#                        file.type = ".txt",
#                        write.file = TRUE)
template_table_attributes(path = folder,
                          data.path = folder,
                          data.table = c("ugga_2018_2024.csv", "ugga_maintenancelog_2018_2024.csv"),
                          write.file = TRUE)
#we want empty to be true for this because we don't include lat/long
#as columns within our dataset but would like to provide them
# template_geographic_coverage(path = folder,
#                              data.path = folder,
#                              data.table = c("ugga_2018_2024.csv"),
#                              empty = TRUE,
#                              write.file = TRUE)
#Step 6: Script your workflow
#that's what this is, silly!

#Step 7: Abstract
#copy-paste the abstract from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 8: Methods
#copy-paste the methods from your Microsoft Word document into methods.txt
#if you want to check your methods for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 9: Additional information
# Include authorship statement in the additional information

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

#Step 14: Categorical variables
# Run this function for your dataset
#THIS WILL ONLY WORK once you have filled out the attributes_chemistry.txt and
#identified which variables are categorical
template_categorical_variables(path = folder,
                               data.path = folder,
                               write.file = TRUE)

#Step 15: Geographic coverage
#copy-paste the bounding_boxes.txt file (or geographic_coverage.txt file) that is Carey Lab specific into your working directory

#Step 16: Custom units
# Copy and paste custom units .txt file from prior year

## Step 17: Obtain a package.id FROM STAGING ENVIRONMENT. ####
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
## Make EML for staging environment
## NOTE: Will need to check geographic coordinates!!!

make_eml(path = folder,
         data.path = folder,
         eml.path = folder,
         dataset.title = "Time series of methane and carbon dioxide diffusive fluxes using an Ultraportable Greenhouse Gas Analyzer (UGGA) for Falling Creek Reservoir and Beaverdam Reservoir in southwestern Virginia, USA during 2018-2024",
         data.table = c("ugga_2018_2024.csv",
                        "site_descriptions.csv",
                        "ugga_maintenancelog_2018_2024.csv"),
         #data.table.name = c("UGGA dataset 2018-2024",
     #                        "Sample site descriptions"),
         data.table.description = c("UGGA diffusive flux dataset from FCR and BVR",
                                    "Descriptions of sampling sites, including lat/long",
                                    "UGGA maitenance log to flag and/or remove erronious casts"),
         other.entity = c('RawData.zip', 
                          "ugga_inspection_2018_2024.Rmd", 
                          "ugga_qaqc_2018_2024.R", 
                          "ugga_FluxCalR_2024.R"),
         #other.entity.name = c('Raw UGGA files', 
                               # "Data visualization script", 
                               # "Data compilation script for 2024", 
       #                        "Data processing script for 2024"),
         other.entity.description = c('Raw data from the Ultraportable Greenhouse Gas Analyzer', 
                                      "Code used to visualize UGGA data from 2018-2024",
                                      "Code used to compile and format 2018-2024 data (after data processing)",
                                      "Code used to process raw 2024 files"),
         temporal.coverage = c("2018-05-07", "2024-12-03"),
         maintenance.description = "ongoing",
         user.domain = "EDI",
         user.id = "ccarey",
         package.id = "edi.1102.5") # this is for staging !
     #package.id = "edi.1082.3") #edi.1082.2 is the most recent published version
