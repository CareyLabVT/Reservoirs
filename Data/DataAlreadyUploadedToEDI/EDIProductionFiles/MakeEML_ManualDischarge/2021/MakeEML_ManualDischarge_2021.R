# Install devtools
#install.packages("devtools")

# Load devtools
library(devtools)
library(tidyverse)
library(lubridate)


# Install and load EMLassemblyline
#package_url <- 'https://cran.r-project.org/bin/windows/contrib/3.5/EML_1.0.3.zip'
#install.packages(package_url, repos = NULL, type = 'source')

#install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

#Step 1: Create a directory for your dataset
#in this case, our directory is Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/Jan2021

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
#?template_core_metadata
#?template_table_attributes
#?template_categorical_variables #don't run this till later
#?template_geographic_coverage


# Import templates for our dataset licensed under CCBY, with 1 table.
template_core_metadata(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ManualDischarge/2021",
                       license = "CCBY",
                       file.type = ".txt",
                       write.file = TRUE)

template_table_attributes(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ManualDischarge/2021",
                          data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ManualDischarge/2021",
                          data.table = c("ManualDischarge_2019_2021.csv"),
                          write.file = TRUE)


#we want empty to be true for this because we don't include lat/long
#as columns within our dataset but would like to provide them
template_geographic_coverage(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ManualDischarge/2021",
                             data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ManualDischarge/2021",
                             data.table = c("Inflow_2013_2021.csv"),
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
#copy-paste the methods from your Microsoft Word document into methods.txt
#if you want to check your methods for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 9: Additional information
# Add additional information for authorship contribution statement!

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
#view_unit_dictionary()

define_catvars(".")

make_eml(path = ".",
         dataset.title = "Manually-collected discharge data for multiple inflow tributaries entering Falling Creek Reservoir, Beaverdam Reservoir, and Carvin's Cove Reservoir, Vinton, Virginia, USA from 2019-2021",
         data.files = "2019_Continuum_Discharge",
         data.files.description = "Reservoir Continuum Manual Discharge Data",
         temporal.coverage = c("2019-02-08", "2019-10-30"),
         maintenance.description = "ongoing",
         user.id =  "ccarey",
         other.entity = c('calculate_discharge_flowmate_data.R', 
                          'SOP for Manual Reservoir Continuum Discharge Data Collection and Calculation'),
         other.entity.description = c('Script used to calculate discharge from flowmate measurements', 
                                      'SOPs for discharge data collection and calculation using flowmeter, salt injection, and velocity float methods') ,
         package.id = "edi.454.4", #### this is the one that I need to change!!!
         user.domain = 'EDI')


