# Install devtools
#install.packages("devtools")

# Load devtools
library(devtools)
library(tidyverse)
library(lubridate)

# add site descriptions file
#Install the required googlesheets4 package
#install.packages('googlesheets4')
#Load the library 
library(googlesheets4)
sites <- read_sheet('https://docs.google.com/spreadsheets/d/1TlQRdjmi_lzwFfQ6Ovv1CAozmCEkHumDmbg_L4A2e-8/edit#gid=124442383')
data <- edi
trim_sites = function(data,sites){
  data_res_site=data%>% #Create a Reservoir/Site combo column
    mutate(res_site = trimws(paste0(Reservoir,Site)))
  sites_merged = sites%>% #Filter to Sites that are in the dataframe
    mutate(res_site = trimws(paste0(Reservoir,Site)))%>%
    filter(res_site%in%data_res_site$res_site)%>%
    select(-res_site)
}
sites_trimmed = trim_sites(data,sites) 
write.csv(sites_trimmed, file.path(folder, "site_descriptions.csv"), row.names=F)# Write to file


# Install and load EMLassemblyline
#package_url <- 'https://cran.r-project.org/bin/windows/contrib/3.5/EML_1.0.3.zip'
#install.packages(package_url, repos = NULL, type = 'source')

#install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

#Step 1: Create a directory for your dataset
#in this case, our directory is Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/Jan2021
folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ManualDischarge/2022"
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
template_core_metadata(path = folder,
                       license = "CCBY",
                       file.type = ".txt",
                       write.file = TRUE)

template_table_attributes(path = folder,
                          data.path = folder,
                          data.table = c("ManualDischarge_2019_2022.csv", "site_descriptions.csv"),
                          write.file = TRUE)

# categorical variables
template_categorical_variables(path = folder,
                               data.path = folder,
                               write.file = TRUE)

#we want empty to be true for this because we don't include lat/long
#as columns within our dataset but would like to provide them
template_geographic_coverage(path = folder,
                             data.path = folder,
                             data.table = c("ManualDischarge_2019_2022.csv"),
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

template_categorical_variables(path = folder,
                               data.path = folder,
                               write.file = TRUE)



## Step 17: Obtain a package.id FROM STAGING ENVIRONMENT. ####
# Go to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using one of the Carey Lab usernames and passwords. 

# Select Tools --> Data Package Identifier Reservations and click 
# "Reserve Next Available Identifier"
# A new value will appear in the "Current data package identifier reservations" 
# table (e.g., edi.123)
# Make note of this value, as it will be your package.id below
make_eml(path = folder,
         dataset.title = "Manually-collected discharge data for multiple inflow tributaries entering Falling Creek Reservoir, Beaverdam Reservoir, and Carvins Cove Reservoir, Vinton and Roanoke, Virginia, USA from 2019-2022",
         data.table = c("ManualDischarge_2019_2022.csv", "site_descriptions.csv"),
         data.table.description = c("Manual Discharge Data",  'Descriptions of sampling sites'),
         temporal.coverage = c("2019-02-08", "2022-12-12"),
         maintenance.description = "ongoing",
         user.id =  "ccarey",
         other.entity = c('Collate_QAQC_ManualDischarge_2022.R', 
                          'SOP for Manual Reservoir Continuum Discharge Data Collection and Calculation.pdf',
                          'CCR_VolumetricFlow_2020_2022_calculations.xlsx'),
         other.entity.description = c('Script used to collate and QAQC data for publication', 
                                      'SOPs for discharge data collection and calculation using flowmeter, salt injection, velocity float, and bucket volumetric methods',
                                      'Example spreadsheet which demonstrates the float method and bucket volumetric method calculations') ,
         package.id = "edi.1017.3", #### this is the one that I need to change!!!
         user.domain = 'EDI')

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

## Step 17: Obtain a package.id. ####
# this is a revision to an existing data package: already have an identifier: 202.X (x = revision number)
## NOTE: Will need to check geographic coordinates!!!

make_eml(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ManualDischarge/2021",
         dataset.title = "Manually-collected discharge data for multiple inflow tributaries entering Falling Creek Reservoir, Beaverdam Reservoir, and Carvins Cove Reservoir, Vinton and Roanoke, Virginia, USA from 2019-2021",
         data.table = "ManualDischarge_2019_2021.csv",
         data.table.description = "Reservoir Continuum Manual Discharge Data",
         temporal.coverage = c("2019-02-08", "2021-11-19"),
         maintenance.description = "ongoing",
         user.id =  "ccarey",
         other.entity = c('Collate_QAQC_ManualDischarge_2021.R', 
                          'SOP for Manual Reservoir Continuum Discharge Data Collection and Calculation.pdf',
                          'calculate_discharge_flowmate_data_Jan2022.R',
                          'CCR_VolumetricFlow_discharge_calc_example.xlsx'),
         other.entity.description = c('Script used to collate and QAQC data for publication', 
                                      'SOPs for discharge data collection and calculation using flowmeter, salt injection, velocity float, and bucket volumetric methods',
                                      'Script used to calculate discharge using flowmate velocity data',
                                      'Example spreadsheet which demonstrates the bucket volumetric calculation') ,
         package.id = "edi.454.5", #### this is the one that I need to change!!!
         user.domain = 'EDI')

## Step 18: Upload revision to EDI
# Go to EDI website: https://portal.edirepository.org/nis/home.jsp and login with Carey Lab ID
# Click: Tools then Evaluate/Upload Data Packages
# Under EML Metadata File, select 'Choose File'
# Select the .xml file of the last revision (i.e., edi.202.4)
# Under Data Upload Options, select 'I want to manually upload the data by selecting...'
# Click 'Upload'
# Select text files and R file associated with the upload
# Then click 'Upload': if everything works, there will be no errors and the dataset will be uploaded!
# Check to make sure everything looks okay on EDI Website

