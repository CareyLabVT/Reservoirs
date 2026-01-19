### MakeEML for Optical Data 2021
### Following: MakeEMLInflow.R and MakeEMLChemistry.R
### 11 May 2021, A Hounshell

# (install and) Load EMLassemblyline #####
# install.packages('devtools')

# devtools::install_github("EDIorg/EDIutils")
# devtools::install_github("EDIorg/taxonomyCleanr")
# devtools::install_github("EDIorg/EMLassemblyline")

#note that EMLassemblyline has an absurd number of dependencies and you
#may exceed your API rate limit; if this happens, you will have to wait an
#hour and try again or get a personal authentification token (?? I think)
#for github which allows you to submit more than 60 API requests in an hour
library(EMLassemblyline)


#Step 1: Create a directory for your dataset
#in this case, our directory is ResFDOM/EDI_2021

# Get and set your working directory
getwd()


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
# view_unit_dictionary()
#put flag codes and site codes in the definitions cell
#force reservoir to categorical

#Step 16: Custom units
# Copy and pased custom units .txt file from prior year

#Step 14: Categorical variables
# Run this function for your dataset
#THIS WILL ONLY WORK once you have filled out the attributes_chemistry.txt and
#identified which variables are categorical

# None found!

#Step 15: Geographic coverage
#copy-paste the bounding_boxes.txt file (or geographic_coverage.txt file) that is Carey Lab specific into your working directory

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

make_eml(
  path = "./EDI_2025",
  data.path = "./EDI_2025",
  eml.path = "./EDI_2025",
  dataset.title = "Time series of optical measurements (absorbance, fluorescence) for Beaverdam Reservoir, Carvins Cove Reservoir, and Falling Creek Reservoir in Southwestern Virginia, USA 2019-2025",
  temporal.coverage = c("2019-04-29", "2025-04-16"),
  maintenance.description = 'completed',
  data.table = c("OpticalData_2019_2025.csv",
                 "site_descriptions.csv"),
  data.table.description = c("OpticalDataset",
                             "Description, latitude, and longitude of reservoir sampling sites"),
  other.entity= c('EEMs_inspection_2019_2025.Rmd',
                  'EEMs_publishing_QAQC_2019_2025.R',
                  'Abs_QA_QC_2019_2020.R',
                  'EEMCorr_Fl.m',
                  'PARAFAC_Mod.m',
                  'EEMs_pfiles_2021_2025.zip',
                  'EEMs_pfiles_2019_2020.zip',
                  'CDOM_Correction_2019_2020.zip',
                  'CDOM_data_2021_2025.zip'),
  other.entity.description = c("Data visualization script",
                               
                               "R script to QA/QC and combine optical data from 2019-2025",
                               
                               "R script for QA/QC of optical data and PARAFAC data from 2019-2020",
                               
                               "Matlab code used to correct EEMs data and calculate metrics",
                               
                               "Matlab code used to generate a PARAFAC model on the corrected EEMs data",
                               
                               "This zip folder contains corrected EEMs files ('p_ files') for 2021 - 2025 samples. See README file for file types and descriptions.",
                               
                               "This zip folder contains corrected EEMs files ('p_ files') for 2019 - 2020 samples used in the PARAFAC model
                               and the OpenFluor output used to identify PARAFAC components. See README file for file types and descriptions.",
                               
                               "This zip folder contains CSV files for 2019-2020 absorbance corrections for all measured absorbance samples. 
                               See README file for file types and descriptions.",
                               
                               "This zip folder contains CSV files for raw absorbance data from 2021-2025 for all measured absorbance samples.
                               See README file for file types and descriptions."
                               
                               ),
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.1772.5')

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
# 

make_eml(
  path = "./EDI_2021",
  data.path = "./EDI_2021",
  eml.path = "./EDI_2021",
  dataset.title = "Time series of optical measurements (absorbance, fluorescence) for Beaverdam and Falling Creek Reservoir in Southwestern Virginia, USA 2019-2020",
  temporal.coverage = c("2019-04-29", "2020-03-30"),
  maintenance.description = 'completed',
  data.table = "20210511_OpticalData.csv",
  data.table.description = "OpticalDataset",
  other.entity= c('Abs_QA_QC.R','PARAFAC_Mod.m','EEMs_pfiles.zip','CDOM_Correction.zip'),
  other.entity.description = c("R script for QA/QC of absorbance and fluorescence data",
                               "Matlab code used to generate a PARAFAC model on the corrected EEMs data",
                               "This zip folder contains corrected EEMs files ('p_ files') for all samples used in the PARAFAC model
                               and the OpenFluor output used to identify PARAFAC components. See README file for file types and descriptions.",
                               "This zip folder contains CSV files for absorbance corrections for all measured absorbance samples. 
                               See README file for file types and descriptions."),
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.841.1')

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