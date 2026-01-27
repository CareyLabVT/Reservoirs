### Make EML for Eddy Flux data
### From: 4 April 2020 to 5 April 2021
### UPDATE WITH ALL 2021 DATA IN JANUARY!
### Following: MakeEMLInflow.R and MakeEMLChemistry.R
### 4 October 2021, A. Hounshell


# (install and) Load EMLassemblyline #####
# install.packages('devtools')
#library(devtools)

#devtools::install_github("EDIorg/EDIutils")
#devtools::install_github("EDIorg/taxonomyCleanr")
#devtools::install_github("EDIorg/EMLassemblyline")
#remotes::install_github("EDIorg/EMLassemblyline")
#remotes::install_github("ropensci/taxize", dependencies = TRUE)
#install.packages("taxize")
#note that EMLassemblyline has an absurd number of dependencies and you
#may exceed your API rate limit; if this happens, you will have to wait an
#hour and try again or get a personal authentification token (?? I think)
#for github which allows you to submit more than 60 API requests in an hour
#library(EMLassemblyline)

# load data packages
pacman::p_load(devtools, EMLassemblyline, here, xml2, XML)


# All the files you need are found in this folder
  folder <- paste0(here(),"/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_EddyFlux/2025/")



#Step 1: Create a directory for your dataset
#in this case, our directory is EddyFlux/EDI

#Step 2: Move your dataset to the directory
# Current dataset: 20211008_EddyPro_Cleaned.csv
# Update with all 2021 data in January!

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
# template_core_metadata(path = "C:/Users/ahoun/Desktop/EddyFlux/EDI",
#                        license = "CCBY",
#                        file.type = ".txt",
#                        write.file = TRUE)

template_table_attributes(path = folder,
                          data.path = folder,
                          data.table = "eddy-flux_2020_2025.csv",
                          write.file = TRUE)


#we want empty to be true for this because we don't include lat/long
#as columns within our dataset but would like to provide them
# template_geographic_coverage(path = folder,
#                              data.path = folder,
#                              data.table = c("20211008_EddyPro_Cleaned.csv"),
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
eml_file <- make_eml(
  path = folder,
  data.path = folder,
  eml.path = folder,
  dataset.title = "Time series of carbon dioxide and methane fluxes measured with eddy covariance for Falling Creek Reservoir in southwestern Virginia, USA during 2020-2025",
  temporal.coverage = c("2020-04-04", "2025-12-31"),
  maintenance.description = 'ongoing',
  data.table = c("eddy-flux_2020_2025.csv"),
  data.table.description = c("EC data processed with EddyPro and cleaned-up from the default EddyPro output (i.e., customize the default error code -9999 to NA, remove outliers, and select variables) with eddy-flux_qaqc_2020_2025.R, but not further post-process with eddy-flux_post_processing.Rmd script"),
  #data.table.name = "EC Data for 2020 to 2022",
  other.entity= c("eddy-flux_qaqc_2020_2025.R","eddy-flux_inspection_2020_2025.Rmd","eddy-flux_processing_2020_2025.Rmd","despike.R"),
  other.entity.description = c("R script to clean-up (customize the default error code -9999 to NA, remove outliers, and select variables) EddyPro output",
                               "R Markdown script for using the sourced qaqc script and making qaqc plots",
                               "R Markdown script for post-processing (additional qaqc and gap-filling) of EC data and not used to make eddy-flux_2020_2025.csv",
                               "Despike function for post-processing (additional qaqc and gap-filling) in the script eddy-flux_processing_2020_2025.Rmd"),
  #other.entity.name = c("EddyPro CleanUp_2020_2022","EC Post-processing","Despike function"),
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.692.21', # FOR STAGING
  #package.id = 'edi.1061.4', # FOR FINAL PRODUCTION
  write.file = T, ### write the file to the folder
  return.obj = T) ## return the object so we can get the package.id


# get the package.id from above
package.id = eml_file$packageId

# read in the xml file that you made from the make_eml function
doc <- read_xml(paste0(folder,package.id,".xml"))

# Find the parent node where <licensed> should be added
parent <- xml_find_first(doc, ".//dataset")   # change to your actual parent

# Create <licensed> node with the name of the licence, the url, the identifier
licensed <- xml_add_child(parent, "licensed")

xml_add_child(licensed, "licenseName",
              "Creative Commons Attribution Non Commercial 4.0 International")
xml_add_child(licensed, "url",
              "https://spdx.org/licenses/CC-BY-NC-4.0")
xml_add_child(licensed, "identifier",
              "CC-BY-NC-4.0")

# Find the parent
parent <- xml_find_first(doc, "//dataset")

# Find the nodes
childC <- xml_find_first(parent, "licensed")

# Remove childC from its current position
xml_remove(childC)

# Instructions to get the position for the license. Open doc file and the click on the down arrow next to dataset. Count the number of items to intellectual rights. Put that number below in .where = 12. 
# Insert childC at position 10 (after Intellectual_rights)
xml_add_child(parent, childC, .where = 12)

# Save the file with the changes
write_xml(doc, paste0(folder,package.id,".xml"))







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
# ED Data Package.id (originally reserved on 2 Feb 2022):
## NOTE: Will need to check geographic coordinates!!!
#make_eml(
  # path = "C:/Users/ahoun/Desktop/EddyFlux/EDI",
  # data.path = "C:/Users/ahoun/Desktop/EddyFlux/EDI",
  # eml.path = "C:/Users/ahoun/Desktop/EddyFlux/EDI",
  # dataset.title = "Time series of carbon dioxide and methane fluxes measured with eddy covariance for Falling Creek Reservoir in southwestern Virginia, USA during 2020-2022",
  # temporal.coverage = c("2020-04-04", "2022-01-11"),
  # maintenance.description = 'ongoing',
  # data.table = c("2020to2021_EddyPro_Cleaned.csv"),
  # data.table.description = c("EC Data"),
  # data.table.name = "EC Data",
  # other.entity= c("EddyPro_CleanUp.R","FCR_Process_BD.R","despike.R"),
  # other.entity.description = c("R script to clean-up Eddy Pro output","R script for post-processing of EC data","Depsike function for post-processing"),
  # other.entity.name = c("EddyPro CleanUp","EC Post-processing","Despike function"),
  # user.id = 'ccarey',
  # user.domain = 'EDI',
  # package.id = 'edi.1061.1')

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

