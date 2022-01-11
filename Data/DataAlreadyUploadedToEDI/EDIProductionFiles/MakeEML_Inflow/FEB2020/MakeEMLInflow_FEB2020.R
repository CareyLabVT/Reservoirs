### MakeEMLInflow_2019
### Updated script to publish 2019 Inflow data
### Following: MakeEMLInflow.R and MakeEMLChemistry.R
### 17Dec19 R. Corrigan and A. Hounshell
### Updated: 07 Feb 2020, A. Hounshell

# (install and) Load EMLassemblyline #####
# install.packages('devtools')

devtools::install_github("EDIorg/EMLassemblyline")
#note that EMLassemblyline has an absurd number of dependencies and you
#may exceed your API rate limit; if this happens, you will have to wait an
#hour and try again or get a personal authentification token (?? I think)
#for github which allows you to submit more than 60 API requests in an hour
library(EMLassemblyline)


#Step 1: Create a directory for your dataset
#in this case, our directory is Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/FEB2020

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
template_core_metadata(path = "C:/Users/ahoun/OneDrive/Desktop/Reservoir/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/FEB2020",
                       license = "CCBY",
                       file.type = ".txt",
                       write.file = TRUE)

template_table_attributes(path = "C:/Users/ahoun/OneDrive/Desktop/Reservoir/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/FEB2020",
                          data.path = "C:/Users/ahoun/OneDrive/Desktop/Reservoir/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/FEB2020",
                          data.table = "inflow_for_EDI_2013_FEB2020.csv",
                          write.file = TRUE)


#we want empty to be true for this because we don't include lat/long
#as columns within our dataset but would like to provide them
template_geographic_coverage(path = "C:/Users/ahoun/OneDrive/Desktop/Reservoir/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/FEB2020",
                             data.path = "C:/Users/ahoun/OneDrive/Desktop/Reservoir/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/FEB2020",
                             data.table = "inflow_for_EDI_2013_FEB2020.csv",
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
# No additional information for inflow data

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
template_categorical_variables(path = "C:/Users/ahoun/OneDrive/Desktop/Reservoir/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/FEB2020",
                               data.path = "C:/Users/ahoun/OneDrive/Desktop/Reservoir/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/FEB2020",
                               write.file = TRUE)

#open the created value IN A SPREADSHEET EDITOR and add a definition for each category
# NO CATEGORICAL VARIABLES FOUND FOR INFLOW

#Step 15: Geographic coverage
#copy-paste the bounding_boxes.txt file (or geographic_coverage.txt file) that is Carey Lab specific into your working directory

#Step 16: Custom units
# Copy and pased custom units .txt file from prior year

## Step 17: Obtain a package.id. ####
# INFLOW IS A REVISION: already have an identifier: 202.X (x = revision number)
make_eml(
  path = "C:/Users/ahoun/OneDrive/Desktop/Reservoir/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/FEB2020",
  data.path = "C:/Users/ahoun/OneDrive/Desktop/Reservoir/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/FEB2020",
  eml.path = "C:/Users/ahoun/OneDrive/Desktop/Reservoir/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/FEB2020",
  other.entity= 'Inflow_Aggregation_EDI_FEB2020.R',
  other.entity.description = "QA/QC Code for Discharge aggregation",
  dataset.title = "Discharge time series for the primary inflow tributary entering Falling Creek Reservoir, Vinton, Virginia, USA 2013-2020",
  temporal.coverage = c("2013-05-15", "2020-02-07"),
  maintenance.description = 'ongoing',
  data.table = "inflow_for_EDI_2013_FEB2020.csv",
  data.table.description = "FCR inflow dataset",
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.202.5')

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

