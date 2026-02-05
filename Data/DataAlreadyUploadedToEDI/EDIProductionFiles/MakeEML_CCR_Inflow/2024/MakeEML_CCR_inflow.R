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
pacman::p_load(devtools, EMLassemblyline, here, xml2, XML)

folder <- paste0(here(),"/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCR_Inflow/2024/")

#Step 5: Import the core metadata templates
#Prepare metadata file templates using the 
template_table_attributes(
  path = folder,
  data.path = folder,
  data.table = c('ccr_hpb-inflow_2024_2025.csv','hpb_maintenancelog_2024_2025.csv'))

view_unit_dictionary()


#Step 14: Categorical variables
# View documentation for this function

?template_categorical_variables

# Run this function for your dataset

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

## Step XXX: Make EML metadata file using the EMLassemblyline::make_eml() command ####
# For modules that contain only zip folders, modify and run the following 
# ** double-check that all files are closed before running this command! **

eml_file <- make_eml(
  path = folder,
  data.path = folder,
  eml.path = folder,
  dataset.title = "Stage and Discharge time series for the primary inflow tributary entering Carvins Cove Reservoir, Roanoke, Virginia, USA 2024-2025",
  temporal.coverage = c("2024-04-23", "2025-12-02"),
  maintenance.description = 'ongoing',
  data.table = c("ccr_hpb-inflow_2024_2025.csv", 
                 "hpb_maintenancelog_2024_2025.csv"),
  data.table.description = c("Tributary stage and discharge dataset from 2024-2025",
                             "Maintenace log for sensor"
                             ),
  other.entity= c('Stage_QAQC.Rmd',
                  'HPB_Q_calc.R'),
  other.entity.description = c("Script to QAQC 10-minute stage data",
                               "Script to calculate discharge"
                               ),
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.1781.3', #Staging ID
  #package.id = '', #Publication ID
  write.file = T, ### write the file to the folder
  return.obj = T)

# get the package.id from above
package.id = eml_file$packageId

# read in the xml file that you made from the make_eml function
doc <- read_xml(paste0(folder,"/",package.id,".xml"))

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

# Insert childC at position 10 (after Intellectual_rights)
xml_add_child(parent, childC, .where = 12)

# Save the file with the changes
write_xml(doc, paste0(folder,"/",package.id,".xml"))



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