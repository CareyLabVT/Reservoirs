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

folder <- paste0(here(),"/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_WaterIsotopes_CCR/2025/")




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
  dataset.title = "Time series of stable water isotopes (d18O, d2H) from Carvins Cove Reservoir in Southwestern Virginia, USA 2024-2025",
  temporal.coverage = c("2024-05-22", "2025-04-16"),
  maintenance.description = 'completed',
  data.table = c("WaterIsotopes_CCR_2024_2025.csv",
                 "WaterIsotopes_CCR_precip_2024.csv",
                 "site_descriptions.csv"),
  data.table.description = c("Tributary and reservoir stable water isotope samples",
                             "Precipitation stable water isotope samples",
                             "Description, latitude, and longitude of reservoir sampling sites"),
  other.entity= c('WaterIsotopes_CCR_inspection_2024_2025.Rmd'),
  other.entity.description = c("Data visualization script"
                               ),
  user.id = 'ccarey',
  user.domain = 'EDI',
  # package.id = 'edi.1776.9', #Staging ID
  package.id = 'edi.1745.1', #Publication ID
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
xml_add_child(parent, childC, .where = 13)

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