
library(devtools)
# install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)
pacman::p_load(devtools, EMLassemblyline, here, xml2, XML)


folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLBathymetry/2025"


### FIRST EDI publication
# make_eml(
#   path = ".",
#   dataset.title = "Bathymetry and watershed area for Falling Creek Reservoir, Beaverdam Reservoir, and Carvins Cove Reservoir",
#   temporal.coverage = c("2012-07-12", "2014-07-22"),
#   maintenance.description = 'completed',
#   data.table = c("Bathymetry_comb.csv"),
#   data.table.name = c("Bathymetric summary statistics"),
#   data.table.description = c("Data table including bathymetric summary statistics for both reservoirs"),
#   other.entity = c("Bathymetry.zip","Watersheds.zip"),
#   other.entity.name = c("Bathymetry spatial data","Watershed spatial data"),
#   other.entity.description = c("Spatial data for bathymetry from all reservoirs","Spatial data for watershed area from all reservoirs"),
#   user.id = 'ccarey',
#   user.domain = 'EDI',
#   package.id = 'edi.1254.1')
 

### 2025 updates; STAGING
eml_file <- make_eml(
  path = folder,
  dataset.title = "Bathymetry and watershed area for Falling Creek Reservoir, Beaverdam Reservoir, and Carvins Cove Reservoir",
  temporal.coverage = c("2012-07-12", "2024-04-14"),
  maintenance.description = 'completed',
  data.table = c("Bathymetry_combined_Final.csv"),
  data.table.name = c("Bathymetric summary statistics"),
  data.table.description = c("Data table including bathymetric summary statistics for the three reservoirs"),
  other.entity = c("Bathymetry_Spatial_Data_EDI.zip", "BVR_bathy_fullpond_interp.R",
                   "ADCP SOP.pdf", "FCR_Bathymetry_Maps_in_R.Rmd"),
  other.entity.name = c("Bathymetry spatial data", "Script to interpolate BVR bathymetry",
                        "ADCP SOP", "R markdown to make bathymetry maps in FCR"),
  other.entity.description = c("Spatial data for bathymetry from all reservoirs",
                               "Script to interpolate BVR bathymetry to full pond values",
                               "SOP for ADCP operation and post-processing of data and GIS file generation",
                               "Rmd script that makes bathymetry maps in R for FCR and BVR using VMT processed ADCP data"),
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.971.7', #971 is the staging number
  #publication number is 1254.1 currently the 2026 publication will be .2
  write.file = T, ### write the file to the folder
  return.obj = T) 


#### update license after making EML
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
xml_add_child(parent, childC, .where = 18)

# Save the file with the changes
write_xml(doc, paste0(folder,"/",package.id,".xml"))








