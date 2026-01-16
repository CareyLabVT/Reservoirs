# Steps for setting up EML metadata ####
# Updated : 14 Jan 2026 - ABP update the license info in XML

#library(devtools)
#install_github("EDIorg/EMLassemblyline", force=T)
#library(EMLassemblyline)

pacman::p_load(devtools, EMLassemblyline, here, xml2, XML)

#set up permissions
#gs4_auth(cache = FALSE)


folder <- paste0(here(),"/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2025/")

# Import templates for dataset licensed under CCBY, with 2 tables.
#template_core_metadata(path = folder,
#                 license = "CCBY",
#                 file.type = ".txt",
#                 write.file = TRUE)

template_table_attributes(path = folder,
                          data.path = folder,
                          data.table = c("secchi_2013_2025.csv",
                                         "handheld-sensors_2013_2025.csv",
                                         "secchi_maintenancelog_2013_2025.csv",
                                         "handheld-sensors_maintenancelog_2013_2025.csv",
                                         "site_descriptions.csv"))

### Note that in the secchi_maintenancelog the end_parameter column is NA so it needs to be set as a character
# or else you get this warning:

# Categorical variables (secchi_maintenancelog_2013_2023.csv, Required) - Variables defined as categorical 
# will be reclassified as 'character' until these issues are fixed:
# 1.  Missing categorical variable metadata. Variables are listed as 'categorical' 
# in the table attributes metadata but are not found in the categorical variables metadata. 
# These variables are missing: end_parameter

              
template_categorical_variables(path = folder,
                               data.path = folder,
                               write.file = TRUE)

template_geographic_coverage(path = folder,
                            data.path = folder,
                            data.table = c("secchi_2013-2025.csv",
                                           "handheld-sensors_2013-2025.csv",
                                           "secchi_maintenancelog_2013_2025.csv",
                                           "handheld-sensors_maintenancelog_2013_2025.csv",
                                           "site_descriptions.csv"),
                            empty = TRUE,
                            write.file = TRUE)


# Run this function for staging data
elm_file <- make_eml(path = folder,
         dataset.title = "Secchi depth data and discrete depth profiles of water temperature, dissolved oxygen, 
         conductivity, specific conductance, photosynthetic active radiation, oxidation-reduction potential, 
         and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, 
         and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2025",
         data.path = folder,
         eml.path = folder,
         data.table = c("secchi_2013_2025.csv",
                        "handheld-sensors_2013_2025.csv",
                        "secchi_maintenancelog_2013_2025.csv",
                        "handheld-sensors_maintenancelog_2013_2025.csv",
                        "site_descriptions.csv"),
         data.table.description = c("Secchi depth data from five reservoirs in southwestern Virginia", 
                                    "Discrete depths of water temperature, dissolved oxygen, conductivity, specific conductance, 
                                    photosynthetic active radiation, oxidation-reduction potential, and pH in five southwestern Virginia reservoirs",
                                    "Secchi maintenance log for observations that are removed or flagged in the qaqc script",
                                    "Handheld sensors maintenance log for observation that are removed or flagged in the qaqc script",
                                    "List of reservoirs, sites, description of the site, and coordinates"),
         other.entity = c("secchi_qaqc_2013_2025.R","secchi_inspection_2013_2025.Rmd",
                          "handheld-sensors_qaqc_2013_2025.R","handheld-sensors_inspection_2013_2025.Rmd"),
         other.entity.description = c("Secchi QAQC script for observations from 2013-2025",
                                      "Secchi visualization script for full dataset",
                                      "Handheld sensors QAQC script for observations from 2013-2025",
                                      "Handheld sensors visualization script for full dataset"),
         temporal.coverage = c("2013-08-30", "2025-12-02"),
         maintenance.description = "ongoing", 
         user.domain = "EDI",
         user.id = "ccarey",
         package.id = "edi.1140.7", #This is for staging
        # package.id = "edi.198.13",  #need a new one each year. This one is for the production enviornment
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

# Insert childC at position 10 (after Intellectual_rights)
xml_add_child(parent, childC, .where = 10)

# Save the file with the changes
write_xml(doc, paste0(folder,package.id,".xml"))


#staging environment - https://portal-s.edirepository.org/nis/login.jsp


################################
# Run this function when ready for REAL EDI environment
# make_eml(path = folder,
#          dataset.title = "Secchi depth data and discrete depth profiles of water temperature, dissolved oxygen, 
#          conductivity, specific conductance, photosynthetic active radiation, oxidation-reduction potential, 
#          and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, 
#          and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2025",
#          data.path = folder,
#          eml.path = folder,
#          data.table = c("secchi_2013_2025.csv",
#                         "handheld-sensors_2013_2025.csv",
#                         "secchi_maintenancelog_2013_2025.csv",
#                         "handheld-sensors_maintenancelog_2013_2025.csv",
#                         "site_descriptions.csv"),
#          data.table.description = c("Secchi depth data from five reservoirs in southwestern Virginia", 
#                                     "Discrete depths of water temperature, dissolved oxygen, conductivity, specific conductance, 
#                                     photosynthetic active radiation, oxidation-reduction potential, and pH in five southwestern Virginia reservoirs",
#                                     "Secchi maintenance log for observations that are removed or flagged in the qaqc script",
#                                     "YSI PAR profiles maintenance log for observation that are removed or flagged in the qaqc script",
#                                     "List of reservoirs, sites, description of the site, and coordinates"),
#          other.entity = c("secchi_qaqc_2013_2024.R","secchi_inspection_2013_2025.Rmd",
#                           "handheld-sensors_qaqc_2013_2025.R","handheld-sensors_inspection_2013_2025.Rmd"),
#          other.entity.description = c("Secchi QAQC script for observations from 2013-2025",
#                                       "Secchi visualization script for full dataset",
#                                       "YSI and PAR QAQC script for observations from 2013-2025",
#                                       "YSI and PAR visualization script for full dataset"),
#          temporal.coverage = c("2013-08-30", "2025-12-02"),
#          maintenance.description = "ongoing", 
#          user.domain = "EDI",
#          user.id = "ccarey",
#          package.id = "edi.198.13")  #need a new one each year         
#         # 2022 is 198.9 or 198.10 (republished)
#         # 2023 is 198.11
#         # 2024 is 198.12
#         # 2025 is 198.13

