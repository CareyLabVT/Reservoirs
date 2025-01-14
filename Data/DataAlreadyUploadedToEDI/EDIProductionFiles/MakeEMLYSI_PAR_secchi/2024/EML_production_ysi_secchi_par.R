# Steps for setting up EML metadata ####
library(devtools)
#install_github("EDIorg/EMLassemblyline", force=T)
library(EMLassemblyline)


#set up permissions
#gs4_auth(cache = FALSE)


folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2024"

# Import templates for dataset licensed under CCBY, with 2 tables.
template_core_metadata(path = folder,
                 license = "CCBY",
                 file.type = ".txt",
                 write.file = TRUE)

template_table_attributes(path = folder,
                          data.path = folder,
                          data.table = c("Secchi_2013_2024.csv",
                                         "YSI_PAR_profiles_2013_2024.csv",
                                         "Secchi_maintenancelog_2013_2024.csv",
                                         "YSI_PAR_profiles_maintenancelog_2013_2024.csv",
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
                            data.table = c("Secchi_2013-2024.csv",
                                           "YSI_PAR_profiles_2013-2024.csv",
                                           "Secchi_maintenancelog_2013_2024.csv",
                                           "YSI_maintenancelog_2013_2024.csv",
                                           "site_descriptions.csv"),
                            empty = TRUE,
                            write.file = TRUE)


# Run this function for staging data
make_eml(path = folder,
         dataset.title = "Secchi depth data and discrete depth profiles of water temperature, dissolved oxygen, 
         conductivity, specific conductance, photosynthetic active radiation, oxidation-reduction potential, 
         and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, 
         and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2024",
         data.path = folder,
         eml.path = folder,
         data.table = c("Secchi_2013_2024.csv",
                        "YSI_PAR_profiles_2013_2024.csv",
                        "Secchi_maintenancelog_2013_2024.csv",
                        "YSI_PAR_profiles_maintenancelog_2013_2024.csv",
                        "site_descriptions.csv"),
         data.table.description = c("Secchi depth data from five reservoirs in southwestern Virginia", 
                                    "Discrete depths of water temperature, dissolved oxygen, conductivity, specific conductance, 
                                    photosynthetic active radiation, oxidation-reduction potential, and pH in five southwestern Virginia reservoirs",
                                    "Secchi maintenance log for observations that are removed or flagged in the qaqc script",
                                    "YSI PAR profiles maintenance log for observation that are removed or flagged in the qaqc script",
                                    "List of reservoirs, sites, description of the site, and coordinates"),
         other.entity = c("Secchi_qaqc_2013_2024.R","Secchi_inspection_2013_2024.Rmd",
                          "YSI_PAR_profiles_qaqc_2013_2024.R","YSI_PAR_profiles_inspection_2013_2024.Rmd"),
         other.entity.description = c("Secchi QAQC script for observations from 2013-2024",
                                      "Secchi visualization script for full dataset",
                                      "YSI and PAR QAQC script for observations from 2013-2024",
                                      "YSI and PAR visualization script for full dataset"),
         temporal.coverage = c("2013-08-30", "2024-12-17"),
         maintenance.description = "ongoing", 
         user.domain = "EDI",
         user.id = "ccarey",
        package.id = "edi.1140.1") #This is for staging

#staging environment - https://portal-s.edirepository.org/nis/login.jsp


################################
# Run this function when ready for REAL EDI environment
make_eml(path = folder,
         dataset.title = "Secchi depth data and discrete depth profiles of water temperature, dissolved oxygen, 
         conductivity, specific conductance, photosynthetic active radiation, oxidation-reduction potential, 
         and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, 
         and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2024",
         data.path = folder,
         eml.path = folder,
         data.table = c("Secchi_2013_2024.csv",
                        "YSI_PAR_profiles_2013_2024.csv",
                        "Secchi_maintenancelog_2013_2024.csv",
                        "YSI_PAR_profiles_maintenancelog_2013_2024.csv",
                        "site_descriptions.csv"),
         data.table.description = c("Secchi depth data from five reservoirs in southwestern Virginia", 
                                    "Discrete depths of water temperature, dissolved oxygen, conductivity, specific conductance, 
                                    photosynthetic active radiation, oxidation-reduction potential, and pH in five southwestern Virginia reservoirs",
                                    "Secchi maintenance log for observations that are removed or flagged in the qaqc script",
                                    "YSI PAR profiles maintenance log for observation that are removed or flagged in the qaqc script",
                                    "List of reservoirs, sites, description of the site, and coordinates"),
         other.entity = c("Secchi_qaqc_2013_2024.R","Secchi_inspection_2013_2024.Rmd",
                          "YSI_PAR_profiles_qaqc_2013_2024.R","YSI_PAR_profiles_inspection_2013_2024.Rmd"),
         other.entity.description = c("Secchi QAQC script for observations from 2013-2024",
                                      "Secchi visualization script for full dataset",
                                      "YSI and PAR QAQC script for observations from 2013-2024",
                                      "YSI and PAR visualization script for full dataset"),
         temporal.coverage = c("2013-08-30", "2024-12-17"),
         maintenance.description = "ongoing", 
         user.domain = "EDI",
         user.id = "ccarey",
         package.id = "edi.198.13")  #need a new one each year         
        # 2022 is 198.9 or 198.10 (republished)
        # 2023 is 198.11
        # 2024 is 198.12
        # 2025 is 198.13

