# Steps for setting up EML metadata ####
library(devtools)
#install_github("EDIorg/EMLassemblyline", force=T)
library(EMLassemblyline)


#set up permissions
#gs4_auth(cache = FALSE)


folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023"

# Import templates for dataset licensed under CCBY, with 2 tables.
template_core_metadata(path = folder,
                 license = "CCBY",
                 file.type = ".txt",
                 write.file = TRUE)

template_table_attributes(path = folder,
                          data.path = folder,
                          data.table = c("secchi_depth_2013_2023.csv",
                                         "YSI_PAR_profiles_2013_2023.csv",
                                         "secchi_maintenancelog_2013_2023.csv",
                                         "YSI_PAR_profiles_maintenancelog_2013_2023.csv",
                                         "site_descriptions.csv"))
              
template_categorical_variables(path = folder,
                               data.path = folder,
                               write.file = TRUE)

# template_geographic_coverage(path = folder,
#                              data.path = folder,
#                              data.table = c("Secchi_depth_2013-2022.csv",
#                                             "YSI_PAR_profiles_2013-2022.csv",
#                                             "Secchi_maintenancelog_2013_2023.csv",
#                                             "YSI_maintenancelog_2013_2023.csv",
#                                             "site_descriptions.csv"),
#                              empty = TRUE,
#                              write.file = TRUE)


# Run this function for staging data
make_eml(path = folder,
         dataset.title = "Secchi depth data and discrete depth profiles of water temperature, dissolved oxygen, 
         conductivity, specific conductance, photosynthetic active radiation, oxidation-reduction potential, 
         and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, 
         and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2023",
         data.path = folder,
         eml.path = folder,
         data.table = c("secchi_2013_2023.csv",
                        "YSI_PAR_profiles_2013_2023.csv",
                        "secchi_maintenancelog_2013_2023.csv",
                        "YSI_PAR_profiles_maintenancelog_2013_2023.csv",
                        "site_descriptions.csv"),
         data.table.description = c("Secchi depth data from five reservoirs in southwestern Virginia", 
                                    "Discrete depths of water temperature, dissolved oxygen, conductivity, specific conductance, 
                                    photosynthetic active radiation, oxidation-reduction potential, and pH in five southwestern Virginia reservoirs",
                                    "Secchi maintenance log for observations that are removed or flagged in the qaqc script",
                                    "YSI PAR profiles maintenance log for observation that are removed or flagged in the qaqc script",
                                    "List of reservoirs, sites, description of the site, and coordinates"),
         other.entity = c("secchi_qaqc_2013_2023.R","secchi_inspection_2013_2023.Rmd",
                          "YSI_PAR_profiles_qaqc_2013_2023.R","YSI_PAR_profiles_inspection_2013_2023.Rmd"),
         other.entity.description = c("Secchi QAQC script for observations from 2013-2023",
                                      "Secchi visualization script for full dataset",
                                      "YSI and PAR QAQC script for observations from 2013-2023",
                                      "YSI and PAR visualization script for full dataset"),
         temporal.coverage = c("2013-08-30", "2023-12-04"),
         maintenance.description = "ongoing", 
         user.domain = "EDI",
         user.id = "ccarey",
         package.id = "edi.1105.7") 

#staging environment - https://portal-s.edirepository.org/nis/login.jsp


################################
# Run this function when ready for REAL EDI environment
make_eml(path = folder,
         dataset.title = "Secchi depth data and discrete depth profiles of water temperature, dissolved oxygen, conductivity, specific conductance, photosynthetic active radiation, redox potential, and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2023",
         data.path = folder,
         eml.path = folder,
         data.table = c("Secchi_depth_2013_2023.csv",
                        "YSI_PAR_profiles_2013_2023.csv",
                        "Secchi_maintenancelog_2013_2023.csv",
                        "YSI_maintenancelog_2013_2023.csv",
                        "site_descriptions.csv"),
         data.table.description = c("Secchi depth data from five reservoirs in southwestern Virginia", 
                                    "Discrete depths of water temperature, dissolved oxygen, conductivity, specific conductance, photosynthetic active radiation, redox potential, and pH in five southwestern Virginia reservoirs"),
         other.entity = c("YSI_qaqc_2023_2023.R", "Secchi_qaqc_2023_2023.R",
                          "YSI_inspection_2013_2023.Rmd", "Secchi_inspection_2013_2023.Rmd"),
         other.entity.description = c("YSI and PAR QAQC script for most recent year of data publication",
                                      "Secchi QAQC script for most recent year of data publication",
                                      "YSI and PAR visualization script for full dataset",
                                      "Secchi visualization script for full dataset"),
         temporal.coverage = c("2013-08-30", "2023-12-04"),
         maintenance.description = "ongoing", 
         user.domain = "EDI",
         user.id = "ccarey",
         package.id = "edi.198.11")  #need a new one each year         
        # 2022 is 198.9 or 198.10 (republished)
        # 2023 is 198.11
        # 2024 is 198.12

