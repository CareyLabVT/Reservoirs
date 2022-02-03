# Steps for setting up EML metadata ####
library(devtools)
install_github("EDIorg/EMLassemblyline", force=T)
library(EMLassemblyline)

# Import templates for dataset licensed under CCBY, with 2 tables.
template_core_metadata(path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021",
                 license = "CCBY",
                 file.type = ".txt",
                 write.file = TRUE)

template_table_attributes(path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021",
                          data.path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021",
                          data.table = c("Secchi_depth_2013-2021.csv",
                                         "YSI_PAR_profiles_2013-2021.csv"))
              
template_categorical_variables(path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021",
                               data.path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021",
                               write.file = TRUE)

template_geographic_coverage(path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021",
                             data.path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021",
                             data.table = c("Secchi_depth_2013-2021.csv",
                                            "YSI_PAR_profiles_2013-2021.csv"),
                             empty = TRUE,
                             write.file = TRUE)


# Run this function for staging data
make_eml(path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021",
         dataset.title = "Secchi depth data and discrete depth profiles of photosynthetically active radiation, temperature, dissolved oxygen, and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2021",
         data.path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021",
         eml.path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021",
         data.table = c("Secchi_depth_2013-2021.csv",
                        "YSI_PAR_profiles_2013-2021.csv"),
         data.table.description = c("Secchi depth data from five reservoirs in southwestern Virginia", 
                                    "Discrete depths of water temperature, dissolved oxygen, conductivity, photosynthetically active radiation, redox potential, and pH in five southwestern Virginia reservoirs"),
         temporal.coverage = c("2013-08-30", "2021-12-06"),
         maintenance.description = "ongoing", 
         user.domain = "EDI",
         user.id = "ccarey",
         package.id = "edi.715.20") #need a new one each year

#staging environment - https://portal-s.edirepository.org/nis/login.jsp


################################
# Run this function when ready for REAL EDI environment
make_eml(path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021",
         dataset.title = "Secchi depth data and discrete depth profiles of photosynthetically active radiation, temperature, dissolved oxygen, and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2021",
         data.path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021",
         eml.path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2021",
         data.table = c("Secchi_depth_2013-2021.csv",
                        "YSI_PAR_profiles_2013-2021.csv"),
         data.table.description = c("Secchi depth data from five reservoirs in southwestern Virginia", 
                                    "Discrete depths of water temperature, dissolved oxygen, conductivity, photosynthetically active radiation, redox potential, and pH in five southwestern Virginia reservoirs"),
         temporal.coverage = c("2013-08-30", "2021-12-06"),
         maintenance.description = "ongoing", 
         user.domain = "EDI",
         user.id = "ccarey",
         package.id = "edi.198.9") #2021 is 198.9

