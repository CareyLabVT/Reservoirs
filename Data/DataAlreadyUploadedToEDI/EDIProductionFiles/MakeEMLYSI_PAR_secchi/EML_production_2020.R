#NOTE TO HEATHER FOR DECEMBER 2020: HELLO FUTURE YOU!
#IMPORTANT TO UPDATE THE QUOTES AROUND THE RESERVOIR GEOGRAPHIC DESCRIPTION IN THE GEOGRAPHIC COVERAGE TXT FILE.
# RIGHT NOW THERE IS NO MAP SHOWING ON THE EDI WEBPAGE, SO THIS SHOULD HOPEFULLY RESOLVE THE PROBLEM? NOTE FROM 4MAR2020 CCC&HLW
# DON'T FORGET DIACRITIC REMOVAL.

#Updated by JHW 1/11/21

# Steps for setting up EML metadata ####
library(devtools)
install_github("EDIorg/EMLassemblyline", force=T)
library(EMLassemblyline)
getwd()
# Import templates for dataset licensed under CCBY, with 2 tables.
template_core_metadata(path = "/Users/jacobwynne/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi",
                 license = "CCBY",
                 file.type = ".txt",
                 write.file = TRUE)

template_table_attributes(path = "/Users/jacobwynne/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi",
                          data.path = "/Users/jacobwynne/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi",
                          data.table = c("Secchi_depth_2013-2020.csv",
                                         "YSI_PAR_profiles_2013-2020.csv"))
              
template_categorical_variables(path = "/Users/jacobwynne/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi",
                               data.path = "/Users/jacobwynne/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi",
                               write.file = TRUE)


template_geographic_coverage(path = "/Users/jacobwynne/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi",
                             data.path = "/Users/jacobwynne/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi",
                             data.table = c("Secchi_depth_2013-2020.csv",
                                            "YSI_PAR_profiles_2013-2020.csv"),
                             empty = TRUE,
                             write.file = TRUE)

################################

#Staging
make_eml(path = "/Users/jacobwynne/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi",
         dataset.title = "Secchi depth data and discrete depth profiles of photosynthetically active radiation, temperature, dissolved oxygen, and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2020",
         data.path = "/Users/jacobwynne/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi",
         eml.path = "/Users/jacobwynne/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi",
         data.table = c("Secchi_depth_2013-2020.csv",
                        "YSI_PAR_profiles_2013-2020.csv"),
         data.table.description = c("Secchi depth data from five reservoirs in southwestern Virginia", 
                                    "Discrete depths of water temperature, dissolved oxygen, conductivity, photosynthetically active radiation, redox potential, and pH in five southwestern Virginia reservoirs"),
         temporal.coverage = c("2013-08-30", "2020-12-02"),
         maintenance.description = "ongoing", 
         user.domain = "EDI",
         user.id = "ccarey",
         package.id = "edi.125.21")


# Run this function
make_eml(path = "/Users/jacobwynne/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi",
         dataset.title = "Secchi depth data and discrete depth profiles of photosynthetically active radiation, temperature, dissolved oxygen, and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2020",
         data.path = "/Users/jacobwynne/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi",
         eml.path = "/Users/jacobwynne/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi",
         data.table = c("Secchi_depth_2013-2020.csv",
                        "YSI_PAR_profiles_2013-2020.csv"),
         data.table.description = c("Secchi depth data from five reservoirs in southwestern Virginia", 
                                    "Discrete depths of water temperature, dissolved oxygen, conductivity, photosynthetically active radiation, redox potential, and pH in five southwestern Virginia reservoirs"),
         temporal.coverage = c("2013-08-30", "2020-12-02"),
         maintenance.description = "ongoing", 
         user.domain = "EDI",
         user.id = "ccarey",
         package.id = "edi.198.8")
