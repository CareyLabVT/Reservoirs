# Steps for setting up EML metadata ####
library(devtools)
#install_github("EDIorg/EMLassemblyline", force=T)
library(EMLassemblyline)

#Create site description file
#Install the required googlesheets4 package
#install.packages('googlesheets4')

#Load the library 
library(googlesheets4)
library(tidyverse)

#set up permissions
#gs4_auth(cache = FALSE)

#read in all sites from google drive
sites <- read_sheet('https://docs.google.com/spreadsheets/d/1TlQRdjmi_lzwFfQ6Ovv1CAozmCEkHumDmbg_L4A2e-8/edit#gid=124442383')

#read in ysi df
data<- read.csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023/Data/YSI_PAR_profiles_2013-2023.csv") 

#only select sites that are in your df
trim_sites = function(data,sites){
  data_res_site=data%>% #Create a Reservoir/Site combo column
    mutate(res_site = trimws(paste0(Reservoir,Site)))
  sites_merged = sites%>% #Filter to Sites that are in the dataframe
    mutate(res_site = trimws(paste0(Reservoir,Site)))%>%
    filter(res_site%in%data_res_site$res_site)%>%
    select(-res_site)
}
sites_trimmed = trim_sites(data,sites) 

#save as a csv
write.csv(sites_trimmed,"/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023/Data/site_descriptions.csv", row.names = FALSE)

# Import templates for dataset licensed under CCBY, with 2 tables.
template_core_metadata(path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023",
                 license = "CCBY",
                 file.type = ".txt",
                 write.file = TRUE)

template_table_attributes(path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023",
                          data.path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023/Data",
                          data.table = c("Secchi_depth_2013-2023.csv",
                                         "YSI_PAR_profiles_2013-2023.csv",
                                         "Secchi_MaintenanceLog_2013_2023.csv",
                                         "YSI_MaintenanceLog_2013_2023.csv",
                                         "site_descriptions.csv"))
              
template_categorical_variables(path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023",
                               data.path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023/Data",
                               write.file = TRUE)

template_geographic_coverage(path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023",
                             data.path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023/Data",
                             data.table = c("Secchi_depth_2013-2022.csv",
                                            "YSI_PAR_profiles_2013-2022.csv",
                                            "Secchi_MaintenanceLog_2013_2023.csv",
                                            "YSI_MaintenanceLog_2013_2023.csv",
                                            "site_descriptions.csv"),
                             empty = TRUE,
                             write.file = TRUE)


# Run this function for staging data
make_eml(path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023",
         dataset.title = "Secchi depth data and discrete depth profiles of water temperature, dissolved oxygen, conductivity, specific conductance, photosynthetic active radiation, redox potential, and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2023",
         data.path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023/Data",
         eml.path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023",
         data.table = c("Secchi_depth_2013-2023.csv",
                        "YSI_PAR_profiles_2013-2023.csv",
                        "Secchi_MaintenanceLog_2013_2023.csv",
                        "YSI_MaintenanceLog_2013_2023.csv",
                        "site_descriptions.csv"),
         data.table.description = c("Secchi depth data from five reservoirs in southwestern Virginia", 
                                    "Discrete depths of water temperature, dissolved oxygen, conductivity, specific conductance, photosynthetic active radiation, redox potential, and pH in five southwestern Virginia reservoirs"),
         other.entity = c("YSI_qaqc_2013_2023.R", "Secchi_qaqc_2013_2023.R",
                          "YSI_inspection_2013_2023.Rmd", "Secchi_inspection_2013_2023.Rmd"),
         other.entity.description = c("YSI and PAR QAQC script for most recent data publication",
                                      "Secchi QAQC script for most recent data publication",
                                      "YSI and PAR visualization script for full dataset",
                                      "Secchi visualization script for full dataset"),
         temporal.coverage = c("2013-08-30", "2023-12-04"),
         maintenance.description = "ongoing", 
         user.domain = "EDI",
         user.id = "ccarey",
         package.id = "edi.1105.2") 

#staging environment - https://portal-s.edirepository.org/nis/login.jsp


################################
# Run this function when ready for REAL EDI environment
make_eml(path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023",
         dataset.title = "Secchi depth data and discrete depth profiles of water temperature, dissolved oxygen, conductivity, specific conductance, photosynthetic active radiation, redox potential, and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2023",
         data.path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023/Data",
         eml.path = "/Users/heatherwander/Documents/VirginiaTech/research/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2023",
         data.table = c("Secchi_depth_2013-2023.csv",
                        "YSI_PAR_profiles_2013-2023.csv",
                        "Secchi_MaintenanceLog_2013_2023.csv",
                        "YSI_MaintenanceLog_2013_2023.csv",
                        "site_descriptions.csv"),
         data.table.description = c("Secchi depth data from five reservoirs in southwestern Virginia", 
                                    "Discrete depths of water temperature, dissolved oxygen, conductivity, specific conductance, photosynthetic active radiation, redox potential, and pH in five southwestern Virginia reservoirs"),
         other.entity = c("YSI_qaqc_2013_2023.R", "Secchi_qaqc_2013_2023.R",
                          "YSI_inspection_2013_2023.Rmd", "Secchi_inspection_2013_2023.Rmd"),
         other.entity.description = c("YSI and PAR QAQC script for most recent data publication",
                                      "Secchi QAQC script for most recent data publication",
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

