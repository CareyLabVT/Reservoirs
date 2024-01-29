#Make Zooplankton EML
#Cayelan Carey, based on EDI workshop 24 May 2018
#updated 24 Aug 2023 HLW

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

#read in zoop df
data<- read.csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023/zooplankton_summary_2014_2022.csv") 

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
write.csv(sites_trimmed,"./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023/site_descriptions.csv", row.names = FALSE)

# Install devtools
#install.packages("devtools")

# Load devtools
library(devtools)

# Install and load EMLassemblyline
#install_github("EDIorg/EMLassemblyline")

library(EMLassemblyline)

# Import templates for our dataset licensed under CCBY, with 1 table.
template_core_metadata(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023",
                       license = "CCBY",
                       file.type = ".txt",
                       write.file = TRUE)

template_table_attributes(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023",
                          data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023",
                          data.table = c("zoop_summary_2014_2022.csv", "zoop_raw_dens_2019_2022.csv",
                                         "zoop_raw_biom_2019_2022.csv", "site_descriptions.csv"),
                          write.file = TRUE)

#we want empty to be true for this because we don't include lat/long
#as columns within our dataset but would like to provide them
template_geographic_coverage(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023",
                             data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023",
                             data.table = c("zoop_summary_2014_2022.csv", "zoop_raw_dens_2019_2022.csv",
                                            "zoop_raw_biom_2019_2022.csv", "site_descriptions.csv"),
                             empty = TRUE,
                             write.file = TRUE)

#THIS WILL ONLY WORK once you have filled out the attributes_zooplankton.txt and
#identified which variables are categorical
template_categorical_variables(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023",
                               data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023",
                               write.file = TRUE)

# Make eml for staging environment
#https://portal-s.edirepository.org/nis/home.jsp
make_eml(
  path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023",
  data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023",
  eml.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023",
  dataset.title = "Crustacean and rotifer density and biomass for Beaverdam Reservoir, Falling Creek Reservoir, Carvins Cove Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2014-2022",
  temporal.coverage = c("2014-04-04", "2022-07-01"),
  maintenance.description = 'ongoing',
  data.table = c("zoop_summary_2014_2022.csv", "zoop_raw_dens_2019_2022.csv",
                 "zoop_raw_biom_2019_2022.csv", "site_descriptions.csv"),
  data.table.description = c("Reservoir zooplankton dataset", "Zooplankton counts used to calculate density",
                             "Micrometer measurements, microscope objectives, and taxonomic identification of individual zooplankton used to calculate biomass",
                             "Description, latitude, and longitude of reservoir sampling sites"),
  other.entity = c("zoop_qaqc_2014_2022.R",
                   "zoop_length_weight_conversions_2019_2022.zip"),
  other.entity.description = c("Zooplankton QAQC and visualization script",
                               "Length-weight conversion parameters used to calculate crustacean and rotifer biomass during 2019-2022"),
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.1090.32') #reserve new staging environment package id each year

#------------------------------------------------------------------------------#
# Make eml for production environment
#https://portal.edirepository.org/nis/logout
make_eml(
  path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023",
  data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023",
  eml.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023",
  dataset.title = "Crustacean and rotifer density and biomass for Beaverdam Reservoir, Falling Creek Reservoir, Carvins Cove Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2014-2022",
  temporal.coverage = c("2014-04-04", "2022-07-01"),
  maintenance.description = 'ongoing',
  data.table = c("zoop_summary_2014_2022.csv", "zoop_raw_dens_2019_2022.csv",
                 "zoop_raw_biom_2019_2022.csv", "site_descriptions.csv"),
  data.table.description = c("Reservoir zooplankton dataset", "Zooplankton counts used to calculate density",
                             "Micrometer measurements, microscope objectives, and taxonomic identification of individual zooplankton used to calculate biomass",
                             "Description, latitude, and longitude of reservoir sampling sites"),
  other.entity = c("zoop_qaqc_2014_2022.R",
                   "zoop_length_weight_conversions_2019_2022.zip"),
  other.entity.description = c("Zooplankton QAQC and visualization script",
                               "Length-weight conversion parameters used to calculate crustacean and rotifer biomass during 2019-2022"),
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.197.3') #DO NOT REQUEST A NEW PACKAGE ID, SIMPLY INCREASE THE LAST DIGIT HERE BY 1 TO UPDATE THE CURRENT PUBLICATION
  # 197.3 for 2024 pub
