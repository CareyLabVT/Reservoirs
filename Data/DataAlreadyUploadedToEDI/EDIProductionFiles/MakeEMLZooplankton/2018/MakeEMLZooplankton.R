#Make Zooplankton EML
#Cayelan Carey, based on EDI workshop 24 May 2018
#updated 5 Jul 2023 HLW

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
data<- read.csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/Z023/zooplankton.csv") 

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
write.csv(sites_trimmed,"./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/2022/Data/site_descriptions.csv", row.names = FALSE)

# Install devtools
install.packages("devtools")

# Load devtools
library(devtools)

# Install and load EMLassemblyline
install_github("EDIorg/EMLassemblyline")

library(EMLassemblyline)

setwd("~/Dropbox/ComputerFiles/Virginia_Tech/Falling Creek/DataForWebsite/Github/ReservoirData/Formatted_Data/MakeEMLZooplankton")

data<-read.csv('zooplankton.csv', header=TRUE)
View(data)

import_templates(path = "~/Dropbox/ComputerFiles/Virginia_Tech/Falling Creek/DataForWebsite/Github/ReservoirData/Formatted_Data/MakeEMLZooplankton",
                 license = "CCBY", #use CCBY instead of CCBO so that data users need to cite our package
                 data.files = c("zooplankton")) #csv file name

define_catvars(path = "~/Dropbox/ComputerFiles/Virginia_Tech/Falling Creek/DataForWebsite/Github/ReservoirData/Formatted_Data/MakeEMLZooplankton")

make_eml(path = "/Users/cayelan/Dropbox/ComputerFiles/Virginia_Tech/Falling Creek/DataForWebsite/Github/ReservoirData/Formatted_Data/MakeEMLZooplankton",
         dataset.title = "Crustacean zooplankton density and biomass and rotifer density for Beaverdam Reservoir, Carvins Cove Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2014-2016",
         data.files = c("zooplankton"),
         data.files.description = c("Reservoir zooplankton dataset"), #short title
         data.files.quote.character = c("\""),
         temporal.coverage = c("2014-04-04", "2016-10-25"),
         geographic.description = "Southwestern Virginia, USA, North America",
         maintenance.description = "completed", 
         user.id = "carylab0", #your personal ID, will be Carey Lab ID eventually!
         package.id = "edi.198.1") #from EDI portal, login, and then reserve a package ID via the
          #Data Package Identifier Reservations
