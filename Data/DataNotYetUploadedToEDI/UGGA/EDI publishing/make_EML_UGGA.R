# Install and load devtools
#install.packages("devtools")
library(devtools)

# Set working directory
setwd("./Data/DataNotYetUploadedToEDI/UGGA/EDI publishing/")

# Install and load EMLassemblyline
#install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)
library(tidyverse)

## load historical data 
historical_data  <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1082/2/dd66453fae01815ee574bd69bb9fb213") 

#Load current data
current_data <- read_csv('UGGA_L1.csv')

## combine all data
flux_all <- bind_rows(historical_data, current_data)
min(flux_all$Date)
max(flux_all$Date)

write.csv(flux_all, "UGGA_2018_2023.csv", row.names = F)

# Add site descriptions

#Install the required googlesheets4 package
#install.packages('googlesheets4')
#Load the library 
library(googlesheets4)
sites <- read_sheet('https://docs.google.com/spreadsheets/d/1TlQRdjmi_lzwFfQ6Ovv1CAozmCEkHumDmbg_L4A2e-8/edit#gid=124442383')
data <- flux_all #This is the line you need to modify!
trim_sites = function(data,sites){
  data_res_site=data%>% #Create a Reservoir/Site combo column
    mutate(res_site = trimws(paste0(Reservoir,Site)))
  sites_merged = sites%>% #Filter to Sites that are in the dataframe
    mutate(res_site = trimws(paste0(Reservoir,Site)))%>%
    filter(res_site%in%data_res_site$res_site)%>%
    select(-res_site)
}
sites_trimmed = trim_sites(data,sites) 
write.csv(sites_trimmed,"site_descriptions.csv", row.names=F)# Write to file

# Make the EML for EDI ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make_eml(path = getwd(),
         dataset.title = "Time series of methane and carbon dioxide diffusive fluxes using an Ultraportable Greenhouse Gas Analyzer (UGGA) for Falling Creek Reservoir and Beaverdam Reservoir in southwestern Virginia, USA during 2018-2023",
         data.table = c("UGGA_2018_2023.csv",
                        "site_descriptions.csv"),
         data.table.name = c("UGGA dataset 2018-2023",
                             "Sample site descriptions"),
         data.table.description = c("UGGA diffusive flux dataset from FCR and BVR",
                                    "Descriptions of sampling sites, including lat/long"),
         other.entity = c('RawData.zip', 
                          "UGGA_inspection_2018_2023.Rmd", 
                          "UGGA_qaqc_2023.R", 
                          "UGGA_FluxCalR_2023.R"),
         other.entity.name = c('Raw UGGA files', 
                               "Data visualization script", 
                               "Data compilation script for 2023", 
                               "Data processing script for 2023"),
         other.entity.description = c('Raw data from the Ultraportable Greenhouse Gas Analyzer', 
                                      "Code used to visualize UGGA data from 2018-2023",
                                      "Code used to compile and format 2023 data (after data processing)",
                                      "Code used to process raw 2023 files"),
         temporal.coverage = c("2018-05-07", "2023-12-04"),
         maintenance.description = "ongoing",
         user.domain = "EDI",
         user.id = "ccarey",
         package.id = "edi.1102.4")
