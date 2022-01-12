# Install devtools
#install.packages("devtools")

# Load devtools
library(devtools)
library(tidyverse)
library(lubridate)


# Install and load EMLassemblyline
package_url <- 'https://cran.r-project.org/bin/windows/contrib/3.5/EML_1.0.3.zip'
install.packages(package_url, repos = NULL, type = 'source')

install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)
setwd("C:/Users/wwoel/Desktop/Reservoirs_2/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLContinuumDischarge")
data <- read.csv("./2019_Continuum_Discharge.csv")

# Import templates for an example dataset licensed under CC0, with 2 tables located in at "path"
import_templates(path = ".",
                license = "CCBY",
                data.files = "2019_Continuum_Discharge")

#view_unit_dictionary()

define_catvars(".")

make_eml(path = ".",
         dataset.title = "Manually-collected discharge data for multiple inflow tributaries entering Falling Creek Reservoir and Beaverdam Reservoir, Vinton, Virginia, USA in 2019",
         data.files = "2019_Continuum_Discharge",
         data.files.description = "Reservoir Continuum Manual Discharge Data",
         temporal.coverage = c("2019-02-08", "2019-10-30"),
         maintenance.description = "ongoing",
         user.id =  "ccarey",
         other.entity = c('calculate_discharge_flowmate_data.R', 
                          'SOP for Manual Reservoir Continuum Discharge Data Collection and Calculation'),
         other.entity.description = c('Script used to calculate discharge from flowmate measurements', 
                                      'SOPs for discharge data collection and calculation using flowmeter, salt injection, and velocity float methods') ,
         package.id = "edi.454.4", #### this is the one that I need to change!!!
         user.domain = 'EDI')


