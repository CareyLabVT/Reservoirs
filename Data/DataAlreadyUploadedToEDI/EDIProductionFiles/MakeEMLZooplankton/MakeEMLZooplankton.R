#Make Zooplankton EML
#Cayelan Carey, based on EDI workshop 24 May 2018

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
         user.id = c("carylab0", "ccarey"), #your personal ID, will be Carey Lab ID eventually!
         affiliation = c("EDI", "EDI"),
         package.id = "edi.197.2") #from EDI portal, login, and then reserve a package ID via the
          #Data Package Identifier Reservations



