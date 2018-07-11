# Steps for setting up EML metadata ####

library(EMLassemblyline)

# Import templates for dataset licensed under CCBY, with 2 tables.
import_templates(path = "./Formatted_Data/make_eml_YSI_PAR_secchi",
                 license = "CCBY",
                 data.files = c("Secchi_depth",
                                "YSI_PAR_profiles"))

define_catvars(path = "./Formatted_Data/make_eml_YSI_PAR_secchi")
################################
library(devtools)
install_github("EDIorg/EMLassemblyline", force=T)
library(EMLassemblyline)

# Run this function
make_eml(path = "./Formatted_Data/MakeEMLYSI_PAR_secchi",
         dataset.title = "Secchi depth data and discrete depth profiles of photosynthetically active radiation, temperature, dissolved oxygen, and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2017",
         data.files = c("Secchi_depth",
                        "YSI_PAR_profiles"),
         data.files.description = c("Secchi depth data from five reservoirs in southwestern Virginia", 
                                    "Discrete depths of water temperature, dissolved oxygen, conductivity, photosynthetically active radiation, redox potential, and pH in five southwestern Virginia reservoirs"),
         temporal.coverage = c("2013-08-30", "2017-12-11"),
         geographic.description = "Southwestern Virginia, USA, North America",
         maintenance.description = "ongoing", 
         user.id = c("carylab5", "ccarey"),
         affiliation = c("EDI", "EDI"),
         package.id = "edi.198.3")

