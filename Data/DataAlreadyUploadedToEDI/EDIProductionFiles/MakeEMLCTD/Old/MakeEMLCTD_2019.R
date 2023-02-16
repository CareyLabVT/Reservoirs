# Install and load devtools
install.packages("devtools")
library(devtools)

# Install and load EMLassemblyline
install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

# Import Templates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
import_templates(path = "C:/Users/Owner/Desktop/EDI_CTD_upload", 
                 license = "CCBY", 
                 data.files = "CTD_Meta_13_17")

# Define Categorical Variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
define_catvars(path = "C:/Users/Owner/Desktop/EDI_CTD_upload")

# Make the EML for EDI ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

make_eml(path = "C:/Users/Owner/Desktop/EDI_CTD_upload",
         dataset.title = "Time series of high-frequency profiles of depth, temperature, dissolved oxygen, conductivity, specific conductivity, chlorophyll a, turbidity, pH, and oxidation-reduction potential for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in Southwestern Virginia, USA 2013-2017",
         data.files = "CTD_Meta_13_17.csv",
         data.files.description = "Reservoir CTD dataset",
         data.files.quote.character = "\"",
         temporal.coverage = c("2013-03-07", "2017-12-10"),
         #geographic.description = "Southwestern Virginia, USA, North America", # This argument is only required if bounding_boxes.txt is absent.
         maintenance.description = "ongoing", 
         user.id = "carylab7",
         package.id = "edi.200.2")
