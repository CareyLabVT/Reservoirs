library(EMLassemblyline)

folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLBathymetry/2025"


### OLD 
# make_eml(
#   path = ".",
#   dataset.title = "Bathymetry and watershed area for Falling Creek Reservoir, Beaverdam Reservoir, and Carvins Cove Reservoir",
#   temporal.coverage = c("2012-07-12", "2014-07-22"),
#   maintenance.description = 'completed',
#   data.table = c("Bathymetry_comb.csv"),
#   data.table.name = c("Bathymetric summary statistics"),
#   data.table.description = c("Data table including bathymetric summary statistics for both reservoirs"),
#   other.entity = c("Bathymetry.zip","Watersheds.zip"),
#   other.entity.name = c("Bathymetry spatial data","Watershed spatial data"),
#   other.entity.description = c("Spatial data for bathymetry from all reservoirs","Spatial data for watershed area from all reservoirs"),
#   user.id = 'ccarey',
#   user.domain = 'EDI',
#   package.id = 'edi.1254.1')


### 2025 updates 
make_eml(
  path = folder,
  dataset.title = "Bathymetry and watershed area for Falling Creek Reservoir, Beaverdam Reservoir, and Carvins Cove Reservoir",
  temporal.coverage = c("2012-07-12", "2024-04-14"),
  maintenance.description = 'completed',
  data.table = c("Bathymetry_comb.csv"),
  data.table.name = c("Bathymetric summary statistics"),
  data.table.description = c("Data table including bathymetric summary statistics for the three reservoirs"),
  other.entity = c("Bathymetry_Spatial_Data.zip", "ADCP SOP.pdf"),
  other.entity.name = c("Bathymetry spatial data", "ADCP SOP"),
  other.entity.description = c("Spatial data for bathymetry from all reservoirs", "SOP for ADCP operation and post-processing of data and GIS file generation"),
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.971.5') #971 is the staging number






