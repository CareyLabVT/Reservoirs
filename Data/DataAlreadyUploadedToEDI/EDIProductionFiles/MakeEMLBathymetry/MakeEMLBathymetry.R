library(EMLassemblyline)

make_eml(
  path = ".",
  dataset.title = "Bathymetry and watershed area for Falling Creek Reservoir, Beaverdam Reservoir, and Carvins Cove Reservoir",
  temporal.coverage = c("2012-07-12", "2014-07-22"),
  maintenance.description = 'completed',
  data.table = c("Bathymetry_comb.csv"),
  data.table.name = c("Bathymetric summary statistics"),
  data.table.description = c("Data table including bathymetric summary statistics for both reservoirs"),
  other.entity = c("Bathymetry.zip","Watersheds.zip"),
  other.entity.name = c("Bathymetry spatial data","Watershed spatial data"),
  other.entity.description = c("Spatial data for bathymetry from all reservoirs","Spatial data for watershed area from all reservoirs"),
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.971.3')
