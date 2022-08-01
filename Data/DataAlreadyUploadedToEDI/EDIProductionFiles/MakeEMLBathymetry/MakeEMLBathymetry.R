library(EMLassemblyline)

make_eml(
  path = ".",
  dataset.title = "Bathymetry for Falling Creek Reservoir and Beaverdam Reservoir, Virginia, USA",
  temporal.coverage = c("2012-07-12", "2014-07-22"),
  maintenance.description = 'completed',
  data.table = c("Bathymetry_comb.csv"),
  data.table.name = c("Bathymetric summary statistics"),
  data.table.description = c("Data table including bathymetric summary statistics for both reservoirs"),
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.939.2')
