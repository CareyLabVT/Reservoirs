

keyword<-c("Lake",	
           "reservoir",	
           "lake",
           "reservoirs",	
           "meteorology",	
           "high-frequency",	
           "air temperature",	
           "relative humidity",	
           "PAR",	
           "photosynthetically active radiation",	
           "infrared radiation",	
           "shortwave radiation",	
           "meteorological data",	
           "albedo",	
           "precipitation",	
           "rainfall",	
           "wind speed",	
           "wind velocity",	
           "barometric pressure",	
           "Carey Lab",	
           "Virginia Teach",	
           "Smart Reservoir",	
           "Carvins Cove Reservoir",	
           "Western Virginia Water Authority")

Thesaurus<-c("cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "cuahsi controlled vocabulary",
                       "carey lab controlled vocabulary",
                       "carey lab controlled vocabulary",
                       "carey lab controlled vocabulary",
                       "carey lab controlled vocabulary",
                       "carey lab controlled vocabulary")



df2<-data.frame(keyword, Thesaurus)

names(df2)=c("keyword", "keywordThesaurus")

setwd('./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCRMetData/2021')

write.table(df2, file="keywords.txt", sep="\t", quote=F, row.names=F)
