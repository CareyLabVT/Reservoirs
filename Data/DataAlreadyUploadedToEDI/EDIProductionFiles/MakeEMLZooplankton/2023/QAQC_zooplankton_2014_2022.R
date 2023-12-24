#Zooplankton QAQC script for visualizing and merging files
#Created 5 Jul 2023

#read in libraries
#if (!require("pacman"))install.packages("pacman")
pacman::p_load(ggplot2, tidyverse)

#read in new zoop files
zoop <- read.csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023/Data/EDI_zoop_taxa_2019-2022.csv")

zoop_dens <- read.csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023/Data/EDI_zoop_raw_dens_2019-2022.csv",
                      na.strings = "")

zoop_biom <- read.csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023/Data/EDI_zoop_raw_biom_2019-2022.csv",
                      na.strings = "")

#get zoop date into correct format
zoop$DateTime <- as.POSIXct(zoop$DateTime, format = "%Y-%m-%d %H:%M:%S", tz="EST")

#hypo tows
ggplot(data=subset(zoop, CollectionMethod %in% c("Tow") & StartDepth_m %in% c(7, 8, 9,10) & 
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")), 
       aes(DateTime, Density_IndPerL, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon)

ggplot(data=subset(zoop, CollectionMethod %in% c("Tow") & StartDepth_m %in% c(7, 8, 9,10) & 
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")), 
       aes(DateTime, Biomass_ugL, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon)

ggplot(data=subset(zoop, CollectionMethod %in% c("Tow") & StartDepth_m %in% c(7, 8, 9,10) & 
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")), 
       aes(DateTime, MeanLength_mm, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon)

ggplot(data=subset(zoop, CollectionMethod %in% c("Tow") & StartDepth_m %in% c(7, 8, 9,10) & 
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")), 
       aes(DateTime, MeanWeight_ug, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon)

#hypo schindlers
ggplot(data=subset(zoop, CollectionMethod %in% c("Schindler") & StartDepth_m %in% c(7, 8, 9,10) & 
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")),
       aes(DateTime, Density_IndPerL, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon)

ggplot(data=subset(zoop, CollectionMethod %in% c("Schindler") & StartDepth_m %in% c(7, 8, 9,10) & 
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")), 
       aes(DateTime, Biomass_ugL, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon)

ggplot(data=subset(zoop, CollectionMethod %in% c("Schindler") & StartDepth_m %in% c(7, 8, 9,10) & 
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")), 
       aes(DateTime, MeanLength_mm, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon)

ggplot(data=subset(zoop, CollectionMethod %in% c("Schindler") & StartDepth_m %in% c(7, 8, 9,10) & 
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")), 
       aes(DateTime, MeanWeight_ug, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon)
  

#epi tows
ggplot(data=subset(zoop, CollectionMethod %in% c("Tow") & StartDepth_m <=6.9 &
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")), 
       aes(DateTime, Density_IndPerL, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon, scales="free")

ggplot(data=subset(zoop, CollectionMethod %in% c("Tow") & StartDepth_m <=6.9 &
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")), 
       aes(DateTime, Biomass_ugL, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon, scales="free")

ggplot(data=subset(zoop, CollectionMethod %in% c("Tow") & StartDepth_m <=6.9 &
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")),
       aes(DateTime, MeanLength_mm, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon, scales="free")

ggplot(data=subset(zoop, CollectionMethod %in% c("Tow") & StartDepth_m <=6.9 &
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")),
       aes(DateTime, MeanWeight_ug, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon, scales="free")

#epi schindlers
ggplot(data=subset(zoop, CollectionMethod %in% c("Schindler") & StartDepth_m <=6.9 &
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")), 
       aes(DateTime, Density_IndPerL, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon, scales="free")

ggplot(data=subset(zoop, CollectionMethod %in% c("Schindler") & StartDepth_m <=6.9 &
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")), 
       aes(DateTime, Biomass_ugL, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon, scales="free")

ggplot(data=subset(zoop, CollectionMethod %in% c("Schindler") & StartDepth_m <=6.9 &
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")), 
       aes(DateTime, MeanLength_mm, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon, scales="free")

ggplot(data=subset(zoop, CollectionMethod %in% c("Schindler") & StartDepth_m <=6.9 &
                     Taxon %in% c("Cladocera", "Copepoda", "Rotifera")), 
       aes(DateTime, MeanWeight_ug, color=as.factor(StartDepth_m))) + 
  geom_point() + theme_bw() + facet_wrap(~Reservoir+Site+Taxon, scales="free")


#raw # check
ggplot(data=subset(zoop_dens, CollectionMethod %in% c("Tow")), 
       aes(DateTime, Zooplankton_No., color=as.factor(StartDepth_m))) + geom_point() +
  theme_bw() + facet_wrap(~Reservoir+Site)

ggplot(data=subset(zoop_dens, CollectionMethod %in% c("Schindler")), 
       aes(DateTime, Zooplankton_No., color=as.factor(StartDepth_m))) + geom_point() +
  theme_bw() + facet_wrap(~Reservoir+Site)

#-------------------------------------------------------------------------------#
#combine new + old zoop files

#read in old files (both from JPD, but 2014 data never published to EDI)
old <- read.csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023/Data/zooplankton.csv")
fcr <- read.csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023/Data/FCR_zooplankton_2014.csv")

#get 2014 file into right date format
fcr$DateTime <- as.POSIXct(paste(fcr$DateTime, "12:00"), format = "%m/%d/%y %H:%M", tz="EST")
old$DateTime <- as.POSIXct(old$DateTime, format = "%Y-%m-%d %H:%M:%S", tz="EST")

#merge old dfs
old_final <- rbind(old, fcr)

#add a rep column to JPD data
old_final$Rep <- 1

#change order of cols
old_final <- old_final |> select(Reservoir, Site, DateTime, 
                                 StartDepth_m, EndDepth_m, Rep,
                                 CollectionMethod, Taxon, 
                                 Density_IndPerL, MeanLength_mm,
                                 MeanWeight_ug, Biomass_ugL,
                                 Flag_Length, Flag_Weight,
                                 Flag_Biomass)

#merge old df with new df
zoops_final <- rbind(old_final, zoop)

#if dens = 0, add NA to other cols
zoops_final$MeanLength_mm[zoops_final$Density_IndPerL==0] <- NA
zoops_final$MeanWeight_ug[zoops_final$Density_IndPerL==0] <- NA
zoops_final$Biomass_ugL[zoops_final$Density_IndPerL==0] <- NA
  
#make sure marks in ocular micrometer col is numeric (NA warning bc NAs in this column)
zoop_biom$MarksInOcularMicrometer_No. <- as.numeric(zoop_biom$MarksInOcularMicrometer_No.)

#export final dfs
write.csv(zoops_final, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023/zooplankton_summary_2014_2022.csv", row.names=F)
write.csv(zoop_dens, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023/zoop_raw_dens_2019_2022.csv", row.names=F)
write.csv(zoop_biom, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023/zoop_raw_biom_2019_2022.csv", row.names=F)
