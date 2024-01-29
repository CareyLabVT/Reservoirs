# zoop_qaqc_2014_2022.R
# Zooplankton QAQC script for visualizing data and merging files
# Created by HLW
# First developed: 05 Jul 2023
# Last edited: 25 Jan 2024

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
zoop$DateTime <- lubridate::as_datetime(zoop$DateTime, tz="EST")

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

#if density is NA, drop the row
zoops_final <- zoops_final[!is.na(zoops_final$Density_IndPerL),]

#make sure all taxa are uppercase
zoops_final$Taxon <- str_to_sentence(zoops_final$Taxon)

#drop GWR schindler data that has a start depth of 12m and end depth of 9m (2016-10-17 09:00:00)
zoops_final <- zoops_final |> filter(!(StartDepth_m==12 & EndDepth_m==9))

#change end depth of 0 --> 0.1
zoops_final$EndDepth_m[zoops_final$EndDepth_m==0] <- 0.1

#make sure zoop # is numeric and remove rows that are NA
zoop_dens <- zoop_dens |> 
  mutate(Zooplankton_No. = as.numeric(Zooplankton_No.)) |> 
  filter(!is.na(Zooplankton_No.)) |> 
  mutate(DateTime = lubridate::as_datetime(DateTime, tz="EST"))
                               
#biomass
zoop_biom <- zoop_biom |>   
  mutate(DateTime = lubridate::as_datetime(DateTime, tz="EST"))
  
#convert datetimes to character for EDI upload
zoops_final$DateTime <- format(zoops_final$DateTime, "%Y-%m-%d %H:%M:%S")
zoop_dens$DateTime <- format(zoop_dens$DateTime, "%Y-%m-%d %H:%M:%S")
zoop_biom$DateTime <- format(zoop_biom$DateTime, "%Y-%m-%d %H:%M:%S")

#rename flags
zoops_final <- zoops_final |> rename(Flag_MeanLength_mm = Flag_Length,
                                     Flag_MeanWeight_ug = Flag_Weight,
                                     Flag_Biomass_ugL = Flag_Biomass)

#export final dfs
write.csv(zoops_final, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023/zoop_summary_2014_2022.csv", row.names=F)
write.csv(zoop_dens, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023/zoop_raw_dens_2019_2022.csv", row.names=F)
write.csv(zoop_biom, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2023/zoop_raw_biom_2019_2022.csv", row.names=F)
