# zoop_qaqc_2014_2025.R
# Zooplankton QAQC script for visualizing data and merging files
# Created by HLW
# First developed: 05 Jul 2023
# Last edited: 08 August 2025
# adding in 2022-2025 BVR zoop samples

#read in libraries
#if (!require("pacman"))install.packages("pacman")
pacman::p_load(ggplot2, tidyverse)

#pull zoop files off of EDI (v4)
inUrl1  <-  "https://pasta.lternet.edu/package/data/eml/edi/197/4/9eb6db370194bd3b2824726d89a008a6" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")
zoop <-read.csv(infile1,header=T) 

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/197/4/0661e346ff63648f31221d9a1b6dbf32" 
infile2 <- tempfile()
download.file(inUrl2,infile2,method="curl")
zoop_dens <- read.csv(infile2,header=T) 

inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/197/4/a528fb7ef5b0ce473f54b206f7bbd551" 
infile3 <- tempfile()
download.file(inUrl3,infile3,method="curl")
zoop_biom <- read.csv(infile3,header=T) 

#get zoop date into correct format
zoop$DateTime <- lubridate::as_datetime(zoop$DateTime, tz="EST")

#------------------------------------------------------------------------------#
# quick visualizations for 2019-present zoops (doesn't work pre-2019 bc cladocerans, copepods, and rotifers were not calculated in summary file)
# see code associated with Wander et al. 2025 Ecological Modelling paper for how to aggregate 2014-2016 zoop data

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
#combine new (2022-2025) + old (2014-2022) zoop files

##### summary file ####
new_summary <- read.csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2025/Data/EDI_zoop_summary_2022-2025.csv")

#update DateTime format
new_summary$DateTime <- as.POSIXct(new_summary$DateTime, format = "%Y-%m-%d %H:%M:%S", tz="EST")

#merge dfs
summary_merged <- rbind(zoop, new_summary)

#### dens file ####
new_dens <- read.csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2025/Data/EDI_zoop_raw_dens_2022-2025.csv")

#update DateTime format
new_dens$DateTime <- as.POSIXct(new_dens$DateTime, format = "%Y-%m-%d %H:%M:%S", tz="EST")

#merge dfs
dens_merged <- rbind(zoop_dens, new_dens)

#### biom file ####
new_biom <- read.csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2025/Data/EDI_zoop_raw_biom_2022-2025.csv")

#update DateTime format
new_biom$DateTime <- as.POSIXct(new_biom$DateTime, format = "%Y-%m-%d %H:%M:%S", tz="EST")

#merge dfs
biom_merged <- rbind(zoop_biom, new_biom)


#------------------------------------------------------------------------------
#if dens = 0, add NA to other cols
summary_merged$MeanLength_mm[summary_merged$Density_IndPerL==0] <- NA
summary_merged$MeanWeight_ug[summary_merged$Density_IndPerL==0] <- NA
summary_merged$Biomass_ugL[summary_merged$Density_IndPerL==0] <- NA
  
#make sure marks in ocular micrometer col is numeric (NA warning bc NAs in this column)
biom_merged$MarksInOcularMicrometer_No. <- as.numeric(biom_merged$MarksInOcularMicrometer_No.)

#if density is NA, drop the row
summary_merged <- summary_merged[!is.na(summary_merged$Density_IndPerL),]

#change end depth of 0 --> 0.1
summary_merged$EndDepth_m[summary_merged$EndDepth_m==0] <- 0.1
biom_merged$EndDepth_m[biom_merged$EndDepth_m==0] <- 0.1
dens_merged$EndDepth_m[dens_merged$EndDepth_m==0] <- 0.1

#make sure zoop # is numeric and remove rows that are NA
dens_merged <- dens_merged |> 
  mutate(Zooplankton_No. = as.numeric(Zooplankton_No.)) |> 
  filter(!is.na(Zooplankton_No.)) |> 
  mutate(DateTime = lubridate::as_datetime(DateTime, tz="EST"))
                               
#biomass
biom_merged <- biom_merged |>   
  mutate(DateTime = lubridate::as_datetime(DateTime, tz="EST"))

#if time is midnight, add 1 s
summary_merged$DateTime[substr(summary_merged$DateTime,12,13) == ""] <-  
  summary_merged$DateTime[substr(summary_merged$DateTime,12,13) == ""] + 1

dens_merged$DateTime[substr(dens_merged$DateTime,12,13) == ""] <-  
  dens_merged$DateTime[substr(dens_merged$DateTime,12,13) == ""] + 1

biom_merged$DateTime[substr(biom_merged$DateTime,12,13) == ""] <-  
  biom_merged$DateTime[substr(biom_merged$DateTime,12,13) == ""] + 1
                        
#make sure there are no duplicate rows
summary_merged <- summary_merged |> distinct()

#export final dfs
write.csv(summary_merged, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2025/zoop_summary_2014_2025.csv", row.names=F)
write.csv(dens_merged, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2025/zoop_raw_dens_2019_2025.csv", row.names=F)
write.csv(biom_merged, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2025/zoop_raw_biom_2019_2025.csv", row.names=F)
