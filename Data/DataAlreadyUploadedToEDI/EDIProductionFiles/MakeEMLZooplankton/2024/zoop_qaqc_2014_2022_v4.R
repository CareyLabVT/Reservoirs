# zoop_qaqc_2014_2022.R
# Zooplankton QAQC script for visualizing data and merging files
# Created by HLW
# First developed: 05 Jul 2023
# Last edited: 16 Oct 2024
# The ONLY thing that differentiates this data product (Oct 2024) from 
# Jan 2024 pub is that 7 total rotifer density values have been updates!

#read in libraries
#if (!require("pacman"))install.packages("pacman")
pacman::p_load(ggplot2, tidyverse)

#pull zoop files off of EDI (v3)
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/197/3/9eb6db370194bd3b2824726d89a008a6" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")
zoop <-read.csv(infile1,header=T) 

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/197/3/0661e346ff63648f31221d9a1b6dbf32" 
infile2 <- tempfile()
download.file(inUrl2,infile2,method="curl")
zoop_dens <- read.csv(infile2,header=T) 

inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/197/3/a528fb7ef5b0ce473f54b206f7bbd551" 
infile3 <- tempfile()
download.file(inUrl3,infile3,method="curl")
zoop_biom <- read.csv(infile3,header=T) 

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
                                 Flag_Biomass) |> 
 rename("Flag_MeanLength_mm" = "Flag_Length",
        "Flag_MeanWeight_ug" = "Flag_Weight",
        "Flag_Biomass_ugL" = "Flag_Biomass")

#merge old df with new df
zoops_final <- rbind(old_final, zoop)

#-------------------------------------------------------------------------------#
# replace n = 7 total rotifer density values bc they were calculated incorrectly

zoops_final$Density_IndPerL[zoops_final$DateTime %in% as.POSIXct("2014-05-14 10:40:00") & 
                              zoops_final$Taxon=="Total rotifers"] <- 66.19

zoops_final$Density_IndPerL[zoops_final$DateTime %in% as.POSIXct("2014-05-29 11:10:00") & 
                       zoops_final$Taxon=="Total rotifers"] <- 20.71

zoops_final$Density_IndPerL[zoops_final$DateTime %in% as.POSIXct("2014-06-04 12:00:00") & 
                              zoops_final$Taxon=="Total rotifers"] <- 48.35

zoops_final$Density_IndPerL[zoops_final$DateTime %in% as.POSIXct("2014-07-02 10:50:00") & 
                              zoops_final$Taxon=="Total rotifers"] <- 11.33

zoops_final$Density_IndPerL[zoops_final$DateTime %in% as.POSIXct("2014-07-23 11:45:00") & 
                       zoops_final$Taxon=="Total rotifers"] <- 63.14

zoops_final$Density_IndPerL[zoops_final$DateTime %in% as.POSIXct("2014-08-13 10:05:00") & 
                              zoops_final$Taxon=="Total rotifers"] <- 23.91

zoops_final$Density_IndPerL[zoops_final$DateTime %in% as.POSIXct("2014-09-04 09:45:00") & 
                              zoops_final$Taxon=="Total rotifers"] <- 43.55

#------------------------------------------------------------------------------
#if dens = 0, add NA to other cols
zoops_final$MeanLength_mm[zoops_final$Density_IndPerL==0] <- NA
zoops_final$MeanWeight_ug[zoops_final$Density_IndPerL==0] <- NA
zoops_final$Biomass_ugL[zoops_final$Density_IndPerL==0] <- NA
  
#make sure marks in ocular micrometer col is numeric (NA warning bc NAs in this column)
zoop_biom$MarksInOcularMicrometer_No. <- as.numeric(zoop_biom$MarksInOcularMicrometer_No.)

#if density is NA, drop the row
zoops_final <- zoops_final[!is.na(zoops_final$Density_IndPerL),]

#make sure all taxa are uppercase
#zoops_final$Taxon <- str_to_sentence(zoops_final$Taxon)

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

#if time is midnight, add 1 s
zoops_final$DateTime[substr(zoops_final$DateTime,12,13) == ""] <-  
  zoops_final$DateTime[substr(zoops_final$DateTime,12,13) == ""] + 1

zoop_dens$DateTime[substr(zoop_dens$DateTime,12,13) == ""] <-  
  zoop_dens$DateTime[substr(zoop_dens$DateTime,12,13) == ""] + 1

zoop_biom$DateTime[substr(zoop_biom$DateTime,12,13) == ""] <-  
  zoop_biom$DateTime[substr(zoop_biom$DateTime,12,13) == ""] + 1
                        
#drop the one duplicate chaoborus sample on 2014-06-02
zoops_final <- zoops_final |> distinct()

#fix a couple of misspelled taxa
zoops_final$Taxon[zoops_final$Taxon=="Finilia"] <- "Filinia"
zoops_final$Taxon[zoops_final$Taxon=="bipalpus"] <- "Bipalpus"
zoops_final$Taxon[zoops_final$Taxon=="Nauplius"] <- "Nauplii"

#export final dfs
write.csv(zoops_final, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2024/zoop_summary_2014_2022.csv", row.names=F)
write.csv(zoop_dens, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2024/zoop_raw_dens_2019_2022.csv", row.names=F)
write.csv(zoop_biom, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLZooplankton/2024/zoop_raw_biom_2019_2022.csv", row.names=F)
