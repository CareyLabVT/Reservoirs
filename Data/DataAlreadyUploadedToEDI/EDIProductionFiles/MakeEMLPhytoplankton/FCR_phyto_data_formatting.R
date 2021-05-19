#Phytoplankton Sample Entry
#Author: Mary Lofton
#Date: 17JUN20

#load packages
#install.packages('pacman')
pacman::p_load(tidyverse, lubridate)
rm(list=ls())

#read in data

dat <- dir(path = "C:/Users/Mary Lofton/Dropbox/Ch_2/BV_concentration/Pelagic/Site_50_QAQC", pattern = paste0(".csv")) %>% 
  map_df(~ read_csv(file.path(path = "C:/Users/Mary Lofton/Dropbox/Ch_2/BV_concentration/Pelagic/Site_50_QAQC", .), col_types = cols(.default = "c")))

dat1 <- dat %>% 
  mutate(BV_um3mL = as.double(BV_um3mL)) %>%
  select(Sample_date, Site, Depth, Genus, BV_um3mL) %>%
  mutate(Genus = ifelse(Genus == "unicell Chlorophyte spp. 1" | Genus == "unicell Chlorophyte sp. 1" | Genus == "chlorophyte spp. 1" | Genus == "Chlorophyte spp. 1" | Genus == "Chlorella" | Genus == "Chorella","Chlorophyte sp. 1",Genus)) %>%
  mutate(Genus = ifelse(Genus == "unicell Chlorophyte spp. 2" | Genus == "unicell Chlorophyte sp. 2" | Genus == "Chlorophyte spp. 2" | Genus == "Chlorophyte sp. 2" | Genus == "unicell Chlorophyte spp. 6" | Genus == "unicell Chlorophyte sp. 6" | Genus == "Chlorophyte spp. 6" | Genus == "Chlorophyte sp. 6","Rhodomonas",Genus)) %>%
  mutate(Genus = ifelse(Genus == "Chlorophyte spp. 3" | Genus == "unicell Chlorophyte spp. 3","Chlorophyte sp. 3",Genus)) %>%
  mutate(Genus = ifelse(Genus == "unicell Chlorophyte spp. 4","Chlorophyte sp. 4",Genus)) %>%
  mutate(Genus = ifelse(Genus == "chlorophyte spp. 5","Chlorophyte sp. 5",Genus)) %>%
  mutate(Genus = ifelse(Genus == "Euglena?","Euglena",Genus)) %>%
  mutate(Genus = ifelse(Genus == "Desmid spp. 1","Desmid sp. 1",Genus)) %>%
  mutate(Genus = ifelse(Genus == "filament" | Genus == "Psuedanabaena" | Genus == "Pseduanabaena","Pseudanabaena",Genus)) %>%
  mutate(Genus = ifelse(Genus == "dinoflagellate" | Genus == "Gymnodinium?" | Genus == "Gymnodinium??" | Genus == "Gymnodinuim" | Genus == "Gymnodinum" | Genus == "Gymnodinium" | Genus == "naked dino","Parvodinium",Genus)) %>%
  mutate(Genus = ifelse(Genus == "Mononastix","Monomastix",Genus)) %>%
  mutate(Genus = ifelse(Genus == "Woronchinia" | Genus == "Woronochinia","Woronichinia",Genus)) %>%
  mutate(Genus = ifelse(Genus == "Ankistodesmus" ,"Ankistrodesmus",Genus)) %>%
  mutate(Genus = ifelse(Genus == "Aphanothece" ,"Aphanocapsa",Genus)) %>%
  mutate(Genus = ifelse(Genus == "Dolichosphermum","Dolichospermum",Genus)) %>%
  mutate(Genus = ifelse(Genus == "Dysmorphococcus","Gloeodinium",Genus)) %>%
  mutate(Genus = ifelse(Genus == "Selenastrium","Selenastrum",Genus)) %>%
  mutate(Genus = ifelse(Genus == "dinoflagellate cyst","Prorocentrum",Genus)) %>%
  mutate(Genus = ifelse(Genus == "Dicytosphaerium" | Genus == "Dictyospaerium" | Genus == "Dicytospherium","Dictyosphaerium",Genus)) %>%
  mutate(Genus = ifelse(Genus == "Euglenoid/Cryptomonoid" | Genus == "Euglenoid/Cryptomonoid 2" | Genus == "Cryptophyte","Cryptomonas",Genus)) %>%
  mutate(Sample_date = ifelse(Sample_date == "7/10/2017","2017-07-10",Sample_date)) %>%
  mutate(Sample_date = ifelse(Sample_date == "2019-10-18","2019-10-16",Sample_date)) %>%
  filter(!Genus == "unknown")

dat1$Reservoir <- "FCR"

dat2 <- dat1 %>%
  group_by(Reservoir, Site, Sample_date, Genus, Depth) %>%
  summarize(BV_um3mL = sum(BV_um3mL, na.rm = TRUE))

dat3 <- dat2 %>% 
  rename(Date = Sample_date,
         Depth_m = Depth)

dat4 <- dat3[c(6,2,1,3,4,5)] %>%
  arrange(Date)

write.csv(dat3, file = "./00_Data_files/EDI_phytos/phytoplankton.csv",row.names = FALSE)
