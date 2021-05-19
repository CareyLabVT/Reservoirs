#Phytoplankton Sample Entry
#Author: Mary Lofton
#Date: 21MAY20

#load packages
#install.packages('pacman')
pacman::p_load(readxl, zoo, tidyverse)

rm(list=ls())

#source shape equations
source("./00_Data_files/EDI_phytos/shape_BV_functions.R")

#Tasks:
#1. Enter basic ID information for each sample 
#2. Calculate biovolume for each genus and total biovolume
#3. Calculate biovolume concentration (so incorporate area counted)
#4. Visualize each sample by biovolume and/or relative proportion of each division
#5. Visualize effort curve for sample
#6. Populate metadata logs with appropriate info (slope effort curve, etc.)

#Task 1: Enter basic ID information for each sample

############################################################
#needs to be in format yyyy-mm-dd
Sample_date = "2016-07-25"

#choose from 50, 45, 20
Site = 50

#choose from R (recruitment) or P (pelagic)
Sample_type = "P"

#in meters (for pelagic samples only)
Depth = 6

#choose from Y or P (for recruitment samples only)
Rep = NA

#needs to be in format yyyy-mm-dd
Count_date = "2020-06-15"

#in mls
Volume_filtered = 20

#number of grids counted
n = 8

############################################################

#Task 2: Calculate biovolume for each genus

dat <- read_xlsx("C:/Users/Mary Lofton/Dropbox/Ch_2/Countsheets/Pelagic/Site_50_QAQC/F25JUL16P50_6.xlsx", sheet = "Count") %>%
  mutate(Field = na.locf(Field),
         Genus = na.locf(Genus),
         Shape = na.locf(Shape),
         Tally = na.locf(Tally))

scan <- read_xlsx("C:/Users/Mary Lofton/Dropbox/Ch_2/Countsheets/Pelagic/Site_50_QAQC/F25JUL16P50_6.xlsx", sheet = "Scan") %>%
  mutate(Field = na.locf(Field),
         Genus = na.locf(Genus),
         Shape = na.locf(Shape),
         Tally = na.locf(Tally)) %>%
  mutate(Tally = n/80)


#calculate biovolumes for count
final <- data.frame(Genus = character(),
                    BV_um3 = double())

for (i in 1:length(unique(dat$Shape))){
  
  Shape = unique(dat$Shape)[i]
  
  shape_dat = dat %>% filter(Shape == unique(Shape)[i])
  
  if(!shape_dat$Shape[1] %in% c("prolate spheroid","sphere","dome (half prolate spheroid)",
                                "ellipsoid","2 half ellipsoids","cylinder","2 truncated cones",
                                "cylinder and 2 cones","cylinder + 2 cones","sickle-shaped prism",
                                "2 cones","box","elliptic prism","2 ellipsoids","cone + half sphere",
                                "cone and half sphere","box and two cylinders","box + 2 cylinders",
                                "box and 2 cylinders","prism on parallelogram","cylinder + cone","cylinder and cone",
                                "cube")){
    print("No match for shape:")
    print(shape_dat$Shape[1])
  }
  
  for (j in 1:length(unique(shape_dat$Genus))){
    
    genus_dat = shape_dat %>% filter(Genus == unique(Genus)[j])
    
    tally = genus_dat$Tally[1]
    
    measurements = genus_dat %>%
      select(Measurement:m10) %>%
      gather(m1:m10, key = m, value = um) %>%
      group_by(Measurement) %>%
      summarize(um_avg = mean(um, na.rm = TRUE)) %>%
      arrange(Measurement)
    
    m <- measurements$um_avg
    
    if (Shape == "prolate spheroid"){
      BV = prolate.spheroid(m[1],m[2])*tally
    } else if (Shape == "sphere"){
      BV = sphere(m[1])*tally
    } else if (Shape == "dome (half prolate spheroid)"){
      BV = dome(m[1],m[2])*tally
    } else if (Shape == "ellipsoid" | Shape == "2 half ellipsoids"){
      BV = ellipsoid(m[1],m[2],m[3])*tally
    } else if (Shape == "cylinder"){
      BV = cylinder(m[1],m[2])*tally
    } else if (Shape == "2 truncated cones"){
      BV = two.truncated.cones(m[1],m[2],m[3])*tally
    } else if (Shape == "cylinder and 2 cones" |Shape == "cylinder + 2 cones"){
      BV = cylinder.and.two.cones(m[1],m[2],m[3])*tally
    } else if (Shape == "sickle-shaped prism"){
      BV = sickle.shaped.prism(m[1],m[2],m[3],m[4],m[5])*tally
    } else if (Shape == "2 cones"){
      BV = two.cones(m[1],m[2])*tally
    } else if (Shape == "box"){
      BV = box.bv(m[1],m[2],m[3]*tally)
    } else if (Shape == "elliptic prism"){
      BV = elliptic.prism(m[1],m[2],m[3])*tally
    } else if (Shape == "2 ellipsoids"){
      BV = two.ellipsoids(m[1],m[2],m[3])*tally
    } else if (Shape == "cone + half sphere" |Shape == "cone and half sphere"){
      BV = cone.and.half.sphere(m[1],m[2])*tally
    } else if (Shape == "box and 2 cylinders" |Shape == "box + 2 cylinders" | Shape == "box and 2 cylinders"){
      BV = box.and.two.cylinders(m[1],m[2],m[3],m[4],m[5])*tally
    } else if (Shape == "prism on parallelogram"){
      BV = prism.on.parallelogram(m[1],m[2],m[3])*tally
    } else if (Shape == "cylinder + cone" | Shape == "cylinder and cone"){
      BV = cylinder.and.cone(m[1],m[2],m[3])*tally
    } else if (Shape == "cube"){
      BV = cube(m[1])*tally
    }
    
    if (!is.na(genus_dat$Colonial[1])){
      
      colonies = genus_dat[1,] %>%
        select(c1:c10)
      
      avg_colony_size = rowMeans(colonies[1,], na.rm = TRUE)
      
      BV_tot <- BV*avg_colony_size
    } else {BV_tot = BV}
    
    final_row <- data.frame(Genus = as.character(genus_dat$Genus[1]),
                            BV_um3 = as.double(BV_tot))
    
    final <- rbind(final, final_row)
    
  }
  
}

#calculate biovolumes for scan
final_scan <- data.frame(Genus = character(),
                         BV_um3 = double())

if(!length(scan$Genus) == 0){
  
  for (i in 1:length(unique(scan$Shape))){
    
    Shape = unique(scan$Shape)[i]
    
    shape_dat = scan %>% filter(Shape == unique(Shape)[i])
    
    if(!shape_dat$Shape[1] %in% c("prolate spheroid","sphere","dome (half prolate spheroid)",
                                  "ellipsoid","2 half ellipsoids","cylinder","2 truncated cones",
                                  "cylinder and 2 cones","cylinder + 2 cones","sickle-shaped prism",
                                  "2 cones","box","elliptic prism","2 ellipsoids","cone + half sphere",
                                  "cone and half sphere","box and two cylinders","box + 2 cylinders",
                                  "box and 2 cylinders","prism on parallelogram","cylinder + cone","cylinder and cone",
                                  "cube")){
      print("No match for shape:")
      print(shape_dat$Shape[1])
    }
    
    for (j in 1:length(unique(shape_dat$Genus))){
      
      genus_dat = shape_dat %>% filter(Genus == unique(Genus)[j])
      
      tally = genus_dat$Tally[1]
      
      measurements = genus_dat %>%
        select(Measurement:m10) %>%
        gather(m1:m10, key = m, value = um) %>%
        group_by(Measurement) %>%
        summarize(um_avg = mean(um, na.rm = TRUE)) %>%
        arrange(Measurement)
      
      m <- measurements$um_avg
      
      if (Shape == "prolate spheroid"){
        BV = prolate.spheroid(m[1],m[2])*tally
      } else if (Shape == "sphere"){
        BV = sphere(m[1])*tally
      } else if (Shape == "dome (half prolate spheroid)"){
        BV = dome(m[1],m[2])*tally
      } else if (Shape == "ellipsoid" | Shape == "2 half ellipsoids"){
        BV = ellipsoid(m[1],m[2],m[3])*tally
      } else if (Shape == "cylinder"){
        BV = cylinder(m[1],m[2])*tally
      } else if (Shape == "2 truncated cones"){
        BV = two.truncated.cones(m[1],m[2],m[3])*tally
      } else if (Shape == "cylinder and 2 cones" |Shape == "cylinder + 2 cones"){
        BV = cylinder.and.two.cones(m[1],m[2],m[3])*tally
      } else if (Shape == "sickle-shaped prism"){
        BV = sickle.shaped.prism(m[1],m[2],m[3],m[4],m[5])*tally
      } else if (Shape == "2 cones"){
        BV = two.cones(m[1],m[2])*tally
      } else if (Shape == "box"){
        BV = box.bv(m[1],m[2],m[3]*tally)
      } else if (Shape == "elliptic prism"){
        BV = elliptic.prism(m[1],m[2],m[3])*tally
      } else if (Shape == "2 ellipsoids"){
        BV = two.ellipsoids(m[1],m[2],m[3])*tally
      } else if (Shape == "cone + half sphere" |Shape == "cone and half sphere"){
        BV = cone.and.half.sphere(m[1],m[2])*tally
      } else if (Shape == "box and 2 cylinders" |Shape == "box + 2 cylinders" | Shape == "box and 2 cylinders"){
        BV = box.and.two.cylinders(m[1],m[2],m[3],m[4],m[5])*tally
      } else if (Shape == "prism on parallelogram"){
        BV = prism.on.parallelogram(m[1],m[2],m[3])*tally
      } else if (Shape == "cylinder + cone" | Shape == "cylinder and cone"){
        BV = cylinder.and.cone(m[1],m[2],m[3])*tally
      } else if (Shape == "cube"){
        BV = cube(m[1])*tally
      }
      
      if (!is.na(genus_dat$Colonial[1])){
        
        colonies = genus_dat[1,] %>%
          select(c1:c10)
        
        avg_colony_size = rowMeans(colonies[1,], na.rm = TRUE)
        
        BV_tot <- BV*avg_colony_size
      } else {BV_tot = BV}
      
      final_row <- data.frame(Genus = as.character(genus_dat$Genus[1]),
                              BV_um3 = as.double(BV_tot))
      
      final_scan <- rbind(final_scan, final_row)
      
    }
    
  }
}


#combine count and scan data
scan_genera <- unique(final_scan$Genus)

if(!length(scan$Genus) == 0){
  
  for (q in 1:length(scan_genera)){
    
    scan_genus <- final_scan %>%
      filter(Genus == scan_genera[q])
    
    if(scan_genera[q] %in% unique(final$Genus)){
      dat_genus <- final %>%
        filter(Genus == scan_genera[q])
      final <- final %>%
        filter(!Genus == scan_genera[q])
      dat_genus$BV_um3 <- dat_genus$BV_um3 + scan_genus$BV_um3
      final <- bind_rows(final, dat_genus)
    } else
    {
      final <- bind_rows(final, scan_genus)
    }
    
  }
}
#Task 3 get biovolume concentrations

final1 <- bv.concentration(final, Volume_filtered, n)

final1$Sample_date <- as.Date(Sample_date)
final1$Site <- Site
final1$Depth <- Depth
final1$Rep <- Rep
final1$Count_date <- as.Date(Count_date)
final1$Sample_type <- Sample_type

write.csv(final1, file = "C:/Users/Mary Lofton/Dropbox/Ch_2/BV_concentration/Pelagic/Site_50_QAQC/F25JUL16P50_6.csv", row.names = FALSE)



