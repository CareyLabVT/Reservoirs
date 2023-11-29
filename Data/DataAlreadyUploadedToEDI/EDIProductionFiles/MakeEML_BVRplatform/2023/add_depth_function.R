add_depth <- function(data_file, depth_offset, output, Date_offset, Offset_column1, Offset_column2, round_digits)
{
  data <- read_csv(data_file)
  depth <- read_csv(depth_offset)

   # EXO=data%>%
   #   select(Reservoir, Site, DateTime, starts_with("EXO"))
  

  # Select the sensors on the temp string because they are stationary.
  # Then pivot the data frame longer to merge the offset file so you can add a depth to each sensor reading
  long=data%>%
    select(Reservoir, Site, DateTime, starts_with("Depth"), starts_with("Ther"), starts_with("RDO"), starts_with("Lvl"))%>%
    dplyr::rename("Depth_m"=starts_with("Depth"))%>% 
    pivot_longer(-c(Reservoir,Site,DateTime,Depth_m), names_to="variable", values_to="observation", values_drop_na=FALSE)%>%
    separate(variable,c("variable","units","Position"),"_")%>%
    mutate(Position=as.numeric(Position))%>%
    left_join(.,depth, by=c("Position","Reservoir","Site"))#add in the offset file
    
  
  # The pressure sensor was moved to be in line with the bottom thermistor. The top two thermistors had slid closer to each other
  # and were re-secured about a meter a part from each other. Because of this we need to filter before 2021-04-05 13:20:00 EST
  # and after. The top two thermistors exact offset will have to be determined again when the water level is high enough again. 
  
  # If the sensors string was never moved then Date_offset is NULL
  if (is.null(Date_offset)){
    long_depth<-long%>%
      mutate(sensor_depth=Depth_m-.[[Offset_column1]])%>%#find the depth of the sensor using the specified offset column
      mutate(rounded_depth=round(sensor_depth, round_digits))%>%#rounded the depth to 0.5 if I did to 1 there would be duplicates
      filter(!is.na(observation))%>%
      filter(!is.na(sensor_depth))
      
  }else{
   
  pre_depth=long%>%
    filter(DateTime<=ymd(Date_offset))%>%
    mutate(sensor_depth= Depth_m- .[[Offset_column1]])%>% #this gives you the depth of the thermistors from the surface
    mutate(rounded_depth=round(sensor_depth, round_digits)) #Round to digits specified in function
     
  
  post_depth=long%>%
    filter(DateTime>ymd(Date_offset))%>%
    mutate(sensor_depth= Depth_m- .[[Offset_column1]])%>% #this gives you the depth of the thermistor from the surface
    mutate(rounded_depth=round(sensor_depth, round_digits)) #Round the digit specified in the function
     
  
  
  long_depth<-pre_depth%>%
    bind_rows(.,post_depth)%>% # bind the 2 separate data frames
    filter(!is.na(observation))%>% # take out readings that are NA
    filter(!is.na(sensor_depth)) # remove observations if there is no depth associated with it
  }
  
  # only select the columns you want
  final<-long_depth%>%
    select(Reservoir, Site, DateTime, variable, Position, sensor_depth, rounded_depth, observation)

  # If you want to save the output then give the file a name
  if(!is.null(output)){
  write.csv(final, output, row.names = FALSE)
  }
  
  
  return(final)
  
}

# Instructions on how to use function. Need to use tidyverse 

# data_file = the file of most recent data either from EDI or GitHub. Currently reads in the L1 file
# depth_offset = the file of depth of each sensor relative to each other. This file for BVR is on GitHub
# output = the path where you would like the data saved
# Date_offset = the date we moved the sensors so we know where to split the file. If you don't need to split the file put NULL
# Offset_column1 = name of the column in the depth_offset file to subtract against the actual depth to get the sensor depth
# Offset_column2 = name of the second column if applicable for the column with the depth offsets
# round_digits = number of digits you would like to round to

# Using the function
# add_depth(
# data_file <- "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv",
# depth_offset <- "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv",
# output <- "output_wdepths_2020_2023.csv", 
# Date_offset<- "2021-04-05",
# Offset_column1<- "Offset_before_05APR21",
# Offset_column2 <- "Offset_after_05APR21",
# round_digits = 2
# )

 