# This script is used by the EDI data package reviewer to visualize a few plots for QAQC purposes
# By: Adrienne Breef-Pilz

#### Load Packages ####

pacman::p_load(tidyverse, lubridate, scattermore)

#### This section is from EDI R Code section ####


# SECTION YOU NEED TO CHANGE
# Need to update the URL each time from EDI
inUrl1  <- "https://pasta-s.lternet.edu/package/data/eml/edi/143/27/d4c74bbb3b86ea293e5c52136347fbb0" 

# CHANGE HERE Current Start and ends times

current_time_start="2023-01-01 00:00:00, tz=UTC"
current_time_end="2023-12-31 23:59:00, tz=UTC"

# DON'T NEED TO CHANGE ANYTHING BELOW HERE

infile1 <- tempfile()
# Download the data here from the above URL
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

# read in the data file using 
dt1 <-read_csv(infile1)

# Set the current year we are publishing

current <- dt1%>%
  filter(DateTime>=ymd_hms(current_time_start) & DateTime<ymd_hms(current_time_end))


# plotting function to make a plot from all the data and then looking at the current data

all_plot<-function(Var){
  
    all<- ggplot() +
      geom_scattermore(data=dt1, aes_string(x="DateTime", y=Var))+
      ggtitle("All",Var) +
      theme_bw()
    
    cur<- ggplot() +
      geom_scattermore(data=current, aes_string(x="DateTime", y=Var), pointsize = 3) +
      ggtitle("Current",Var) +
      theme_bw()
    

    newlist <- list(all,cur)
  
  return(newlist)
}
 
#####Choose Variables for plots ##### 

# Use the function and pick a variable to plot. Put variable in quotes
# Below these are examples. Choose variables you think are worth looking at

all_plot(Var="PAR_Total_mmol_m2")

all_plot(Var="RDO_mgL_6")
               