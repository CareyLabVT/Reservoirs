#Metals Sample ID Generator
#Author: CEW
#Written 09 Nov 23

#Purpose: to generate sample ID names in a spreadsheet to send for ICPMS analysis

#Load all of the packages you will need (and probably more)
rm(list=ls(all=TRUE))
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(stringr)
library(ggplot2)
library(readr)

# need to start with field dates; update for each Jeff run
# make sure to edit both Date and SaveFileLocation before proceeding!!!
Date <- c('05/05/2025', '05/12/2025', '05/19/2025', '05/26/2025') #creates a vector of all of the dates
SaveFileLocation <- '~/Documents/Metals2025SampleListA.csv' # fill in your own save file location and file name


# more background stuff to generate the list
SampleList <- as.character() #creates an empty vector that will be filled with sample IDs
n <- length(Date) #this will help us set up our for loop below


# this nested for loop will go through all of the dates in our 'Date' vector and append them with the sample numbers
# in this case, 'i' refers to the Date and 'j' refers to the sample number
for(i in 1:n){
  for (j in 5:37) { # adjust j as needed, 1:30 is good for a routine FCR/BVR day, check sample key for other reservoirs/depths
    SampleList <- c(SampleList, paste(Date[i], j, sep = ' - '))
  }
}


# note: will need to manually delete the entries for samples w/ particulates
# check the data frame to make sure that the nested for loop worked
frame1 <- data.frame(SampleList) 

# now write the csv to your computer
data.frame(SampleList) %>% 
  write_excel_csv(SaveFileLocation, delim = ',')
# done! :)