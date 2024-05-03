# Metals Heatmap script with function
# Author: Adrienne Breef-Pilz
# Created: 10 APR 2024
# For: Carly Bauer to make heat maps


# Read in Packages
pacman::p_load(tidyverse, gridExtra)

# source the heatmap function from GitHub
source("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/Heatmap_EDI_function.R")

# Read in the data you want to use. You will have to change the path of this file because it doesn't exsist. 
metals <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLmetals/Metals_2014_2023.csv")

# filter the data to the time you want. Right now this is just for 2021.

met_2021 <- metals |>
  filter(year(DateTime)==2021)

### Make the baseheat map using the heatmap_EDI function. 
# You will have to run this for the different sites and elements.
# data = the name of your data frame with filtered data
# reservoir = the name of the reservoir which is used for filtering
# site = the number of the site you want which is used for filtering
# z = the column you want for your heatmap
# It will give you a warning because the points are all very similar. That's ok!


heatAl <- heatmap_EDI(data=met_2021, reservoir = "FCR", site = 50, z = "TBa_mgL")

# look at the basic plot
print(heatAl)

### Extra things you may want to change on the plot. 
# This is not the end all list but just 
# ones I usuall change or like to have the option to. 

heatAl2 <- heatAl + 
  # add a title
  ggtitle("FCR Total Alumnium from 2021") +  
  # change the x axis labels depending on how many breaks you want and how you want it labeled
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0)) +
  # adjust the size and the position of the title
  theme(plot.title = element_text(size=22, hjust = 0.5),
        axis.text=element_text(size=16), #change font size of axis text
        axis.title=element_text(size=20), #change font size of axis titles
        legend.text=element_text(size=20), #change font size of legend text
        legend.title=element_text(size=20)) #change font size of legend title   
  

# Print the heat map and see what it looks like
print(heatAl2)

### save the heatmap
# change the name of the file you are saving
# decide on where the file will be saved, file type, and the size of the image
ggsave("heatAl2.pdf", width = 20, height = 20, units = "cm")

### Putting multiple plots together into one file
# You can add extra things here too. See https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
heat_all <- gridExtra::grid.arrange(heatAl2, heatAl, nrow = 1, ncol = 2)

### save plot
# change the name of the file you are saving
# decide on where the file will be saved, file type, and the size of the image
ggsave("All_FCR.pdf", width = 20, height = 20, units = "cm")


