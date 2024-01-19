# Title: Raw Files for the FCRMet_inspection.Rmd
# Author: Adrienne Breef-Pilz
# Date Made: 18 January 2024
# Last edited: 18 January 2024

# This code is the links to the raw data files on EDI. When you are staging the Met data for EDI, copy the list of files below
# and the tibble which lists all the arguments needed for the qaqc function. During EDI we want to run all the raw files through
# the qaqc function to generate the new csv each year. However, we do not want to publish the links to all of our 
# raw files when we publish to the production enviornment. Before publishing for REAL, replace the links and information
# for the tibble here. Any questions ask CCC or the point person. 


# List of the streaming raw data files from the Gateway. Since they are so large we have to make raw files for each year
# Make sure you have a legacy file for 2023 as 2024 will be in the FCRmet.csv file

ds <- c(
  'https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataAlreadyUploadedToEDI/CollatedDataForEDI/MetData/RawMetData_2015_2016.csv',
  'https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataAlreadyUploadedToEDI/CollatedDataForEDI/MetData/RawMetData_2017.csv',
  'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCRmet_legacy_2018.csv',
  'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCRmet_legacy_2019.csv',
  'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCRmet_legacy_2020.csv',
  'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCRmet_legacy_2021.csv',
  'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCRmet_legacy_2022.csv',
  # THE CURRENT YEARS FILE
  'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data/FCRmet.csv'
)


# Read in and make a data frame of the manual downloaded files. I make these each year with the raw files we have downloaded.
# See ManualDownloadsSCCData for the script on making the file for each year. 
# Link here : https://github.com/CareyLabVT/ManualDownloadsSCCData/tree/master/MetStation

man_files <- c(
  'https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/MetStation/FCRMet_ManualDownloads_2015_2016.csv','https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/MetStation/FCRMet_ManualDownloads_2017.csv',
  'https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/MetStation/FCRMet_ManualDownloads_2018.csv',
  'https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/MetStation/FCRMet_ManualDownloads_2019.csv',
  'https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/MetStation/FCRMet_ManualDownloads_2020.csv',
  'https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/MetStation/FCRMet_ManualDownloads_2021.csv',
  'https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/MetStation/FCRMet_ManualDownloads_2022.csv',
  'https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/MetStation/FCRMet_ManualDownloads_2023.csv'
)

# Make a tibble of the arguments going into the function. Each row are the arguments that will go into the function. 
#Make sure you have the correct raw files that go with the manual files. 
#If you want the same argument just repeat it each time. 
# Each row must be filled, even if it is just a NULL.

state_met <- tibble(data_file = ds, 
                    data2_file = man_files,
                    maintenance_file = rep('https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/MET_maintenancelog_new.csv',each=8),
                    met_infrad = rep('https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCR_Met_Infrad_DOY_Avg_2018.csv', each=8),
                    output_file = I(list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)),
                    start_date = I(list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)),
                    end_date = I(list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)),
                    notes = rep(TRUE, each=8))