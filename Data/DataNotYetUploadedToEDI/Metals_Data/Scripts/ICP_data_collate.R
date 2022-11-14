# Working with the ICP data
# Read in a combine all ICP data from 2022
# Add the correct reservoir, site and depth to the observations

#By Adrienne Breef-Pilz 14 NOV 22


#install.packages("googlesheets4") ABP working on a way to read in the date and times from the nutrient data
pacman::p_load(tidyverse,lubridate)

# column names when there are 31 columns
COL_NAMES=c('#','ID', '7Li','23Na',	'24Mg',	'27Al', '29Si',	'31P',	'34S', '35Cl', '39K',	'44Ca',	'48Ti', '51V', '52Cr',
            '54Fe',	'55Mn',	'59Co',	'60Ni',	'65Cu',	'66Zn',	'75As',	'78Se',	'88Sr',	'95Mo',	'107Ag', '111Cd',
            '120Sn', '138Ba','208Pb',	'238U')

# This code was for how you work with the excel files which is the format the original metals files data
# comes in

# For loop to read in files. Right now can only do one folder at a time 
# mydir="G:/Shared drives/Reservoir Group/Field Work/Metals Sampling Data/2021_ICPMS"
# mydir = "./Data/DataNotYetUploadedToEDI/ABP_Conductivity/Data/ICP_data"
# 
# myfiles = list.files(path=mydir, pattern=".xlsx", full.names=TRUE)
# 
# #taking out the the Temp Test files
# myfilesBVR <- myfiles[ !grepl("CR6_BVR_TempTest*", myfiles) ]#exclude the Temp test data
# 
# #combine the files
# #create an out.file for the combined data
# out.file<-""
# #for loop to combine the files
# for(i in 1:length(myfiles)){
#   sheetnames<-excel_sheets(myfiles[1])
#   correctfiles <- sheetnames[ grepl("Cissy*|Nick*", sheetnames) ]#select the from Cissy
#   
#   file<-read_excel(myfiles[1], sheet = correctfiles, skip=6, col_names=COL_NAMES)
#   #if(is.na(file)){
#   #  file<-read_excel(myfiles[i], sheet = "Cissy", skip=6, col_names=COL_NAMES)
#   #}
#   out.file <- rbind(out.file, file)
#}
  

#For loop to read in csv files from the haven't uploaded to EDI folder. 
# This format requires someone to save it as a csv and upload it to the correct folder
# Right now can only do one folder at a time. Each folder is seperated by year. 

mydircsv = "./Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/2022"

# List all of the files in the directory above
 myfilescsv = list.files(path=mydircsv, pattern="", full.names=TRUE)

 #combine the files into one large data frame
 #create an out.file for the combined data
 out.file<-""
 #for loop to combine the files
 for(i in 1:length(myfilescsv)){
 
   file<-read_csv(myfilescsv[i], skip=5) # read in the file with no header
   # the if statement is to add the headers based on if there is a column of row numbers or not
   if(ncol(file)==30){
     colnames(file) <- c('ID', '7Li','23Na',	'24Mg',	'27Al', '29Si',	'31P',	'34S', '35Cl', '39K',	'44Ca',	'48Ti', '51V', '52Cr',
                       '54Fe',	'55Mn',	'59Co',	'60Ni',	'65Cu',	'66Zn',	'75As',	'78Se',	'88Sr',	'95Mo',	'107Ag', '111Cd',
                       '120Sn', '138Ba','208Pb',	'238U')
   }else{
     colnames(file)=COL_NAMES
     file<-file%>%select(!'#') # take out the row number column
   }
     
   file2<-file%>%
     filter(ID!="")%>% # take out the row when ID is blank
     separate(ID,c("Date","Sample"),"-") # separate time and sample number into two columns
     

   out.file <- rbind(out.file, file2)
}



# Read in the file the links sample number to depth, reservoir and site
df_sample<- read_csv("./Data/DataNotYetUploadedToEDI/Metals_Data/Scripts/Metals_Sample_Depth.csv")

# Do some QAQC and create and date column, depth column, reservoir, filtered
ICP=out.file%>%
  filter(Sample!="")%>%
  mutate(Sample=as.numeric(Sample))%>% # change Sample column to numeric so we can merge better
  merge(.,df_sample,by='Sample', all=T) # merge by the Sample number 
  

#change the date format

ICP$Date <- as.POSIXct(ICP$Date, format = "%m/%d/%Y", tz = "UTC") 

# Make the numbers into numeric instead of character
ICP[, c(3:31)] <- sapply(ICP[, c(3:31)], as.numeric)

#divide by 1000 to get ppm as opposed to ppb
ICP[, c(3:31)] <- ICP[, c(3:31)]/1000 

#write to a csv
write.csv(ICP,paste0(mydircsv,'/ICP_data_2022.csv'))

# Read in the times metals and nutrients data were taken to add times collected to observations
# ABP still working on this because I am having authentification errors

excel_nuts_times="G:/Shared drives/Reservoir Group/2025_Desktop/2022 Reservoir Field Sheets"


nuttimes<-read_sheet('1VGqarE2FUpphqSDyCagCfAvAG_zYJlvbIRGBF6Br_pQ')




