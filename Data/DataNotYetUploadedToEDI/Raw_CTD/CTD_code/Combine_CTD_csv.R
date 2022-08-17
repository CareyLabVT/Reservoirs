# This script is to combine the csv output into a season csv

if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
library(lubridate)
library(plyr)
library(readr)
library(dplyr)
library(tidyverse)

# Let's set it up for BVR 01 


#time to now play with FCR data!
#Gateway has missing data sections so combine manual data for EDI

#combine into one csv for the output from the new CTD 
mydir = "Data/DataNotYetUploadedToEDI/Raw_CTD/csv_outputs/New_CTD_S8188"
myfiles = list.files(path=mydir, pattern="S8188*", full.names=TRUE)#list the files from BVR platform


#create dataframe for the for loop
bvrCTD01<-""

#combine all of the files into one data sheet, have to come back and fix this loop
for(j in 1:length(myfiles)){
  fcrheader2<-read.csv(myfiles[j], as.is=T) #get header minus wonky Campbell rows
  bvrCTD01=rbind(fcrheader2, bvrCTD01)
}

#filter out blank rows
new=bvrCTD01%>%
  filter(Date!="")%>%
  filter(Depth_m>0)

#convert columns to numeric
new[, c(2:13)] <- sapply(new[, c(2:13)], as.numeric)

#add columns 
new$SN=8188
new$pH=NA
new$ORP_mV=NA

new=new %>% relocate(pH, .after=DO_pSat)%>%
  relocate(ORP_mV, .after=pH)

#combine into one csv for the output from the old CTD 
myfiles2 = list.files(path=mydir, pattern="S7809*", full.names=TRUE)#list the files from BVR platform


#create dataframe for the for loop
bvrCTD02<-""

#combine all of the files into one data sheet, have to come back and fix this loop
for(j in 1:length(myfiles2)){
  fcrheader3<-read.csv(myfiles2[j], as.is=T) #get header minus wonky Campbell rows
  bvrCTD02=rbind(fcrheader3, bvrCTD02)
}

#filter out blank rows
old=bvrCTD02%>%
  filter(Date!="")%>%
  filter(Depth_m>0)

#convert columns to numeric
old[, c(2:13)] <- sapply(old[, c(2:13)], as.numeric)

#add new columns
old$SN=7809

com=rbind(old,new)

#combine the casts from S7809 and S8188
com$Date=as.Date(com$Date)

#add in catwalk data 
cat=read_csv("Data/DataNotYetUploadedToEDI/Raw_CTD/csv_outputs/New_CTD_S8188/fcre-waterquality.csv")

new$Date=as.Date(new$Date)

cdom=new%>%
  filter(Date<"2022-07-13 00:00")%>%
  filter(Depth_m>1.2 & Depth_m<1.8)

ca=cat%>%
  filter(TIMESTAMP>"2022-05-24 00:00")
ca$Date=as.Date(ca$TIMESTAMP)

plot(ca$Date, ca$fDOM_RFU_1, type="l")
points(cdom$Date, cdom$CDOM_ugL, type="p", col="red")

#Graph just from FCR 
com%>%
  filter(Date<"2022-07-14")%>%
  #filter(Cond_uScm>0 & Cond_uScm<100)%>%
ggplot(data=., aes(x=PAR, y=Depth_m, col=as.factor(SN)))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap( ~ Date, ncol=2)

#Round the depth column to the hundreths columns
com$round_depth=round(com$Depth_m, digits=2)
  


# R Script to compare methods in the Analytical Lab
# Authors: Bobbie Niederlehner
# Last Edited: 06/07/2022

# Steps
#     1) load the libraries and the data
#     2) remove lines with missing values for either analysis
#     3) OPTIONAL: restrict the concentration range being analyzed
#     4) Paired T-Test- Test differences = 0 with paired T (NOT a preferred way to look at these data but magnitude of difference is interesting)
#     5) Bland Altman - Visualize differences over concentration (difference over mean concentration)
#     6) Passing Bablok - Test coincidence across concentrations - regression of new vs reference method.  Does it have a slope of 1 and an intercept of 0


# BACKGROUND INFO: Some web resources explaining method comparison regressions
# https://www.r-bloggers.com/deming-and-passing-bablok-regression-in-r/
# https://www.r-bloggers.com/2015/09/deming-and-passing-bablok-regression-in-r/
# http://labrtorian.com/tag/passing-bablok/
# https://www.r-bloggers.com/2016/08/a-shiny-app-for-passing-bablok-and-deming-regression/
# https://bahar.shinyapps.io/method_compare/


library("mcr", lib.loc="~/R/win-library/3.4")
library("BlandAltmanLeh", lib.loc="~/R/win-library/3.5")

setwd ("C:/Users/bniederl/Desktop/Stats in progress/Examples/Method Comparison Regression")

# read in the data consisting of Sample ID and concentrations from 2 methods
data0 = read.table ("data/FIA_NH4_salicylate_switch_21feb22.txt",header=TRUE, 
                    colClasses = c ("integer", "numeric", "numeric"))

# remove extraneous variables
data1 <- data0[c("ID", "NH4_phenolate", "NH4_salicylate")]

# remove lines with an NA in either method
data2 <- data1[complete.cases(data1),]

# (Optional) to subset by concentration if you want to look only at values above a detection limit
# data3 <-  subset(data2, NH4_phenolate>10.68 & NH4_salicylate > 10.68)

# finalize the data to be analyzed
data.final <- data2


# NOTE: the order of the variables in each analysis can matter
#     FOR paired T-test it doesn't matter.  Just pay attention to sign for difference.  Difference is calculated as 1st specified (x) minus 2nd specified (y) 
#     FOR Bland Altman it doesn't matter,  Default difference is 1st specified (x) minus 2nd specified (y) 
#     FOR Passing Bablok first listed is reference method (x, Method1, or current),  second listed is is test method (y, Method2, or new)
#       The intercept is often smean difference, but will have the reverse sign.  


# Paired T Test
sink (file = "results/mcr_results.txt", append=TRUE)
a <- t.test(data.final$NH4_phenolate, data.final$NH4_salicylate,
            paired=TRUE,
            conf.level=0.95, data=data.final)
print (a)
sink ()


#  Bland Altman Plots (difference over mean)
#      Estimates an agreement interval, within which 95% of the differences fall. 
#      BUT criterion for agreement should be decided on apriori and is completely based on judgement - what can you tolerate?
ba <- bland.altman.stats(data.final$NH4_phenolate,data.final$NH4_salicylate)
print(ba)
sink (file = "results/mcr_results.txt", append=TRUE)
print ("Bland Altman")
print (c("mean difference", "lower limit", "upper limit","sample size"))
print (c(ba$mean.diffs, ba$lower.limit, ba$upper.limit, ba$based.on))
sink ()

jpeg(file="results//NH4_method_switch_BA.jpeg",height=300, width=700)
bland.altman.plot(data.final$NH4_phenolate, data.final$NH4_salicylate, xlab = "Mean Concentration (ug/L)", ylab = "Difference (ug/L)",main = "NH4 Bland Altman")
dev.off()

#     IF you prefer GGPLOT2 specify that graph.sys  
#         geom_count = TRUE handles overlapping points by making the plot symbol larger
ba2 <- bland.altman.plot (data.final$NH4_phenolate, data.final$NH4_salicylate, graph.sys = "ggplot2", geom_count = TRUE)
print (ba2 +
         xlab("Mean Concentration (ug/L)") + 
         ylab ("Difference (ug/L)") +
         ggtitle("Bland Altman plot") )


# Method Comparison Regression using nonparametric Passing Bablok (MORE RESISTANT TO OUTLIERS) 
#     The first listed variable definitely ends up on the X axis and documentation says it is the "reference" method
#     The second listed variable ends up on the y axis and documentation says it is the "test" method
PB.reg <- mcreg(data.final$NH4_phenolate,data.final$NH4_salicylate, method.reg = "PaBa",mref.name = 'phenolate', mtest.name = 'salicylate')

sink (file = "results/mcr_results.txt", append=TRUE)
print ("")
print ("Passing Bablok")
printSummary (PB.reg)
print ("pearson correlation coefficient")
cor.test(data.final$NH4_phenolate,data.final$NH4_salicylate,
         method = "pearson")
sink ()

## Custom plots
jpeg(file="results/NH4_Method_Switch_PB.jpeg",height=300, width=700)
MCResult.plot(PB.reg, equal.axis = TRUE, x.lab = "Reference Method - Phenolate", y.lab = "Test Method - Salicylate", 
              points.col = "black", points.pch = 1, 
              ci.area = TRUE, ci.area.col = "gray", 
              identity.col="red3",
              main = "NH4 Method Switch - Passing Bablok", sub = "", add.grid = FALSE, points.cex = 1) 
dev.off()


# a residuals plot
jpeg(file="results/NH4_Method_Switch_PB_Residuals.jpeg",height=300, width=700)
MCResult.plotResiduals (PB.reg, 
                        ci.area = TRUE, ci.area.col = "gray", 
                        main = "NH4 Method Switch - Passing Bablok Residuals") 
dev.off()

# a bias plot
jpeg(file="results/NH4_Method_Switch_PB_Bias.jpeg",height=300, width=700)
MCResult.plotBias (PB.reg, 
                   ci.area = TRUE, ci.area.col = "gray", 
                   main = "NH4 Method Switch - Passing Bablok Bias") 
dev.off()





#write.csv(bvrCTD01, "Data/DataNotYetUploadedToEDI/Raw_CTD/CTD_season_csvs/BVR01_2022.csv", row.names=FALSE)
