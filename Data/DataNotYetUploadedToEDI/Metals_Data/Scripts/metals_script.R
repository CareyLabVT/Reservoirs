#install.packages("readxl")
#library(readxl)
#metals2021<-read_excel("metals_9_27.xlsx")

##MODIFY FOR EACH DATASHEET**

#metals_10_12<-read.csv("metals_10_14.csv")

#metals_10_28<-read.csv("metals_10_28.csv")

metal_read<-read.csv("metals_8_12.csv")
#pick out only Fe and Mn columns with date ID column
Fe_Mn<-cbind(metal_read$X, metal_read$X54Fe..STDR., metal_read$X55Mn..STDR.)
#convert from matrix to data frame
Fe_Mn<-as.data.frame(Fe_Mn)



#get rid of empty space in Fe_Mn dataframe
Fe_Mn<-Fe_Mn[-c(1:2),]
###########################

names(Fe_Mn)<-c("date_id","Fe(ppm)","Mn(ppm)")
#remove commas from numbers
Fe_Mn$`Fe(ppm)`<-gsub(",","",Fe_Mn$`Fe(ppm)`)

Fe_Mn$`Mn(ppm)`<-gsub(",","",Fe_Mn$`Mn(ppm)`)
#convert Fe and Mn values to ppm
Fe_Mn$`Fe(ppm)`<-as.numeric(Fe_Mn$`Fe(ppm)`)/1000
Fe_Mn$`Mn(ppm)`<-as.numeric(Fe_Mn$`Mn(ppm)`)/1000


sampleID<-regexpr("- [[:digit:]]*",Fe_Mn$date_id)
sampleIDs<-regmatches(Fe_Mn$date_id,sampleID)
#get rid of all dashes and spaces, then convert IDS into numeric form
cleanIDs<-gsub("- ","",sampleIDs)
cleanIDs<-as.numeric(cleanIDs)

#convert sample No into depths

##sometimes final depth for BVR is 10 m 
depths<-c()
for (i in seq(1,length(cleanIDs))){
  ID<-cleanIDs[i]
  if (ID==1 | ID==2){depths<-c(depths,0.1)}
  else if (ID==3 | ID==4){depths<-c(depths,1.6)}
  else if (ID==5 | ID==6){depths<-c(depths,3.8)}
  else if (ID==7 | ID==8){depths<-c(depths,5.0)}
  else if (ID==9 | ID==10){depths<-c(depths,6.2)}
  else if (ID==11 | ID==12){depths<-c(depths,8.0)}
  else if (ID==13 | ID==14){depths<-c(depths,9.0)}
  else if (ID==15 | ID==16){depths<-c(depths,0.1)}
  else if (ID==17 | ID==18){depths<-c(depths,0.1)}
  #BVR
  else if (ID==19 | ID==20){depths<-c(depths,0.1)}
  else if (ID==21 | ID==22){depths<-c(depths,3.0)}
  else if (ID==23 | ID==24){depths<-c(depths,6.0)}
  else if (ID==25 | ID==26){depths<-c(depths,9.0)}
  else if (ID==27 | ID==28){depths<-c(depths,11.0)}
  #ISCO
  else if (ID==29 | ID==30){depths<-c(depths,0.1)}
  #BVR 40
  else if (ID==31 | ID==32){depths<-c(depths,0.1)}
  else if (ID==33 | ID==34){depths<-c(depths,3.0)}
  else if (ID==35 | ID==36){depths<-c(depths,4.0)}
  #CCR 50
  else if (ID==37 | ID==38){depths<-c(depths,0.1)}
  else if (ID==39 | ID==40){depths<-c(depths,3.0)}
  else if (ID==41 | ID==42){depths<-c(depths,5.0)}
  else if (ID==43 | ID==44){depths<-c(depths,9.0)}
  #defunct CCR depths
  else if (ID==45 | ID==46){depths<-c(depths,12.0)}
  else if (ID==47 | ID==48){depths<-c(depths,15.0)}
  else if (ID==49 | ID==50){depths<-c(depths,18.0)}
  #updated CCR depths
  else if (ID==51 | ID==52){depths<-c(depths,1.5)}
  else if (ID==53 | ID==54){depths<-c(depths,6.0)}
  else if (ID==55 | ID==56){depths<-c(depths,15.0)}
  else if (ID==57 | ID==58){depths<-c(depths, 21.0)}
  #for CCR tributaries
  else {depths<-c(depths,0.1)}
  
}

##divide depths in half
depths<-c(depths[c(T,F)])

#append clean ID column
Fe_Mn<-cbind(Fe_Mn,cleanIDs)

Fe_Mn$date_id<-gsub("- [[:digit:]]*", "", Fe_Mn$date_id)

#take only half the dates (have to check if the dates are matched up)
sampleDates<-c(Fe_Mn$date_id[c(T,F)])

# iterate through IDs and sort into soluble vs. insoluble by even/odd ID number


##NEED TO MAKE SURE ONLY CONSECUTIVE SAMPLE IDs GET PAIRED UP

# iterate through and the code should hopefully flag where there's an uneven number of samples...
#should be 45

#consecutive.lst<-c()
#noncon.lst<-c()

#last<-length(Fe_Mn$cleanIDs)

#for (i in seq(1,last)){
#  if (diff(c(as.numeric(Fe_Mn$cleanIDs[i]),as.numeric(Fe_Mn$cleanIDs[i+1])))==1){
#    consecutive.lst<-c(consecutive.lst,Fe_Mn$cleanIDs[i])}
#  else if (as.numeric(Fe_Mn$cleanIDs[i])%%2==0){
#    consecutive.lst<-c(consecutive.lst,Fe_Mn$cleanIDs[i])
#  }
#    else {noncon.lst<-c(noncon.lst,i)}
  
#}


#insert a dummy row where there aren't matching samples for each site
#this is if an item on the nonconsecutive indices list is exactly two from the next ID, which means there's exactly one missing from the sequence

#install.packages("DataCombine")
#library(DataCombine)
#for (t in noncon.lst){
#  if (diff(c(Fe_Mn$cleanIDs[t],Fe_Mn$cleanIDs[t+1]))==2){
#    #somehow the new row isn't inserting?
#    Fe_Mn<-InsertRow(Fe_Mn, NewRow=c(Fe_Mn$date_id[t],-999,-999,as.numeric(Fe_Mn$cleanIDs[t])+1),RowNum=t+1)
#  }
#}

##even=solubles
##odd=totals
SFe<-c()
TFe<-c()
SMn<-c()
TMn<-c()


for (i in seq(1,length(Fe_Mn$cleanIDs))){
  if (as.numeric(Fe_Mn$cleanIDs[i])%%2==1){
    TFe<-c(TFe,Fe_Mn$`Fe(ppm)`[i])
    TMn<-c(TMn,Fe_Mn$`Mn(ppm)`[i])}
   else{
      SFe<-c(SFe,Fe_Mn$`Fe(ppm)`[i])
      SMn<-c(SMn,Fe_Mn$`Mn(ppm)`[i])
    }
}

#populate reservoir column with either BVR, FCR or CCR
reservoir<-c()

for (i in seq(1,length(Fe_Mn$cleanIDs))){
  if (as.numeric(Fe_Mn$cleanIDs[i])<19) {
    reservoir<-c(reservoir,"FCR")}
  else if (Fe_Mn$cleanIDs[i]==29 | Fe_Mn$cleanIDs==30){
    reservoir<-c(reservoir,"FCR")
  }
    else if (as.numeric(Fe_Mn$cleanIDs[i])>18 & as.numeric(Fe_Mn$cleanIDs[i]) < 29 | as.numeric(Fe_Mn$cleanIDs[i]) > 30 & as.numeric(Fe_Mn$cleanIDs[i]) < 37 ){
      reservoir<-c(reservoir,"BVR")}
  else {reservoir <-c(reservoir, "CCR")}
    }

#take only half of reservoir values
reservoir<-c(reservoir[c(T,F)])

#populate column with site numbers
##samples 15 and 16 are from inflow (100)
##samples 17 and 18 are from wetland (200)

#replace with 100 or 200 if inflow (include ISCO A and B) or wetland
#upstream Beaver site is 200 
#FCR dam is 50
siteNo<-rep(50,length(Fe_Mn$cleanIDs))
for (i in seq(1,length(Fe_Mn$cleanIDs))){
  if (as.numeric(Fe_Mn$cleanIDs[i])==15 | as.numeric(Fe_Mn$cleanIDs[i])==16 | as.numeric(Fe_Mn$cleanIDs[i])==29 | as.numeric(Fe_Mn$cleanIDs[i])==30){
    siteNo[i]=100}
  else if(as.numeric(Fe_Mn$cleanIDs[i])==17 | as.numeric(Fe_Mn$cleanIDs[i])==18){
    siteNo[i]=200}
  else if (as.numeric(Fe_Mn$cleanIDs[i])>=31 & as.numeric(Fe_Mn$cleanIDs[i])<=36){
    siteNo[i]=40}
  else if (as.numeric(Fe_Mn$cleanIDs[i])==59 | as.numeric(Fe_Mn$cleanIDs[i])==60){
    siteNo[i]=301}
  else if (as.numeric(Fe_Mn$cleanIDs[i])==61 | as.numeric(Fe_Mn$cleanIDs[i])==62){
    siteNo[i]=501}
  else if (as.numeric(Fe_Mn$cleanIDs[i])==63 | as.numeric(Fe_Mn$cleanIDs[i])==64){
    siteNo[i]=400}
  else if (as.numeric(Fe_Mn$cleanIDs[i])==65 | as.numeric(Fe_Mn$cleanIDs[i])==66){
    siteNo[i]=201}
  }
#take only half of site Nos

siteNo<-c(siteNo[c(T,F)])

finalDF<-cbind(reservoir,siteNo, sampleDates,TFe,TMn,SFe,SMn)

metalsnumeric<-as.data.frame(apply(finalDF[,4:7],2,as.numeric))

##replace all negative values with zero
##set all pos. values below detectability to detectability

flagReader<-function(index, threshold){
  flag<-c()
  metalscol<-metalsnumeric[,index]
  for (i in seq(length(metalscol))){
  if (metalscol[i]<threshold & metalscol[i]>0){
    flag<-c(flag,3)
    }
    else if (metalscol[i]==-.999 | metalscol[i]==-999){flag<-c(flag,1)}
    else if (metalscol[i]<0){flag<-c(flag,4)
    }
    else {flag<-c(flag,0)}
  }
  return (flag)}
  
replace.metals<-function(index,threshold){
  metals.column.new<-c()
  metals.column<-metalsnumeric[,index]
  for (i in seq(1,length(metals.column))){
if (metals.column[i]<threshold & metals.column[i]>0){
metals.column.new<-c(metals.column.new,threshold)
}else if (metals.column[i]<0){
metals.column.new<-c(metals.column.new,0)
}else(metals.column.new<-c(metals.column.new,metals.column[i]))}
  return(metals.column.new)}


## threshold is limit of detectability 
#Fe threshold=0.005 ppm
#Mn=0.0001 ppm

flagTFe<-flagReader(1, 0.005)
flagSFe<-flagReader(3, 0.005)
flagTMn<-flagReader(2,0.0001)
flagSMn<-flagReader(4,0.0001)

metalsnumeric[,1]<-replace.metals(1,0.005)
metalsnumeric[,3]<-replace.metals(3,0.005)
metalsnumeric[,2]<-replace.metals(2,0.0001)
metalsnumeric[,4]<-replace.metals(4,0.0001)



FinalDF_flags<-as.data.frame(cbind(reservoir,siteNo, sampleDates, depths, metalsnumeric, rep(0, length(flagTFe)), flagTFe, flagTMn, flagSFe, flagSMn))

#write out data frame to csv, and copy from csv into Metals 2021 Google Sheets

##change for each datasheet

fname<-"metals_7_19_processed.csv"

write.csv(FinalDF_flags,fname, row.names = TRUE)

