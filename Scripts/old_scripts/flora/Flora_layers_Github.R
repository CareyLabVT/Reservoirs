###Layers file for Flora
#Author: Mary Lofton
#Date: 14JUL17

#Activate plyr
library(plyr)

#Import your data - ONLY WORKS FOR NUMERIC DATES!!
#File format should be csv with following structure:
#Column 1: numeric date
#Column 2: depth (m)
#Column 3: total concentration of phytos according to Flora (ugL)
#Column 4: green algae
#Column 5: cyanobacteria
#Column 6: diatoms
#Column 7: cryptophytes
Flora <- read.csv(file=file.choose())
Flora <- data.frame(Flora)

#Write a function that returns the closest value
#xv is vector, sv is specific value
closest<-function(xv, sv){
  xv[which.min(abs(xv-sv))]}

#Initialize an empty vector with the correct number of rows and columns 
temp<-matrix(data=NA, ncol=7, nrow=15)
super_final<-matrix(data=NA, ncol=1, nrow=0)

#Assign name to datetime column
dates<-unique(Flora$Date)

#For-loop to retrieve row with closest function and fill in matrix
#NOTE: the current depths are for GWR, but of course you would need to change these if
#you were working with a different reservoir!!
for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(Flora, Flora$Date == j)
  
  a <- q[q[, "depth"] == closest(q$depth,0.1),]
  means_a<-colMeans(a)
  temp[1,] <- means_a
  
  b <- q[q[, "depth"] == closest(q$depth,1),]
  means_b<-colMeans(b)
  temp[2,] <- means_b
  
  c <- q[q[, "depth"] == closest(q$depth,2),]
  means_c<-colMeans(c)
  temp[3,] <- means_c
  
  d <- q[q[, "depth"] == closest(q$depth,3),]
  means_d<-colMeans(d)
  temp[4,] <- means_d
  
  e <- q[q[, "depth"] == closest(q$depth,4),]
  means_e<-colMeans(e)
  temp[5,] <- means_e
  
  f <- q[q[, "depth"] == closest(q$depth,5),]
  means_f<-colMeans(f)
  temp[6,] <- means_f
  
  g <- q[q[, "depth"] == closest(q$depth,6),]
  means_g<-colMeans(g)
  temp[7,] <- means_g
  
  h <- q[q[, "depth"] == closest(q$depth,7),]
  means_h<-colMeans(h)
  temp[8,] <- means_h
  
  m <- q[q[, "depth"] == closest(q$depth,8),]
  means_m<-colMeans(m)
  temp[9,] <- means_m
  
  n <- q[q[, "depth"] == closest(q$depth,9),]
  means_n<-colMeans(n)
  temp[10,] <- means_n
  
  o <- q[q[, "depth"] == closest(q$depth,10),]
  means_o<-colMeans(o)
  temp[11,] <- means_o
  
  t <- q[q[, "depth"] == closest(q$depth,11),]
  means_t<-colMeans(t)
  temp[12,] <- means_t
  
  u <- q[q[, "depth"] == closest(q$depth,12),]
  means_u<-colMeans(u)
  temp[13,] <- means_u
  
  v <- q[q[, "depth"] == closest(q$depth,13),]
  means_v<-colMeans(v)
  temp[14,] <- means_v
  
  w <- q[q[, "depth"] == closest(q$depth,14),]
  means_w<-colMeans(w)
  temp[15,] <- means_w
  
  final <- temp
  colnames(final) <- c("datetime","depth","total","green","cyano","diatom","crypto")
  final <- data.frame(final)
  super_final <- rbind.fill.matrix(super_final,final)
}

super_final<-super_final[,-1]


#Export your data!
write.csv(super_final, "GWR_Floralayers.csv")
  
  

