#Shape BV functions from Hillenbrand et al. 1999
#Author: Mary Lofton
#Date: 06FEB19

sphere = function(d){
  BV = (pi/6)*d^3
  return(BV)
}

prolate.spheroid = function(d,h){
  BV = (pi/6)*d^2*h
  return(BV)
}

dome = function(d,h){
  BV = ((pi/6)*d^2*h)/2
  return(BV)
}

ellipsoid = function(a,b,h){
  BV = (pi/6)*a*b*h
  return(BV)
}

cylinder = function(d,h){
  BV = (pi/4)*d^2*h
  return(BV)
}

two.truncated.cones = function(d1,d2,h){
  r1 = d1/2
  r2 = d2/2
  BV = 2*(pi/3)*(r1^2 + r1*r2 + r2^2)*h
  return(BV)
}

cylinder.and.two.cones = function(d,h,z){
  BV = (pi/4)*d^2*(h + (z/2))
  return(BV)
}

sickle.shaped.prism = function(a,a2,b,b2,c){
  BV = (pi/4)*c*(a*b - a2*b2)
  return(BV)
}

two.cones = function(d,z){
  BV = (pi/6)*d^2*z
  return(BV)
}

elliptic.prism = function(a,b,c){
  BV = (pi/4)*a*b*c
  return(BV)
}

box.bv = function(a,b,c){
  BV = a*b*c
  return(BV)
}

two.ellipsoids = function(a,b,h){
  BV = (pi/6)*a*b*h*2
  return(BV)
}

cone.and.half.sphere = function(d,z){
  BV = (pi/12)*d^2*(z+d)
  return(BV)
}

box.and.two.cylinders = function(a,b,c,d,h){
  BV = a*b*c + 2*((pi/4)*d^2*h)
  return(BV)
}

prism.on.parallelogram = function(a,b,c){
  BV = 0.5*a*b*c
  return(BV)
}

cylinder.and.cone = function(d,h,z){
  BV = ((pi/12)*d^2*z) + ((pi/4)*d^2*h)
  return(BV)
}

cube = function(l){
  BV = l^3
  return(BV)
}

#need to add function for cymbelloid

#function to calculate biovolume concentration
#df is data frame of Genera and total BV counted in sample
#v is volume filtered for each slide rep
#n is number of grids counted total among all three slide reps

bv.concentration = function(df, v, n){
  
  h_mm = (v*1000)/(pi*12.5^2)
  
  vol_counted_mm3 = (250*0.001)^2*h_mm*n*3 #multiplied by 3 because of 3 slide reps
  
  vol_counted_mL = vol_counted_mm3/1000
  
  df$BV_um3mL <- df$BV_um3/vol_counted_mL
  
  return(df)
  
}
