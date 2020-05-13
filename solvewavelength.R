#this project is designed to asist with analysis of data when 
#performing single slit diffraction experiments.
h<-(6.62606957*10^-34) #Planks constant
O<-2.081641e-33        #this constant represents the difraction angle when the slit width = the wavelength

#O must be less than pi/2
solveslitwidth<-function(Y,O){(Y*h)/sin(O)}
#this function is designed to return a slit width
#when presented with the difraction angle and the wavelength.

solvediffractionangle<-function(Y,A){sin(pi*(Y*h)/A)}
#This function is designed to solve for the difraction angle when 
#presented with the wavelength and slit width.

solvewavelength<-function(A,O){
  slitwidth<- sample(A, size=1)
  difractionangle<- sample(O, size=1)
  (slitwidth*sin(difractionangle))/h}
#This function will return the wavelength when 
#presented with the diffraction angle and slit width

project<-function(y=NULL,a=NULL,o=NULL){
  y
  a 
  o 
 if(y>0) (Y=y) else (Y<-solvewavelength(A,O))
 if(a>0) (A<-solveslitwidth(Y,O)) else (A=a)
 if(o>0) (O<-solvediffractionangle(Y,A)) else (O=o)
  solveslitwidth(Y=y,O=o)
  solvediffractionangle(Y=y,A=a)
  solvewavelength(A=a,O=o)}
#the intention of this function was to cobine the previous function in order to solve for the
#missing value when you imput two of the three unfortunatly is does not work


