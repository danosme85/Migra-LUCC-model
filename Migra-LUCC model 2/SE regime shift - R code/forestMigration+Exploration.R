# phase analysis of 
# SE regime shift model based on 
# Regime shifts in a socio-ecological model of farmland abandonment 
#    Joana Figueiredo • Henrique M. Pereira 
#    Landscape Ecol (2011) 26:737–749 DOI 10.1007/s10980-011-9605-3 
# remitances added


# march 13, 2017
# new version to make a series of figures
# set up some infrastructure to explore multiple figures

# choose some parameters
# sequence of images
# time series
# phase plots



# fixed equations
# converted figures March 10,2017
# modified by GDP 	March 9, 2017

# more on package phaseR
# https://journal.r-project.org/archive/2014-2/grayling.pdf
# short tutorial
# http://www.magesblog.com/2014/11/phase-plane-analysis-in-r.html
# lab
# https://www.macalester.edu/~kaplan/math135/pplane.pdf

# install.packages("phaseR")
# https://cran.r-project.org/web/packages/phaseR/phaseR.pdf


# some notes on justification of rough values
# Global average remittance costs 10% -> 7% of money
# percent of income remitted ~ 10 %
# “World Bank Group. 2016. Migration and Remittances Factbook 2016, 3rd Edition. Washington, DC: World Bank
# https:// openknowledge.worldbank.org/handle/10986/23743 License: CC BY 3.0 IGO.”
# remitances 30-50% household income in lat america
# Remittances and Development: Lessons from Latin America
# edited by Pablo Fajnzylber, J. Humberto Lopez

# load library
library(phaseR)


# set directories
# use files from dropox
mainDir <- "~/Box Sync/Migra-LUCC model 2/SE regime shift - R code"
outputDir <- "FM-Figures-Mar2017"

# set directory for R code
setwd(mainDir)

# return to directory to save images
# create directory if it doesn't exist
dir.create(file.path(mainDir, outputDir), showWarnings = FALSE)
setwd(file.path(mainDir, outputDir))




# remove previously assigned values
rm(elip,lambda,omega,gamma,s,w,h,p,z)

#parameter values for old model
elip 	<-0.05
ratioFA <-0.9   # how effective harvesting vs. forest growth
lambda  <-ratioFA* elip
omega 	<- 0.1
gamma 	<- 1
s 		<-0.05

# new parameters
h <- 0.5				# h utility of ag
w <- h/gamma 		# h <- w*gamma, w = h/gamma
p <- 0.0001			# p is proportion of urban utility that is remitance) 
z <- p/gamma 		# z <- p/gamma 
					# conversion of utility to labour 1 is one urban resident's urban util equal's 1 rural labour
k <- l				# or if k = 1/z - remitances make up for labour

###########
# functions
###########




# function for Forest (F) Migration (M) dynamics based on migrants sending back remittances
# that are used to increase residents utility thus decreasing desire to migration
# z is payment
FMutilR <- function (t, y, parameters) # computes the derivative at a point for the ODE system
{  
  # parameters
  # order: elip, lambda, omega, w, z, s
  # eqn: dF/dt = elip*(1-F)*F - lambda * (1-M) * F  	
	
  F<-y[1] ;   M<-y[2]
  elip <-parameters[1] ; lambda  <-parameters[2] # forest
  omega<- parameters[3]
  w <-parameters[4] ; z <-parameters[5]; k <- parameters[6]; s <- parameters[7]

  dy <- numeric(2)
  dy[1] <-(elip*F*(1-F))-(lambda*(1-M)*F) #Forest

# notes on equation
# The proportion of individuals that will decide to migrate 
# is proportion that has a threshold less than M/P,
# This proportion is given by the cumulative distribution function of the logistic distribution
#   minus the proportion of individuals from the population that has already migrated
#  (see a similar algorithm in Iwasa et al. 2010). 

  mu  <-  (w *(1-F)+ z*M)/(2*(1-M))	 # average migration threshold of population of resident
  cdf <-	  1/(1 + exp(1)^((mu-M)/s))	 # migration function

  dy[2] <- omega*(cdf - M)
  
  return(list(dy))
}






# function for Forest (F) Migration (M) dynamics based on migrants sending back remittances
# that are used to increase/maintain ag
# z is payment
# k is conversion of util to Resident eqivalent labout
FMagR <- function (t, y, parameters) # computes the derivative at a point for the ODE system
{  
  # parameters
  # order: elip, lambda, omega, w, z, s
  # eqn: dF/dt = elip*(1-F)*F - lambda * (1-M) * F  	
	
  F<-y[1] ;   M<-y[2]
  elip <-parameters[1] ; lambda  <-parameters[2] # forest
  omega<- parameters[3]
  w <-parameters[4] ; z <-parameters[5]; k <- parameters[6]; s <- parameters[7]

  dy <- numeric(2)
  dy[1] <-(elip*F*(1-F))-(lambda*(1-M + k*z*M)*F) #Forest

# notes on equation
# The proportion of individuals that will decide to migrate 
# is proportion that has a threshold less than M/P,
# This proportion is given by the cumulative distribution function of the logistic distribution
#   minus the proportion of individuals from the population that has already migrated
#  (see a similar algorithm in Iwasa et al. 2010). 

  mu  <-  (w *(1-F)+ z*M)/(2*(1-M))	 # average migration threshold of population of resident
  cdf <-	  1/(1 + exp(1)^((mu-M)/s))	 # migration function

  dy[2] <- omega*(cdf - M)
  
  return(list(dy))
}




# do things for a FM function
phasePlot <- function(FM, elip,lambda,omega,w,z,k,s,tSteps) 
{
  FM.flowField  <- flowField(FM, x.lim = c(0.0, 1.0), y.lim = c(0.0, 1.0),
              xlab="Forest", ylab="Migrant", 
              arrow.type="proportional", #so that length reflects magnitude of derivative.
              parameters = c(elip,lambda,omega,w,z,k,s), 
              points = 11, add = FALSE)  
  
  FM.nullclines <- nullclines(FM, x.lim = c(0.00, 1.0), y.lim = c(0.00, 1.0), 
                              parameters = c(elip,lambda,omega,w,z,k,s), 
                              system = "two.dim",
                              points = tSteps, colour = c("green", "violet"))  

  pts <- matrix(c(.15, .15, .15, .85, .85, .15, .85, .85), ncol = 2, nrow = 4, byrow = TRUE)
  
  FM.trajectory <- trajectory(FM, y0 = pts, t.end = tSteps, 
                              parameters = c(elip,lambda,omega,w,z,k,s), 
                              colour="gray51")
}

###########
# main loop 
###########

# remitances to consumption
# make 3 by 3 window
quartz(width=10,height=8)
par(mfrow= c(3,3)) 
par(mar=c(4, 4, 2, 2))  #it goes c(bottom, left, top, right) 

k<-1 # not used
for(p in c(0.001,0.1,0.2)) {
	z <- p/gamma	
	for(s in c(0.01,0.025,0.05)) {
		phasePlot(FMutilR, elip,lambda,omega,w,z,k,s, 250)
		title(main=paste0("remit=",p," s = ", s))
	}	
}
quartz.save("Consumption - phase remit Vs. s.pdf", type = "pdf")


# remitances to ag labour
# make 3 by 3 window
quartz(width=10,height=8)
par(mfrow= c(3,3)) 
par(mar=c(4, 4, 2, 2))  #it goes c(bottom, left, top, right) 

p<-0.1 # defined
z <- p/gamma	
for(k in c(0.5,5,10)) {
	for(s in c(0.01,0.025,0.05)) {
		phasePlot(FMagR, elip,lambda,omega,w,z,k,s, 250)
		title(main=paste0("remit=",p,"labour=",k," s = ", s))
	}	
}
quartz.save("Labour - phase labour conversion Vs. s.pdf", type = "pdf")



