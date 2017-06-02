# model of forest ag dynamics with migration and remitances
# for paper Peterson, Ospina, Crepin

# SE regime shift model based on 
# Regime shifts in a socio-ecological model of farmland abandonment 
#    Joana Figueiredo • Henrique M. Pereira 
#    Landscape Ecol (2011) 26:737–749 DOI 10.1007/s10980-011-9605-3 
# remitances added

# v1 	added new FM4 function with seed availablity -Garry Peterson  May 2, 2017
# 		seed FM for FM4

# Garry Peterson  March 16, 2017

# load library
library(phaseR)


# set directories
# use files from dropox
mainDir <- "~/Box Sync/PhD/proyectos/Migra-LUCC model/R/Migra-LUCC/"
outputDir <- "figures"

# set directory for R code
setwd(mainDir)

# load file with equations
source("FM (G_17.04)_DO.R")

# return to directory to save images
# create directory if it doesn't exist
dir.create(file.path(mainDir, outputDir), showWarnings = FALSE)
setwd(file.path(mainDir, outputDir))

###########
# main loop 
###########


# set parameters
  # parameters
  # 1. forestGrowth				elipson			F/time
  # 2. forestClearingRate		lambda			F/ (time * people)
  # 3. migrationDecisionRate	omega			decisions/time
  # 4. decisionSd				s				deviation amoung decision threshold in population					
  # 5. urbanUtil				gamma			utils/person
  # 6. agUtil					h				utils
  # 7. prRemit					p				proportion
  # 8. LabCost					k				people/util
  # 9. CvsL					 	beta			consumption vs. labour (.5 = 50:50)

elipson <- 0.05
# elipson/lambda  - how effective harvesting vs. forest growth
lambda  <- 0.75 *elipson # max harvesting less than forest growth
omega 	<- 0.1
gamma 	<- 1
s 		<-0.05
# (1-F)h/(1-M) - ag util per person ; gamma = urban
# h/(1-M) v.s h is value of Ag/person w/ no migrant all ag
# so h<gamma otherwise no migration
h <- 0.5
p<-0.10
k<-5	# if k=1/gamma*p migration makes labour 
beta<-1	# all consumption



# make 4 by 3 window
quartz(width=10,height=8)
par(mfrow= c(3,4)) 
par(mar=c(4, 4, 2, 2))  #it goes c(bottom, left, top, right) 

beta<-1
for(s in c(0.01,0.025,0.05)) {
	for(p in c(0.0,0.05,0.1,0.2)) {

		phasePlotFM(FM3, 250, .125, elipson, lambda, omega,s, gamma,h,p,k,beta) 
		title(main=paste0("remit=",p," s = ", s))
	}	
}
quartz.save("Consumption - phase remit Vs. s.pdf", type = "pdf")

# make 4 by 3 window
quartz(width=10,height=8)
par(mfrow= c(3,4)) 
par(mar=c(4, 4, 2, 2))  #it goes c(bottom, left, top, right) 

beta<-0
for(s in c(0.01,0.025,0.05)) {
	for(p in c(0.0,0.05,0.1,0.2)) {
		phasePlotFM(FM3, 250, .125, elipson, lambda, omega,s, gamma,h,p,k,beta) 
		title(main=paste0("remit=",p," s = ", s))
	}	
}
quartz.save("Ag Labour - phase remit Vs. s.pdf", type = "pdf")


# make 4 by 3 window
quartz(width=10,height=8)
par(mfrow= c(3,4)) 
par(mar=c(4, 4, 2, 2))  #it goes c(bottom, left, top, right) 

beta<-0
s<-0.025
for(p in c(0.05,0.1,0.2)) {
	for(k in c(1,5,10,20)) {
		phasePlotFM(FM3, 250, .125, elipson, lambda, omega,s, gamma,h,p,k,beta) 
		title(main=paste0("labourCost=",k," remit=",p," s = ", s))
	}	
}
quartz.save("Ag Labour - phase labour vs. remit.pdf", type = "pdf")


# make 4 by 3 window
quartz(width=10,height=8)
par(mfrow= c(3,3)) 
par(mar=c(4, 4, 2, 2))  #it goes c(bottom, left, top, right) 

s <-0.05
p <-0.10
k<-5
for(beta in seq(0,1,1/8)) {
		phasePlotFM(FM3, 250, .125, elipson, lambda, omega,s, gamma,h,p,k,beta) 
		title(main=paste0("Beta=",beta," p=",p," s = ", s))
}
quartz.save("Beta.pdf", type = "pdf")

# make 5 by 3 window
quartz(width=12,height=8)
par(mfrow= c(3,5)) 
par(mar=c(4, 4, 2, 2))  #it goes c(bottom, left, top, right) 

omega<- 0.05
s <-0.05
p <-0.10
k <-5

for(p in c(0.0,0.1,0.2)) {
	for(beta in seq(0,1,1/4)) {
		phasePlotFM(FM3, 250, .125, elipson, lambda, omega,s, gamma,h,p,k,beta) 
		title(main=paste0("Beta=",beta," p=",p," s = ", s))
	}	
}
quartz.save("Beta vs Remit omega-0.05.pdf", type = "pdf")

# make 5 by 3 window
quartz(width=12,height=8)
par(mfrow= c(3,5)) 
par(mar=c(4, 4, 2, 2))  #it goes c(bottom, left, top, right) 

omega<- 0.05
s <-0.05
p <-0.125
k <-5

for(s in c(0.01,0.05,0.1)) {
	for(beta in seq(0,1,1/4)) {
		phasePlotFM(FM3, 250, .125, elipson, lambda, omega,s, gamma,h,p,k,beta) 
		title(main=paste0("Beta=",beta," p=",p," s = ", s))
	}	
}
quartz.save("Beta vs s omeg=0.05 p=0.125.pdf", type = "pdf")

# make 5 by 3 window
quartz(width=12,height=8)
par(mfrow= c(3,5)) 
par(mar=c(4, 4, 2, 2))  #it goes c(bottom, left, top, right) 

omega<- 0.05
s <-0.05
p <-0.125
k <-5
s<-0.05

for(beta in seq(0,1,1/2)) {
	for(seedSat in c(4,8,12,16,100)) {
		phasePlotFM(FM4, 250, .125, elipson, lambda, omega,s, gamma,h,p,k,beta, seedSat) 
		title(main=paste0("Beta=",beta,"seedSat = ", seedSat))
	}	
}
quartz.save("Beta vs SeedSat.pdf", type = "pdf")








###########
# test compare FM3 vs. FM4
##########

p <- 0.1
s <- 0.05
seedSat<-5

# make 4 by 3 window
quartz(width=10,height=8)
par(mfcol= c(3,4)) 
par(mar=c(4, 4, 2, 2))  #it goes c(bottom, left, top, right) 

for(modelType in c(0,1)) {
	if(modelType==0) {
		for(beta in c(0,1)) {
			for(p in c(0.0,0.1,0.2)) {
				phasePlotFM(FM3, 250, .125, elipson, lambda, omega,s, gamma,h,p,k,beta, seedSat) 
				title(main=paste0("FM3 remit=",p," Beta = ", beta))
			}
		}	
	}
	else {
		p<-0.1
		for(beta in c(0,1)) {
			for(seedSat in c(5,25,625)) {
				phasePlotFM(FM4, 250, .125, elipson, lambda, omega,s, gamma,h,p,k,beta, seedSat) 
				title(main=paste0("FM4 ", "seedSat= ", seedSat," remit=",p," Beta = ", beta))
			}
		}
	}	
}
quartz.save("Consumption - phase remit Vs. s.pdf", type = "pdf")


