# model of forest ag dynamics with migration and remitances
# for paper Peterson, Ospina, Crepin

# SE regime shift model based on 
# Regime shifts in a socio-ecological model of farmland abandonment 
#    Joana Figueiredo • Henrique M. Pereira 
#    Landscape Ecol (2011) 26:737–749 DOI 10.1007/s10980-011-9605-3 
# remitances added

# Garry Peterson  March 16, 207

# load library
library(phaseR)


# set directories
# use files from dropox
mainDir <- "~/Box Sync/Migra-LUCC model 2/SE regime shift - R code"
outputDir <- "FM-Figures-Mar2017"

# set directory for R code
setwd(mainDir)

# load file with equations
source("Forest_Migra+Remit (Garry march17).R")

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
lambda  <- 0.9 *elipson # max harvesting less than forest growth
omega 	<- 0.1
gamma 	<- 1
s 		<-0.05
# (1-F)h/(1-M) - ag util per person ; gamma = urban
# h/(1-M) v.s h is value of Ag/person w/ no migrant all ag
# so h<gamma otherwise no migration
h <- 1
p<-0.10
k<-5	# if k=1/gamma*p migration makes labour 
beta<-0	# all consumption


# make 4 by 3 window
quartz(width=10,height=6)
par(mfrow= c(2,3)) 
par(mar=c(4, 4, 2, 2))  #it goes c(bottom, left, top, right) 

beta<-0.5
for(gamma in c(1,2)) {
	for(p in c(0.0,0.1,0.2)) {
		phasePlotFM(FM3, 250, .125, elipson, lambda, omega,s, gamma,h,p,k,beta) 
		title(main=paste0("gamma= ", gamma, " ; ", "remit=",p))
	}	
}
quartz.save("Consumption (gamma, p).pdf", type = "pdf")

# # make 4 by 3 window
# quartz(width=10,height=8)
# par(mfrow= c(3,4)) 
# par(mar=c(4, 4, 2, 2))  #it goes c(bottom, left, top, right) 
# 
# beta<-0
# for(s in c(0.01,0.025,0.05)) {
# 	for(p in c(0.0,0.05,0.1,0.2)) {
# 		phasePlotFM(FM3, 250, .125, elipson, lambda, omega,s, gamma,h,p,k,beta) 
# 		title(main=paste0("remit=",p," s = ", s))
# 	}	
# }
# quartz.save("Ag Labour - phase remit Vs. s.pdf", type = "pdf")
# 
# # 
# # make 4 by 3 window
# quartz(width=10,height=8)
# par(mfrow= c(3,4)) 
# par(mar=c(4, 4, 2, 2))  #it goes c(bottom, left, top, right) 
# 
# beta<-0
# s<-0.025
# for(k in c(1,5,10)) {
# 	for(p in c(0.05,0.1,0.2)) {
# 		phasePlotFM(FM3, 250, .125, elipson, lambda, omega,s, gamma,h,p,k,beta) 
# 		title(main=paste0("labourCost=",k," remit=",p," s = ", s))
# 	}	
# }
# quartz.save("Ag Labour - phase labour vs. remit.pdf", type = "pdf")
# 
# 
# # make 4 by 3 window
# quartz(width=10,height=8)
# par(mfrow= c(3,3)) 
# par(mar=c(4, 4, 2, 2))  #it goes c(bottom, left, top, right) 
# 
# s <-0.05
# p <-0.10
# 
# for(beta in seq(0,1,1/8)) {
# 		phasePlotFM(FM3, 250, .125, elipson, lambda, omega,s, gamma,h,p,k,beta) 
# 		title(main=paste0("Beta=",beta," p=",p," s = ", s))
# 	}	
# }
# quartz.save("Beta.pdf", type = "pdf")
# 
# 
# 
