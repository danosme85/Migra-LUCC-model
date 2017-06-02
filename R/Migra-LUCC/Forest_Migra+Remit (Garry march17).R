# function for Forest (F) Migration (M) dynamics based on migrants sending back remittances
# general model

# model of forest ag dynamics with migration and remitances
# for paper Peterson, Ospina, Crepin

# model version 3

library(phaseR)

FM3 <- function (t, y, parameters) # computes the derivative at a point for the ODE system
{  
  # parameters
  # 1. forestGrowth				elipson			F/time
  # 2. forestClearingRate		lambda			F/ (time * people)
  # 3. migrationDecisionRate	omega			decisions/time
  # 4. decisionSd				s				deviation amoung decision threshold in population					
  # 5. urbanUtil				gamma			utils/person
  # 6. agUtil					h				utils
  # 7. prRemit					p				proportion
  # 8. LabCost					k				people/util
  # 9. CvsL					 	Beta			consumption vs. labour (.5 = 50:50)
	
  	F<-y[1]
  	M<-y[2]
	dy <- numeric(2)

	forestGrowth			<-parameters[1]
	forestClearingRate		<-parameters[2]
	migrationDecisionRate	<-parameters[3]
	decisionSd				<-parameters[4]				
	urbanUtil				<-parameters[5]	
	agUtil					<-parameters[6]	
	prRemit					<-parameters[7]	
	LabCost					<-parameters[8]	
	CvsL					<-parameters[9]	
 

	# Forest/Agriculture Dynamics
	boughtlabour <- CvsL*LabCost*prRemit*urbanUtil*M
  	dy[1] <- forestGrowth*F*(1-F) - forestClearingRate*(1-M + boughtlabour)*F #Forest

	# Migration Dynamics

	# The proportion of individuals that will decide to migrate 
	# is proportion that has a threshold less than M/P,
	# This proportion is given by the cumulative distribution function of the logistic distribution
	#   minus the proportion of individuals from the population that has already migrated
	# From Regime shifts in a socio-ecological model of farmland abandonment 
	#    Joana Figueiredo • Henrique M. Pereira 
	#    Landscape Ecol (2011) 26:737–749 DOI 10.1007/s10980-011-9605-3 

 	relUtil<-agUtil/urbanUtil
 
 	# average migration threshold of population of resident
  	mu  <-  (relUtil *(1-F) + (1-CvsL)* prRemit * M)/(2*(1-prRemit)*(1-M))
  	cdf <-	  1/(1 + exp(1)^((mu-M)/decisionSd))	 # migration function
	dy[2] <- migrationDecisionRate*(cdf - M)
  
  return(list(dy))
}





# make an image for an FM function
phasePlotFM <-function(FM, tSteps, ptOffSet, forestGrowth,forestClearingRate,migrationDecisionRate,decisionSd,urbanUtil,agUtil,prRemit,LabCost,CvsL) 
{

	hiPtOffset<-	1- ptOffSet
  startPts <- matrix(c(ptOffSet, ptOffSet, ptOffSet, hiPtOffset, hiPtOffset, ptOffSet, hiPtOffset, hiPtOffset), ncol = 2, nrow = 4, byrow = TRUE)

	parametersFM <-c(forestGrowth,forestClearingRate,migrationDecisionRate,decisionSd,urbanUtil,agUtil,prRemit,LabCost,CvsL)	
	
  	FM.flowField  <- flowField(FM, x.lim = c(0.001, 0.99), y.lim = c(0.001, 0.99),
              xlab="Forest", ylab="Migrant", 
              arrow.type="proportional", #so that length reflects magnitude of derivative.
              parameters = parametersFM, 
              points = 11, add = FALSE)  
  
  	FM.nullclines <- nullclines(FM, x.lim = c(0.001, 0.99), y.lim = c(0.001, 0.99), 
                              parameters = parametersFM, 
                              system = "two.dim",
                              points = tSteps, colour = c("green", "violet"))  
 
  	FM.trajectory <- trajectory(FM, y0 = startPts, t.end = tSteps, 
                              parameters = parametersFM, 
                              colour="gray51")
}


