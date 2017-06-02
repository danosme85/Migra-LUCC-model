

FM3b <- function (t, y, parameters) # computes the derivative at a point for the ODE system
{  
  # parameters
  # 1. forest growth rate				eps			F/time
  # 2. forest clearing rate		  lam			F/ (time * people)
  # 3. migration decision rate	ome		  decisions/time
  # 4. decision 'variance'			s				deviation of decision thresholds in population					
  # 5. urban utility				    gam 		utils/person
  # 6. agr. utilily		          h				utils/area
  # 7. prop. remited				    p				proportion
  # 8. labor cost					      k				people/util
  # 9. investment               q       NEW---->>>1/area???
  #10. remittance use	1 	      bet1		consumption (1 = all consumption)
  #11. remittance use	2 	      bet2		NEW---->>>investment (1 = all productivity)
  
  Fo<-y[1] # Forest
  M<-y[2]  # Migration
  dy <- numeric(2)
  
  eps	<-parameters[1] ; lam <-parameters[2] # forest
  ome	<-parameters[3] ;   s <-parameters[4] # migration1
  gam <-parameters[5]	;   h <-parameters[6]	# migration2
    p <-parameters[7]	;   k <-parameters[8]	; q  <-parameters[9]
 bet1 <-parameters[10] ; bet2 <-parameters[11]	; # remittances (new)
  
  
  # Forest/Agriculture Dynamics
  remits <- p*gam*M
  hired_labor <- (1-bet1-bet2)*remits*k
  dy[1] <- eps*Fo*(1-Fo) - lam*(1-M + hired_labor)*Fo #Forest change
  
  # Migration Dynamics
  # The proportion of individuals that will decide to migrate is that has a threshold less than M/P, which is given by the cumulative distribution function of the logistic distribution (CDF) minus the proportion of individuals from the population that has already migrated (from roriginal model)
  
  # Garry's alternative:
  # relative_util<-h/gam ; mui<-((relative_util*(1-Fo)) + ((1-bet)*p*M))/(2*(1-p)*(1-M))
  # average migration threshold of population of resident
  mui  <-  ((((bet2*remits*q)+h)*(1-Fo))+(bet1*remits)) / (2*(1-p)*gam*(1-M))
  cdf <-	  1/(1 + exp((mui-M)/s))	 
  dy[2] <- ome*(cdf - M) # Migrants change
  
  return(list(dy))
}
phasePlotFM3b <-function(FM3b, steps, pts, eps,lam,ome,s,gam,h,p,k,q, bet1,bet2) 
{
  
  ptsB<-	1- pts
  IC <- matrix(c(pts, pts, pts, ptsB, ptsB, pts, ptsB, ptsB), ncol = 2, nrow = 4, byrow = TRUE)
  
  parametersFM3b <-c(eps,lam,ome,s,gam,h,p,k,q, bet1,bet2)	
  
  FM.flowField  <- flowField(FM3b, x.lim = c(0, 1), y.lim = c(0, 1.0001),
                             xlab="Forest", ylab="Migrants", mgp=c(1.9,0.8,0),
                             arrow.type="proportional", #so that length reflects magnitude of derivative.
                             parameters = parametersFM3b, 
                             points = 11, add = FALSE)  
  
  FM.nullclines <- nullclines(FM3b, x.lim = c(0, 1), y.lim = c(0, 1.0001), 
                              parameters = parametersFM3b, 
                              points = steps)  
  
  # FM.trajectory <- trajectory(FM3b, y0 = IC, t.end = steps, 
  #                             parameters = parametersFM3b, 
  #                             colour="gray51")
}


bet1 <- 0.5	# 1 means all hiring labor
bet2 <- 0	# 1 means all investment