
# BACKGROUND
##### 
# model based on 
# Regime shifts in a socio-ecological model of farmland abandonment 
#    Joana Figueiredo • Henrique M. Pereira 
#    Landscape Ecol (2011) 26:737–749 DOI 10.1007/s10980-011-9605-3 

# general model
# function for Forest (F) Migration (M) dynamics based on migrants sending back remittances


# model extension: forest-agriculture dynamics with migration and remittances
# for paper Peterson, Ospina, Crépin

# model version 4		GDP May 3, 2017
# model version 3		GDP
# model version 1   DOM March, 2017 (Original model into R) 
#####

library(phaseR)

# important to remove previously assigned values
rm(steps, pts, eps,lam,ome,s,gam,h,p,k,bet,phi)

# Incorporating mechanisms linking remittances and land use/cover change

FM2 <- function (t, y, parameters) # computes the derivative at a point for the ODE system
{  
  # parameters
  # 1. forest growth rate				eps			F/time
  # 2. forest clearing rate		  lam			F/ (time * people)
  # 3. migration decision rate	ome		  decisions/time
  # 4. decision 'variance'			s				deviation of decision thresholds in population					
  # 5. urban utility				    gam 		utils/person
  # 6. agr. utilily		          h				utils
  
  
  Fo<-y[1] # Forest
  M<-y[2]  # Migration
  dy <- numeric(2)
  
  eps	<-parameters[1] ; lam <-parameters[2] # forest
  ome	<-parameters[3] ;   s <-parameters[4] # migration1
  gam <-parameters[5]	;   h <-parameters[6]	# migration2
  
  
  # Forest/Agriculture Dynamics
  dy[1] <- (eps*Fo*(1-Fo)) - (lam*(1-M)*Fo) #Forest change
  
  # Migration Dynamics
  # The proportion of individuals that will decide to migrate is that has a threshold less than M/P, which is given by the cumulative distribution function of the logistic distribution (CDF) minus the proportion of individuals from the population that has already migrated (from roriginal model)
  

  # average migration threshold of population of resident
  mui  <-  ((1-Fo)*h)/((1-M)*2*gam)
  cdf <-	  1/(1+exp((mui-M)/s))	 
  dy[2] <- ome*(cdf - M) # Migrants change
  
  return(list(dy))
}
phasePlotFM2 <-function(FM2,steps, pts, eps,lam,ome,s,gam,h) 
{
  
  ptsB<-	1- pts
  IC <- matrix(c(pts, pts, pts, ptsB, ptsB, pts, ptsB, ptsB), ncol = 2, nrow = 4, byrow = TRUE)
  
  parametersFM2 <-c(eps,lam,ome,s,gam,h)	
  
  FM.flowField  <- flowField(FM2, x.lim = c(0, 1), y.lim = c(0, 1.0001),
                             xlab="Forest", ylab="Migrants", mgp=c(1.9,0.8,0),
                             arrow.type="proportional", #so that length reflects magnitude of derivative.
                             parameters = parametersFM2, 
                             points = 11, add = FALSE)  
  
  FM.nullclines <- nullclines(FM2, x.lim = c(0, 1), y.lim = c(0, 1.0001), 
                              parameters = parametersFM2, 
                              points = steps)  
  
  # FM.trajectory <- trajectory(FM2, y0 = IC, t.end = steps,
  #                             parameters = parametersFM2,
  #                             colour="gray51")
}

FM3 <- function (t, y, parameters) # computes the derivative at a point for the ODE system
{  
  # parameters
  # 1. forest growth rate				eps			F/time
  # 2. forest clearing rate		  lam			F/ (time * people)
  # 3. migration decision rate	ome		  decisions/time
  # 4. decision 'variance'			s				deviation of decision thresholds in population					
  # 5. urban utility				    gam 		utils/person
  # 6. agr. utilily		          h				NOTE:---->>>>utils/area
  # 7. prop. remited				    p				proportion
  # 8. labor cost					      k				people/util
  # 9. remittance use	 	        bet			consumption vs. labour (1 = all consumption)
  
  Fo<-y[1] # Forest
  M<-y[2]  # Migration
  dy <- numeric(2)
  
  eps	<-parameters[1] ; lam <-parameters[2] # forest
  ome	<-parameters[3] ;   s <-parameters[4] # migration1
  gam <-parameters[5]	;   h <-parameters[6]	# migration2
    p <-parameters[7]	;   k <-parameters[8]	; bet <-parameters[9]	# remittances (new)
  
  
  # Forest/Agriculture Dynamics
  hired_labor <- (1-bet)*k*p*gam*M
  dy[1] <- eps*Fo*(1-Fo) - lam*(1-M + hired_labor)*Fo #Forest change
  
  # Migration Dynamics
  # The proportion of individuals that will decide to migrate is that has a threshold less than M/P, which is given by the cumulative distribution function of the logistic distribution (CDF) minus the proportion of individuals from the population that has already migrated (from roriginal model)
  
      # Garry's alternative:
        # relative_util<-h/gam ; mui<-((relative_util*(1-Fo)) + ((1-bet)*p*M))/(2*(1-p)*(1-M))
  # average migration threshold of population of resident
  mui  <-  ((h*(1-Fo))+(bet*p*gam*M)) / (2*(1-p)*gam*(1-M))
  cdf <-	  1/(1 + exp((mui-M)/s))	 
  dy[2] <- ome*(cdf - M) # Migrants change
  
  return(list(dy))
}
phasePlotFM3 <-function(FM3, steps, pts, eps,lam,ome,s,gam,h,p,k,bet) 
{
  
  ptsB<-	1- pts
  IC <- matrix(c(pts, pts, pts, ptsB, ptsB, pts, ptsB, ptsB), ncol = 2, nrow = 4, byrow = TRUE)
  
  parametersFM3 <-c(eps,lam,ome,s,gam,h,p,k,bet)	
  
  FM.flowField  <- flowField(FM3, x.lim = c(0, 1), y.lim = c(0, 1.0001),
                             xlab="Forest", ylab="Migrants", mgp=c(1.9,0.8,0),
                             arrow.type="proportional", #so that length reflects magnitude of derivative.
                             parameters = parametersFM3, 
                             points = 11, add = FALSE)  
  
  FM.nullclines <- nullclines(FM3, x.lim = c(0, 1), y.lim = c(0, 1.0001), 
                              parameters = parametersFM3, 
                              points = steps)  
  
  # FM.trajectory <- trajectory(FM3, y0 = IC, t.end = steps, 
  #                             parameters = parametersFM3, 
  #                             colour="gray51")
}

FM4 <- function (t, y, parameters) # computes the derivative at a point for the ODE system
{  
  # parameters
  # 1. forest growth rate				eps			F/time
  # 2. forest clearing rate		  lam			F/ (time * people)
  # 3. migration decision rate	ome		  decisions/time
  # 4. decision 'variance'			s				deviation of decision thresholds in population					
  # 5. urban utility				    gam 		utils/person
  # 6. agr. utilily		          h				NOTE:---->>>>utils/area
  # 7. prop. remited				    p				proportion
  # 8. labor cost					      k				people/util
  # 9. remittance use	 	        bet			consumption vs. labour (1 = all consumption)
  #10. seed saturation					phi			dimensionless 		
  
  Fo<-y[1] # Forest
  M<-y[2]  # Migration
  dy <- numeric(2)
  
  eps	<-parameters[1] ; lam <-parameters[2] # forest
  ome	<-parameters[3] ;   s <-parameters[4] # migration1
  gam <-parameters[5]	;   h <-parameters[6]	# migration2
    p <-parameters[7]	;   k <-parameters[8]	; bet <-parameters[9]	# remittances (new)
  phi	<-parameters[10]	#  depensation (forest resilience)
  
  # Forest/Agriculture Dynamics

   hired_labor <- (1-bet)*k*p*gam*M
  forest_growth <- eps*(1-exp(-phi*Fo)) # depensation, concave monotonic
  #forest_growth <- eps*((-phi*(1-Fo))+1)# depensation, linear 
  dy[1] <- forest_growth*Fo*(1-Fo) - lam*(1-M + hired_labor)*Fo #Forest change
  
  # Migration Dynamics
  # The proportion of individuals that will decide to migrate is that has a threshold less than M/P, which is given by the cumulative distribution function of the logistic distribution (CDF) minus the proportion of individuals from the population that has already migrated (from roriginal model)
  
  # Garry's alternative:
  # relative_util<-h/gam ; mui<-((relative_util*(1-Fo)) + ((1-bet)*p*M))/(2*(1-p)*(1-M))
  # average migration threshold of population of resident
  mui  <-  ((h*(1-Fo))+(bet*p*gam*M)) / (2*(1-p)*gam*(1-M))
  cdf <-	  1/(1 + exp((mui-M)/s))	 
  dy[2] <- ome*(cdf - M) # Migrants change
  
  return(list(dy))
}
phasePlotFM4 <-function(FM4, steps, pts, eps,lam,ome,s,gam,h,p,k,bet,phi) 
{
  
  ptsB<-	1- pts
  IC <- matrix(c(pts, pts, pts, ptsB, ptsB, pts, ptsB, ptsB), ncol = 2, nrow = 4, byrow = TRUE)
  
  parametersFM4 <-c(eps,lam,ome,s,gam,h,p,k,bet,phi)	
  
  FM.flowField  <- flowField(FM4, x.lim = c(0, 1), y.lim = c(0, 1.0001),
                             xlab="Forest", ylab="Migrants", mgp=c(1.9,0.8,0),
                             arrow.type="proportional", #so that length reflects magnitude of derivative.
                             parameters = parametersFM4, 
                             points = 11, add = FALSE)  
  
  FM.nullclines <- nullclines(FM4, x.lim = c(0, 1), y.lim = c(0, 1.0001), 
                              parameters = parametersFM4, 
                              points = steps)  
  
  # FM.trajectory <- trajectory(FM4, y0 = IC, t.end = steps, 
  #                             parameters = parametersFM4, 
  #                             colour="gray51")
}


#FIGURES
#####

name<-as.character("FM4 h/γ") # ε γ β λ ω φ
quartz(name, width=7, height=8) ; par(mfcol=c(3,3), mar=c(4,4,2,1)+0.2) #width=7, height=2.5 for 1,3 | width=7, height=8 for 3,3 | width=10, height=8) for 3,4

# default parameter values for original model (simplified)
eps<-0.05 ; lam<-0.02 ; ome<-1 ; h<-1 ; gam<-1 ; s<-0.05  # default parameters
    h=0.5 #agricultural utility is half of urban utility
    gam=2
  lam=0.05 #deforestation rate equals natural regrowth
    s=0.2 #low social bonding 

steps <- 250 ; pts <- 0.3 #

##FM2
for(gam in c(0.5,1,2)) {
  #for(eps in c(0.025,0.05,0.1)) {
  phasePlotFM2(FM2, steps, pts, eps, lam, ome, s, gam, h)
  title(main=paste0("FM2"," h/γ=", h/gam)) # " h/γ=", h/gam 
#}
} 

# Default parameters for FM3 and FM4
#####
#eps <- 0.05 ; lam <- 0.75*eps # max deforeatation rate less than forest growth
#ome <- 0.1 ; gam <- 1 ; s <- 0.05 ; h <- 0.5 ; 
p <- 0.1
k <- 5	# if k=1/gamma*p migration makes labour 
bet<-0.5


##FM3
for(bet in c(0,0.5,1)) {
  for(k in c(1,10,20)){
  phasePlotFM3(FM3, steps, pts, eps, lam, ome, s, gam, h, p, k, bet)
  title(main=paste0("FM3", " β=",bet, " k=",k)) #" β=",bet, " p=",p, " h/γ=", h/gam, " h=", h, " γ=", gam,
  }
} 

##FM4
#####
  phi <- 8  # CM: 4: max ε .98 ; 8: 99% ε reached at .55 forest ; 16: 99% ε reached at .3 forest
            # L:  0: constant (=ε) ; 1=linear increase [0-1]
  
  
for(phi in c(4,8,16)) {
  for(gam in c(1,2,4)){
  phasePlotFM4(FM4, steps, pts, eps, lam, ome, s, gam, h, p, k, bet, phi)
  title(main=paste0("FM4(CM)", " h/γ=",h/gam, " φ=",phi)) # " β=",bet," p=",p, " h/γ=",h/gam,
  }
} # test for FM4 with different portion remitted
#####


mtext(text=paste0("s=", s," ω=",ome, " λ/ε=",lam/eps, " h=",h, " β=",bet," p=",p, " k=",k), cex=0.6, outer=F, line=3, side=1, adj=1.1, col="gray30") # " β=",bet, " h/γ=",h/gam, " p=",p, " k=",k , " 1/pγ=",1/(p*gam)

quartz.save(file=paste0(name,".pdf"), type = "pdf", dpi = 500)


