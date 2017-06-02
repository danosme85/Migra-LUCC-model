## Phase plane analysis using phaseR package
## Source: http://www.magesblog.com/2014/11/phase-plane-analysis-in-r.html

# model based on 
# Regime shifts in a socio-ecological model of farmland abandonment 
#    Joana Figueiredo • Henrique M. Pereira 
#    Landscape Ecol (2011) 26:737–749 DOI 10.1007/s10980-011-9605-3 
# remitances added

# modified by GDP March 9, 2017

# install.packages("phaseR")
# https://cran.r-project.org/web/packages/phaseR/phaseR.pdf


library(phaseR)
# use files from dropox
mainDir <- "/Users/Daniel_Ospina/Box Sync/PhD/proyectos/Migra-LUCC model/R/Migra-LUCC"
outputDir <- "Figures"
setwd(mainDir)
setwd(file.path(mainDir, outputDir))



# remove previously assigned values
rm(eps,ratioFA,lam,ome,gam,s,  w,h,p,z)


#parameter values
eps 	<-0.05
ratioFA <- 1
lam  <-ratioFA* eps
ome 	<- 0.1
gam 	<- 1
s 		<-0.05

# new parameters
h <- 0.5			  # h utility of ag
w <- h/gam 		  # h <- w*gamma, w = h/gamma
p <- 0.0001			# p is proportion of urban utility that is remited) 
z <- p/gam 		  # z <- p/gamma 


FM <- function (t, y, parameters) # computes the derivative at a point for the ODE system
{  
  # parameters
  # order: eps, lambda, omega, w, z, s
  # eqn: dF/dt = elip*(1-F)*F - lambda * (1-M) * F  	
	
  Fo<-y[1] ;   M<-y[2]
  eps <-parameters[1] ; lam <-parameters[2] # forest
  ome <-parameters[3] ;   s <-parameters[4]  # migration
    w <-parameters[5] ;   z <-parameters[6]  # remittances

  dy <- numeric(2)
  dy[1] <-(eps*Fo*(1-Fo))-(lam*(1-M)*Fo) #Forest

  # fix / check eq
  # The proportion of individuals that will decide to migrate will be the proportion that has a threshold less than M/P,
     #   This proportion is given by the cumulative distribution function of the logistic distribution
     #   minus the proportion of individuals from the population that has already migrated
     #  (see a similar algorithm in Iwasa et al. 2010). 

  mu  <-  (w *(1-Fo)+ z*M)/(2*(1-M))	 # average migration threshold of population of resident
  cdf <-	  1/(1 + exp(1)^((mu-M)/s))	 # migration function

  dy[2] <- ome*(cdf - M)
  
  return(list(dy))
}

phasePlot <- function(FM, elip,lambda,omega,w,z,s) 
{
  FM.flowField  <- 
    flowField(FM, x.lim = c(0.001, 1.0), y.lim = c(0.001, 1.0),
              xlab="Forest", ylab="Migrant",
              main=paste0("AUtil=",w," z=",z," s = ", s), 
              arrow.type="proportional", #so that length reflects magnitude of derivative.
              parameters = c(elip,lambda,omega,w,z,s), 
              points = 11, add = FALSE)  
  
  FM.nullclines <- nullclines(FM, x.lim = c(0.0, 1.0), y.lim = c(0.0, 1.0), 
                              parameters = c(elip,lambda,omega,w,z,s), 
                              system = "two.dim",
                              points = 100,colour = c("green", "violet"))  

  pts <- matrix(c(.15, .15, .15, .85, .85, .15, .85, .85), ncol = 2, nrow = 4, byrow = TRUE)
  
  FM.trajectory <- trajectory(FM, y0 = pts, t.end = 100, 
                              parameters = c(elip,lambda,omega,w,z,s), 
                              colour="gray51")
}

# make 3 by 3 window
quartz(width=10,height=8)
par (mfrow= c(3,3)) 
mar=c(1, 4, 1, 1) + 0.1 #it goes c(bottom, left, top, right) 

# seen estimates for 10%
for(a in c(0.1,0.1,0.25)) {
	z <- p/gamma	
	for(s in c(0.01,0.05,0.15)) {
		phasePlot(FM, elip,lambda,omega,w,z,s)
	}	
}
quartz.save("testPhase-p&s.pdf", type = "pdf")







