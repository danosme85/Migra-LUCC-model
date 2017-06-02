
## Phase plane analysis using phaseR package
## Source: http://www.magesblog.com/2014/11/phase-plane-analysis-in-r.html

#install.packages("phaseR")
# https://cran.r-project.org/web/packages/phaseR/phaseR.pdf



library(phaseR)

# important to remove previously assigned values
rm(bmu, h, eta, bet, oma, br, bb, c, a)

bmu<-0.001 ; h<-0.02 ; eta<-0.15 ; bet<-11 ; oma<-0.92 ; br<-0 ; bb<-0.5 ; c<-2 ; a<-1 # default parameter values

nty <-4 #number of trayectories to plot (if chosing random initial conditions)


FP <- function (t, y, parameters) # computes the derivative at a point for the ODE system
{  
  Fo<-y[1] ;   Ag<-y[2] ; rec<-
  bmu <-parameters[1] ; P  <-parameters[2] # in both equations
  eps<-parameters[3] ; lam<-parameters[4] # for Forest eq.
  oma<-parameters[5] ; h  <-parameters[6] ; gam<-parameters[7] ; s<-parameters[8] # for Migra eq.
  
  dy <- numeric(2)
  dy[1] <-(eps*Fo*(1-(Fo/To)))-(lam*(P-M)*Fo) #Forest
  #dy[2] <-oma*((P/(1+(exp(1))^((((((To-Fo)*h)/(P-M))/(2*gam))-(M/P))/s)))-M) #Migration
  
  # Explore the implications of different functional forms of an endogenous ω
    # ω = a*M ;
  #dy[2] <- a*M* ((P/(1+(exp(1))^((((((To-M)*h)/(P-M))/(2*gam))-(M/P))/s)))-M)
  
    # ω  = a*M^2 ;
  #dy[2] <- a*(M)^2* ((P/(1+(exp(1))^((((((To-M)*h)/(P-M))/(2*gam))-(M/P))/s)))-M)
  
    # ω = a*log(1+M)
  dy[2] <- a*log(1+(M))* ((P/(1+(exp(1))^((((((To-M)*h)/(P-M))/(2*gam))-(M/P))/s)))-M)
  
  # TO-DO ω with logistic and threshold functions (more parameters)
  
  return(list(dy))
}

phasePlot <- function(FP, To, P, eps, lam, oma, h, gam, s) 
{
  FP.flowField  <- 
    flowField(FP, x.lim = c(0.01, 99.9), y.lim = c(0.01, 99.9),
              #Error in if ((dx[i, j] != 0) | (dy[i, j] != 0)) { :                                                   missing value where TRUE/FALSE needed
              #Solved by reducing limits from [0, 100] to [0.01, 99.9]
              xlab="F", ylab="M",
              #main=paste0("ω = ", oma), #other symbols λ ω 
              #arrow.type="proportional", #so that length reflects magnitude of derivative.
              parameters = c(To, P, eps, lam, oma, h, gam, s), 
              points = 11, add = FALSE)  
  
  FP.nullclines <- nullclines(FP, x.lim = c(0.01, 99.9), y.lim = c(0.01, 99.9), 
                              parameters = c(To, P, eps, lam, oma, h, gam, s), 
                              points = 500)  
  
  # random initial conditions (above) or systematically chosen (below)
  #y0 <- matrix(data=sample(1:100, nty*2, replace=T), ncol = 2, nrow = nty*2, byrow = TRUE) 
   y0 <- matrix(c(25, 25, 25, 75, 75, 25, 75, 75), ncol = 2, nrow = 4, byrow = TRUE)
  FP.trajectory <- trajectory(FP, y0 = y0, t.end = 500, 
                              parameters = c(To, P, eps, lam, oma, h, gam, s), 
                              colour="gray51")
}

#quartz("phase portraits endogenous ω (2)", 8, 8)
#par(mfrow=c(2,2))
phasePlot(FP, To=To, P=P, eps=eps, lam=lam, oma=oma, h=h, gam=gam, s=s)
#mtext("h = 2", cex=0.8, line=0.5, adj=-0) 

#mtext("base equations", cex=0.8, line=0.5, adj=-0) #other symbols λ ω
#mtext("endogenous ω (a*M); a = 1/P", cex=0.8, line=0.5, adj=-0) #other symbols λ ω 
#mtext("endogenous ω (a*M^2); a = 1/P", cex=0.8, line=0.5, adj=-0) #other symbols λ ω 
mtext("endogenous ω (a*log(1+M)); a = 1/(log(1+P))", cex=0.8, line=0.5, adj=-0) #other symbols λ ω 
#mtext("a=1/P", cex=0.6, line=1.5, adj=1) #other symbols λ ω 


quartz.save("PP endogenous ω (2).pdf", type = "pdf", dpi = 500)




