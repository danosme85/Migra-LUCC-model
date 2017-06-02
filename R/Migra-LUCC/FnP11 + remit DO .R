
# model based on 
# Regime shifts in a socio-ecological model of farmland abandonment 
#    Joana Figueiredo • Henrique M. Pereira 
#    Landscape Ecol (2011) 26:737–749 DOI 10.1007/s10980-011-9605-3 

## Adding a remittance mechanism

library(phaseR)

mainDir <- "/Users/Daniel_Ospina/Box Sync/PhD/proyectos/Migra-LUCC model/R/Migra-LUCC"
outputDir <- "Figures"
setwd(mainDir)
setwd(file.path(mainDir, outputDir))


# important to remove previously assigned values
rm(P,To,s,gam,h,eps,lam,oma,a)

To<-100 ; P<-100 ; eps<-2 ; lam<-0.02 ; oma <- 1 ; h<-1 ; gam<-1 ; s<-0.05  # default parameter values

nty <-4 #number of trayectories to plot (if chosing random initial conditions)

# To explore the implications of different functional forms of an endogenous ω
a = 1/P 
a = 1/(log(1+P))

# Incorporating mechanisms linking remittances and land use/cover change
r = 0.05*gam # share of city earnings remitted


FP <- function (t, y, parameters) # computes the derivative at a point for the ODE system
{  
  Fo<-y[1] ;   M<-y[2]
  To <-parameters[1] ; P  <-parameters[2] # in both equations
  eps<-parameters[3] ; lam<-parameters[4] # for Forest eq.
  oma<-parameters[5] ; h  <-parameters[6] ; gam<-parameters[7] ; s<-parameters[8] # for Migra eq.
  #a  <-parameters[9] # new parameter to endogenize oma
  r<-parameters[9] # new paramenter to incorporate remittances
  
  dy <- numeric(2)
  dy[1] <-(eps*Fo*(1-(Fo/To)))-(lam*(P-M)*Fo) #Forest original
  z<-r*(M/P)
  #dy[1] <-(eps*Fo*(1-(Fo/To)))-((lam+z)*(P-M)*Fo) # land clearing capability expanded by remittances
  
  #miu<-((To-Fo)*h)/((P-M)*2*gam) #original
  miu<-((To-Fo)*(h+z))/((P-M)*2*gam) #intensification capability expanded via remittances
  cdf<- 1/(1+exp((miu-(M/P))/s))
  dy[2] <-oma*((P*cdf)-M) #Migration
  
  # Explore the implications of different functional forms of an endogenous ω
  #dy[2] <- (a/P)*M*(1-(M/P))* ((P*cdf)-M) # logistic
  
  # with a = 1/P 
  #dy[2] <- a*M* ((P*cdf)-M)   # linear: ω = a*M 
  #dy[2] <- a*(M^2)* ((P*cdf)-M)  # quadratic: ω  = a*M^2 
  
  # with a = 1/(log1p(P))
  #dy[2] <- a*log1p(M)* ((P*cdf)-M)  # log: ω = a*log(1+M)
  

  # TO-DO ω with lhreshold functions (more parameters)
  
  return(list(dy))
}

phasePlot <- function(FP, To, P, eps, lam, oma, h, gam, s, r) 
{
  FP.flowField  <- 
    flowField(FP, x.lim = c(0.01, 99.9), y.lim = c(0.01, 99.9),
              #Error in if ((dx[i, j] != 0) | (dy[i, j] != 0)) { :                                                   missing value where TRUE/FALSE needed
              #Solved by reducing limits from [0, 100] to [0, 99.9]
              xlab="F", ylab="M",
              main=paste0("r = ", r), #other symbols λ ω
              #arrow.type="proportional", #so that length reflects magnitude of derivative.
              parameters = c(To, P, eps, lam, oma, h, gam, s, r), 
              points = 11, add = FALSE)  
  
  FP.nullclines <- nullclines(FP, x.lim = c(0.01, 99.9), y.lim = c(0.01, 99.9), 
                              parameters = c(To, P, eps, lam, oma, h, gam, s, r), 
                              points = 500)  
  
  # random initial conditions (above) or systematically chosen (below)
  #y0 <- matrix(data=sample(1:100, nty*2, replace=T), ncol = 2, nrow = nty*2, byrow = TRUE) 
  y0 <- matrix(c(25, 25, 25, 75, 75, 25, 75, 75), ncol = 2, nrow = 4, byrow = TRUE)
  FP.trajectory <- trajectory(FP, y0 = y0, t.end = 500, 
                              parameters = c(To, P, eps, lam, oma, h, gam, s, r), 
                              colour="gray51")
}

quartz("PP_FnP11_remit for intensification", width=7, height=7)
par(mfrow=c(2,2), mar=c(4,4,2,1)+0.2)

for(r in c(0,0.5,1,2)) {
  phasePlot(FP, To, P, eps, lam, oma, h, gam, s, r)
}
mtext("(A(h+z))/2gamR", cex=0.8, line=2, adj=0)
mtext("z=r(M/P)", cex=0.8, line=1, adj=0) #other symbols λ ω
quartz.save("PP_FnP11_remit for intensification.pdf", type = "pdf", dpi = 500)

#phasePlot(FP, FP, To, P, eps, lam, oma, h, gam, s, a)
#mtext("h = 2", cex=0.8, line=0.5, adj=-0) 

#mtext("base equations", cex=0.8, line=0.5, adj=-0) #other symbols λ ω
#mtext("endogenous ω (a*M); a = 1/P", cex=0.8, line=0.5, adj=-0) #other symbols λ ω 
#mtext("endogenous ω (a*M^2); a = 1/P", cex=0.8, line=0.5, adj=-0) #other symbols λ ω 
#mtext("endogenous ω (a*log(1+M)); a = 1/(log(1+P))", cex=0.8, line=0.5, adj=-0) #other symbols λ ω 
#mtext("a=1/P", cex=0.6, line=1.5, adj=1) #other symbols λ ω 






