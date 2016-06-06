library(minpack.lm) # nonlinear least-squares regression
library(deSolve) # ode solver

#*************************DATA***************************************
mammals <- read.csv("nagupande_mammals_R.csv") # read in dataset
mammals$tot_kill <- rowSums(mammals[,4:9]) # add all species to give totals shot per month

# plot of numbers of mammals shot by month of the study
plot(mammals$month_tot, mammals$tot_kill,pch=19,col="blue",bty="n", cex.main=0.8,
     xlab="Month of study",ylab="Numbers of mammals shot",
     main="The numbers of mammals shot each month during the Nagupande game experiment")
#*******************************************************************

#*********************MODEL OF NUMBERS KILLED PER MONTH***********

month_kill <- function(params){
  k_1 <- params[1]
  k_2 <- params[2]
  N_k <- numeric(length(dat[,1]))
  month <- c(1:length(dat[,1]))
  
  for   (i in 1:length(dat[,1])) {
  N_k[i] <- k_1 * ( exp(k_2*(month[i] - 1)) - exp(k_2 * month[i]))
  }
  return(N_k)
}
 
#***********************FUNCTION TO FIT MODEL*********************

mod.residual <- function(params,dat=mammals){ # function to run through nlls + calc residuals
   # parameters for the estimation routine
    k_1 <- params[1] # -K/N(0) / k_2
    k_2 <- params[2] # r - K

    # solve model
    out <- month_kill(params)
    #return(out)
    # evaluate predicted vs obs
    ssqres <- out - dat$tot_kill
    return(ssqres)
}
#*********************FIT MODEL**********************
# all months included
dat <- mammals[-c(39:60),]
fit <- nls.lm(par=c(k_1=2000,k_2=-0.07),fn=mod.residual,dat=dat)
fitted.model <- month_kill(params=c(coef(fit)[1],coef(fit)[2]))
#lines(dat$month_tot,fitted.model)
coef(fit)
# removing first two months
dat <- mammals[-c(1:2,39:60),]
fit2 <- nls.lm(par=c(k_1=2000,k_2=-0.07),fn=mod.residual,dat=dat)
fitted.model2 <- month_kill(params=c(coef(fit2)[1],coef(fit2)[2]))
lines(dat$month_tot,fitted.model2,col="red",lwd=2)
coef(fit2)
legend("topright", bty="n",legend=c("Observed","Predicted"),fill=c("blue","red"))

#******************CALCULATING NUMBERS OF HOSTS AT START*******************************
# parameters
growth.rate <- seq(0,0.015,0.001)  # r
num.start <- function(k_1,k_2,r){
  N0 <- -k_1 * k_2 / (r - k_2)
  return(N0)
}
number.mammals <- num.start(coef(fit2)[1],coef(fit2)[2],growth.rate)

#***********************MODEL OF NUMBERS OF HOSTS OVER TIME***********************************
host.mod <- function(t,N,params){ 
  # parameters
  k_2 <- params[1] # 
  r <- params[2] # growth rate
  # ODE
  dN.dt <- (r - (r - k_2)) * N
  # list containing the derivatives
  xdot <- c(dN.dt)
  return(list(xdot))
}

host.sim <- function(k_2,r,n0,t.max=60,ts=1){
  N0 <- n0
  # timesteps
  times<-seq(1,t.max,ts) 
  # initial conditions
  init <- c(number=N0)
  return(data.frame(lsoda(init,times,host.mod,c(k_2,r)))) 
}

est.mammals <- host.sim(k_2=coef(fit2)[2],r=growth.rate[1],n0=number.mammals[1],t.max=60,ts=1) 

plot.func<-function(mod){
  plot(mod$time, mod$number,type="l",main="Predicted numbers of mammals during study",
       ylab="Numbers of mammals",xlab="Month of study",col="blue",cex.main=0.8,ylim=c(0,1500),lwd=2,bty="n",cex.axis=1.2,cex.lab=1.2)
}
plot.func(est.mammals)

est.mammals2 <- host.sim(k_2=coef(fit2)[2],r=growth.rate[length(growth.rate)],n0=number.mammals[length(growth.rate)],t.max=60,ts=1) 
lines(est.mammals2$time,est.mammals2$number,lwd=2,col="red")
legend("topright",legend=c("Growth rate = 0","Growth rate = 1.5%"),fill=c("blue","red"),cex=1.2,bty="n")

#********************************************************************************
# K = 1.571239
# r = 1.5 in moths - 30 days in a month
#1.571239/30
#1.5/30
#coef(fit2) K2 / 30 =  0.0023
