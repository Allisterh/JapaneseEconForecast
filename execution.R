
library(xts)
library(magrittr)
library(vars)
library(glmnet)


rm(list=ls())
dat <- readRDS("data/dat.rds")
hChoises <- c(1,3,6,12)
targetVariables <- scan("txt/targetVariables.txt", character())
winSize <- 60

MSFEs <- list() # listed by horizon, each list model x variable
ARlags <- list() # listed by variable, each list horizon x window
VARlags <- list()
DIlags <- list()
DIfactor <- matrix(NA, nrow=length(hChoises), ncol=winSize, dimnames=list(c(paste("h=",hChoises,sep="")))) # number of factors
DIfactorDyn <- matrix(NA, nrow=length(hChoises), ncol=winSize, dimnames=list(c(paste("h=",hChoises,sep="")))) # nr. dynamic fac
DIfactorR2 <- matrix(NA, nrow=length(hChoises), ncol=winSize, dimnames=list(c(paste("h=",hChoises,sep="")))) # retained variance of X
LASSOcoefs <- list() # listed by variable and horizon, each list window x horizon
LASSOlambda <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                      dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
LASSOsparsityRatio <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                       dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
ENETsparsityRatio <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                            dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))

tictoc::tic()
pb<- txtProgressBar(0,length(hChoises)*length(targetVariables), style=3)
for (horizon in 1:length(hChoises)){
  h <- hChoises[horizon]
  # T1 <- which(index(dat)=="Jul 2008") - h # end of initialisation period
  # T2 <- which(index(dat)=="Jul 2013") - h # end of cv
  T1 <- which(index(dat)==" 7 2008") - h # end of initialisation period
  T2 <- which(index(dat)==" 7 2013") - h # end of cv
  for (var in 1:length(targetVariables)){
    targetVar <- targetVariables[var]
    
    source("models/baseline.r") # 6 secs
    # source("models/ar.r") # 7 mins to execute
    # source("models/var.r") # 20 mins
    source("models/di.r") # 10 mins
    # source("models/lasso.R") # 90 mins to execute
    # source("enet.r")
    
    setTxtProgressBar(pb, 20*(horizon-1)+var)
  }
}


names(MSFEs) <- c(paste("h", hChoises, sep=""))
names(ARlags) <- targetVariables
names(VARlags) <- targetVariables
names(DIlags) <- targetVariables
names(LASSOcoefs) <- targetVariables

tictoc::toc()
