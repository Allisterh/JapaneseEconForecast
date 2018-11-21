
library(xts)
library(magrittr)
library(vars)
library(glmnet)


rm(list=ls())
dat <- readRDS("data/dat.rds")
hChoises <- c(1,3,6,12)
targetVariables <- scan("txt/targetVariables.txt", character())

MSFEs <- list() # listed by horizon, each list model x variable
ARlags <- list() # listed by variable, each list horizon x window
VARlags <- list()
LASSOcoefs <- list() # listed by variable and horizon, each list window x horizon
sparsityRatio <- matrix(NA, nrow=length(targetVariables), ncol=length(hChoises),
                        dimnames = list(targetVariables,c(paste("h=",hChoises,sep=""))))
sparsityRatioENET <- matrix(NA, nrow=length(targetVariables), ncol=length(hChoises),
                        dimnames = list(targetVariables,c(paste("h=",hChoises,sep=""))))

tictoc::tic()
pb<- txtProgressBar(0,length(hChoises)*length(targetVariables), style=3)
for (horizon in 1:length(hChoises)){
  results <- matrix(NA, nrow=7, ncol=length(targetVariables), # stores msfes with horizon fixed
                    dimnames = list(c("SM","RW","AR","VAR","LASSO", "ENET","DI"),targetVariables))
  h <- hChoises[horizon]
  winSize <- 60
  T1 <- which(index(dat)=="Jul 2008") - h # end of initialisation period
  T2 <- which(index(dat)=="Jul 2013") - h # end of cv
  for (var in 1:length(targetVariables)){
    targetVar <- targetVariables[var]
    
    source("models/baseline.r")
    source("models/ar.r")
    source("models/var.r")
    source("models/lasso.r")
    source("enet.r")
    source("factor.r")
    setTxtProgressBar(pb, 20*(horizon-1)+var)
  }
  MSFEs[[horizon]] <- results
}


names(MSFEs) <- c(paste("h", hChoises, sep=""))
names(ARlags) <- targetVariables
names(VARlags) <- targetVariables
names(LASSOcoefs) <- targetVariables

tictoc::toc()


