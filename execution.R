

# load libraries ----------------------------------------------------------

library(xts)
library(magrittr)
library(vars)
library(glmnet)
library(grplasso)

library(foreach)
library(doParallel)
registerDoParallel(detectCores())


# initial setups ----------------------------------------------------------

# rm(list=ls())
dat <- readRDS("data/dat.rds")
hChoises <- c(1,3,6,12)
targetVariables <- scan("txt/targetVariables.txt", character())
winSize <- 60
# source("readResults.R") # load previously saved result



# create placeholders -----------------------------------------------------

# MSFEs <- list() # listed by horizon, each list model x variable
# ARlags <- list() # listed by variable, each list horizon x window
# VARlags <- list()
# DIlags <- list()
# DIfactor <- matrix(NA, nrow=length(hChoises), ncol=winSize, dimnames=list(c(paste("h=",hChoises,sep="")))) # number of factors
# DIfactorDyn <- matrix(NA, nrow=length(hChoises), ncol=winSize, dimnames=list(c(paste("h=",hChoises,sep="")))) # nr. dynamic fac
# DIfactorR2 <- matrix(NA, nrow=length(hChoises), ncol=winSize, dimnames=list(c(paste("h=",hChoises,sep="")))) # retained variance of X
# DIfactorList <- list()
# LASSOcoefs <- list() # listed by variable and horizon, each list window x horizon
# LASSOlambda <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
#                       dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
# LASSOsparsityRatio <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
#                        dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
# LASSO2coefs <- list() # listed by variable and horizon, each list window x horizon
# LASSO2lambda <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
#                       dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
# LASSO2sparsityRatio <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
#                              dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
# ENETcoefs <- list() # listed by variable and horizon, each list window x horizon
# ENETalpha <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
#                     dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
# ENETlambda <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
#                     dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
# ENETsparsityRatio <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
#                             dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
# ENETcv <- list() # listed by variable and horizon, each list nr-of-lambda option and nr-of-alpha
ENETnonzero <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                         dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))

gLASSOcoefs <- list() # listed by variable and horizon, each list window x horizon
gLASSOlambda <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                      dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
gLASSOsparsityRatio <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                       dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
gLASSOnonzero <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                        dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))


# model execution ---------------------------------------------------------

tictoc::tic()
pb<- txtProgressBar(0,length(hChoises)*length(targetVariables), style=3)
for (horizon in 3:length(hChoises)){
  h <- hChoises[horizon]
  # T1 <- which(index(dat)=="Jul 2008") - h # end of initialisation period
  # T2 <- which(index(dat)=="Jul 2013") - h # end of cv
  T1 <- which(index(dat)==" 7 2008") - h # end of initialisation period
  T2 <- which(index(dat)==" 7 2013") - h # end of cv
  for(var in 1:length(targetVariables)){
    targetVar <- targetVariables[var]
    
    # source("models/baseline.r") # 6 secs
    # source("models/ar.r") # 7 mins to execute
    # source("models/var.r") # 20 mins
    # source("models/di.r") # 15 mins
    # source("models/lasso.r") # 30 mins to execute
    # source("models/lasso2.r")
    # source("models/enet.r") # 20 hrs
    source("models/glasso.r")
    
    setTxtProgressBar(pb, (horizon-1)*length(targetVariables)+var)
  }
}
tictoc::toc()


# give names to lists -----------------------------------------------------
names(MSFEs) <- c(paste("h", hChoises, sep=""))
names(ARlags) <- targetVariables
names(VARlags) <- targetVariables
names(DIlags) <- targetVariables
names(DIfactorList) <- c(paste("h", hChoises, sep=""))
names(LASSOcoefs) <- targetVariables
names(LASSO2coefs) <- targetVariables
names(ENETcoefs) <- targetVariables
names(ENETcv) <- targetVariables
names(gLASSOcoefs) <- targetVar



# Standardise to AR -------------------------------------------------------

# MSFEs$h1 <- t(apply(MSFEs$h1, 1, function(x) x*(1/MSFEs$h1["AR",])))
# MSFEs$h3 <- t(apply(MSFEs$h3, 1, function(x) x*(1/MSFEs$h3["AR",])))
# MSFEs$h6 <- t(apply(MSFEs$h6, 1, function(x) x*(1/MSFEs$h6["AR",])))
# MSFEs$h12 <- t(apply(MSFEs$h12, 1, function(x) x*(1/MSFEs$h12["AR",])))


# store results -----------------------------------------------------------

saveRDS(ARlags, "results/ARlags.rds")
saveRDS(VARlags, "results/VARlags.rds")

saveRDS(DIfactor, "results/DIfactor.rds")
saveRDS(DIfactorDyn, "results/DIfactorDyn.rds")
saveRDS(DIfactorR2, "results/DIfactorR2.rds")
saveRDS(DIlags,"results/DIlags.rds")
saveRDS(DIfactorList, "results/DIfactorList.rds")

saveRDS(LASSOcoefs, "results/LASSOcoefs.rds")
saveRDS(LASSOlambda, "results/LASSOlambda.rds")
saveRDS(LASSOsparsityRatio, "results/LASSOsparsityRatio.rds")

saveRDS(LASSO2coefs, "results/LASSO2coefs.rds")
saveRDS(LASSO2lambda, "results/LASSO2lambda.rds")
saveRDS(LASSO2sparsityRatio, "results/LASSO2sparsityRatio.rds")

saveRDS(ENETcoefs, "results/ENETcoefs.rds")
saveRDS(ENETalpha, "results/ENETalpha.rds")
saveRDS(ENETlambda, "results/ENETlambda.rds")
saveRDS(ENETsparsityRatio, "results/ENETsparsityRatio.rds")
saveRDS(ENETcv, "results/ENETcv.rds")
saveRDS(ENETnonzero, "results/ENETnonzero.rds")

saveRDS(gLASSOcoefs, "results/gLASSOcoefs.rds")
saveRDS(gLASSOlambda, "results/gLASSOlambda.rds")
saveRDS(gLASSOsparsityRatio, "results/gLASSOsparsityRatio.rds")
saveRDS(gLASSOnonzero, "results/gLASSOnonzero.rds")

saveRDS(MSFEs, "results/MSFEs.rds")


