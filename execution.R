

# setups ----------------------------------------------------------

library(xts)
library(magrittr)
library(vars)
library(glmnet)
library(grplasso)

library(foreach)
library(doParallel)
registerDoParallel(detectCores())


# rm(list=ls())
dat <- readRDS("data/dat.rds")
hChoises <- c(1,3,6,12)
targetVariables <- scan("txt/targetVariables.txt", character())
winSize <- 60
# source("readResults.R") # load previously saved result

# source("placeholders.r") # load placeholders to store results

# model execution ---------------------------------------------------------

tictoc::tic()
pb<- txtProgressBar(0,length(hChoises)*length(targetVariables), style=3)
for (horizon in 1:length(hChoises)){
  h <- hChoises[horizon]
  T1 <- which(index(dat)==" 7 2008") - h # end of initialisation period
  T2 <- which(index(dat)==" 7 2013") - h # end of cv
  for(var in 1:length(targetVariables)){
    targetVar <- targetVariables[var]
    
    # source("models/ar.r") # 7 mins to execute
    # source("models/di.r") # 15 mins
    # source("models/lasso.r") # 30 mins to execute
    # source("models/lasso2.r")
    source("models/lasso3.r")
    source("models/lasso4.r")
    # source("models/enet.r") # 5 hrs (Lab)
    # source("models/glasso.r") # 5-6hrs (iMac)
    
    setTxtProgressBar(pb, (horizon-1)*length(targetVariables)+var)
  }
}
tictoc::toc()


# give names to lists -----------------------------------------------------
names(ARlags) <- targetVariables
names(ARlagsCV) <- targetVariables
names(VARlags) <- targetVariables
names(DIlags) <- targetVariables
names(DIfactorList) <- c(paste("h", hChoises, sep=""))
names(LASSOcoefs) <- targetVariables
names(LASSO2coefs) <- targetVariables
names(LASSO3coefs) <- targetVariables
names(LASSO4coefs) <- targetVariables
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

saveRDS(DIfactor, "results/DI/DIfactor.rds")
saveRDS(DIfactorDyn, "results/DI/DIfactorDyn.rds")
saveRDS(DIfactorR2, "results/DI/DIfactorR2.rds")
saveRDS(DIlags,"results/DI/DIlags.rds")
saveRDS(DIfactorList, "results/DI/DIfactorList.rds")

saveRDS(LASSOcoefs, "results/LASSO/LASSOcoefs.rds")
saveRDS(LASSOlambda, "results/LASSO/LASSOlambda.rds")
saveRDS(LASSOsparsityRatio, "results/LASSO/LASSOsparsityRatio.rds")

saveRDS(LASSO2coefs, "results/LASSO2/LASSO2coefs.rds")
saveRDS(LASSO2lambda, "results/LASSO2/LASSO2lambda.rds")
saveRDS(LASSO2sparsityRatio, "results/LASSO2/LASSO2sparsityRatio.rds")

saveRDS(ENETcoefs, "results/ENET/ENETcoefs.rds")
saveRDS(ENETalpha, "results/ENET/ENETalpha.rds")
saveRDS(ENETlambda, "results/ENET/ENETlambda.rds")
saveRDS(ENETsparsityRatio, "results/ENET/ENETsparsityRatio.rds")
saveRDS(ENETcv, "results/ENET/ENETcv.rds")
saveRDS(ENETnonzero, "results/ENET/ENETnonzero.rds")

saveRDS(gLASSOcoefs, "results/gLASSO/gLASSOcoefs.rds")
saveRDS(gLASSOlambda, "results/gLASSO/gLASSOlambda.rds")
saveRDS(gLASSOsparsityRatio, "results/gLASSO/gLASSOsparsityRatio.rds")
saveRDS(gLASSOnonzero, "results/gLASSO/gLASSOnonzero.rds")

saveRDS(MSFEs, "results/MSFEs.rds")


