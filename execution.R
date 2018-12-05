

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
    source("models/lasso3.r") # 20 mins (lab)
    source("models/lasso4.r") # 20 mins (lab)
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



# interpret ---------------------------------------------------------------

models <- c("AR","DI","DILASSO","LASSO","LASSO2","LASSO3","LASSO4","ENET","gLASSO")
Results <- list() # count the number outperformed (20 if outperformed in all the variables)
for (k in 1:4){
  results  <- matrix(NA, length(models), length(models),
                   dimnames = list(models, models))
  for (i in 1:length(models)) {
    for (j in 1:length(models)) {
      results[i,j] <- sum(MSFEs[[k]][i,] < MSFEs[[k]][j,])
    }
  diag(results) <- NA
  Results[[k]] <- results
  }
}


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
saveRDS(LASSOnonzero, "results/LASSO/LASSOnonzero.rds")

saveRDS(LASSO2coefs, "results/LASSO2/LASSO2coefs.rds")
saveRDS(LASSO2lambda, "results/LASSO2/LASSO2lambda.rds")
saveRDS(LASSO2sparsityRatio, "results/LASSO2/LASSO2sparsityRatio.rds")
saveRDS(LASSO2nonzero, "results/LASSO2/LASSO2nonzero.rds")

saveRDS(LASSO3coefs, "results/LASSO3/LASSO3coefs.rds")
saveRDS(LASSO3lambda, "results/LASSO3/LASSO3lambda.rds")
saveRDS(LASSO3sparsityRatio, "results/LASSO3/LASSO3sparsityRatio.rds")
saveRDS(LASSO3nonzero, "results/LASSO3/LASSO3nonzero.rds")

saveRDS(LASSO4coefs, "results/LASSO4/LASSO4coefs.rds")
saveRDS(LASSO4lambda, "results/LASSO4/LASSO4lambda.rds")
saveRDS(LASSO4sparsityRatio, "results/LASSO4/LASSO4sparsityRatio.rds")
saveRDS(LASSO4nonzero, "results/LASSO4/LASSO4nonzero.rds")

saveRDS(ENETcoefs, "results/ENET/ENETcoefs.rds")
saveRDS(ENETalpha, "results/ENET/ENETalpha.rds")
saveRDS(ENETlambda, "results/ENET/ENETlambda.rds")
saveRDS(ENETsparsityRatio, "results/ENET/ENETsparsityRatio.rds")
saveRDS(ENETcv, "results/ENET/ENETcv.rds")
saveRDS(ENETnonzero, "results/ENET/ENETnonzero.rds")

saveRDS(ENET2coefs, "results/ENET/ENETcoefs.rds")
saveRDS(ENET2alpha, "results/ENET/ENETalpha.rds")
saveRDS(ENET2lambda, "results/ENET/ENETlambda.rds")
saveRDS(ENET2sparsityRatio, "results/ENET/ENETsparsityRatio.rds")
saveRDS(ENET2cv, "results/ENET/ENETcv.rds")
saveRDS(ENET2nonzero, "results/ENET/ENETnonzero.rds")

saveRDS(gLASSOcoefs, "results/gLASSO/gLASSOcoefs.rds")
saveRDS(gLASSOlambda, "results/gLASSO/gLASSOlambda.rds")
saveRDS(gLASSOsparsityRatio, "results/gLASSO/gLASSOsparsityRatio.rds")
saveRDS(gLASSOnonzero, "results/gLASSO/gLASSOnonzero.rds")

saveRDS(gLASSO2coefs, "results/gLASSO/gLASSOcoefs.rds")
saveRDS(gLASSO2lambda, "results/gLASSO/gLASSOlambda.rds")
saveRDS(gLASSO2sparsityRatio, "results/gLASSO/gLASSOsparsityRatio.rds")
saveRDS(gLASSO2nonzero, "results/gLASSO/gLASSOnonzero.rds")

saveRDS(MSFEs, "results/MSFEs.rds")


