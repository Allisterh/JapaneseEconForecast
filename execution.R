

# setups ----------------------------------------------------------

library(xts)
library(magrittr)
library(tidyverse)
library(glmnet)
library(grplasso)
library(foreach)
library(doParallel)
registerDoParallel(detectCores())
library(openxlsx)


# rm(list=ls())
dat <- readRDS("data/dat.rds")
hChoises <- c(1,3,6,12)
targetVariables <- scan("txt/targetVariables.txt", character(), quiet=T)
winSize <- 60
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
    # source("models/di.r") # 10 mins
    source("models/dilasso.r") # 2 mins
    # source("models/lasso.r") # 20 mins (lab)
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
names(DILASSOcoefs) <- targetVariables
names(LASSOcoefs) <- targetVariables
names(ENETcoefs) <- targetVariables
names(ENETcv) <- targetVariables
names(gLASSOcoefs) <- targetVar


# comparison ---------------------------------------------------------------

## standardise to AR
MSFEs$h1 <- t(apply(MSFEs$h1, 1, function(x) x*(1/MSFEs$h1["AR",])))
MSFEs$h3 <- t(apply(MSFEs$h3, 1, function(x) x*(1/MSFEs$h3["AR",])))
MSFEs$h6 <- t(apply(MSFEs$h6, 1, function(x) x*(1/MSFEs$h6["AR",])))
MSFEs$h12 <- t(apply(MSFEs$h12, 1, function(x) x*(1/MSFEs$h12["AR",])))

## compare the performances of models against each other (How many times does each model outperform the counterparts)
models <- c("AR","DI","DILASSO","LASSO","ENET","gLASSO")
Results <- list()
for (k in 1:4){
  results  <- matrix(NA, length(models), length(models), dimnames = list(models, models))
  for (i in 1:length(models)) {
    for (j in 1:length(models)) {
      results[i,j] <- sum(MSFEs[[k]][i,] < MSFEs[[k]][j,])
    }
  diag(results) <- NA
  Results[[k]] <- results
  }
}



# save results ------------------------------------------------------------
source("saveResults.r") # saves outputs as rds
write.xlsx(MSFEs, "results/excel/MSFEs.xlsx", rowNames=T)
write.xlsx(Results, "results/excel/Results.xlsx", rowNames=T,keepNA=T)
write.xlsx(DILASSOcoefs[[15]], "results/excel/DILASSOcoefs.xlsx", rowNames=T, keepNA=T)

