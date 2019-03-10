

# setups ----------------------------------------------------------

library(xts)
library(magrittr)
library(tidyverse)
library(glmnet)
library(grplasso)
library(foreach)
library(doParallel)
registerDoParallel(detectCores())


rm(list=ls()) # clear workspace
dat <- readRDS("data/dat.rds")
hChoises <- c(1,3,12) # forecasting horizons
targetVariables <- scan("txt/targetVariables.txt", character(), quiet=T) # variables to be forecasted
winSize <- 60 # window size
source("placeholders.r") # load placeholders to store results

# model execution ---------------------------------------------------------

tictoc::tic()
pb<- txtProgressBar(0,length(hChoises)*length(targetVariables), style=3)
for (horizon in 1:length(hChoises)){
  h <- hChoises[horizon]
  T1 <- which(index(dat)=="July 2008") - h # end of initialisation period
  T2 <- which(index(dat)=="July 2013") - h # end of cv
  for(var in 1:length(targetVariables)){
    targetVar <- targetVariables[var]
    # source("models/ar.r") # 7 mins to execute
    # source("models/di.r") # 10 mins
    # source("models/dicv.r") # 3 hrs / 40 min (iMac)
    source("models/dilasso.r") # 1 hr / 15 mins (iMac)
    # source("models/lasso.r") # 1hr / 20 mins (lab)
    # source("models/enet.r") # 3.5-4 hrs (Lab)
    # source("models/glasso.r") # 4 hrs (iMac)
    setTxtProgressBar(pb, (horizon-1)*length(targetVariables)+var)
  }
}
tictoc::toc()
source("saveResults.r")


# give names to list  -----------------------------------------------------
varNameShort <- scan("txt/targetVariablesShort.txt", character(), quiet=T)

names(ARlags) <- varNameShort
names(DIlags) <- varNameShort
names(DICVr2) <- varNameShort
names(DICVlags) <- varNameShort
names(DILASSOcoefs) <- varNameShort
names(DILASSOr2) <- varNameShort
names(LASSOcoefs) <- varNameShort
names(ENETcoefs) <- varNameShort
names(gLASSOcoefs) <- varNameShort

# Standardise MSFE--------------------------------------------------------
MSFE2 <- lapply(1:3, function(h){
  t(apply(MSFEs[[h]],1, function(x) x*(1/MSFEs[[h]]["AR",]))) %>% 
    set_colnames(varNameShort)
}) %>% set_names(c("h1","h3","h12"))


saveRDS(MSFEs, "results/MSFE/MSFEs.rds")
saveRDS(MSFE2, "results/MSFE/MSFE2.rds")
