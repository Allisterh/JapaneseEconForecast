

# setups ----------------------------------------------------------

library(xts)
library(magrittr)
library(tidyverse)
library(glmnet)
library(grplasso)
library(ncvreg)
library(foreach)
library(doParallel)
registerDoParallel(detectCores())
library(openxlsx)


rm(list=ls())
dat <- readRDS("data/dat.rds")
hChoises <- c(1,3,12)
targetVariables <- scan("txt/targetVariables.txt", character(), quiet=T)
winSize <- 60
source("placeholders.r") # load placeholders to store results

# model execution ---------------------------------------------------------

tictoc::tic()
pb<- txtProgressBar(0,length(hChoises)*length(targetVariables), style=3)
for (horizon in 1:length(hChoises)){
  h <- hChoises[horizon]
  T1 <- which(index(dat)==" 7 2008") - h # end of initialisation period
  T2 <- which(index(dat)==" 7 2013") - h # end of cv
  for(var in 1:length(targetVariables)){
    targetVar <- targetVariables[var]
    source("models/ar.r") # 7 mins to execute
    source("models/di.r") # 10 mins
    source("models/dicv.r") # 3 hrs / 40 min (iMac)
    source("models/dilasso.r") # 1 hr / 15 mins (iMac)
    source("models/lasso.r") # 1hr / 20 mins (lab)
    source("models/enet.r") # 3.5-4 hrs (Lab)
    source("models/glasso.r") # 4 hrs (iMac)
    source("models/scad.r") # 10 mins
    setTxtProgressBar(pb, (horizon-1)*length(targetVariables)+var)
  }
}
tictoc::toc()
source("saveResults.r")




# comparison ---------------------------------------------------------------
varNameShort <- scan("txt/targetVariablesShort.txt", character(), quiet=T)
## standardise to AR
MSFE2 <- lapply(1:3, function(h){
  t(apply(MSFEs[[h]],1, function(x) x*(1/MSFEs[[h]]["AR",]))) %>% 
    set_colnames(varNameShort)
}) %>% set_names(c("h1","h3","h12"))


## rank models 

# return model name
MSFE3 <-lapply(1:3, function(h){
  sapply(1:length(varNameShort), function(x) names(sort(MSFEs[[h]][,x]))) %>% 
    set_colnames(varNameShort)
}) %>% set_names(c("h1","h3","h12"))


# return model ranking 
MSFE4 <- lapply(1:3, function(h){
  sapply(1:length(varNameShort), function(x) rank(MSFEs[[h]][,x], ties.method="min")) %>% 
    set_colnames(varNameShort)
}) %>% set_names(c("h1","h3","h12"))


## Comparison (competition) Table, which compares the performances of models against each other 
## i.e., How many times does each model outperform the counterparts

compTable <- lapply(1:3, function(k){ 
  msfe <- MSFEs[[k]]
  subTable <- matrix(NA, nrow(MSFEs$h1), nrow(MSFEs$h1), 
                     dimnames = list(rownames(MSFEs$h1), rownames(MSFEs$h1)))
  for (i in 1:nrow(MSFEs$h1)) {for (j in 1:nrow(MSFEs$h1)) { 
    subTable[i,j] <- sum(msfe[i,] < msfe[j,])   }}
  diag(subTable) <- NA
  return(subTable)
}) %>% set_names(c("h1", "h3", "h12"))

saveRDS(MSFEs, "results/MSFE/MSFEs.rds")
saveRDS(MSFE2, "results/MSFE/MSFE2.rds")
saveRDS(MSFE3, "results/MSFE/MSFE3.rds")
saveRDS(MSFE4, "results/MSFE/MSFE4.rds")
saveRDS(compTable, "results/MSFE/compTable.rds")

# save results ------------------------------------------------------------
source("saveExcels.r") # save reuslts as excel 
