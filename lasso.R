library(magrittr)
library(tidyverse)
library(xts)

rm(list=ls())
dat<- readxl::read_excel("data_JER.xls")
dat<- xts(dat[-1], order.by = dat$date)*100 # change to percent change 
dat[,c(46,131:148)] <- dat[,c(46, 131:148)]/100 # transform back, for series recorded in levels

# dat<- scale(dat)



## In this prototype, I just focus on forecasting IIP (dat[,1])

# forecasting -------------------------------------------------------------
## forecast horizon = 1 (h=1)
## forecasting period = "1995-07-1/2001-02-01"
## use recursive window 
## evaluated on mean squared forecast error (msfe)
h <- 1 

# univariate (baseline) ---------------------------------------------------

start <- 2*(floor(nrow(dat)/3)) # evaluate the best lambda 
# start<-which(index(dat)=="1995-06-01") # last observation for first forecast
end<- nrow(dat) - 1 # last observation for the last forecast (2001-02-01)
predErrAR<- 0
foo<- numeric()
idxTracker<-0
for (t in start:end){
  idxTracker<- 1+ idxTracker
  fitAR<- ar(dat[51:t,])
  predAR <- predict(fitAR, n.ahead=h)$pred[h]
  predErrAR <- (predAR - as.numeric(dat[t+h,51]))^2 + predErrAR
  foo[idxTracker]<- fitAR$order
}
msfeAR <- predErrAR/(end-start)

# VAR (3 var) -------------------------------------------------------------
## Inflation rate(51), short term interest rate(148) and IIP growth(1)
library(vars)
idx<- c(1,51,148)

predErrVAR<- 0
for (t in start:end){
  datVAR <- dat[1:t,idx]
  fitVAR <- VAR(datVAR, lag.max = 12)
  predVAR <- predict(fitVAR, n.ahead=h)
  predErrVAR <- (predVAR$fcst$X51[1] - as.numeric(dat[t+h,51]))^2 + predErrVAR
}
msfeVAR<- predErrVAR/(end-start)

# lasso -------------------------------------------------------------------
library(glmnet)
## lambda and lag selection on cross validation (b/w period T1 n T2, lag.max=12)
lagMax <- 12
lambdaChoises <- 10^(seq(-2,0,len=100)) # lambda choices, selection on CV

cv <- matrix(0, nrow=length(lambdaChoises), ncol=lagMax) # placeholder for cross validation

pb <- txtProgressBar(0,lagMax, style=3)
for (p in 1:lagMax){
  x <- lag.xts(dat, 1:p)
  x <- x[-c(1:p),] # trim x'NAs **Note: I have to remove last obs if h > 1 bc there's no counterpart obs for y**
  y <- lag.xts(dat[,51], -(h-1)) # **Note: remove last (h-1) obs's when h >1 **
  y <- y[-c(1:p),]
  T1 <- floor(nrow(y)/3) # end of initialisation
  T2 <- 2*(floor(nrow(y)/3)) # end of lambda selection
  end<- nrow(y)
  for (t in T1:(T2-1)){ # penalty param / lag selection
    fitLasso <- glmnet(x[1:t],y[1:t], lambda=lambdaChoises, family = "gaussian", alpha=1)
    predLasso <- predict.glmnet(fitLasso, coredata(x[t+1,]))
    cv[,p] <- (predLasso - as.numeric(y[t+1,]))^2 + cv[,p]
    # minIdx <- which.min(predErrLassoLambda)
    # optLam <- lambdaChoises[minIdx]
  }
  cv[,p] <- cv[,p]/(T2-T1) # adjust the difference in length of cv period 
  setTxtProgressBar(pb, p)
}
lamIdx <- which(cv==min(cv), arr.ind = T)[1] # optimal lambda (idx)
optLam <- lambdaChoises[lamIdx]
optLag <- which(cv==min(cv), arr.ind = T)[2] # opt lag

# Now we have selected optimal regularisation parameter and lag length based on cross valisation. 

x <- lag.xts(dat, 1:optLag)
x <- x[-c(1:optLag),] # trim x'NAs **Note: I have to remove last obs if h > 1 bc there's no counterpart obs for y**
y <- lag.xts(dat[,51], -(h-1)) # **Note: remove last (h-1) obs's when h >1 **
y <- y[-c(1:optLag),]

predErrLasso <- 0 
for (t in T2:(end-1)){ # forecast evaluation
  fitLasso <- glmnet(x[1:t], y[1:t], lambda=optLam,family = "gaussian", alpha=1)
  predLasso <- predict.glmnet(fitLasso, coredata(x[t+1,]))
  predErrLasso <- as.numeric(predLasso - as.numeric(y[t+1,]))^2 + predErrLasso
}
msfeLasso <- predErrLasso/(end-T2) 
### Changing lag=12 results in significant decrease in msfe, but this is hard to justify...



############################################################
# predErrLasso <- numeric(12) # each element for each lag (1:12), remove later
# msfeLasso <- numeric(12) 
# lambda_msfe <- numeric(length(lambdaChoises))
# optLams <- matrix(NA, nrow=(end-start), ncol=12) # record lambda chosen for each window/lag
# variablesSelected <- list() # binary, for every variable and every lag
# variablesSelectedSum <- list() # nr of times each variable is used (sum over each lag)
# pTracker <- 0 # index tracker for lag
pb <- txtProgressBar(0, 12, style=3)
tictoc::tic()
for (p in 1:12){   # loop through lags 
  # pTracker <- pTracker +1 # keep track of lag length (tracker=1 if max.lag=1)..... -> p
  # prepare appropriate dependent/explanatory variables 
  x <- lag.xts(dat, 1:p)
  x <- x[-c(1:p),] # trim x'NAs **Note: I have to remove last obs if h > 1 bc there's no counterpart obs for y**
  y <- lag.xts(dat[,51], -(h-1)) # **Note: remove last obs when h >1 **
  y <- y[-c(1:p),]
  # specify where to start/end forecasting 
  # start<-which(index(y)=="1995-06-01") # last observation used for first forecast
  T1 <- floor(nrow(y)/3) # end of initialisation
  T2 <- 2*(floor(nrow(y)/3)) # end of lambda selection
  end<- nrow(y)
  # index tracker and placeholder
  # tTracker <- 0 # index tracker for t
  predErrLassoLambda <- numeric(length(lambdaChoises))
  for (t in T1:(T2-1)){ # penalty param selection
    # tTracker <- tTracker + 1
    fitLasso <- glmnet(x[1:t],y[1:t], lambda=lambdaChoises, family = "gaussian", alpha=1)
    predLasso <- predict.glmnet(fitLasso, coredata(x[t+1,]))
    predErrLassoLambda <- (predLasso - as.numeric(y[t+1,]))^2 + predErrLassoLambda
    minIdx <- which.min(predErrLassoLambda)
    optLam <- lambdaChoises[minIdx]
  }
  for (t in T2:(end-1)){ # forecast evaluation
    fitLasso <- glmnet(x[1:t], y[1:t], lambda=optLam,family = "gaussian", alpha=1)
    predLasso <- predict.glmnet(fitLasso, coredata(x[t+1,]))
    predErrLasso[p] <- (predLasso - as.numeric(y[t+1,]))^2 + predErrLasso[p]
    msfeLasso[p] <- predErrLasso[p]/(end-T2)
  }
  
  
  
######################################################################################
  
  # # variablesSelected[[pTracker]] <- matrix(NA, nrow=100, ncol=ncol(dat)*pTracker)
  # for (t in T2:end){ # second loop, through window 
  #   tTracker <- tTracker + 1 
  #   # select appropriate in-sample observation
  #   x_train <- coredata(x[1:t,]) # in-sample obs to fit the model
  #   y_train <- coredata(y[1:t,]) # Apparently, `cv.glmnet` doesnt accept xts object
  #   # cross validation to find optimal regularisation param (lambda)
  #   fit <- cv.glmnet(x_train, y_train, lambda = lambdaChoises, # Lag selection seems not right...
  #                    family="gaussian", alpha=1, nfolds=10) # Is cross-validation a good approach? (for time series data)
  #   cvScore <- fit$cvm
  #   optLam <- lambdaChoises[which.min(cvScore)]
  #   lambdaSelected[tTracker, pTracker] <- optLam # record selected lambda for each lag and window
  #   # fit the model using selected lambda and make a forecast 
  #   fitOpt <- glmnet(x_train, y_train, lambda = optLam, family = "gaussian", alpha=1)
  #   beta <- fitOpt$beta # coefficients to be used for forecast 
  # 
  #   
  #   
  #   y_pred <- predict.glmnet(fitOpt, coredata(x[t+1,]))
  #   predErrLasso[pTracker] <- as.numeric((y_pred - y[t+h,])^2) + predErrLasso[pTracker]
  #   # record which variables are used 
  #   fitBeta <- as.numeric(fitOpt$beta)
  #   fitBeta[fitBeta < 1e-7] <- 0
  #   fitBeta[fitBeta >= 1e-7] <- 1
  #   variablesSelected[[pTracker]][tTracker,] <- fitBeta
  # }
  # msfeLasso[pTracker] <- predErrLasso[pTracker] / (end-start-h)
  # variablesSelectedSum[[pTracker]] <- 
  #   variablesSelected[[pTracker]] %*% matrix(rep(diag(ncol(dat)), pTracker), ncol=ncol(dat), byrow=T)
  setTxtProgressBar(pb, p)
}
tictoc::toc() ## 7-8 mins
############################################################





# Group lasso -------------------------------------------------------------
library(grplasso)
# define groups
g1<- 18 # end index of group 1; Industrial production and shipment 
g2<- 29 # group 2; Capital Utilisation
g3<- 34 # group 3; Sales
g4<- 46 # group 4; emplyment
g5<- 64 # group 5; price index (slow)
g6<- 67 # group 6; trade 

g7<- 78 # group 7; construction & machinery
g8<- 86 # group 8; price index (fast)
g9<- 94 # group 9; stock market 
g10<- 121 # group 10; banks
g11<- 130 # group 11; inventory
g12<- 148 # group12; interest rate 




# for now, lets ignore lag selection (p=2). Later loop through lag(p)
# prepare appropriate dependent/explanatory variables 

lambdaChoises<- c(100,10,1,0.1,0.01,0.001)
# lambdaChoises <- 10^(seq(-2,0,len=100)) # preferred, but this takes foreever... (grplasso is slow)


cv <- matrix(0, nrow=length(lambdaChoises), ncol=lagMax) # placeholder for cross validation

pb <- txtProgressBar(0,lagMax, style=3)
tictoc::tic()
for (p in 1:lagMax){
  x <- lag.xts(dat, 1:p)
  x <- x[-c(1:p),] # trim x'NAs **Note: I have to remove last obs if h > 1 bc there's no counterpart obs for y**
  y <- lag.xts(dat[,51], -(h-1)) # **Note: remove last (h-1) obs's when h >1 **
  y <- y[-c(1:p),]
  T1 <- floor(nrow(y)/3) # end of initialisation
  T2 <- 2*(floor(nrow(y)/3)) # end of lambda selection
  end<- nrow(y)
  idx<- rep(c(rep(1,g1), rep(2,g2-g1), rep(3,g3-g2), rep(4,g4-g3), 
              rep(5,g5-g4), rep(6,g6-g5),rep(7,g7-g6), rep(8,g8-g7), 
              rep(9,g9-g8), rep(10,g10-g9), rep(11,g11-g10), rep(12,g12-g11)),p)
  pb1 <-txtProgressBar(T1-1,T2-1, style=3)
  for (t in T1:(T2-1)){ # penalty param / lag selection
    fitGLasso <- grplasso(x, y, idx, model=LinReg(),
                                lambda = lambdaChoises, center = F, standardize = F)
    predGLasso <- predict(fitGLasso, newdata=x[t+1,])
    cv[,p] <- (predGLasso-as.numeric(y[t+1,]))^2 + cv[,p]
    setTxtProgressBar(pb1,t)
  }
  cv[,p] <- cv[,p]/(T2-T1)
  setTxtProgressBar(pb, p)
}
lamIdx <- which(cv==min(cv), arr.ind = T)[1] # optimal lambda (idx)
optLam <- lambdaChoises[lamIdx]
optLag <- which(cv==min(cv), arr.ind = T)[2] # opt lag
tictoc::toc()

### evaluation
# Now we have selected optimal regularisation parameter and lag length based on cross valisation. 

x <- lag.xts(dat, 1:optLag)
x <- x[-c(1:optLag),] # trim x'NAs **Note: I have to remove last obs if h > 1 bc there's no counterpart obs for y**
y <- lag.xts(dat[,51], -(h-1)) # **Note: remove last (h-1) obs's when h >1 **
y <- y[-c(1:optLag),]
idx<- rep(c(rep(1,g1), rep(2,g2-g1), rep(3,g3-g2), rep(4,g4-g3), 
            rep(5,g5-g4), rep(6,g6-g5),rep(7,g7-g6), rep(8,g8-g7), 
            rep(9,g9-g8), rep(10,g10-g9), rep(11,g11-g10), rep(12,g12-g11)),optLag)


predErrGLasso <- 0 
pb<-txtProgressBar(T2-1, end-1, style=3)
for (t in T2:(end-1)){ # forecast evaluation
  fitGLasso <- grplasso(x, y, idx, model=LinReg(),
                        lambda = optLam, center = F, standardize = F)
  predGLasso <- predict(fitGLasso, newdata=x[t+1,])
  predErrGLasso <- as.numeric(predLasso - as.numeric(y[t+1,]))^2 + predErrGLasso
  setTxtProgressBar(pb,t)
}
msfeGLasso <- predErrGLasso/(end-T2) 


# 
# p<- 2 # lag, later loop through p as well
# x <- lag.xts(dat, 1:p)
# x <- x[-c(1:p), ] # trim NAs. **Note: remove last (h-1) obs's when h >1 **
# y <- lag.xts(dat[,51],-(h-1)) # **Note: remove last (h-1) obs's when h >1 **
# y <- y[-c(1:p),]
# 
# 
# predErrGLasso <- 0
# pb<- txtProgressBar((start-1), (end-h), style=3)
# tictoc::tic()
# for (t in start:(end-h)){
#   x_sample <- x[1:t,] # in-sample observations 
#   y_sample <- y[1:t,]
#   msfeGLasso_i <- numeric(length(lambdaChoises))
#   for (i in 1:length(lambdaChoises)){
#     lam <- lambdaChoises[i]
#     predErrGLasso_k <- numeric(5)
#     msfeGLasso_k<- numeric(5)
#     for (k in 1:5){ # k-fold cross validation to find optimal lambda, where k=5
#       trainIdx <- sample.split(index(x_sample), SplitRatio = .75) # train-test ratio: 75% and 25%
#       x_train <- x_sample[trainIdx==T,]
#       x_test <- x_sample[trainIdx==F,]
#       y_train <- y_sample[trainIdx==T,]
#       y_test <- y_sample[trainIdx==F,]
#       
#       fitGLasso <- grplasso(x=x_train, y=y_train, idx, model=LinReg(),
#                             lambda = lam, center = F, standardize = F)
#       
#       predGLasso <- predict(fitGLasso, newdata=x_test)
#       predErrGLasso_k[k] <- sum((predGLasso - y_test)^2)
#       msfeGLasso_k[k] <- predErrGLasso_k[k]/length(y_test)
#     }
#     msfeGLasso_i[i] <- mean(msfeGLasso_k)
#   }
#   optLam <- lambdaChoises[which.min(msfeGLasso_i)]
#   predGLasso <- predict(fitGLasso, newdata = x[t+1,])
#   predErrGLasso <- (predGLasso - y[t+1,])^2 + predErrGLasso
#   setTxtProgressBar(pb, t)
# }
# msfeGLasso <- predErrGLasso/(end-start-h)
# tictoc::toc()  ## 9hrs.. Probably I can remove CV to find lambda

# Adaptive lasso ----------------------------------------------------------




