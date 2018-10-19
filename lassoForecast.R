
# prep --------------------------------------------------------------------

library(xts)
rm(list=ls())
dat<- readxl::read_excel("data_JER.xls")
dat<- xts(dat[-1], order.by = dat$date)*100 # change to percent change 
dat[,c(46,131:148)] <- dat[,c(46, 131:148)]/100 # transform back, for series recorded in levels


h <- 1 

# univariate (baseline) ---------------------------------------------------

start <- 2*(floor(nrow(dat)/3)) # evaluate the best lambda 
# start<-which(index(dat)=="1995-06-01") # last observation for first forecast
end<- nrow(dat) - h # last observation for the last forecast (2001-02-01)
predErrAR<- 0
for (t in start:end){
  fitAR<- ar(dat[51:t,], method="ols")
  predAR <- predict(fitAR, n.ahead=h)$pred[h]
  predErrAR <- (predAR - as.numeric(dat[t+h,51]))^2 + predErrAR
}
msfeAR <- predErrAR/(end-start)


# VAR(3) ------------------------------------------------------------------

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


