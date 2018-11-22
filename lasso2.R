# lasso with lag and parameter selected by cv, rolling window


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
  winSize <- T1
  T2 <- 2*(floor(nrow(y)/3)) # end of lambda selection
  end<- nrow(y)
  for (t in 1:winSize){ # penalty param / lag selection
    fitLasso <- glmnet(x[t:(T1+t)],y[t:(T1+t)], lambda=lambdaChoises, family = "gaussian", alpha=1)
    predLasso <- predict.glmnet(fitLasso, coredata(x[T1+t+1,]))
    cv[,p] <- (predLasso - as.numeric(y[T1+t+1,]))^2 + cv[,p]
    # minIdx <- which.min(predErrLassoLambda)
    # optLam <- lambdaChoises[minIdx]
  }
  cv[,p] <- cv[,p]/winSize # adjust the difference in length of cv period 
  setTxtProgressBar(pb, p)
}
lamIdx <- which(cv==min(cv), arr.ind = T)[1] # optimal lambda (idx)
optLam2 <- lambdaChoises[lamIdx]
optLag2 <- which(cv==min(cv), arr.ind = T)[2] # opt lag

# Now we have selected optimal regularisation parameter and lag length based on cross valisation. 

x <- lag.xts(dat, 1:optLag)
x <- x[-c(1:optLag),] # trim x'NAs **Note: I have to remove last obs if h > 1 bc there's no counterpart obs for y**
y <- lag.xts(dat[,51], -(h-1)) # **Note: remove last (h-1) obs's when h >1 **
y <- y[-c(1:optLag),]

predErrLasso <- 0 
for (t in 1:winSize){ # forecast evaluation
  fitLasso <- glmnet(x[(T1+t):(T2+t)], y[(T1+t):(T2+t)], lambda=optLam,family = "gaussian", alpha=1)
  predLasso <- predict.glmnet(fitLasso, coredata(x[T2+t+1,]))
  predErrLasso <- as.numeric(predLasso - as.numeric(y[T2+t+1,]))^2 + predErrLasso
}
msfeLasso2 <- predErrLasso/winSize


rm(cv,fitLasso, pb,predLasso,x,y,end,lagMax, lambdaChoises, lamIdx, p,predErrLasso,t,T1,T2, winSize)

