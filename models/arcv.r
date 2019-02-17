# AR with lags selected by BIC, rolling window


datAR <- dat[,targetVar]
err <- numeric()

# cross validation (lag selection based on MSE)
cv <- 
  foreach(p = 1:12, .combine = "rbind", .inorder = F) %dopar% {
    X <- lag.xts(datAR, 1:p+h-1)
    for (t in 1:winSize){
      fit <- lm(datAR[(h+p-1):(T1-1)+t]~X[(h+p-1):(T1-1)+t])
      pred <- sum(fit$coef * c(1, X[T1+t]))
      err[t] <- as.numeric((datAR[T1+t]-pred)^2)
    }
    err
  }

optLag <- which.min(apply(cv, 1, mean))
ARCVlags[horizon, var] <- optLag

# evaluation
X <- lag.xts(datAR, 1:optLag+h-1)

err <- 
  foreach(t = 1:winSize, .combine="c") %dopar% {
    fit <- lm(datAR[T1:(T2-1)+t]~X[T1:(T2-1)+t])
    pred <- sum(fit$coef * c(1,X[T2+t]))
    as.numeric((datAR[T2+t]-pred)^2)
  }

MSFEs[[horizon]]["ARCV",targetVar] <- mean(err)

# clear workspace
rm(cv,datAR,X,err,optLag)
