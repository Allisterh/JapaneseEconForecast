## lasso-augumented ridge
## Use ridge estimator using variables selected on lasso 

y <- dat[, targetVar] %>%
  set_colnames("y")
X <- lag.xts(dat, 1:4+h-1) # lag = 4

# evaluatoin
optLam <- LASSOlambda[horizon, targetVar]
coefs <- LASSOcoefs[[var]][[horizon]]

predErr <- foreach(t = 1:winSize, .combine = "c") %dopar% { 
  zeros <- which(coefs[t,]==0) # variables with zero coefficient
  if (length(zeros)==ncol(X)) { # just take avg if all coef is zero
    pred <- mean(y[(T1+1):T2+t-1])
  } else {
    fit <- glmnet::glmnet(X[(T1+1):T2+t-1,], y[(T1+1):T2+t-1], lambda = optLam,
                          family = "gaussian", alpha = 0, standardize = F, intercept=T,
                          thresh=1e-15, maxit = 1e07, exclude=zeros)
    pred <- glmnet::predict.glmnet(fit, zoo::coredata(X[T2+t,]))
  }
  err <- as.numeric((pred - y[T2+t])^2)
}



# Notice that the final object `LASSOcoefs` is a list of lists (main list of variables and sub-list of horizons)

MSFEs[[horizon]]["LARIDGE", targetVar] <- mean(predErr)

rm(X, y, coefs, optLam,predErr)