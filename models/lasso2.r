# lasso with regularisation parameter selected by cv, rolling window. Lag = 12
# lasso.r + nest AR + smaller penalty for autolag
## Lags of dependent variable up to the same length as AR is included in LASSO w/o penalty
## Lags of dep var not included in AR gets smaller penalty when included 

# cross validation (b/w period T1 n T2) -----------------------------------

y <- dat[, targetVar] %>% 
  set_colnames("y")
lambdaChoises <- 10^(seq(0,-3,len=100))

X <- lag.xts(dat,1:12+h-1)
pFac <- rep(1,ncol(X)) # penalty factor (control the effect of lambda)
pFac[grep(paste("\\b",targetVar, "\\b", sep=""),names(X))] <- 0.1 # penalise less if autolag


predErrLasso <- 
  foreach(t = 1:winSize, .combine = "cbind") %dopar% {
    lagAR <- ARlags[[var]][horizon,t] # get the result from AR
    pFac[grep(targetVar, names(X))[1:lagAR]] <- 0  # pen factor = 0 if used in AR (nest AR into LASSO)
    fitLasso <- glmnet::glmnet(X[(12+h):T1+t-1,],y[(12+h):T1+t-1], # (p+h):T1 instead of 1:T1 bc first p obs's are missing
                       lambda=lambdaChoises, family="gaussian", alpha=1, penalty.factor=pFac,
                       standardize=F, intercept=F, thresh=1e-15, maxit=1e07)
    predLasso <- glmnet::predict.glmnet(fitLasso, zoo::coredata(X[T1+t,]))
    as.numeric((predLasso - as.numeric(y[T1+t]))^2)
  }
cvScore <- apply(predErrLasso,1,mean) # MSE for each lambda in cross validation period

optLam <- lambdaChoises[which.min(cvScore)]
LASSO2lambda[horizon,targetVar] <- optLam


# forecast evaluation -----------------------------------------------------

eval <- foreach(t = 1:winSize) %dopar% { 
  lagAR <- ARlags[[var]][horizon,t]
  pFac[grep(targetVar, names(X))[1:lagAR]] <- 0 
  fitLasso <- glmnet::glmnet(X[(T1+1):T2+t-1,], y[(T1+1):T2+t-1], lambda = optLam,
                     family = "gaussian", alpha = 1, standardize = F, intercept=F,
                     thresh=1e-15, maxit = 1e+07, penalty.factor = pFac)
  predLasso <- glmnet::predict.glmnet(fitLasso, zoo::coredata(X[T2+t,]))
  err <- as.numeric((predLasso - y[T2+t])^2)
  coefs <- as.numeric(fitLasso$beta)
  list(err, coefs)
}
predErrLasso <- unlist(sapply(eval, function(foo) foo[1]))
coefTracker <- matrix(unlist(sapply(eval, function(foo) foo[2])),
                      nrow=winSize, ncol=ncol(X), byrow=T)

msfeLasso <- mean(predErrLasso)

# interpretation
coefTracker[abs(coefTracker) == 0] <- 0
coefTracker[abs(coefTracker) !=0 ] <- 1 # 1 if coef is selected (non-zero)
LASSO2sparsityRatio[horizon,targetVar] <- mean(coefTracker) # the ratio of non-zero coef
LASSO2nonzero[horizon,targetVar] <- sum(coefTracker)/winSize # average of nonzero coef in each window

# Notice that the final object `LASSOcoefs` is a list of lists (main list of variables and sub-list of horizons)
if (horizon == 1) {LASSO2coefs[[var]] <- list()} # initialise by setting sub-list so that each main list contains sub-lists
LASSO2coefs[[var]][[horizon]] <- coefTracker
if (horizon == 4) {names(LASSO2coefs[[var]]) <- paste("h", hChoises, sep="")}

MSFEs[[horizon]]["LASSO2", targetVar] <- msfeLasso

rm(coefTracker,X, y, cvScore,lambdaChoises,msfeLasso, optLam,predErrLasso,eval, pFac)

