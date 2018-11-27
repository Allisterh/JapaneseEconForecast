# lasso with regularisation parameter selected by cv, rolling window. Lag = 12
# lasso.r + smaller penalty on auto-lag

# cross validation (b/w period T1 n T2)
y <- dat[, targetVar] %>% 
  set_colnames("y")

lambdaChoises <- 10^(seq(-3,0,len=100)) # lambda choices, selection on CV


x <- dat
x[,targetVar] <- 100*x[,targetVar] # multiply target variable by 100, so coef gets 1/100 times smaller hence smaller penalty
X <- lag.xts(x, 1:12+h-1)

predErrLasso <- 
  foreach(t = 1:winSize, .combine = "cbind") %dopar% {
    fitLasso <- glmnet(X[(12+h):T1+t-1,],y[(12+h):T1+t-1], # (p+h):T1 instead of 1:T1 bc first p obs's are missing
                       lambda=lambdaChoises, family="gaussian", alpha=1,standardize=F)
    predLasso <- predict.glmnet(fitLasso, coredata(X[T1+t,]))
    as.numeric((predLasso - as.numeric(y[T1+t]))^2)
  }
cvScore <- apply(predErrLasso,1,mean) # MSE for each lambda in cross validation period

optLam <- lambdaChoises[which.min(cvScore)]
LASSO2lambda[horizon,targetVar] <- optLam

# evaluatoin
predErrLasso <- numeric()
X <- lag.xts(dat, 1:12+h-1)
coefTracker <- matrix(NA, nrow=winSize, ncol=ncol(X)) # keep track of whether coefficiet is zero or non-zer0
for (t in 1:winSize){
  fitLasso <- glmnet(X[(T1+1):T2+t-1,], y[(T1+1):T2+t-1], lambda = optLam,
                     family = "gaussian", alpha = 1, standardize = F)
  predLasso <- predict.glmnet(fitLasso, coredata(X[T2+t,]))
  predErrLasso[t] <- as.numeric((predLasso - y[T2+t])^2)
  coefTracker[t,] <- as.numeric(fitLasso$beta)
}
msfeLasso <- mean(predErrLasso)

# interpretation
coefTracker[coefTracker < 1e-7] <- 0
coefTracker[coefTracker >=1e-7] <- 1 # 1 if coef is selected (non-zero)
LASSO2sparsityRatio[horizon,targetVar] <- sum(coefTracker)/length(coefTracker) # the ratio of non-zero coef

# Notice that the final object `LASSOcoefs` is a list of lists (main list of variables and sub-list of horizons)
if (horizon == 1) {LASSO2coefs[[var]] <- list()} # initialise by setting sub-list so that each main list contains sub-lists
LASSO2coefs[[var]][[horizon]] <- coefTracker
if (horizon == 4) {names(LASSO2coefs[[var]]) <- paste("h", hChoises, sep="")}

MSFEs[[horizon]]["LASSO2", targetVar] <- msfeLasso

rm(coefTracker, fitLasso, predLasso, X, y, cvScore,lambdaChoises,msfeLasso, optLam,predErrLasso,t)

