# lasso with regularisation parameter selected by cv, rolling window. Lag = 12


# cross validation (b/w period T1 n T2)
y <- dat[, targetVar] %>% 
  set_colnames("y")
lambdaChoises <- 10^(seq(-3,0,len=100)) # lambda choices, selection on CV
# lambdaChoises<- c(100,10,1,0.1,0.01,0.001)
predErrLasso <- matrix(NA, nrow=length(lambdaChoises), ncol=winSize) 
X <- lag.xts(dat, 1:12+h-1)

for (t in 1:winSize){
  fitLasso <- glmnet(X[(12+h):T1+t-1,],y[(12+h):T1+t-1], # (p+h):T1 instead of 1:T1 bc first p obs's are missing
                     lambda=lambdaChoises, family="gaussian", alpha=1, standardize=F) # data is already standardised
  predLasso <- predict.glmnet(fitLasso, coredata(X[T1+t,]))
  predErrLasso[,t] <- as.numeric((predLasso - as.numeric(y[T1+t]))^2)
}
cvScore <- apply(predErrLasso,1,mean) # MSE for each lambda in cross validation period

optLam <- lambdaChoises[which.min(cvScore)]
LASSOlambda[horizon,targetVar] <- optLam

# evaluatoin
predErrLasso <- numeric()
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
LASSOsparsityRatio[horizon,targetVar] <- sum(coefTracker)/length(coefTracker) # the ratio of non-zero coef

# Notice that the final object `LASSOcoefs` is a list of lists (main list of variables and sub-list of horizons)
if (horizon == 1) {LASSOcoefs[[var]] <- list()} # initialise by setting sub-list so that each main list contains sub-lists
LASSOcoefs[[var]][[horizon]] <- coefTracker
if (horizon == 4) {names(LASSOcoefs[[var]]) <- paste("h", hChoises, sep="")}

MSFEs[[horizon]]["LASSO", targetVar] <- msfeLasso

rm(coefTracker, fitLasso, predLasso, X, y, cvScore,lambdaChoises,msfeLasso, optLam,predErrLasso,t)

