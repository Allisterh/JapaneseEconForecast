# # lasso with regularisation parameter selected by cv, rolling window. Lag = 4
# 
# # cross validation (b/w period T1 n T2)
y <- dat[, targetVar] %>%
  set_colnames("y")
lambdaChoises <- 10^(seq(0.5,-3,len=100)) # lambda choices, selection on CV

X <- lag.xts(dat, 1:4+h-1) # lag=4

predErr <-
  foreach(t = 1:winSize, .combine = "cbind", .inorder = F) %dopar% {
    fit <- glmnet(X[(4+h):T1+t-1,],y[(4+h):T1+t-1], # (p+h):T1 instead of 1:T1 bc first p obs's are missing
                               lambda=lambdaChoises, family="gaussian", alpha=1, standardize=F, intercept=T,
                               thresh=1e-15, maxit=1e07) # choose smaller thresh if nr of nonzero coef exceeds winSize
    pred <- predict.glmnet(fit, coredata(X[T1+t,]))
    as.numeric((pred - as.numeric(y[T1+t]))^2)
  } # endforeach

cvScore <- apply(predErr,1,mean) # MSE for each lambda in cross validation period
optLam <- lambdaChoises[which.min(cvScore)]


LASSOlambda[horizon,targetVar] <- optLam

# evaluatoin

eval <- foreach(t = 1:winSize) %dopar% { 
  fit <- glmnet(X[(T1+1):T2+t-1,], y[(T1+1):T2+t-1], lambda = optLam,
                             family = "gaussian", alpha = 1, standardize = F, intercept=T,
                             thresh=1e-15, maxit = 1e07)
  pred <- predict.glmnet(fit, coredata(X[T2+t,]))
  err <- as.numeric((pred - y[T2+t])^2)
  coefs <- as.numeric(fit$beta)
  list(err, coefs)
}
predErr <- unlist(sapply(eval, function(foo) foo[1]))
coefTracker <- matrix(unlist(sapply(eval, function(foo) foo[2])),
                      nrow=winSize, ncol=ncol(X), byrow=T)

coefTracker[coefTracker == 0] <- 0
coefTracker[coefTracker != 0] <- 1 # 1 if param is selected (non-zero)

msfeLasso <- mean(predErr)

# interpretation
LASSOnonzero[horizon,targetVar] <- sum(coefTracker)/winSize # avg nr of non-zero coef per window

# Notice that the final object `LASSOcoefs` is a list of lists (main list of variables and sub-list of horizons)
if (horizon == 1) {LASSOcoefs[[var]] <- list()} # initialise by setting sub-list so that each main list contains sub-lists
LASSOcoefs[[var]][[horizon]] <- coefTracker
if (horizon == 3) {names(LASSOcoefs[[var]]) <- paste("h", hChoises, sep="")}

MSFEs[[horizon]]["LASSO", targetVar] <- msfeLasso

rm(coefTracker,X, y, cvScore,lambdaChoises,msfeLasso, optLam,predErr,eval)
