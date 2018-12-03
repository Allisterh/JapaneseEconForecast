# lasso with regularisation parameter selected by cv, rolling window. Lag = 12

# cross validation (b/w period T1 n T2)
y <- dat[, targetVar] %>% 
  set_colnames("y")
lambdaChoises <- 10^(seq(0,-3,len=100)) # lambda choices, selection on CV
# lambdaChoises <- seq(0.5,0.01,-0.01)
# predErrLasso <- matrix(NA, nrow=length(lambdaChoises), ncol=winSize) 
X <- lag.xts(dat, 1:12+h-1)

# predErrLasso <- matrix(NA, ncol=winSize, nrow=100)
# lamUsed <- matrix(NA, ncol=winSize, nrow=100)
# for (t in 1:winSize){
#   fitLasso <- glmnet(X[(12+h):T1+t-1,],y[(12+h):T1+t-1], # (p+h):T1 instead of 1:T1 bc first p obs's are missing
#                      family="gaussian", alpha=1, standardize=F,
#                      thresh=1e-07, maxit=1e+07, nlambda=100) # data is already standardised
#   predLasso <- predict.glmnet(fitLasso, coredata(X[T1+t,]))
#   predErrLasso[1:length(fitLasso$lambda),t] <- as.numeric((predLasso - as.numeric(y[T1+t]))^2)
#   lamUsed[1:(length(fitLasso$lambda)),t] <- fitLasso$lambda
# }


predErrLasso <-
  foreach(t = 1:winSize, .combine = "cbind", .inorder = F) %dopar% {
    fitLasso <- glmnet(X[(12+h):T1+t-1,],y[(12+h):T1+t-1], # (p+h):T1 instead of 1:T1 bc first p obs's are missing
                       lambda=lambdaChoises, family="gaussian", alpha=1, standardize=F, intercept=F,
                       thresh=1e-15, maxit=1e07) # choose smaller thresh if nr of nonzero coef exceeds winSize
    predLasso <- predict.glmnet(fitLasso, coredata(X[T1+t,]))
    as.numeric((predLasso - as.numeric(y[T1+t]))^2)
    # fitLasso <- lars(X[(12+h):T1+t-1,],y[(12+h):T1+t-1], "lasso",
    #                  normalize = F, intercept = F, use.Gram = F)
    # predLasso <- predict.lars(foo, X[T1+t,])$fit
    # as.numeric(predLasso - as.numeric(y[T1+t]))^2
  } # endforeach

cvScore <- apply(predErrLasso,1,mean) # MSE for each lambda in cross validation period
# optLam <- mean(lamUsed[which.min(cvScore),])
optLam <- lambdaChoises[which.min(cvScore)]


LASSOlambda[horizon,targetVar] <- optLam

# evaluatoin

# predErrLasso <- numeric()
# coefTracker <- matrix(NA, nrow=winSize, ncol=ncol(X)) # keep track of whether coefficiet is zero or non-zer0

# for (t in 1:winSize){
#   fitLasso <- glmnet(X[(T1+1):T2+t-1,], y[(T1+1):T2+t-1], lambda = optLam,
#                      family = "gaussian", alpha = 1, standardize = F, 
#                      thresh=1e-15, maxit = 1e+07)
#   predLasso <- predict.glmnet(fitLasso, coredata(X[T2+t,]))
#   predErrLasso[t] <- as.numeric((predLasso - y[T2+t])^2)
#   coefTracker[t,] <- as.numeric(fitLasso$beta)
# }


eval <- foreach(t = 1:winSize) %dopar% { # forecast evaluation, 45 sec
  fitLasso <- glmnet(X[(T1+1):T2+t-1,], y[(T1+1):T2+t-1], lambda = optLam,
                      family = "gaussian", alpha = 1, standardize = F, intercept=F,
                      thresh=1e-15, maxit = 1e07)
  predLasso <- predict.glmnet(fitLasso, coredata(X[T2+t,]))
  err <- as.numeric((predLasso - y[T2+t])^2)
  coefs <- as.numeric(fitLasso$beta)
  list(err, coefs)
}
predErrLasso <- unlist(sapply(eval, function(foo) foo[1]))
coefTracker <- matrix(unlist(sapply(eval, function(foo) foo[2])),
                      nrow=winSize, ncol=ncol(X), byrow=T)

coefTracker[abs(coefTracker) < 1e-7] <- 0
coefTracker[abs(coefTracker) >=1e-7] <- 1 # 1 if coef is selected (non-zero)


msfeLasso <- mean(predErrLasso)

# interpretation


# coefTracker[abs(coefTracker) < 1e-7] <- 0
# coefTracker[abs(coefTracker) >=1e-7] <- 1 # 1 if coef is selected (non-zero)


LASSOsparsityRatio[horizon,targetVar] <- mean(coefTracker) # the ratio of non-zero coef

# Notice that the final object `LASSOcoefs` is a list of lists (main list of variables and sub-list of horizons)
if (horizon == 1) {LASSOcoefs[[var]] <- list()} # initialise by setting sub-list so that each main list contains sub-lists
LASSOcoefs[[var]][[horizon]] <- coefTracker
if (horizon == 4) {names(LASSOcoefs[[var]]) <- paste("h", hChoises, sep="")}

MSFEs[[horizon]]["LASSO", targetVar] <- msfeLasso

rm(coefTracker,X, y, cvScore,lambdaChoises,msfeLasso, optLam,predErrLasso)
# rm(fitLasso, predLasso,t)
