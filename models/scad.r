# lasso with regularisation parameter selected by cv, rolling window. Lag = 4

# cross validation (b/w period T1 n T2)
y <- dat[, targetVar] %>% 
  set_colnames("y")
lambdaChoises <- 10^(seq(0.5,-3,len=100)) # lambda choices, selection on CV

X <- lag.xts(dat, 1:4+h-1)

predErr <-
  foreach(t = 1:winSize, .combine = "cbind", .inorder = F) %dopar% {
    fit <- ncvreg(coredata(X)[(4+h):T1+t-1,],as.numeric(y)[(4+h):T1+t-1], penalty = "SCAD", gamma=3.7,
                  lambda=lambdaChoises, family="gaussian", eps=1e-10, max.iter=1e09)# no option to exclude intercept ?
    pred <- predict(fit, coredata(X[T1+t,]))
    as.numeric((pred - as.numeric(y[T1+t]))^2)
  } # endforeach

cvScore <- apply(predErr,1,mean) # MSE for each lambda in cross validation period
optLam <- lambdaChoises[which.min(cvScore)]


SCADlambda[horizon,targetVar] <- optLam

# evaluatoin

eval <- foreach(t = 1:winSize) %dopar% { 
  fit <- ncvreg(coredata(X)[(T1+1):T2+t-1,], as.numeric(y)[(T1+1):T2+t-1], penalty = "SCAD", gamma=3.7,
                lambda = optLam, family = "gaussian", eps=1e-10, max.iter=1e09)
  pred <- predict(fit, zoo::coredata(X[T2+t,]))
  err <- as.numeric((pred - y[T2+t])^2)
  coefs <- fit$beta
  list(err, coefs)
}
predErr <- unlist(sapply(eval, function(foo) foo[1]))
coefTracker <- matrix(unlist(sapply(eval, function(foo) foo[2])),
                      nrow=winSize, ncol=ncol(X)+1, byrow=T)
coefTracker <- coefTracker[,-1] # we are not interested in intercept

coefTracker[coefTracker == 0] <- 0
coefTracker[coefTracker != 0] <- 1 # 1 if coef is selected (non-zero)


# interpretation


SCADsparsityRatio[horizon,targetVar] <- mean(coefTracker) # the ratio of non-zero coef
SCADnonzero[horizon,targetVar] <- sum(coefTracker)/winSize # avg nr of non-zero coef per window

# Notice that the final object `LASSOcoefs` is a list of lists (main list of variables and sub-list of horizons)
if (horizon == 1) {SCADcoefs[[var]] <- list()} # initialise by setting sub-list so that each main list contains sub-lists
SCADcoefs[[var]][[horizon]] <- coefTracker
if (horizon == 4) {names(SCADcoefs[[var]]) <- paste("h", hChoises, sep="")}

MSFEs[[horizon]]["SCAD", targetVar] <- mean(predErr)

rm(coefTracker,X, y, cvScore,lambdaChoises, optLam,predErr,eval)
