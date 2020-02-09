### VAR(1) to for a strict comparison with our DI model
### see Section 3.2 for more explantions (paragraph starting with `strictly speaking...`)


# elastic net with parameters (lambda and alpha) selected by cv, rolling window (lag=4)


# cross validation (b/w period T1 n T2)
y <- dat[, targetVar] %>% 
  set_colnames("y")

lambdaChoises <- 10^(seq(2,-2,len=100)) # lambda choices (or use the optLam from LASSO to reduce time)

alphaChoises <- seq(0.01, 0.99, 0.02)
predErrEnet <- matrix(NA, nrow=length(lambdaChoises), ncol=winSize)
X <- lag.xts(dat, h)

# cv  ---------------------------------------------------------------------

cvScore <-  # nr-of-lambda x nr-of-alpha matrix containing MSEs for corresponding row and col
  foreach(a = 1:length(alphaChoises), .combine = "cbind", .inorder = F) %dopar% {
    for (t in 1:winSize){
      fitEnet <- glmnet(X[(1+h):T1+t-1,],y[(1+h):T1+t-1], # (p+h):T1 instead of 1:T1 bc first p obs's are missing
                        lambda=lambdaChoises, family="gaussian", alpha=alphaChoises[a],
                        standardize=F, intercept=T, thresh=1e-15, maxit=1e07)
      predEnet <- predict.glmnet(fitEnet, coredata(X[T1+t,]))
      predErrEnet[,t] <- as.numeric((predEnet - as.numeric(y[T1+t]))^2)
    }
    apply(predErrEnet,1,mean) # MSE for each lambda in cross validation period (with fixed alpha)
  }


opts <- which(cvScore==min(cvScore), arr.ind = T) # 2D-indices for optimal lambda[1] and alpha[2]
optLambda <- lambdaChoises[opts[1]]
optAlpha <- alphaChoises[opts[2]]

ENETlambda[horizon, targetVar] <- optLambda
ENETalpha[horizon, targetVar] <- optAlpha


# forecast evaluation -----------------------------------------------------
optLambda <- ENETlambda[horizon, targetVar]
optAlpha <- ENETalpha[horizon, targetVar] 
eval <- 
  foreach(t = 1:winSize) %dopar% { 
    fit <- glmnet(X[(T1+1):T2+t-1,], y[(T1+1):T2+t-1], lambda = optLambda,
                  family = "gaussian", alpha = optAlpha, standardize = F, intercept=T,
                  thresh=1e-15, maxit = 1e07)
    pred <- predict.glmnet(fit, coredata(X[T2+t,]))
    err <- as.numeric((pred - y[T2+t])^2)
    coefs <- as.numeric(fit$beta)
    list(err, coefs)
  }
predErr <- unlist(sapply(eval, function(foo) foo[1]))
coefTracker <- matrix(unlist(sapply(eval, function(foo) foo[2])),
                      nrow=winSize, ncol=ncol(X), byrow=T)


msfeEnet <- mean(predErr)

# interpretation
coefTracker[coefTracker == 0] <- 0
coefTracker[coefTracker != 0] <- 1 # 1 if param is selected (non-zero)
ENETnonzero[horizon,targetVar] <- sum(coefTracker)/winSize # number of non-zero param's
# Notice that the final object `Enetcoefs` is a list of lists (main list of variables and sub-list of horizons)
if (horizon == 1) {ENETcoefs[[var]] <- list()} # initialise by setting sub-list so that each main list contains sub-lists
ENETcoefs[[var]][[horizon]] <- coefTracker
if (horizon == 3) {names(ENETcoefs[[var]]) <- paste("h", hChoises, sep="")}

MSFE1[[horizon]]["ENET", targetVar] <- msfeEnet

rm(coefTracker, X, y, cvScore,alphaChoises, predErrEnet,optLambda,
   msfeEnet, optAlpha, lambdaChoises,eval, opts, predErr)
