# elastic net with parameters (lambda and alpha) selected by cv, rolling window


# cross validation (b/w period T1 n T2)
y <- dat[, targetVar] %>% 
  set_colnames("y")

lambdaChoises <- 10^(seq(-3,0,len=100)) # lambda choices (or use the optLam from LASSO to reduce time)
# lambdaChoises <- LASSOlambda[horizon,var]
alphaChoises <- seq(0.01, 0.99, 0.02) 
predErrEnet <- matrix(NA, nrow=length(lambdaChoises), ncol=winSize)
# predErrEnet <- numeric() # in case we use the same lambda as LASSO

tictoc::tic()
cvScore <-  # nr-of-lambda x nr-of-alpha
  foreach(a = 1:length(alphaChoises), .combine = "cbind", .inorder = F) %dopar% {
    for (t in 1:winSize){
      X <- lag.xts(dat, 1:12+h-1)
      fitEnet <- glmnet(X[(12+h):T1+t-1,],y[(12+h):T1+t-1], # (p+h):T1 instead of 1:T1 bc first p obs's are missing
                         lambda=lambdaChoises, family="gaussian", alpha=alphaChoises[a])
      predEnet <- predict.glmnet(fitEnet, coredata(X[T1+t,]))
      predErrEnet[,t] <- as.numeric((predEnet - as.numeric(y[T1+t]))^2)
  }
  # mean(predErrEnet) # in case we use the same lambda as LASSO
  apply(predErrEnet,1,mean) # MSE for each lambda in cross validation period (with fixed alpha)
}
tictoc::toc()
 
opts <- which(cvScore==min(cvScore), arr.ind = T) # 2D-indices for optimal lambda[1] and alpha[2]
optLambda <- lambdaChoises[opts[1]]
optAlpha <- alphaChoises[opts[2]]

ENETlambda[horizon, targetVar] <- optLambda
ENETalpha[horizon, targetVar] <- optAlpha

# Enetlambda[horizon,targetVar] <- optLam

# evaluatoin
predErrEnet <- numeric()
X <- lag.xts(dat, 1:12+h-1)
coefTracker <- matrix(NA, nrow=winSize, ncol=ncol(X)) # keep track of whether coefficiet is zero or non-zer0
for (t in 1:winSize){
  fitEnet <- glmnet(X[(T1+1):T2+t-1,], y[(T1+1):T2+t-1], 
                    lambda = LASSOlambda[horizon,var], family = "gaussian", alpha = optAlpha)
  predEnet <- predict.glmnet(fitEnet, coredata(X[T2+t,]))
  predErrEnet[t] <- as.numeric((predEnet - y[T2+t])^2)
  coefTracker[t,] <- as.numeric(fitEnet$beta)
}
msfeEnet <- mean(predErrEnet)

# interpretation
coefTracker[abs(coefTracker) < 1e-7] <- 0
coefTracker[abs(coefTracker) >=1e-7] <- 1 # 1 if coef is selected (non-zero)
ENETsparsityRatio[horizon,targetVar] <- mean(coefTracker) # the ratio of non-zero coef

# Notice that the final object `Enetcoefs` is a list of lists (main list of variables and sub-list of horizons)
if (horizon == 1) {ENETcoefs[[var]] <- list()} # initialise by setting sub-list so that each main list contains sub-lists
ENETcoefs[[var]][[horizon]] <- coefTracker
if (horizon == 4) {names(ENETcoefs[[var]]) <- paste("h", hChoises, sep="")}

MSFEs[[horizon]]["ENET", targetVar] <- msfeEnet

rm(coefTracker, fitEnet, predEnet, X, y, cvScore,alphaChoises,
   msfeEnet, optAlpha,predErrEnet,t,opts, lambdaChoises)

