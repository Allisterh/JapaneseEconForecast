
# DILASSO ----------------------------------------------------------------------
## Diffusion index model using lasso to further reduce dimensions
## After estimating twenty factors in the first step, all the twenty factors and twelve lags are included in the second step,
## within which the lasso selects factors and lags. 


# parameter selection -----------------------------------------------------
## First step to estimate factors up to rmax.
## Since its the same as dicv, we use `DICVfactorCVlist`


## cross-validate lambda
lambdaChoises <- 10^seq(0,-2,len=100)
yLag <- lag.xts(dat[,targetVar], 1:12+h-1)
cvScore <-
  foreach(t=1:winSize, .combine = "cbind", .inorder = F) %dopar% {
    Fhat <- scale(DICVfactorCVlist[[horizon]][[t]])
    X <- na.omit(merge.xts(Fhat, yLag)[index(Fhat),])
    Xtrain <- X[-nrow(X),]
    fit <- glmnet(Xtrain, dat[index(Xtrain),targetVar], "gaussian", alpha=1,
                  lambda = lambdaChoises, standardize = F, intercept = T,
                  thresh = 1e-15, maxit=1e07)
    pred <- predict.glmnet(fit, coredata(X[nrow(X),]))
    as.numeric(pred-as.numeric(dat[index(X[nrow(X),]), targetVar]))^2
  }
optLam <- lambdaChoises[which.min(apply(cvScore,1,sum))]
DILASSOlambda[horizon, targetVar] <- optLam

# evaluate using optimum lambda selected by cv

# DIfactorList <- readRDS("results/DI/DIfactorList.rds")
eval <-
  foreach(t=1:winSize) %dopar% {
    Fhat <- scale(DICVfactorList[[horizon]][[t]])
    X <- merge.xts(yLag, Fhat)[index(Fhat),]
    Xtrain <- X[-nrow(X),]
    fit <- glmnet(Xtrain, dat[index(Xtrain),targetVar], "gaussian", alpha=1,
                  lambda = optLam, standardize = F, intercept = T,
                  thresh = 1e-15, maxit=1e07)
    pred <- predict.glmnet(fit, coredata(X[nrow(X),]))
    err <- as.numeric(pred-as.numeric(dat[index(X[nrow(X),]), targetVar]))^2
    coef <- as.numeric(fit$beta)
    list(err,coef)
  }

predErr <- unlist(sapply(eval, function(foo) foo[1]))
bar <- sapply(eval, function(foo) foo[2])
coefTracker<- matrix(NA, nrow=winSize, ncol=12+20,
                     dimnames=list(paste("Win", 1:winSize, sep=""),
                                   c(paste("lag",1:12,sep=""),paste("F",1:20,sep=""))))
for(i in 1:winSize){coefTracker[i,1:length(bar[[i]])]<-bar[[i]]}

coefTracker[coefTracker == 0] <- 0
coefTracker[coefTracker != 0] <- 1 # 1 if coef is selected (non-zero)

# save results
MSFEs[[horizon]]["DILASSO", targetVar] <- mean(predErr)
DILASSOnonzero[horizon,targetVar] <- sum(coefTracker,na.rm=T)/winSize # avg nr of non-zero coef per window
if (horizon == 1) {DILASSOcoefs[[var]] <- list()} # initialise by setting sub-list so that each main list contains sub-lists
DILASSOcoefs[[var]][[horizon]] <- coefTracker
if (horizon == 3) {names(DILASSOcoefs[[var]]) <- paste("h", hChoises, sep="")}



# R2 ----------------------------------------------------------------------
datLag <- lag.xts(dat, h)
if (horizon==1) DILASSOr2[[var]] <- list()
for (t in 1:winSize){
  if (t==1) {DILASSOr2[[var]][[horizon]] <- matrix(NA, nrow=winSize, ncol=ncol(dat))}
  DILASSOr2[[var]][[horizon]][t,] <-
    foreach(i = 1:ncol(dat), .combine = "c") %dopar% {
      FhatAll <- DICVfactorList[[horizon]][[t]]
      FhatSelect <- DILASSOcoefs[[var]][[horizon]][t,13:32]
      Fhat <- t(apply(FhatAll, 1, function(x) x*FhatSelect))
      fit <- lm(datLag[T1:T2+t,i]~Fhat)
      r2 <- summary(fit)$r.squared
    }
}
if (horizon==3) names(DILASSOr2[[var]]) <- c("h1", "h3", "h12")


# clear workspace
rm(datLag, bar, coefTracker,cvScore,eval, yLag,i,lambdaChoises, optLam,predErr,t)
