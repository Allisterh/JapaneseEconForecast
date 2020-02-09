########################################################################
### This file examines the forecasting performances of the lasso in which
### standard k-fold cross varidation is used to choose the regularisation 
### parameter (lambda) instead of rolling window scheme to address reviewer's suggestion. 
### The result is not included in the manuscript since it shows no significant improvement 
### and the authors think that k-fold cross varidation may not be appropriate since it is 
### subjected to data leakage, i.e. the model utilises the information unavailable at the time it makes a prediction 
### (for instance, using data from 2017 and 2019 as a training set to predict 2018 is unappropriate)
########################################################################


# k-fold cross validation (time series structure ignored) -----------------
k = 10 # nr of cross validation
y <- dat[, targetVar] %>%
  set_colnames("y")
lambdaChoises <- 10^(seq(0.5,-3,len=100)) # lambda choices, selection on CV
X <- lag.xts(dat, 1:4+h-1) # lag=4

y_tv <- y[(h+4):T2,] # `tv` set contains both training and validation set
X_tv <- X[(h+4):T2,] # remove first h obs's because its missing 

set.seed(146)

err <- 
  foreach(i = 1:k, .combine = "rbind", .inorder = F) %dopar% {
    idx <- caTools::sample.split(y_tv, SplitRatio = .8)
    fit <- glmnet(subset(X_tv, idx==T),subset(y_tv,idx==T),lambda=lambdaChoises, 
                  family="gaussian", alpha=1, standardize=F, intercept=T, thresh=1e-15, maxit=1e07)
    pred <- predict.glmnet(fit, coredata(subset(X_tv, idx==F)))
    diff <- (as.numeric(subset(y_tv, idx==F)) - pred)^2
    apply(diff,2,mean)
  }

cvScore <- apply(err,2,mean) 
optLam <- lambdaChoises[which.min(cvScore)]

LASSOkfcvLambda[horizon,targetVar] <- optLam


# evaluatoin
eval <- foreach(t = 1:winSize, .combine = "rbind", .inorder = F) %dopar% { 
  fit <- glmnet(X[(T1+1):T2+t-1,], y[(T1+1):T2+t-1], lambda = optLam,
                family = "gaussian", alpha = 1, standardize = F, intercept=T,
                thresh=1e-15, maxit = 1e07)
  pred <- predict.glmnet(fit, coredata(X[T2+t,]))
  as.numeric((pred - y[T2+t])^2)
}

MSFE_DIlag[[horizon]]["CV", targetVar] <- mean(eval)

rm(k,lambdaChoises,X,y,X_tv,y_tv,err,cvScore,optLam,eval)
