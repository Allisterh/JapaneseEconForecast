
# DILASSO ----------------------------------------------------------------------
## Diffusion index model using lasso to further reduce dimensions


# parameter selection -----------------------------------------------------
## first, we need to estimate factors for period b/w t=1 and T1.
# if (var==1){
#   datLag <- lag.xts(dat, h)
#   FactorCV <- 
#     foreach(t = 1:winSize) %dopar% {
#       x <- datLag[h:T1+t,] # to estimate factor structure
#       N <- ncol(x)
#       T <- nrow(x)
#       eig <- eigen(x%*%t(x) / (T*N))
#       vec <- eig$vectors
#       # select nr of factors based on IC by Bai & Ng (2002)
#       Rmax <- 20 # max nr of factors
#       IC <- numeric(Rmax) # Information criteria. Each element for different r's (nr of factors)
#       for (r in Rmax:1){
#         Fhat <- sqrt(T)*vec[,1:r] # estimated factor, T-by-r matrix
#         Lhat <- t(Fhat)%*%x / T # estimated factor loading (transposed), r-by-N 
#         reduc <- Fhat%*%Lhat # reduced data (like predicted value)
#         loss <- sum(diag(t(x-reduc)%*%(x-reduc))) / (N*T) # information loss when low-dimensionalised (b/w 0-1)
#         if (r==Rmax){minLoss <- loss} # define `loss` under maximum lag length considered (see Section 5, Bai & Ng, 2002)
#         IC[r] <- loss + r*minLoss* (N+T)/(N*T) * log(min(c(N,T))) # IC that Bai&Ng refer to as PC_{p2}
#       }
#       optFac <- which.min(IC) # optimal number of factors
#       Fhat <- as.matrix(sqrt(T)*vec[,1:optFac]) %>% #`as.matrix` to provide names in case optFac=1
#         set_colnames(paste("F",1:optFac, sep="")) 
#        xts(Fhat, order.by = index(x)) # corresponding factor
#     } # endforeach
# } # endif 
# 
# ## cross-validate lambda 
# lambdaChoises <- 10^seq(0,-2,len=100)
# yLag <- lag.xts(dat[,targetVar], 1:12+h-1) 
# cvScore <- 
#   foreach(t=1:winSize, .combine = "cbind", .inorder = F) %dopar% {
#     Fhat <- scale(FactorCV[[t]])
#     X <- na.omit(merge.xts(Fhat, yLag)[index(Fhat),]) 
#     Xtrain <- X[-nrow(X),]
#     fit <- glmnet(Xtrain, dat[index(Xtrain),targetVar], "gaussian", alpha=1, 
#                   lambda = lambdaChoises, standardize = F, intercept = T,
#                   thresh = 1e-15, maxit=1e07)
#     pred <- predict.glmnet(fit, coredata(X[nrow(X),]))
#     as.numeric(pred-as.numeric(dat[index(X[nrow(X),]), targetVar]))^2
#   }
# optLam <- lambdaChoises[which.min(apply(cvScore,1,sum))]
# DILASSOlambda[horizon, targetVar] <- optLam
# 
# # evaluate using optimum lambda selected by cv
# 
# # DIfactorList <- readRDS("results/DI/DIfactorList.rds")
# eval <- 
#   foreach(t=1:winSize) %dopar% {
#     Fhat <- scale(DIfactorList[[horizon]][[t]])
#     X <- merge.xts(yLag, Fhat)[index(Fhat),] 
#     Xtrain <- X[-nrow(X),]
#     fit <- glmnet(Xtrain, dat[index(Xtrain),targetVar], "gaussian", alpha=1, 
#                   lambda = optLam, standardize = F, intercept = T,
#                   thresh = 1e-15, maxit=1e07)
#     pred <- predict.glmnet(fit, coredata(X[nrow(X),]))
#     err <- as.numeric(pred-as.numeric(dat[index(X[nrow(X),]), targetVar]))^2
#     coef <- as.numeric(fit$beta)
#     list(err,coef)
#   }
# 
# predErr <- unlist(sapply(eval, function(foo) foo[1]))
# bar <- sapply(eval, function(foo) foo[2])
# coefTracker<- matrix(NA, nrow=winSize, ncol=12+20,
#                      dimnames=list(paste("Win", 1:winSize, sep=""),
#                                    c(paste("lag",1:12,sep=""),paste("F",1:20,sep=""))))
# for(i in 1:winSize){coefTracker[i,1:length(bar[[i]])]<-bar[[i]]}
# 
# coefTracker[coefTracker == 0] <- 0
# coefTracker[coefTracker != 0] <- 1 # 1 if coef is selected (non-zero)
# 
# # save results
# MSFEs[[horizon]]["DILASSO", targetVar] <- mean(predErr)
# DILASSOsparsityRatio[horizon,targetVar] <- mean(coefTracker,na.rm=T) # the ratio of non-zero coef
# DILASSOnonzero[horizon,targetVar] <- sum(coefTracker,na.rm=T)/winSize # avg nr of non-zero coef per window
# if (horizon == 1) {DILASSOcoefs[[var]] <- list()} # initialise by setting sub-list so that each main list contains sub-lists
# DILASSOcoefs[[var]][[horizon]] <- coefTracker
# if (horizon == 4) {names(DILASSOcoefs[[var]]) <- paste("h", hChoises, sep="")}
# 


# R2 ----------------------------------------------------------------------
# datLag <- lag.xts(dat, h)
# if (horizon==1) DILASSOr2[[var]] <- list()
# for (t in 1:winSize){
#   if (t==1) {DILASSOr2[[var]][[horizon]] <- matrix(NA, nrow=winSize, ncol=ncol(dat))}
#   DILASSOr2[[var]][[horizon]][t,] <-
#     foreach(i = 1:ncol(dat), .combine = "c") %dopar% {
#       FhatAll <- DIfactorList[[horizon]][[t]]
#       FhatSelect <- DILASSOcoefs[[var]][[horizon]][t,1:ncol(FhatAll)+12]
#       Fhat <- t(apply(FhatAll, 1, function(x) x*FhatSelect))
#       fit <- lm(datLag[T1:T2+t,i]~Fhat)
#       r2 <- summary(fit)$r.squared
#     }
# }
if (horizon==4) names(DILASSOr2[[var]]) <- c("h1", "h3", "h6", "h12")


# clear workspace
# if (var==1) rm(datLag)
# rm(bar, coefTracker,cvScore,eval, yLag,i,lambdaChoises, optLam,predErr,t)
# if (var==length(targetVariables)) rm(FactorCV)
