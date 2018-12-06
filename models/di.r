
# DI ----------------------------------------------------------------------

## Two step approach using PCA. First step to estimate principal components, 
## and the second to make a forecast using pc estimated in the first step. 
## Notice that PCs are identical irrespective of what variable to forecast

# first step (only for the first element in each horizon) -----------------
if (var==1){ # Factors are identical for the same horizon 
  factorVARlags <- numeric()  # track number of lags in Factor VAR
  datLag <- lag.xts(dat, h)
  DIfactorList[[horizon]] <- list()
  for (t in 1:winSize){
    x <- datLag[T1:T2+t,] # to estimate factor structure
    N <- ncol(x)
    T <- nrow(x)
    eig <- eigen(x%*%t(x) / (T*N))
    vec <- eig$vectors
    # select nr of factors based on IC by Bai & Ng (2002)
    Rmax <- 20 # max nr of factors
    IC <- numeric(Rmax) # Information criteria. Each element for different r's (nr of factors)
    varianceRetained <- numeric(Rmax)
    for (r in Rmax:1){
      Fhat <- sqrt(T)*vec[,1:r] # estimated factor, T-by-r matrix
      Lhat <- t(Fhat)%*%x / T # estimated factor loading (transposed), r-by-N 
      reduc <- Fhat%*%Lhat # reduced data (like predicted value)
      loss <- sum(diag(t(x-reduc)%*%(x-reduc))) / (N*T) # information loss when low-dimensionalised (b/w 0-1)
      varianceRetained[r] <- 1- loss/((sum(diag(t(x)%*%x)))/(N*T)) # Propotion of variance explained by factors (similar to R2)
      if (r==Rmax){minLoss <- loss} # define `loss` under maximum lag length considered (see Section 5, Bai & Ng, 2002)
      IC[r] <- loss + r*minLoss* (N+T)/(N*T) * log(min(c(N,T))) # IC that Bai&Ng refer to as PC_{p2}
    }
    optFac <- which.min(IC) # optimal number of factors
    DIfactor[horizon,t] <- optFac # keep track of nr-of-factors 
    DIfactorR2[horizon,t] <- varianceRetained[optFac]
    Fhat <- as.matrix(sqrt(T)*vec[,1:optFac]) %>% #`as.matrix` to provide names in case optFac=1
      set_colnames(paste("F",1:optFac, sep="")) # corresponding factor
    DIfactorList[[horizon]][[t]] <- xts(Fhat, order.by = index(x))
    # Estimate dynamic factor (Amengual & Watson, 2007)
    Lhat <- t(Fhat)%*%x / T
    FhatVAR <- vars::VAR(Fhat, lag.max = 2, ic="SC") # assume VAR structure in factor
    factorVARlags[t] <- FhatVAR$p  
    yHat <- x[-(1:factorVARlags[t]),] - fitted(FhatVAR) %*% Lhat # remove first p obs of `Fhat` bc no `fitted(FhatVAR)`
    T <- nrow(yHat)
    ICdyn <- numeric(IC[r])
    for (q in optFac:1) {
      eta <- sqrt(T) * eigen(yHat%*%t(yHat)/(N*T))$vectors[,1:q]
      gamma <- t(eta)%*%yHat / T
      reducDyn <- eta %*% gamma
      lossDyn <- sum(diag(t(yHat-reducDyn)%*%(yHat-reducDyn))) / (N*T)
      if (q==optFac) {minLossDyn <- lossDyn}
      ICdyn[q] <- lossDyn + q*minLossDyn * ((N+T)/(N*T)) * log(min(c(N,T)))
    }  
    DIfactorDyn[horizon,t] <- which.min(ICdyn)
  }
} # endif

# second step ----------------------------------------------------
y <- dat[, targetVar] %>% 
  set_colnames("y")

## Diffusion Index: y ~ F + lag(y), lag selection by bic
### fit model
eval <-
  foreach(t=1:winSize) %dopar% {
    Fhat <- DIfactorList[[horizon]][[t]]
    bic<-1e10
    for (p in 1:12){
      lagsCand <- lag.xts(dat[,targetVar], 1:p+h-1) %>% 
        set_colnames(c(paste("lag",1:p,sep="")))
      XCand <- merge.xts(Fhat,lagsCand)[index(Fhat),]
      datCand <- merge.xts(y,XCand)[index(Fhat)] 
      fitCand <- lm(y~.-1, data=datCand[-nrow(datCand),]) 
      bicCand <- -2*logLik(fitCand) + log(nrow(XCand)-1)*ncol(XCand)
      if (bicCand < bic){
        bic <- bicCand
        optP<- p
        fit <- fitCand
        datDI <- datCand
      }
    }
    ### forecast
    pred <- predict(fit, newdata = datDI[nrow(datDI),-1])
    err <- as.numeric((pred-datDI[nrow(datDI),1])^2)
    list(err, optP)
  }
predErr <- unlist(sapply(eval, function(foo) foo[1]))
if (horizon==1){ # create placeholders inside each lists
  DIlags[[var]] <- matrix(NA, nrow=length(hChoises), ncol=winSize, 
                          dimnames = list(c(paste("h=",hChoises,sep=""))))
}
DIlags[[var]][horizon,] <- unlist(sapply(eval, function(foo) foo[2]))
MSFEs[[horizon]]["DI", targetVar] <- mean(predErr)

# clear workspace
if (var==1){rm(x,eig,vec,N,T ,Rmax,IC ,varianceRetained, Lhat,reduc,loss,minLoss,optFac,t,
               FhatVAR,factorVARlags,yHat,ICdyn,q,eta,gamma,reducDyn,lossDyn,minLossDyn,r,datLag,Fhat)}
rm(y,eval, predErr)
