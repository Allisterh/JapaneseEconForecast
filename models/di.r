
# PCA ---------------------------------------------------------------------

if (var==1){
  optFacs <- numeric() # keep track of the number of (static) factors
  optFacsDyn <- numeric() # track nr of dyn factors
  factorR2 <- numeric() # retained variance of X when r factors are used
  optPs <- numeric() # keep track of the number of lags in DI forecast
  factorVARlags <- numeric()  # track number of lags in Factor VAR
}

for (t in 1:winSize){
  # estimate factors (We dont need to estimate factors everytime since factors are the same for each variable)
  if (var==1){ # Re-estimate factors only when x changes (i.e. when h changes)
    x <- dat[(T1+1):T2+(t-1),]
    eig <- eigen(x%*%t(x) / (nrow(x)*ncol(x)))
    vec <- eig$vectors
    N <- ncol(x)
    T <- nrow(x)
    # select nr of factors based on IC by Bai & Ng (2002)
    Rmax <- 20 # max nr of factors
    IC <- numeric(Rmax) # Placeholder of IC for each r (nr of factors)
    varianceRetained<- numeric(Rmax)
    for (r in Rmax:1){
      # eigVal <- eig$values[1:r]
      Fhat <- sqrt(T)*vec[,1:r] # estimated factor, T-by-r matrix
      Lhat <- t(Fhat)%*%x / T # estimated factor loading (transposed), r-by-N 
      reduc <- Fhat%*%Lhat # reduced data (like predicted value)
      loss <- sum(diag(t(x-reduc)%*%(x-reduc))) / (N*T) # information loss when low-dimensionalised (b/w 0-1)
      varianceRetained[r] <- 1- loss/((sum(diag(t(x)%*%x)))/(N*T)) # how much factors explain the variance (similar to R2)
      if (r==Rmax){minLoss <- loss} # define `loss` under maximum lag length considered (see Section 5, Bai & Ng, 2002)
      IC[r] <- loss + r*minLoss* (N+T)/(N*T) * log(min(c(N,T))) # IC that Bai&Ng refer to as PC_{p2}
    }
    optFac <- which.min(IC) # optimal number of factors
    optFacs[t] <- optFac # keep track of nr-of-factors 
    factorR2[t] <- varianceRetained[optFac]
    Fhat <- as.matrix(sqrt(T)*vec[,1:optFac]) %>% #`as.matrix` to provide names in case optFac=1
      set_colnames(paste("F",1:optFac, sep="")) # corresponding factor
    
    # Estimate dynamic factor (Amengual & Watson, 2007)
    Lhat <- t(Fhat)%*%x / T
    FhatVAR <- VAR(Fhat, lag.max = 2, ic="SC") # assume VAR structure in factor
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
    optFacsDyn[t] <- which.min(ICdyn)
  } # endif
  
  # forecasts
  predErrDI <- numeric()
  y <- lag.xts(dat[,targetVar], k=-h) %>% 
    set_colnames("y") 

  ## Diffusion Index: y ~ F + lag(y), lag selection by bic
  bic<-1e10
  for (p in 1:12){
    lagsCand <- lag.xts(dat[,targetVar], k=1:p-1) %>% 
      magrittr::extract((T1+1):T2+(t-1),) %>% 
      set_colnames(c(paste("lag",1:p,sep="")))
    XCand <- xts(cbind(Fhat,lagsCand),order.by = index(dat[(T1+1):T2+(t-1),]))
    datDICand <- na.omit(merge.xts(y,XCand)) # Xcand is always shorer. `na.omit` adjusts data period to `(T1+1):T2+(t-1)`
    fitDICand <- lm(y~.-1, data=datDICand) 
    bicCand <- -2*logLik(fitDICand) + log(nrow(XCand))*ncol(XCand)
    if (bicCand < bic){
      bic <- bicCand
      optPs[t]<- p
      lags <-lagsCand
      X <- XCand
      datDI <- datDICand
      fitDI <- fitDICand
    }
  }
  predDI <- predict(fitDI, newdata = X[nrow(X),])
  predErrDI[t] <- as.numeric((predDI-dat[T2+h+t-1,targetVar])^2)
}
msfeDI <- mean(predErrDI)

# save results
if (horizon==1){ # create placeholders inside each lists
  DIlags[[var]] <- matrix(NA, nrow=length(hChoises), ncol=winSize, 
                          dimnames = list(c(paste("h=",hChoises,sep=""))))
  DIfactor[[var]] <- matrix(NA, nrow=length(hChoises), ncol=winSize, 
                            dimnames = list(c(paste("h=",hChoises,sep=""))))
  DIfactorDyn[[var]] <- matrix(NA, nrow=length(hChoises), ncol=winSize, 
                               dimnames = list(c(paste("h=",hChoises,sep=""))))
  DIfactorR2[[var]] <- matrix(NA, nrow=length(hChoises), ncol=winSize, 
                               dimnames = list(c(paste("h=",hChoises,sep=""))))
}
DIlags[[var]][horizon,] <- optPs
DIfactor[[var]][horizon,] <- optFacs
DIfactorDyn[[var]][horizon,] <- optFacsDyn
DIfactorR2[[var]][horizon,] <- factorR2
MSFEs[[horizon]]["DI", targetVar] <- msfeDI

# # clear workspace
rm(datDI, datDICand, eig,FhatVAR, fitDI, fitDICand, gamma, lags, lagsCand,
   Lhat,reduc,reducDyn,vec,x,X,XCand,y, yHat,bic, bicCand, eta, factorVARlags, IC, ICdyn,
   loss, lossDyn,minLoss, minLossDyn, msfeDI, N, optFac, optPs,p,
   predDI, predErrDI, q,r,Rmax,t,T)
if (var==length(targetVariables)) rm(Fhat, optFacs, optFacsDyn)
