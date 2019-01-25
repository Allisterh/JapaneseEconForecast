# DI with number of factors selected like tuning param ------------------------


## In this model, we determine the number of factors based on h step ahead MSFE. 
## i.e., compare MSEs of second sub-period uding differing number of factors

# CV: first step (only for the first element in each horizon) -----------------
Rmax <- 20 # max nr of factors
if (var==1){ # Factors are identical for the same horizon
  datLag <- lag.xts(dat, h)
  DICVfactorCVlist[[horizon]]  <-
    foreach(t = 1:winSize) %dopar% {
      x <- datLag[h:T1+t,] # to estimate factor structure
      N <- ncol(x)
      T <- nrow(x)
      eig <- eigen(x%*%t(x) / (T*N))
      vec <- eig$vectors
      xts(sqrt(T)*vec[,1:Rmax], order.by = index(x))
    }
}

# CV: second step
y <- dat[, targetVar] %>%
  set_colnames("y")
cvScore <-
  foreach(t = 1:winSize, .combine = "cbind", .inorder = F) %dopar% {
    err <- numeric()
    for (r in 1:Rmax){
      Fhat <- DICVfactorCVlist[[horizon]][[t]][,1:r]
      bic<-1e10
      for (p in 1:12){
        lagsCand <- lag.xts(dat[,targetVar], 1:p+h-1) %>%
          set_colnames(c(paste("lag",1:p,sep="")))
        XCand <- merge.xts(Fhat,lagsCand)[index(Fhat),]
        datCand <- merge.xts(y,XCand)[index(Fhat)]
        fitCand <- lm(y~., data=datCand[-nrow(datCand),])
        bicCand <- -2*logLik(fitCand) + log(nrow(XCand)-1)*(ncol(XCand)+1) # -1 to exclude test set and +1 to include intercept
        if (bicCand < bic){
          bic <- bicCand
          optP<- p
          fit <- fitCand
          datDI <- datCand
        } # endif
      } # endfor, p
      pred <- predict(fit, newdata = datDI[nrow(datDI),-1])
      err[r] <- as.numeric((pred-datDI[nrow(datDI),1])^2)
    }
    err
  }

optFac <- which.min(apply(cvScore,1,sum))
DICVfactor[horizon, targetVar] <- optFac


# evaluation --------------------------------------------------------------

# first step (only for the first element in each horizon) -----------------
if (var==1){ # Factors are identical for the same horizon
  datLag <- lag.xts(dat, h)
  DICVfactorList[[horizon]] <- list()
  for (t in 1:winSize){
    x <- datLag[T1:T2+t,] # to estimate factor structure
    N <- ncol(x)
    T <- nrow(x)
    eig <- eigen(x%*%t(x) / (T*N))
    vec <- eig$vectors

    Fhat <- as.matrix(sqrt(T)*vec[,1:Rmax]) %>%
      set_colnames(paste("F",1:Rmax, sep=""))
    DICVfactorList[[horizon]][[t]] <- xts(Fhat, order.by = index(x))
  }
} # endif

# second step ----------------------------------------------------


eval <-
  foreach(t=1:winSize) %dopar% {
### fit model (select lag order i.t.o. BIC)
    optFac <- DICVfactor[horizon, targetVar]
    Fhat <- DICVfactorList[[horizon]][[t]][,1:optFac]
    bic<-1e10
    for (p in 1:12){
      lagsCand <- lag.xts(dat[,targetVar], 1:p+h-1) %>%
        set_colnames(c(paste("lag",1:p,sep="")))
      XCand <- merge.xts(Fhat,lagsCand)[index(Fhat),]
      datCand <- merge.xts(y,XCand)[index(Fhat)]
      fitCand <- lm(y~., data=datCand[-nrow(datCand),])
      bicCand <- -2*logLik(fitCand) + log(nrow(XCand)-1)*(ncol(XCand)+1)
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
  DICVlags[[var]] <- matrix(NA, nrow=length(hChoises), ncol=winSize,
                           dimnames = list(c(paste("h=",hChoises,sep=""))))
}
DICVlags[[var]][horizon,] <- unlist(sapply(eval, function(foo) foo[2]))
MSFEs[[horizon]]["DICV", targetVar] <- mean(predErr)


# R2 by factor ------------------------------------------------------------
datLag <- lag.xts(dat, h)
if (horizon==1) DICVr2[[var]] <- list() # create placeholde inside the list(DICVr2)
for (t in 1:winSize){
  if (t==1) {DICVr2[[var]][[horizon]] <- matrix(NA, nrow=winSize, ncol=ncol(dat))} # create another placeholder
  DICVr2[[var]][[horizon]][t,] <-
    foreach(i = 1:ncol(dat), .combine = "c") %dopar% {
      optFac <- DICVfactor[horizon, targetVar]
      Fhat <- DICVfactorList[[horizon]][[t]][,1:optFac]
      fit <- lm(datLag[T1:T2+t,i]~Fhat)
      r2 <- summary(fit)$r.squared
    }
}
if (horizon==3) names(DICVr2[[var]]) <- c("h1", "h3", "h12")

# clear workspace ---------------------------------------------------------
if (var==1) rm(datLag,eig,N,T,vec,Fhat,x)
rm(cvScore,eval,y,optFac,predErr,Rmax,t)
