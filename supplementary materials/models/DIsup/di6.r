
# DI ----------------------------------------------------------------------

## Two step approach using PCA. First step to estimate principal components, 
## and the second to make a forecast using pc estimated in the first step. 
## Notice that PCs are identical irrespective of what variable to forecast

# first step (only for the first element in each horizon) -----------------
if (var==1){ # Factors are identical for the same horizon 
  datLag <- lag.xts(dat, h)
  DI6factorList[[horizon]] <- list()
  for (t in 1:winSize){
    x <- datLag[T1:T2+t,] # to estimate factor structure
    N <- ncol(x)
    T <- nrow(x)
    eig <- eigen(x%*%t(x) / (T*N))
    vec <- eig$vectors
    
    optFac <- 6 # optimal number of factors
    
    Fhat <- as.matrix(sqrt(T)*vec[,1:optFac]) %>% #`as.matrix` to provide names in case optFac=1
      set_colnames(paste("F",1:optFac, sep="")) # corresponding factor
    DI6factorList[[horizon]][[t]] <- xts(Fhat, order.by = index(x))
  }
} # endif

# second step ----------------------------------------------------
y <- dat[, targetVar] %>% 
  set_colnames("y")

### fit model
eval <-
  foreach(t=1:winSize) %dopar% {
    Fhat <- DI6factorList[[horizon]][[t]]
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
  DI6lags[[var]] <- matrix(NA, nrow=length(hChoises), ncol=winSize, 
                           dimnames = list(c(paste("h=",hChoises,sep=""))))
}
DI6lags[[var]][horizon,] <- unlist(sapply(eval, function(foo) foo[2]))
MSFEs[[horizon]]["DI6", targetVar] <- mean(predErr)

# clear workspace
if (var==1){rm(x,eig,vec,N,T, optFac,t,datLag,Fhat)}
rm(y,eval, predErr)
