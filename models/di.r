
# DI ----------------------------------------------------------------------

## Two step approach using PCA. First step to estimate principal components, 
## and the second to make a forecast using pc estimated in the first step. 
## Notice that PCs are same irrespective of what variable to forecast

# first step (only for the first element in each horizon) -----------------
if (var==1){ # Factors are identical for the same horizon 
  datLag <- lag.xts(dat, h) # take h lags so we can forecast h ahead
  DIfactorList[[horizon]] <- list()
  DIeig[[horizon]] <- matrix(NA, nrow=T2-T1+1, ncol=winSize) # a matrix to store eigenvalues. each col contains eigenvalues for the specific window
  for (t in 1:winSize){
    x <- datLag[T1:T2+t,] # to estimate factor structure
    N <- ncol(x) # number of predictors
    T <- nrow(x) # nr of observetions
    eig <- eigen(x%*%t(x) / (T*N)) 
    vec <- eig$vectors
    # select nr of factors based on IC by Bai & Ng (2002)
    Rmax <- 20 # max nr of factors
    IC <- numeric(Rmax) # Information criteria. Each element for different r's (nr of factors)
    for (r in Rmax:1){
      Fhat <- sqrt(T)*vec[,1:r] # estimated factor, T-by-r matrix
      Lhat <- t(Fhat)%*%x / T # estimated factor loading (transposed), r-by-N 
      reduc <- Fhat%*%Lhat # reduced data (like predicted value)
      loss <- sum(diag(t(x-reduc)%*%(x-reduc))) / (N*T) # First term of rhs of Eq (16). Information loss when low-dimensionalised (b/w 0-1)
      if (r==Rmax){minLoss <- loss} # sigma^2 in Eq (16). `loss` under maximum lag length considered [rmax] (see Section 5, Bai & Ng, 2002)
      IC[r] <- loss + r*minLoss* (N+T)/(N*T) * log(min(c(N,T))) # Eq (16) or IC that Bai&Ng refer to as PC_{p2}
    } # endfor (factors)
    optFac <- which.min(IC) # optimal number of factors
    DIfactor[horizon,t] <- optFac # keep track of nr-of-factors 
    Fhat <- as.matrix(sqrt(T)*vec[,1:optFac]) %>% #`as.matrix` to provide names in case optFac=1
      set_colnames(paste("F",1:optFac, sep="")) # corresponding factor
    DIfactorList[[horizon]][[t]] <- xts(Fhat, order.by = index(x)) # Store estimated factor so that it can be used later for other varibales w/o the first step
    DIeig[[horizon]][,t] <- eig$values # store eigenvalues for ER/GR
  } # endfor (rolling)
} # endif (var==1)

# second step ----------------------------------------------------
y <- dat[, targetVar] %>% 
  set_colnames("y")  # X is already lagged, no need to take lead for y

## Diffusion Index: y ~ F + lag(y), lag selection by bic
### fit model
eval <-
  foreach(t=1:winSize) %dopar% {
    Fhat <- DIfactorList[[horizon]][[t]] # load `Fhat` estimated in the first step 
                                         # (No need to estimate factors every time b/c factors are smae for all the variable)
    bic<-1e10 
    for (p in 1:12){
      lagsCand <- lag.xts(dat[,targetVar], 1:p+h-1) %>% 
        set_colnames(c(paste("lag",1:p,sep="")))
      XCand <- merge.xts(Fhat,lagsCand)[index(Fhat),]
      datCand <- merge.xts(y,XCand)[index(Fhat)] 
      fitCand <- lm(y~., data=datCand[-nrow(datCand),]) # remove last row so it can be used for evaluation (to get prediction error)
      bicCand <- -2*logLik(fitCand) + log(nrow(XCand)-1)*(ncol(XCand)+1) # -1 to exclude the test set and +1 to include intercept
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
predErr <- unlist(sapply(eval, function(foo) foo[1])) # prediction errors

if (horizon==1){ # create placeholders inside each lists
  DIlags[[var]] <- matrix(NA, nrow=length(hChoises), ncol=winSize, 
                          dimnames = list(c(paste("h=",hChoises,sep=""))))
}
DIlags[[var]][horizon,] <- unlist(sapply(eval, function(foo) foo[2])) # keep track of lag orders of Eq (12)
MSFEs[[horizon]]["DI", targetVar] <- mean(predErr)

# clear workspace
if (var==1){rm(x,eig,vec,N,T ,Rmax,IC ,Lhat,reduc,loss,minLoss,optFac,t ,r,datLag,Fhat)}
rm(y,eval, predErr)
