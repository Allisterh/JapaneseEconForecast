########################################################################
### This file examines the forecasting performances of DI model with lagged factors 
### and no lags of the target variable to address reviewer's suggestion. 
###The result appears to show that including lagged factors cause 
### overfitting, hence omitted from the final manuscript
########################################################################


# Initial Setting (Number of factors and lags) ----------------------------
NrFactor <- 4
Lag <- 4 # Lag set to be four to align VAR 
outputName <- "F4"

# first step (only for the first element in each horizon) -----------------
if (var==1){ # Factors are identical for the same horizon 
  datLag <- lag.xts(dat, h) # take h lags so we can forecast h ahead
  DIfactorLag[[horizon]] <- list()
  for (t in 1:winSize){
    x <- datLag[T1:T2+t,] # to estimate factor structure
    N <- ncol(x) # number of predictors
    T <- nrow(x) # nr of observetions
    eig <- eigen(x%*%t(x) / (T*N)) 
    vec <- eig$vectors
    Fhat <- as.matrix(sqrt(T)*vec[,1:NrFactor]) %>% #`as.matrix` to provide names in case optFac=1
      set_colnames(paste("F",1:NrFactor, sep="")) # corresponding factor
    DIfactorLag[[horizon]][[t]] <- lag.xts(xts(Fhat, order.by = index(x)),0:(Lag-1)) # Store estimated factor so that it can be used later for other varibales w/o the first step
  } # endfor (rolling)
} # endif (var==1)

# second step ----------------------------------------------------
y <- dat[, targetVar] %>% 
  set_colnames("y")  # X is already lagged, no need to take lead for y

## Diffusion Index: y ~ F + F's lag (without lags of the target variable)
### fit model
err <-
  foreach(t=1:winSize, .combine = "cbind", .inorder = F) %dopar% {
    Fhat <- DIfactorLag[[horizon]][[t]]
    datDI <- merge.xts(y,Fhat)[index(Fhat[complete.cases(Fhat),]),]
    fit <- lm(y~., data=datDI[-nrow(datDI),])
    pred <- predict(fit, newdata = datDI[nrow(datDI),-1]) 
    err <- as.numeric((pred-datDI[nrow(datDI),1])^2)
  }

MSFE_DIlag[[horizon]][outputName, targetVar] <- mean(err)

# clear workspace
if (var==1){rm(datLag,eig, Fhat, vec, x,N, NrFactor,outputName,t, Lag,T)}
rm(y,err)
