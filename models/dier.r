# Eigenvalue ration method (Ahn and Horestein, 2013)
# We use eigenvalues stored in calculation for DIBN ("models/di.r")

DIeig <- readRDS("results/DI/DIeig.rds")
er <- lapply(DIeig, function(x){x[-nrow(x),]/x[-1,]}) # ER estimator
DIERfactor <- t(sapply(er, function(x) apply(x, 2, which.max))) # number of factors chosen by ER

# second equation (see `di.r` for detail) ------------------------------
# Here, we use `DIfactorList` estimated in `di.r` This object contains facotrs up to the number of factors used in DIBN, 
# but we can still use it since nr of factors happens to smaller for all horizons and variables in ER
y <- dat[, targetVar] %>% 
  set_colnames("y")  # X is already lagged, no need to take lead for y

## Diffusion Index: y ~ F + lag(y), lag selection by bic
### fit model
eval <-
  foreach(t=1:winSize) %dopar% {
    nf <- DIERfactor[horizon, t]
    Fhat <- DIfactorList[[horizon]][[t]][,1:nf]
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
  DIERlags[[var]] <- matrix(NA, nrow=length(hChoises), ncol=winSize, 
                          dimnames = list(c(paste("h=",hChoises,sep=""))))
}
DIERlags[[var]][horizon,] <- unlist(sapply(eval, function(foo) foo[2])) # keep track of lag orders of Eq (12)
MSFEs[[horizon]]["DIER", targetVar] <- mean(predErr)

# clear workspace
rm(y,eval, predErr)

saveRDS(DIERlags, "results/DIER/DIERlags.rds")
saveRDS(DIERfactor,"results/DIER/DIERfactor.rds")
saveRDS(er,"results/DIER/er.rds")
