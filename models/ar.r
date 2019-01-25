# AR with lags selected by BIC, rolling window


predErrAR<- numeric()
lagLenAR<- numeric() # track lag length for each window
datAR <- dat[,targetVar]

# bic for lag selection
for (t in 1:winSize){ # rolling window
  # select optimal lag length based on bic
  bic<- 1e10 # Initial BIC. Set ramdom large value so `bicCand < bic` is always true for the first element of loop (p=1)
  for (p in 1:12){
    XCand <- lag.xts(datAR, 1:p+h-1)
    datARlmCand <- merge.xts(datAR,XCand) %>%    # notice first row of `datARlm`
      set_colnames(c("y", paste("lag", 1:p, sep=""))) #  has same index as p+1th row of X
    fitARCand <- lm(y~., data=datARlmCand[(T1+1):T2+(t-1),])
    bicCand <- -as.numeric(-2*logLik(fitARCand) + log(winSize)*ncol(datARlmCand))
    if (bicCand < bic){
      bic <- bicCand
      lagLenAR[t] <- p
      X <- XCand
      datARlm <- datARlmCand
      fitAR <- fitARCand
    }
  }
  # forecast
  predAR <- predict(fitAR, newdata=datARlm[T2+t, -1])
  predErrAR[t] <- as.numeric((predAR-datARlm[T2+t,1])^2)
}

msfeAR <- mean(predErrAR)

# save results
if (horizon==1){ARlags[[var]] <- matrix(NA, nrow=length(hChoises), ncol=winSize, # create a placeholder inside a list
                                         dimnames = list(c(paste("h=",hChoises,sep=""))))}
ARlags[[var]][horizon,] <- lagLenAR

MSFEs[[horizon]]["AR", targetVar]<- msfeAR


# clear workspace
rm(datAR, datARlm, datARlmCand, fitAR,fitARCand, X, XCand,
   bic, bicCand, p, predAR, predErrAR, t, lagLenAR, msfeAR)
