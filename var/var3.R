# three-variable VAR with flexible lag length selected by AIC, rolling window
if (targetVar %in% c("OutputIncome_IPtotal","PriceIndicesWages_CPIlessFood", 
                     "InterestRates_overnightAvgMonth")){
  idx <- c("OutputIncome_IPtotal","PriceIndicesWages_CPIlessFood",
           "InterestRates_overnightAvgMonth")
} else {
  idx <- c("OutputIncome_IPtotal","PriceIndicesWages_CPIlessFood",
           "InterestRates_overnightAvgMonth", targetVar)
}


# winSize <- floor((nrow(dat)-h)/3)
# T1 <- winSize
# T2 <- 2*(floor((nrow(dat)-h)/3)) # end of evaluation period for lag length
# end <- nrow(dat) - h # last observation for the last forecast (2001-02-01)
datVAR <- dat[,idx]
predErrVAR <- numeric()
lagLenVAR<- numeric() # track lag length for each window
# pb <- txtProgressBar(0,winSize,style=3)
for (t in 1:winSize){
  bic<- 1e10
  for (p in 1:12){
    Xcand <- lag.xts(datVAR, 1:p+h-1)
    datVARlmCand <- merge.xts(datVAR[,targetVar], Xcand) %>% 
      set_colnames(c("y", paste(paste("X", 1:length(idx), sep=""), 
                                paste("lag",rep(1:p,each=length(idx)),sep=""),sep="_")))
    fitVARCand<- lm(y~.-1, data=datVARlmCand[(T1+1):T2+t-1])
    bicCand <- as.numeric(2*logLik(fitVARCand) + log(winSize)*(ncol(datVARlmCand)-1))
    if (bicCand < bic){
      bic <- bicCand
      lagLenVAR[t] <- p
      X <- Xcand
      datVARlm <- datVARlmCand
      fitVAR <- fitVARCand
    } # endif
  } # endfor (p)
  # forecast 
  predVAR <- as.numeric(predict(fitVAR, newdata=datVARlm[T2+t,-1]))
  predErrVAR[t] <- as.numeric((predVAR - dat[T2+h+t-1,targetVar])^2)
  # datVAR <- dat[(T1+1):T2+t-1,idx]
  # fitVAR <- VAR(datVAR, lag.max = 12, ic="SC") # `ic="AIC"` makes mse too high...
  # lagLenVAR[t] <- fitVAR$p
  # predVAR <- predict(fitVAR, n.ahead=h)$fcst
  # predErrVAR <- (predVAR[["OutputIncome_IPtotal"]][1,1] - 
  #                   as.numeric(dat[T2+h+t-1,"OutputIncome_IPtotal"]))^2
  # setTxtProgressBar(pb,t)
}
msfeVAR<- mean(predErrVAR)

results["VAR",targetVar] <- msfeVAR


rm(datVAR,datVARlm, datVARlmCand,fitVAR,fitVARCand,X,Xcand,
   bic, bicCand, idx,p, predErrVAR, predVAR, t)
