# three-, or four-variable VAR with lag length selected by BIC, rolling window

if (targetVar %in% c("OutputIncome_IPtotal","PriceIndicesWages_CPIlessFood", 
                     "InterestRates_overnightAvgMonth")){
  idx <- c("OutputIncome_IPtotal","PriceIndicesWages_CPIlessFood",
           "InterestRates_overnightAvgMonth")
} else {
  idx <- c("OutputIncome_IPtotal","PriceIndicesWages_CPIlessFood",
           "InterestRates_overnightAvgMonth", targetVar)
}

datVAR <- dat[,idx]
predErrVAR <- numeric()
lagLenVAR<- numeric() # track lag length for each window

for (t in 1:winSize){
  bic<- 1e10
  for (p in 1:12){
    Xcand <- lag.xts(datVAR, 1:p+h-1)
    datVARlmCand <- merge.xts(datVAR[,targetVar], Xcand) %>% 
      set_colnames(c("y", paste(paste("X", 1:length(idx), sep=""), 
                                paste("lag",rep(1:p,each=length(idx)),sep=""),sep="_")))
    fitVARCand<- lm(y~.-1, data=datVARlmCand[(T1+1):T2+t-1])
    bicCand <- as.numeric(-2*logLik(fitVARCand) + log(winSize)*(ncol(datVARlmCand)-1))
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
}
msfeVAR<- mean(predErrVAR)


# save results
if (horizon==1){VARlags[[var]] <- matrix(NA, nrow=length(hChoises), ncol=winSize, # create a placeholder inside a list
                                        dimnames = list(c(paste("h=",hChoises,sep=""))))}
VARlags[[var]][horizon,] <- lagLenVAR

MSFEs[[horizon]]["VAR", targetVar] <- msfeVAR

# clear workspace
rm(datVAR,datVARlm, datVARlmCand,fitVAR,fitVARCand,X,Xcand,
   bic, bicCand, idx,p, predErrVAR, predVAR, t, msfeVAR, lagLenVAR)