# VAR with three variables (Inflation rate(51), short term interest rate(148) and IIP growth(1)) 
# Case 1: flexible lag length

### This doesnt accout for direct forecasting ###

library(vars)
idx<- c(1,51,148)
lagLenVAR <- numeric()
T2 <- 2*(floor((nrow(dat)-h)/3)) # end of evaluation period for lag length
end <- nrow(dat) - h # last observation for the last forecast (2001-02-01)
idxTracker <- 0
predErrVAR<- 0
for (t in T2:(end-h)){
  idxTracker <- idxTracker +1
  datVAR <- dat[1:t,idx]
  fitVAR <- VAR(datVAR, lag.max = 12, ic="AIC")
  lagLenVAR[idxTracker] <- fitVAR$p
  predVAR <- predict(fitVAR, n.ahead=h)
  predErrVAR <- (predVAR$fcst$X51[1] - as.numeric(dat[t+h,51]))^2 + predErrVAR
}
msfeVAR<- predErrVAR/(end-T2+1)


rm(datVAR,fitVAR,predVAR,end, idx, idxTracker, predErrVAR,t,T2)
