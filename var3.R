# three-variable VAR with flexible lag length selected by AIC, rolling window


library(vars)
idx<- c(1,51,148)

winSize <- floor((nrow(dat)-h)/3)
T1 <- winSize
T2 <- 2*(floor((nrow(dat)-h)/3)) # end of evaluation period for lag length
end <- nrow(dat) - h # last observation for the last forecast (2001-02-01)
predErrVAR3<- 0
lagLenVAR3<- 0 # track lag length for each window

for (t in 1:winSize){
  datVAR <- dat[(T1+t):(T2+t-h),idx]
  fitVAR3 <- VAR(datVAR, lag.max = 12, ic="AIC")
  lagLenVAR3[t] <- fitVAR3$p
  predVAR3 <- predict(fitVAR3, n.ahead=h)
  predErrVAR3 <- (predVAR3$fcst$X51[1] - as.numeric(dat[T2+t,51]))^2 + predErrVAR3
}
msfeVAR3<- predErrVAR3/winSize


rm(datVAR,fitVAR3, predVAR3,end, idx, predErrVAR3, t,T1,T2, winSize)
