# AR with fixed lag, rolling window

winSize <- floor((nrow(dat)-h)/3)
T1 <-  winSize # end of initialisation period
T2 <- 2*(floor(nrow(dat)/3)) # end of evaluation period for lag length
end <- nrow(dat) - h # last observation for the last forecast (2001-02-01)

predErrAR4 <- numeric(12)
msfeAR4 <- numeric(12)
for (p in 1:12){ # loop through p
  for (t in 1:winSize){
    fitAR4 <- ar(dat[t:(T1+t), 51], aic=F, order.max = p, demean=F, intercept=T, method="ols")
    beta <- matrix(c(fitAR4$x.intercept, fitAR4$ar[1:p]))
    predAR4 <- cbind(1, t(coredata(dat)[(T1+t):(T1+t-p+1),51])) %*% beta %>% as.numeric
    predErrAR4[p] <- (predAR4 - dat[T1+t+1,51])^2 + predErrAR4[p]
  }
  msfeAR4[p] <- predErrAR4[p] / (T2-T1+1)
}

optLagAR4 <- which.min(msfeAR4)

# repeat the same with p=optLag using evaluation period
p <- optLagAR4
# x <- lag.xts(dat[,51], k=1:p)
# x<- x[-c(1:p),]
# y <- lag.xts(dat[,51], k= 1-h)
# y<- y[-c(1:p)]
# T1 <- floor(nrow(x)/3) # end of initialisation period
# T2 <- 2*(floor(nrow(x)/3)) # end of evaluation period for lag length
# end <- nrow(x) - h # last observation for the last forecast (2001-02-01)
predErrAR4 <- 0
for (t in 1:winSize){
  fitAR4 <- ar(dat[(T1+t):(T2+t-h), 51], aic=F, order.max = p, demean=F, intercept=T, method="ols")
  beta <- matrix(c(fitAR4$x.intercept, fitAR4$ar[1:p]))
  predAR4 <- cbind(1, t(coredata(dat)[(T2+t-h):(T2+t-h-p+1),51])) %*% beta %>% as.numeric
  predErrAR4 <- (predAR4 - dat[T2+t,51])^2 %>% as.numeric+ predErrAR4
}
msfeAR4 <- predErrAR4/winSize

rm(beta, fitAR4,end,p, predAR4, predErrAR4,t,T1,T2,winSize)