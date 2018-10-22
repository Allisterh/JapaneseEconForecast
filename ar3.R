# AR with fleible lag len, rolling window

# THIS CODE IONLY ACCOUNTS FOR RECURSIVE FORECAST, NEED FIXING!!
winSize <- (floor((nrow(dat)-h)/3))
T1 <- floor((nrow(dat)-h)/3)
T2 <- 2*(floor((nrow(dat)-h)/3)) # end of evaluation period for lag length
end <- nrow(dat) - h # last observation for the last forecast (2001-02-01)
predErrAR3<- 0
lagLenAR3<- numeric() # track lag length for each window
idxTracker<-0
for (t in 1:winSize){
  idxTracker<- 1+ idxTracker
  fitAR3<- ar(dat[(T1+t):(T2+t),51],method="ols", demean=F,intercept=T)
  p <- fitAR3$order
  if (p==0){
    predAR3 <- dat[T2+t,51] %>% as.numeric
  } else {
    beta <- matrix(c(fitAR3$x.intercept, fitAR3$ar[1:p]))
    predAR3 <- cbind(1, t(coredata(dat)[(T2+t):(T2+t-p+1),51])) %*% beta %>% as.numeric
  }
  # predAR <- predict(fitAR, n.ahead=h)$pred[h]
  predErrAR3 <- (predAR3 - as.numeric(dat[t+h,51]))^2 + predErrAR3
  lagLenAR3[idxTracker]<- fitAR3$order
}
msfeAR3 <- predErrAR3/winSize

rm(fitAR3, end, idxTracker,predAR3,predErrAR3,t,T1,T2,winSize, beta,p)

