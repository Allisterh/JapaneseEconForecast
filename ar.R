# baseline AR model
# Case 1: allow for flexible lag structure 

# THIS CODE IONLY ACCOUNTS FOR RECURSIVE FORECAST, NEED FIXING!!

T2 <- 2*(floor((nrow(dat)-h)/3)) # end of evaluation period for lag length
end <- nrow(dat) - h # last observation for the last forecast (2001-02-01)
predErrAR<- 0
lagLenAR<- numeric() # track lag length for each window
idxTracker<-0
for (t in T2:end){
  idxTracker<- 1+ idxTracker
  fitAR<- ar(dat[1:t,51],method="ols",aic=T, 
             order.max = 12, demean=F,intercept=T)
  p <- fitAR$order
  beta <- matrix(c(fitAR$x.intercept, fitAR$ar[1:p]))
  predAR <- cbind(1, t(coredata(dat)[t:(t-p+1),51])) %*% beta %>% as.numeric
  # predAR <- predict(fitAR, n.ahead=h)$pred[h]
  predErrAR <- (predAR - as.numeric(dat[t+h,51]))^2 + predErrAR
  lagLenAR[idxTracker]<- fitAR$order
}
msfeAR <- predErrAR/(end-T2+1)

rm(fitAR, end, idxTracker,predAR,predErrAR,t,T2, beta,p)


