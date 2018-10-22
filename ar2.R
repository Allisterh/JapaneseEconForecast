# baseline AR model 

# case 2: fixed lag length (selected by cross validation)

# T1 <- floor((nrow(dat)-h)/3) # end of initialisation period
# T2 <- 2*(floor(nrow(dat)/3)) # end of evaluation period for lag length
# end <- nrow(dat) - h # last observation for the last forecast (2001-02-01)

predErrAR2 <- numeric(12)
msfeAR2 <- numeric(12)
for (p in 1:12){ # loop through p
  x <- lag.xts(dat[,51], k=1:p)
  x<- x[-c(1:p),]
  y <- lag.xts(dat[,51], k= 1-h)
  y<- y[-c(1:p)]
  T1 <- floor(nrow(x)/3) # end of initialisation period
  T2 <- 2*(floor(nrow(x)/3)) # end of evaluation period for lag length
  for (t in T1:(T2-1)){
    # fitAR2 <- ar(dat[1:t, 51], aic=F, order.max = p, demean=F, intercept=T, method="ols")
    # beta <- matrix(c(fitAR2$x.intercept, fitAR2$ar[1:p]))
    # predAR2 <- cbind(1, t(coredata(dat)[t:(t-p+1),51])) %*% beta %>% as.numeric
    # predErrAR2[p] <- (predAR2 - dat[t+1,51])^2 + predErrAR2[p]
    fitAR2 <- lm(y[1:t,]~x[1:t,])
    predAR2 <- cbind(1, x[t+1,]) %*% as.matrix(fitAR2$coef) %>% as.numeric
    predErrAR2[p] <- (predAR2 - y[t+1,])^2 %>% as.numeric + predErrAR2[p]
  }
  msfeAR2[p] <- predErrAR2[p] / (T2-T1+1)
}

optLag <- which.min(msfeAR2)

# repeat the same with p=optLag using evaluation period
p <- optLag
x <- lag.xts(dat[,51], k=1:p)
x<- x[-c(1:p),]
y <- lag.xts(dat[,51], k= 1-h)
y<- y[-c(1:p)]
T1 <- floor(nrow(x)/3) # end of initialisation period
T2 <- 2*(floor(nrow(x)/3)) # end of evaluation period for lag length
end <- nrow(x) - h # last observation for the last forecast (2001-02-01)
predErrAR2 <- 0
for (t in T2:(end-1)){
  fitAR2 <- lm(y[1:t,]~x[1:t,]) # might be better to start from T1 instead of 1, or use rolling window
  predAR2 <- cbind(1, x[t+1,]) %*% as.matrix(fitAR2$coef) %>% as.numeric
  predErrAR2 <- (predAR2 - y[t+1,])^2 %>% as.numeric + predErrAR2
}
msfeAR2 <- predErrAR2/(end-T2+1)

rm(fitAR2,x,y, end, p,predAR2,predErrAR2,t,T1,T2)
