# var with fixed lag length selected by cv, rolling window

library(vars)

idx<- c(1,51,148)
predErrVAR4<- numeric(12)
T1 <- floor((nrow(dat)-h)/3) # end of initialisation period
T2 <- 2*(floor((nrow(dat)-h)/3)) # end of evaluation period for lag length
end <- nrow(dat)-h
pb <- txtProgressBar(0,12,style=3)
winSize <- T2-T1 
for (p in 1:12){
  for (t in 1:winSize){
    datVAR <- dat[t:(T1+t), idx]
    fitVAR4 <- VAR(datVAR, p=p)
    predVAR4 <- predict(fitVAR4, n.ahead=h)
    predErrVAR4[p] <- (predVAR4$fcst$X51[1] - as.numeric(dat[T1+t+h,51]))^2 + predErrVAR4[p]
  }
  setTxtProgressBar(pb, p, label="cross validation")
}
optLagVAR4 <- which.min(predErrVAR4)

predErrVAR4 <-0
winSize<- end-T2
for (t in 1:winSize){
  datVAR <- dat[(T1+t):(T2+t-h), idx]
  fitVAR4 <- VAR(datVAR, p=optLagVAR4)
  predVAR4 <- predict(fitVAR4, n.ahead=h)
  predErrVAR4 <- (predVAR4$fcst$X51[1] - as.numeric(dat[T2+t,51]))^2 + predErrVAR4
}
msfeVAR4 <- predErrVAR4/winSize


rm(datVAR, fitVAR4,pb, predVAR4, end,idx,predErrVAR4,p,t,T1,T2,winSize)
