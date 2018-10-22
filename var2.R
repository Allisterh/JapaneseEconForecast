# VAR with three variables (Inflation rate(51), short term interest rate(148) and IIP growth(1)) 
# Case 2: flexible lag length selected by cross validation
library(vars)

idx<- c(1,51,148)
predErrVAR2<- numeric(12)


T1 <- floor(nrow(dat)/3) # end of initialisation period
T2 <- 2*(floor(nrow(dat)/3)) # end of evaluation period for lag length
end <- nrow(dat)
for (p in 1:12){
  for (t in T1:(T2-1)){
    datVAR <- dat[1:t, idx]
    fitVAR2 <- VAR(datVAR, p=p)
    predVAR2 <- predict(fitVAR2, n.ahead=h)
    predErrVAR2[p] <- (predVAR2$fcst$X51[1] - as.numeric(dat[t+h,51]))^2 + predErrVAR2[p]
  }
}
optLag <- which.min(predErrVAR2)

predErrVAR2 <-0

for (t in T2:(end-1)){
  datVAR <- dat[1:t, idx]
  fitVAR2 <- VAR(datVAR, p=optLag)
  predVAR2 <- predict(fitVAR2, n.ahead=h)
  predErrVAR2 <- (predVAR2$fcst$X51[1] - as.numeric(dat[t+h,51]))^2 + predErrVAR2
}
msfeVAR2 <- predErrVAR2/(end-T2)
