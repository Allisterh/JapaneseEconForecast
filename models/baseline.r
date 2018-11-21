# baseline models (samle mean and ramdom walk)


# sample mean -------------------------------------------------------------

predErrSM <- numeric()
for (t in 1:winSize){
  predSM <- mean(dat[(T1+1):T2+(t-1),targetVar])
  predErrSM[t] <- as.numeric((predSM - dat[T2+t, targetVar])^2) 
}
msfeSM <- mean(predErrSM)
results["SM", targetVar] <- msfeSM


# randomWalk --------------------------------------------------------------

predErrRW <- numeric()
for (t in 1:winSize){
  predRW <- as.numeric(dat[T2+t-1,targetVar])
  predErrRW[t] <- as.numeric((predRW - dat[T2+h+t-1,targetVar])^2) 
}
msfeRW <- mean(predErrRW)
results["RW", targetVar] <- msfeRW

rm(predErrRW, predErrSM,predRW,predSM,t, msfeSM, msfeRW)
