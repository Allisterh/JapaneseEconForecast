# baseline models (samle mean and ramdom walk)

# sample mean -------------------------------------------------------------

predErrSM <- numeric()
for (t in 1:winSize){
  predSM <- mean(dat[(T1+1):T2+(t-1),targetVar])
  predErrSM[t] <- as.numeric((predSM - dat[T2+t, targetVar])^2) 
}
msfeSM <- mean(predErrSM)


# randomWalk --------------------------------------------------------------

predErrRW <- numeric()
for (t in 1:winSize){
  predRW <- as.numeric(dat[T2+t-1,targetVar])
  predErrRW[t] <- as.numeric((predRW - dat[T2+h+t-1,targetVar])^2) 
}
msfeRW <- mean(predErrRW)


# save results 
if (var==1) {MSFEs[[horizon]] <- matrix(NA,nrow=9, ncol=length(targetVariables), 
                                        dimnames=list(c("SM","RW","AR","VAR", "DI","LASSO", "LASSO2","ENET", "gLASSO"), targetVariables))}
MSFEs[[horizon]]["SM", targetVar] <- msfeSM
MSFEs[[horizon]]["RW", targetVar] <- msfeRW
# claer workspace
rm(predErrRW, predErrSM,predRW,predSM,t, msfeSM, msfeRW)
