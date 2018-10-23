# baseline models (samle mean and ramdom walk)


# sample mean -------------------------------------------------------------

## expanding window
winSize <- floor((nrow(dat)-h)/3)
T1 <- winSize
T2 <- 2*(floor((nrow(dat)-h)/3)) # end of evaluation period for lag length
end <- nrow(dat) - h # last observation for the last forecast (2001-02-01)

predErrSM<-0
for (t in T2:end){
  predSM <- mean(dat[1:t,51])
  predErrSM <- (predSM - dat[t+h,51])^2 %>% as.numeric + predErrSM
}
msfeSM <- predErrSM/(end-T2+1)

## rolling window 
predErrSM2 <- 0
for (t in 1:winSize){
  predSM <- mean(dat[(T1+t):(T2+t-h),51])
  predErrSM2 <- (predSM - mean(dat[t, 51]))^2 %>% as.numeric + predErrSM2
}
msfeSM2 <- predErrSM2/winSize


# randomWalk --------------------------------------------------------------

predErrRW <- 0
for (t in T2:end){
  predRW <- dat[t,51] %>% as.numeric
  predErrRW <- (predRW - dat[t+h,51])^2 %>% as.numeric + predErrRW
}
msfeRW <- predErrRW/(end-T2+1)

rm(end,predErrRW,predErrSM, predErrSM2,predRW,predSM,t,T1,T2,winSize)
