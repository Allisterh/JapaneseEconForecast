library(magrittr)
library(tidyverse)
library(xts)

# rm(list=ls())
dat<- readxl::read_excel("data_JER.xls")
dat<- xts(dat[-1], order.by = dat$date)*100 # change to percent change 
dat[,c(46,131:148)] <- dat[,c(46, 131:148)]/100 # transform back, for series recorded in levels

# PCA ---------------------------------------------------------------------
r <- 6 # number of factors 
h <- 1

# 
# start <- 2*(floor(nrow(dat)/3)) # evaluate the best lambda 
# # start<-which(index(dat)=="1995-06-01") # last observation for first forecast
# end<- nrow(dat)-h
# 
# predErrPCA<- 0
# 
# for (t in start:(end-h)){
#   # estimate factor by PCA
#   x <- dat[1:(t-h),]
#   pc <- eigen(x%*%t(x) / (nrow(x)*ncol(x)))$vectors
#   Fhat <- sqrt(nrow(x))*pc[,1:r]
#   # fit regression to get coef for beta
#   datPCA <- coredata(cbind(dat[(h+1):t,51], Fhat)) # dependent variable on first col, r factors from second to last
#   
#   fitPCA <- VAR(datPCA,12)
#   predPCA<- predict(fitPCA, n.ahead=h)
#   predErrPCA <- (predPCA$fcst$X51[1] - as.numeric(dat[t+h,51]))^2 + predErrPCA
#   
#   # fitPCA<- ar(X1~.,datPCA)
#   # # estim new factor fore forecasting 
#   # x <- dat[1:(t+1),] 
#   # pc <- eigen(x%*%t(x) / (nrow(x)*ncol(x)))$vectors
#   # Fhat <- sqrt(nrow(x))*pc[t+1,1:r]
#   # predPCA<- sum(Fhat * fitPCA$coef)
#   # predErrPCA <- as.numeric(predPCA - dat[t+h,1])^2 + predErrPCA
# }
# msfePCA <- predErrPCA/(end-start-h)



### no lag selection version

start <- 2*(floor(nrow(dat)/3)) # evaluate the best lambda 
end<- nrow(dat)
predErrPCA <- numeric(12)
pb <- txtProgressBar(start-1, end-1, style=3)
for (t in start:(end-1)){
  x <- dat[1:t,]
  pc <- eigen(x%*%t(x) / (nrow(x)*ncol(x)))$vectors
  Fhat <- sqrt(nrow(x))*pc[,1:r]
  datPCA <- cbind(dat[1:t, 51], Fhat)
  # forecast
  x2 <- dat[1:(t+1), ]
  pc2 <- eigen(x2%*%t(x2) / (nrow(x2)*ncol(x2)))$vectors
  Fhat2 <- sqrt(nrow(x2))*pc2[,1:r]
  datPCA2 <- cbind(dat[1:(t+1),51],Fhat2)
  for (p in 1:12){
    x <- lag.xts(datPCA, 1:p)
    y <- lag.xts(datPCA[,1], h-1)
    fitPCA <- lm(y~x-1)
    # forecast
    x2 <- lag.xts(datPCA2, 1:p)
    y2 <- lag.xts(datPCA2[,1], h-1)
    predPCA <- sum(coef(fitPCA)*x2[1+t,])
    predErrPCA[p] <- as.numeric(predPCA-y2[t+1])^2 + predErrPCA[p]
  }
  setTxtProgressBar(pb, t)
}
msfePCA <- predErrPCA/(end-start)

