# setwd("data")

library(xts)
library(x12)
library(urca)

rm(list=ls())

# combine all the series together 
## Data spans from Apr. 2003 to Jun. 2018 to obtain balanced panel data  
## and the methodological break in money stock statistics in Apr. 2003
foo <- merge.xts(readRDS("rds/boj.rds"), readRDS("rds/ds.rds"), readRDS("rds/meti.rds"), 
                 readRDS("rds/mhlw.rds"), readRDS("rds/mic.rds"), readRDS("rds/mlit.rds"), 
                 readRDS("rds/needs.rds"))["200304/201806"]
# sort series based on the variabls names (see `dataOrder for more`)
dat <- foo[,scan("txt/dataOrder.txt", character())]

# seasonal adjustment 
SAidx <- which(names(dat) %in% scan("txt/SAvars.txt", character())) # Series require SA 
for (i in SAidx){ # execution takes around 120 seconds
  dat[,i] <- x12(ts(dat[,i], start=start(dat), end=end(dat), frequency=12))@d11
}
# Test for stationarity
adfResult <- vector() # placeholder to store the result of ADF test
ppResult <- vector() # the one for Phillips-Perron
for (i in 1:ncol(dat)){
  # adf
  adf <- ur.df(dat[,i], type="trend", lags=12, selectlags="Fixed")
  adfResult[i] <- (adf@teststat[1] > adf@cval[1,2]) # True if unit root
  # pp
  pp <- ur.pp(dat[,i], type="Z-tau", model="trend", use.lag=12)
  ppResult[i] <- pp@teststat > pp@cval[1,2]
}
either <- adfResult==1 | ppResult ==1 # True if either ADF or PP exhibits UR
both <- adfResult==1 & ppResult == 1
## All the series except two accept the null hypotheses of UR. Since the two series are...
## ... in the same category as the ones with UR, I conclude all series are not stationary. 

# Transform to stationality (In general, take difference for interest rate and log-diff for the rest)
diffVars <- grep("EmploymentHours_UnempRate|*JobOfferRate*|^InterestRates_", names(dat)) 

dat[,diffVars] <- diff.xts(dat[,diffVars])
dat[,-diffVars] <- diff.xts(dat[, -diffVars], log=T)
dat <- dat[-1,]

# test again for UR
adfResult <- vector() # placeholder to store the result of ADF test
ppResult <- vector() # the one for Phillips-Perron
for (i in 1:ncol(dat)){
  # adf
  adf <- ur.df(dat[,i], type="trend", lags=12, selectlags="Fixed")
  adfResult[i] <- (adf@teststat[1] > adf@cval[1,2]) # True if unit root
  # pp
  pp <- ur.pp(dat[,i], type="Z-tau", model="trend", use.lag=12)
  ppResult[i] <- pp@teststat > pp@cval[1,2]
}
either <- adfResult==1 | ppResult ==1 # True if either ADF or PP exhibits UR
both <- adfResult==1 & ppResult == 1
# There are still some series that adf test fails to reject null hypothesis of UR,
# but since PP rejects the null for all series and it makes more economical sense to treat them as d(1), 
# we conclude that the series are now stationary

###### Run this if you want to take second difference to make sure the stationarity ######
# # take second difference for series that either ADF or PP fails to reject null
# diff2Vars <- which(adfResult==1 | ppResult==1)
# dat[, diff2Vars] <- diff.xts(dat[, diff2Vars])
# dat<- dat[-1,]

# # UR test 
# adfResult <- vector() # placeholder to store the result of ADF test
# ppResult <- vector() # the one for Phillips-Perron
# for (i in 1:ncol(dat)){
#   # adf
#   adf <- ur.df(dat[,i], type="trend", lags=12, selectlags="Fixed")
#   adfResult[i] <- (adf@teststat[1] > adf@cval[1,2]) # True if unit root
#   # pp
#   pp <- ur.pp(dat[,i], type="Z-tau", model="trend", use.lag=12)
#   ppResult[i] <- pp@teststat > pp@cval[1,2]
# }
# # (adfResult==1 | ppResult==1) # False if both ADF and PP rejects null

dat <- scale(dat) # standardise to mean=0, sd=1 for later implementation of Lasso

saveRDS(dat, "dat.rds")
