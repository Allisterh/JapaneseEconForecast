# setup
rm(list=ls())
dat<- readxl::read_excel("data_JER.xls")
dat <- xts(scale(dat[-1]), order.by = dat$date)

h <- 1