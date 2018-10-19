library(BigVAR)

fitVARL<-constructModel(coredata(dat),p=12,"Basic", gran=c(150,10))
plot(fitVARL)
tictoc::tic()
varlResult<- cv.BigVAR(fitVARL)
tictoc::toc() ## 10 mins

betas <- varlResult@betaPred
betas[betas< 1e-7] <- 0
betas[betas>= 1e-7] <- 1
apply(betas,2,sum) 
VARXForecastEval(coredata(dat),p=12, h=1, iterated = T, X=NULL, s=NULL,
                 T1=floor(nrow(dat)/3),T2=2*(floor(nrow(dat)/3)), IC="AIC")
