# Group lasso -------------------------------------------------------------

# define groups
output<- grep("OutputIncome_", names(dat)) 
employment <- grep("EmploymentHours_", names(dat))
sales <- grep("RetailManufacturingTradeSales_", names(dat))
consumption <- grep("Consumption_", names(dat))
housing <- grep("HousingStartsSales_", names(dat))
inventory <- grep("InventoriesOrders_", names(dat))
stock <- grep("StockPrices_", names(dat))
exchange <- grep("ExchangeRates_", names(dat))
interest <- grep("InterestRates_", names(dat))
money <- grep("MoneyCreditQuantityAggregate_", names(dat))
price <- grep("PriceIndicesWages_", names(dat))

idxList <- list(output, employment, sales, consumption, housing, inventory, stock, exchange, interest, money,price)
idx <- integer()
for (i in 1:12){
  for (j in 1:length(idxList)){
    idx <- append(idx, rep(length(idxList)*(i-1)+j, length(idxList[[j]])))
  }
}

y <- dat[, targetVar] %>% 
  set_colnames("y")
X <- lag.xts(dat, 1:12+h-1)
# lambdaChoises <- 10^(seq(-2,0,len=100)) # lambda choices, selection on CV
lambdaChoises<- c(1,0.3,0.1,0.03,0.01,0.003,0.001)
predErr <- matrix(NA, nrow=length(lambdaChoises), ncol=winSize) 


pb <- txtProgressBar(0, winSize, style=3)
tictoc::tic()

for (t in 1:winSize){ # penalty param / lag selection
  
  fitGLasso <- grplasso(X[(12+h):T1+t-1,],y[(12+h):T1+t-1], idx, model=LinReg(),
                        lambda = lambdaChoises, center = F, standardize = F)
  predGLasso <- predict(fitGLasso, newdata=X[T1+t,])
  predErr[,t] <- as.numeric((predGLasso-as.numeric(y[t+1,]))^2)
  setTxtProgressBar(pb,t)
}
cv <- apply(predErr,1,mean)
optLam <- lambdaChoises[which.min(cv)]


lamIdx <- which(cv==min(cv), arr.ind = T)[1] # optimal lambda (idx)
optLam <- lambdaChoises[lamIdx]
optLag <- which(cv==min(cv), arr.ind = T)[2] # opt lag
tictoc::toc()

### evaluation
# Now we have selected optimal regularisation parameter and lag length based on cross valisation. 

x <- lag.xts(dat, 1:optLag)
x <- x[-c(1:optLag),] # trim x'NAs **Note: I have to remove last obs if h > 1 bc there's no counterpart obs for y**
y <- lag.xts(dat[,51], -(h-1)) # **Note: remove last (h-1) obs's when h >1 **
y <- y[-c(1:optLag),]
idx<- rep(c(rep(1,g1), rep(2,g2-g1), rep(3,g3-g2), rep(4,g4-g3), 
            rep(5,g5-g4), rep(6,g6-g5),rep(7,g7-g6), rep(8,g8-g7), 
            rep(9,g9-g8), rep(10,g10-g9), rep(11,g11-g10), rep(12,g12-g11)),optLag)


predErrGLasso <- 0 
pb<-txtProgressBar(T2-1, end-1, style=3)
for (t in T2:(end-1)){ # forecast evaluation
  fitGLasso <- grplasso(x, y, idx, model=LinReg(),
                        lambda = optLam, center = F, standardize = F)
  predGLasso <- predict(fitGLasso, newdata=x[t+1,])
  predErrGLasso <- as.numeric(predLasso - as.numeric(y[t+1,]))^2 + predErrGLasso
  setTxtProgressBar(pb,t)
}
msfeGLasso <- predErrGLasso/(end-T2) 

