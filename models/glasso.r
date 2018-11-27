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
lambdaChoises <- 10^(seq(-3,0,len=10)) # lambda choices, selection on CV
# lambdaChoises<- c(3,1,0.3,0.1,0.03,0.01,0.003,0.001, 0.0003, 0.0001)
predErr <- matrix(NA, nrow=length(lambdaChoises), ncol=winSize) 


# pb <- txtProgressBar(0, winSize, style=3)
tictoc::tic()

predErr <-
  foreach(t = 1:winSize, .combine = "cbind") %dopar% { # penalty param 3.5 hrs wo parallelising
    fitGLasso <- grplasso(X[(12+h):T1+t-1,],y[(12+h):T1+t-1], idx, model=LinReg(),
                          lambda = lambdaChoises, center = F, standardize = F, control=grpl.control(trace=0))
    predGLasso <- predict(fitGLasso, newdata=X[T1+t,])
    as.numeric((predGLasso-as.numeric(y[T1+t,]))^2)
    # setTxtProgressBar(pb,t)
  }
tictoc::toc()

cv <- apply(predErr,1,mean)
optLam <- lambdaChoises[which.min(cv)]

gLASSOlambda[horizon,targetVar] <- optLam


# lamIdx <- which(cv==min(cv), arr.ind = T)[1] # optimal lambda (idx)
# optLam <- lambdaChoises[lamIdx]
# optLag <- which(cv==min(cv), arr.ind = T)[2] # opt lag

### evaluation
# Now we have selected optimal regularisation parameter based on cross valisation. 
coefTracker <- matrix(NA, nrow=winSize, ncol=ncol(X))
predErrGLasso <- numeric()
# pb<-txtProgressBar(0, winSize, style=3)
for (t in 1:winSize){ # forecast evaluation
  fitGLasso <- grplasso(X[(T1+1):T2+t-1,], y[(T1+1):T2+t-1], idx, model=LinReg(),
                        lambda = optLam, center = F, standardize = F)
  predGLasso <- predict(fitGLasso, newdata=X[T2+t,])
  predErrGLasso[t] <- as.numeric((predGLasso - as.numeric(y[T2+t,]))^2)
  coefTracker[t,] <- as.numeric(fitGLasso$coef)
  # setTxtProgressBar(pb,t)
}
msfeGLasso <- mean(predErrGLasso)

coefTracker[coefTracker < 1e-7] <- 0
coefTracker[coefTracker >=1e-7] <- 1 # 1 if coef is selected (non-zero)

# save results
gLASSOsparsityRatio[horizon,targetVar] <- sum(coefTracker)/length(coefTracker) # the ratio of non-zero coef

if (horizon == 1) {gLASSOcoefs[[var]] <- list()} # initialise by setting sub-list so that each main list contains sub-lists
gLASSOcoefs[[var]][[horizon]] <- coefTracker
if (horizon == 4) {names(gLASSOcoefs[[var]]) <- paste("h", hChoises, sep="")}

MSFEs[[horizon]]["gLASSO", targetVar] <- msfeGLasso

rm(output, employment, sales, consumption, housing, inventory, stock,
   exchange, interest, money,price, idx,idxList,i,j) 


