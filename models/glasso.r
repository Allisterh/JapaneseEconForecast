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


# cross validation --------------------------------------------------------

# lambdaChoises <- 10^(seq(3,-2,len=10))
tictoc::tic()
lambdaChoises <- 100:1

# predErr <- matrix(NA, ncol=winSize, nrow=length(lambdaChoises))

# for (t in 1:winSize){ # penalty param / lag selection
#   print(paste("-------- t =", t, "-------"))
#   fitGLasso <- grplasso(X[(12+h):T1+t-1,],y[(12+h):T1+t-1], idx, model=LinReg(), # 1 min
#                         lambda = lambdaChoises, center = F, standardize = F, 
#                         control = grpl.control(max.iter=1e07, tol=1e-15, trace=1))
#   predGLasso <- predict(fitGLasso, newdata=X[T1+t,])
#   predErr[,t] <- as.numeric((predGLasso-as.numeric(y[T1+t,]))^2)
# }

predErr <-  # 30min
foreach(t=1:winSize, .combine = "cbind", .inorder = F) %dopar% { # penalty param / lag selection
  fitGLasso <- grplasso(X[(12+h):T1+t-1,],y[(12+h):T1+t-1], idx, model=LinReg(), # 1 min
                        lambda = lambdaChoises, center = F, standardize = F,
                        control = grpl.control(max.iter=1e07, tol=1e-15, trace=1))
  predGLasso <- predict(fitGLasso, newdata=X[T1+t,])
  as.numeric((predGLasso-as.numeric(y[T1+t,]))^2)
}
cv <- apply(predErr,1,mean)
optLam <- lambdaChoises[which.min(cv)]
gLASSOlambda[horizon,targetVar] <- optLam # save optimal lambda
tictoc::toc()

# evaluation --------------------------------------------------------------

# predErr <- numeric()
# coefTracker <- matrix(NA, nrow=winSize, ncol=ncol(X))
# for (t in 1:winSize){ # forecast evaluation, 45 sec
#   fitGLasso <- grplasso(X[(T1+1):T2+t-1,], y[(T1+1):T2+t-1], idx, model=LinReg(),
#                         lambda = optLam, center = F, standardize = F, 
#                         control = grpl.control(max.iter=1e07, tol=1e-07,trace=0))
#   predGLasso <- predict(fitGLasso, newdata=X[T2+t,])
#   predErr[t] <- as.numeric(predGLasso - y[T2+t,])^2
#   coefTracker[t,] <- fitGLasso$coef
# }

eval <-
  foreach(t = 1:winSize) %dopar% { # forecast evaluation, 45 sec
    fitGLasso <- grplasso(X[(T1+1):T2+t-1,], y[(T1+1):T2+t-1], idx, model=LinReg(),
                          lambda = optLam, center = F, standardize = F,
                          control = grpl.control(max.iter=1e07, tol=1e-10,trace=0))
    predGLasso <- predict(fitGLasso, newdata=X[T2+t,])
    err <- as.numeric(predGLasso - y[T2+t,])^2
    coefs <- fitGLasso$coef
    list(err, coefs)
  }
predErr <- unlist(sapply(eval, function(foo) foo[1]))
coefTracker <- matrix(unlist(sapply(eval, function(foo) foo[2])),
                      nrow=winSize, ncol=ncol(X), byrow=T)

coefTracker[abs(coefTracker) < 1e-10] <- 0
coefTracker[abs(coefTracker) >=1e-10] <- 1 # 1 if coef is selected (non-zero)

# save results ------------------------------------------------------------

MSFEs[[horizon]]["gLASSO", targetVar] <- mean(predErr)
gLASSOsparsityRatio[horizon,targetVar] <- mean(coefTracker) # the ratio of non-zero coef
gLASSOnonzero[horizon,targetVar] <- sum(coefTracker) # the ratio of non-zero coef

if (horizon == 1) {gLASSOcoefs[[var]] <- list()} # initialise by setting sub-list so that each main list contains sub-lists
gLASSOcoefs[[var]][[horizon]] <- coefTracker
if (horizon == 4) {names(gLASSOcoefs[[var]]) <- paste("h", hChoises, sep="")}


# clear workspace ---------------------------------------------------------
rm(output, employment, sales, consumption, housing, inventory, stock, exchange,
   interest, money,price, idxList, idx,i,j,y,X, lambdaChoises, predErr, cv,
   optLam, coefTracker,eval)

