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
for (i in 1:4){
  for (j in 1:length(idxList)){
    idx <- append(idx, rep(length(idxList)*(i-1)+j, length(idxList[[j]])))
  }
}
grpIdx <- idx[1:ncol(dat)] # used later in `interpretations.r`
y <- dat[, targetVar] %>% 
  set_colnames("y")
X <- lag.xts(dat, 1:4+h-1)


# cross validation --------------------------------------------------------

lambdaChoises <- 100:1

predErr <-  # 30min
  foreach(t=1:winSize, .combine = "cbind", .inorder = F) %dopar% { # penalty param
    fitGLasso <- grplasso(X[(4+h):T1+t-1,],y[(4+h):T1+t-1], idx, model=LinReg(), 
                          lambda = lambdaChoises, center = F, standardize = F,
                          control = grpl.control(max.iter=1e07, tol=1e-15, trace=0))
    predGLasso <- predict(fitGLasso, newdata=X[T1+t,])
    as.numeric((predGLasso-as.numeric(y[T1+t,]))^2)
  }
cv <- apply(predErr,1,mean)
optLam <- lambdaChoises[which.min(cv)]
gLASSOlambda[horizon,targetVar] <- optLam # save optimal lambda

# evaluation --------------------------------------------------------------

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

coefTracker[abs(coefTracker) == 0] <- 0
coefTracker[abs(coefTracker) != 0] <- 1 # 1 if coef is selected (non-zero)

# save results ------------------------------------------------------------

MSFEs[[horizon]]["gLASSO", targetVar] <- mean(predErr)
gLASSOsparsityRatio[horizon,targetVar] <- mean(coefTracker) # the ratio of non-zero coef
gLASSOnonzero[horizon,targetVar] <- sum(coefTracker)/winSize # avg nr of nonzero per window

if (horizon == 1) {gLASSOcoefs[[var]] <- list()} # initialise by setting sub-list so that each main list contains sub-lists
gLASSOcoefs[[var]][[horizon]] <- coefTracker
if (horizon == 4) {names(gLASSOcoefs[[var]]) <- paste("h", hChoises, sep="")}


# clear workspace ---------------------------------------------------------
rm(output, employment, sales, consumption, housing, inventory, stock, exchange,
   interest, money,price, idxList, idx,i,j,y,X, lambdaChoises, predErr, cv,
   optLam, coefTracker,eval)

