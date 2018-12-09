source("readResults.R")


# DI ----------------------------------------------------------------------

## Stock and Watson (1999, 2002), Shintani (2005) and Ludvingson and Ng (2009) point out that 
## the first factor has high correlation to real output, thus can be interpreted as real factor. 

vec1 <- xts(eigen(dat%*%t(dat) /(nrow(dat)*ncol(dat)))$vectors, order.by = index(dat))[,1]
png("results/png/DI/DIfirstFactor.png")
plot.xts(merge.xts(dat[,"OutputIncome_IPtotal"], scale(vec1)))
dev.off()
cor(merge.xts(dat[,"OutputIncome_IPtotal"], vec1))

# datDI <- dat["200807/201806",]
# vec1 <- xts(eigen(datDI%*%t(datDI) /(nrow(datDI)*ncol(datDI)))$vectors, order.by = index(datDI))[,1]
# png("results/png/DI/DIfirstFactor.png")
# plot.xts(merge.xts(datDI[,"OutputIncome_IPtotal"], scale(vec1)))
# dev.off()
# cor(merge.xts(datDI[,"OutputIncome_IPtotal"], vec1))

## Recall that we estimate factors for each window, i.e. interpretations may differ for different windows,
## We therefore look at all the factors at the same time to see if which variable are explained well by factors,
## and whose variances are not well captured by factors. This is done by regressing each variables onto 
## the estimated factors. (see also `model/di.r`)

## `DIinterpret.rds` contains the R2 of each variables onto factors for each horizon. 
## Notice that although we slide horizons, we regress variables on factors within the same time period. 
## The R2 therefore shows the similar result for every horizon and we take the average of the four horizons
grp <- as.factor(scan("txt/groups.txt",character(),quiet=T)) # `scan` doesnt support `what=factor()`
levels(facLev) <- c("Output&Income","Employment&Hours","Retail&Manufacturing&Trade-Sales","Consumption",
                 "Housing-Starts&Sales","Inventories&Orders","Stock-Prices","Exchange-Rates",
                 "Interest-Rates","Money&Credit-Quantity-Aggregate","Price-Indices&Wages")

data.frame(Rsq=apply(DIinterpret, 2, mean)) %>% 
  rownames_to_column("variables") %>% 
  separate(variables, c("grp","variables"), sep="_") %>% 
  # mutate_at(vars(grp), factor)
  ggplot(aes(1:ncol(dat), Rsq, fill=grp)) +
  geom_bar(stat="identity") +
  labs(x="variables", y="R-square") +
  theme(legend.position = "right") +
  ggsave("results/png/DI/FactorsR2.png")

# DILASSO -----------------------------------------------------------------
for (i in 1:length(targetVariables)) {
  as.data.frame(sapply(DILASSOcoefs[[i]],function(foo) foo)) %>% 
    gather(horizon, selected) %>%
    add_column(winID=rep(1:winSize, (12+20)*4),
               varID=rep(1:(12+20),4,each=winSize)) %>%
    unite(ID, horizon, varID) %>%
    spread(winID,selected) %>%
    remove_rownames %>% column_to_rownames("ID") %>%
    apply(1,sum, na.rm=T) %>%
    as.data.frame %>% rownames_to_column() %>%
    set_colnames(c("ID", "selected")) %>%
    separate(ID, c("horizon","varID"), sep="_") %>%
    mutate_at(vars(varID), as.numeric) %>%
    dplyr::arrange(horizon, varID) %>% 
    add_column(grp=rep(c(rep("lag",12),rep("factor",20)),4)) %>% 
    ggplot(aes(varID, selected, fill=grp)) +
    geom_bar(stat="identity") + 
    facet_grid(.~horizon) +
    theme(legend.position = "none") +
    ggsave(paste("results/png/DILASSO/",i,".", targetVariables[i],".png",sep="" ))
}
  

foo <- list()
for(i in 1:length(targetVariables)){
  foo[[i]]<- matrix(NA,nrow=4, ncol=32, dimnames=list(paste("h=",hChoises,sep=""), c(paste("lag",1:12,sep=""),paste("F",1:20,sep=""))))
  for (h in 1:4){
    foo[[i]][h,]<- apply(DILASSOcoefs[[i]][[h]], 2,sum, na.rm=T)
  }
  if (i==length(targetVariables)) names(foo) <- targetVariables
}


# GLASSO ------------------------------------------------------------------
lassoFam <- c("LASSO","ENET","GLASSO")
lassoFamCoef <- list(LASSOcoefs, ENETcoefs, gLASSOcoefs)
for (j in 1:3){
  for (i in 1:length(targetVariables)){
    coefs <- lassoFamCoef[[j]]
    as.data.frame(sapply(coefs[[i]],function(foo) foo)) %>% 
      gather(horizon, selected) %>% 
      add_column(winID=rep(1:winSize, ncol(dat)*4*4),
                 varID=rep(1:(ncol(dat)*4),4,each=winSize)) %>%
      unite(ID, horizon, varID) %>%
      spread(winID,selected) %>%
      remove_rownames %>% column_to_rownames("ID") %>%
      apply(1,sum) %>%
      as.data.frame %>% rownames_to_column() %>%
      set_colnames(c("ID", "selected")) %>%
      separate(ID, c("horizon","varID"), sep="_") %>%
      mutate_at(vars(varID), as.numeric) %>%
      dplyr::arrange(horizon, varID) %>% 
      add_column(grp=paste(rep(grp,4), paste("lag",rep(1:4,4,each=ncol(dat)),sep=""),sep="_")) %>% 
      ggplot(aes(varID, selected, fill=grp)) +
      geom_bar(stat="identity") + 
      facet_grid(.~horizon) +
      theme(legend.position = "none") +
      ggsave(paste("results/png/",lassoFam[j],"/",i,".", targetVariables[i],".png",sep="" ))
  }
}    

ggplot(bar, aes(varID, selected), fill=grp) +
  geom_bar(stat="identity") 
matrix(bar)
# spread(varID, selected)
ggplot(bar, aes(x=varID, y=selected)) + 
  geom_bar(stat="identity") 

bar %>% 
  fct_inorder()

bar$baz <- 1:nrow(bar)
spread(bar,varID,-winID) %>% View
