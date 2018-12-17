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
# levels(grp) <- c("Output&Income","Employment&Hours","Retail&Manufacturing&Trade-Sales","Consumption",
#                  "Housing-Starts&Sales","Inventories&Orders","Stock-Prices","Exchange-Rates",
#                  "Interest-Rates","Money&Credit-Quantity-Aggregate","Price-Indices&Wages")
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
    labs(title=paste("DILASSO:", targetVariables[i]),
         x= "lags and factors", y="number of times varaibles are nonzero") +
    ylim(0,60) + 
    theme(legend.position = "none") +
    ggsave(paste("results/png/DILASSO/",i,".", targetVariables[i],".png",sep="" ))
}
  

dilassoCollapse <- list()
for(i in 1:length(targetVariables)){
  dilassoCollapse[[i]]<- matrix(NA,nrow=4, ncol=32, dimnames=list(paste("h=",hChoises,sep=""), c(paste("lag",1:12,sep=""),paste("F",1:20,sep=""))))
  for (h in 1:4){
    dilassoCollapse[[i]][h,]<- apply(DILASSOcoefs[[i]][[h]], 2,sum, na.rm=T)
  }
  if (i==length(targetVariables)) names(dilassoCollapse) <- cols
}

write.xlsx(dilassoCollapse, "foo.xlsx")


# LASSO-family ------------------------------------------------------------------
lassoFam <- c("LASSO","ENET","GLASSO", "SCAD")
lassoFamCoef <- list(LASSOcoefs, ENETcoefs, gLASSOcoefs, SCADcoefs)
for (j in 1:4){
  coefs <- lassoFamCoef[[j]]
  for (i in 1:length(targetVariables)){
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
      labs(title=paste(lassoFam[j], targetVariables[i], sep=": "),
           x= "variables", y="number of times varaibles are nonzero") +
      ylim(0,60) +
      theme(legend.position = "none") +
      ggsave(paste("results/png/",lassoFam[j],"/",i,".", targetVariables[i],".png",sep="" ))
  }
}    



# see characterisctics by variables group ---------------------------------

## define function 
collapseCoef <- function(h=1:4,v=c(1:20),m=c("DILASSO","LASSO","ENET","gLASSO", "SCAD","DICV","DILASSO")){
  if (m=="DILASSO") coefs <- DILASSOcoefs
  if (m=="LASSO") coefs <- LASSOcoefs
  if (m=="ENET") coefs <- ENETcoefs
  if (m=="gLASSO") coefs <- gLASSOcoefs
  if (m=="SCAD") coefs <- SCADcoefs
  if (m=="DICV") coefs <- DICVr2
  if (m=="DILASSO") coefs <- DILASSOr2
  collapsed <- coefs[[v]][[h]] %>% 
    as.data.frame %>% 
    rownames_to_column("winID") %>% 
    gather("winID") %>%
    set_colnames(c("winID","varID", "selected")) %>% 
    arrange(winID)
  return(collapsed$selected)  
}

### DICV
dicvCollapse <- lapply(1:length(targetVariables), function(v){
  data.frame(winID=rep(1:60, each=ncol(dat)), varID=rep(1:(ncol(dat)), winSize), 
             grp=rep(grp, winSize),
             sapply(1:4, collapseCoef, v, m="DICV") %>% set_colnames(c("h1","h3","h6","h12")))
}) %>% set_names(cols)

dicvTable <- lapply(1:length(targetVariables), function(v){
  dicvCollapse[[v]] %>% 
    group_by(grp) %>% 
    summarise_at(c("h1","h3","h6","h12"),funs(mean)) %>% 
    as.data.frame %>% column_to_rownames("grp")
}) %>% set_names(cols)
write.xlsx(dicvTable$EmpInd, "baz.xlsx", rowNames=T)

### DILASSO 
dilassoCollapse <- lapply(1:length(targetVariables), function(v){
  data.frame(winID=rep(1:60, each=ncol(dat)), varID=rep(1:(ncol(dat)), winSize), 
             grp=rep(grp, winSize),
             sapply(1:4, collapseCoef, v, m="DILASSO") %>% set_colnames(c("h1","h3","h6","h12")))
}) %>% set_names(cols)

dilassoTable <- lapply(1:length(targetVariables), function(v){
  dilassoCollapse[[v]] %>% 
    group_by(grp) %>% 
    summarise_at(c("h1","h3","h6","h12"),funs(mean)) %>% 
    as.data.frame %>% column_to_rownames("grp")
}) %>% set_names(cols)
write.xlsx(dilassoTable$CPI, "foobar.xlsx", rowNames=T)


### lasso
lassoCollapse <- lapply(1:length(targetVariables), function(v){
  data.frame(winID=rep(1:60, each=ncol(dat)*4), varID=rep(1:(ncol(dat)*4), winSize), 
             grp=rep(grp, winSize), lag=rep(1:4,each=ncol(dat)),
             grpLag=paste(rep(grp, winSize),"_lag",rep(1:4,each=ncol(dat)),sep=""),
             sapply(1:4, collapseCoef, v, m="LASSO") %>% set_colnames(c("h1","h3","h6","h12")))
}) %>% set_names(cols)

lassoTable <- lapply(1:length(targetVariables), function(v){
  lassoCollapse[[v]] %>% 
    group_by(grpLag) %>% 
    summarise_at(c("h1","h3","h6","h12"),funs(mean)) %>% 
    as.data.frame %>% column_to_rownames("grpLag")
}) %>% set_names(cols)

write.xlsx(lassoTable$EmpInd, "foo.xlsx", rowNames=T)

lassoGrpRank <- lapply(1:length(cols), function(v){
  sapply(1:ncol(lassoTable[[v]]), function(x) names(sort(t(lassoTable[[v]])[x,], decreasing=T)))
}) %>% set_names(cols)

lassoGrpRank$CPI

### ENET
enetCollapse <- lapply(1:length(targetVariables), function(v){
  data.frame(winID=rep(1:60, each=ncol(dat)*4), varID=rep(1:(ncol(dat)*4), winSize),
             grp=rep(grp, winSize), lag=rep(1:4,each=ncol(dat)),
             grpLag=paste(rep(grp, winSize),"_lag",rep(1:4,each=ncol(dat)),sep=""),
             sapply(1:4, collapseCoef, v, m="ENET") %>% set_colnames(c("h1","h3","h6","h12")))
}) %>% set_names(cols)

enetTable <- lapply(1:length(targetVariables), function(v){
  enetCollapse[[v]] %>% 
    group_by(grpLag) %>% 
    summarise_at(c("h1","h3","h6","h12"),funs(mean)) %>% 
    as.data.frame %>% column_to_rownames("grpLag")
}) %>% set_names(cols)

write.xlsx(enetTable$EmpInd, "foo.xlsx", rowNames=T)

enetGrpRank <- lapply(1:length(cols), function(v){
  sapply(1:ncol(enetTable[[v]]), function(x) names(sort(t(enetTable[[v]])[x,], decreasing=T)))
}) %>% set_names(cols)

### glasso
glassoCollapse <- lapply(1:length(targetVariables), function(v){
  data.frame(winID=rep(1:60, each=ncol(dat)*4), varID=rep(1:(ncol(dat)*4), winSize),
             grp=rep(grp, winSize), lag=rep(1:4,each=ncol(dat)),
             grpLag=paste(rep(grp, winSize),"_lag",rep(1:4,each=ncol(dat)),sep=""),
             sapply(1:4, collapseCoef, v, m="gLASSO") %>% set_colnames(c("h1","h3","h6","h12")))
}) %>% set_names(cols)

glassoTable <- lapply(1:length(targetVariables), function(v){
  glassoCollapse[[v]] %>% 
    group_by(lag) %>% 
    summarise_at(c("h1","h3","h6","h12"),funs(mean)) %>% 
    as.data.frame %>% column_to_rownames("lag")
}) %>% set_names(cols)

write.xlsx(glassoTable$IPtotal, "bar.xlsx", rowNames=T)

glassoGrpRank <- lapply(1:length(cols), function(v){
  sapply(1:ncol(glassoTable[[v]]), function(x) names(sort(t(glassoTable[[v]])[x,], decreasing=T)))
}) %>% set_names(cols)
glassoGrpRank

### SCAD
scadCollapse <- lapply(1:length(targetVariables), function(v){
  data.frame(winID=rep(1:60, each=ncol(dat)*4), varID=rep(1:(ncol(dat)*4), winSize),
             grp=rep(grp, winSize), lag=rep(1:4,each=ncol(dat)),
             grpLag=paste(rep(grp, winSize),"_lag",rep(1:4,each=ncol(dat)),sep=""),
             sapply(1:4, collapseCoef, v, m="SCAD") %>% set_colnames(c("h1","h3","h6","h12")))
}) %>% set_names(cols)

scadTable <- lapply(1:length(targetVariables), function(v){
  scadCollapse[[v]] %>% 
    group_by(grpLag) %>% 
    summarise_at(c("h1","h3","h6","h12"),funs(mean)) %>% 
    as.data.frame %>% column_to_rownames("grpLag")
}) %>% set_names(cols)

write.xlsx(scadTable$IPtotal , "foo.xlsx", rowNames=T)

scadGrpRank <- lapply(1:length(cols), function(v){
  sapply(1:ncol(scadTable[[v]]), function(x) names(sort(t(scadTable[[v]])[x,], decreasing=T)))
}) %>% set_names(cols)
scadGrpRank$CPI[1:5,]

lassoGrpRank$CPI[1:5,]
