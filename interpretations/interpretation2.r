
# Table 6 (rev) -----------------------------------------------------------
# Change the model/approaches accordingly (`LASSO`, `DICV`, etc)

varNameShort <- scan("txt/targetVariablesShort.txt", character(), quiet=T)

grp <- c(rep("Output and Income",14),
         rep("Employment and Hours", 16),
         rep("Retail, Manufacturing and Trade Sales",6),
         rep("Consumption",4),
         rep("Housing Starts and Sales",8),
         rep("Inventories and Orders",26),
         rep("Stock Price",4),
         rep("Exchange Rates",3),
         rep("Interest Rates",15),
         rep("Money and Credit Quality Aggregate",9),
         rep("Price Indexes and Wages",22))

# Reshape matrix  ---------------------------------------------------------

# First, we transform list of wide matrices into long ones (LASSOcoefs, DICVr2, etc).
# This is because the shape of the matrix in DICVr2 is not favorable for the analysis

# transformation for pen reg
foo<- list(LASSOcoefs, ENETcoefs, gLASSOcoefs)
qux <- c("LASSOcoefs", "ENETcoefs", "gLASSOcoefs")
for (r in 1:3){
  bar <- foo[[r]]
  baz <- lapply(bar, function(x){
    lapply(x, function(y){
      data.frame(win=rep(1:60,each=508),
                 lag=rep(1:4,each=127,times=60),
                 var=rep(1:127,times=240),
                 grp=rep(grp, times=240), # grp based on variables (lags ignored, note the diff to `glasso.r`)
                 coef=as.numeric(t(y))) # technically, this is not `coef`, but `r2` for DI 
    })
  })
  quux <- data.frame()
  for (j in 1:20){
    for (i in 1:3){
      quuz <- cbind(baz[[j]][[i]],hor=paste("h",hChoises[i],sep=""),target=varNameShort[[j]])
      quux <- rbind(quux,quuz)
    }
  }
  assign(paste(qux[r],"Long",sep=""),quux)
}

# transformation for DI
foo<- list(DICVr2, DILASSOr2)
qux <- c("DICVr2","DILASSOr2")
for (r in 1:2){
  bar <- foo[[r]]
  baz <- lapply(bar, function(x){
    lapply(x, function(y){
      data.frame(win=rep(1:60,each=127),
                 var=rep(1:127,times=60),
                 grp=rep(grp, times=60), # grp based on variables (lags ignored, note the diff to `glasso.r`)
                 coef=as.numeric(t(y))) # technically, this is not `coef`, but `r2` for DI 
    })
  })
  quux <- data.frame()
  for (j in 1:20){
    for (i in 1:3){
      quuz <- cbind(baz[[j]][[i]],hor=paste("h",hChoises[i],sep=""),target=varNameShort[[j]])
      quux <- rbind(quux,quuz)
    }
  }
  assign(paste(qux[r],"Long",sep=""),quux)
}


rm(foo, bar, baz, qux, quux, quuz,r,i,j)


# create table 6 ----------------------------------------------------------
## a matrix representing tha ranking of the frequency of each group being selected. (`ranking`)

## Version 2: Variables are regarded as "selected" if one out of four lags is selected. 
## Averaged over window. Summrised by groups and horizons
met <- list(LASSOcoefsLong, ENETcoefsLong, gLASSOcoefsLong)
foo <- c("LASSO", "ENET", "gLASSO")
tar <- c("CPI","IPtotal","EmpInd")

ranking <- data.frame(row.names = 1:11)
for (h in 1:3) {
  for (i in 1:3){
    for (j in 1:3){
      bar <- met[[i]] %>%  # Change horizon and approach accordingly (`LASSO`/`H1`)
        group_by(win,var,grp,hor,target) %>%
        dplyr::summarise(coef=mean(coef)) %>% 
        mutate(coefBi = ifelse(coef!=0,1,0)) %>% # coefBi is 0 if one or more lags are selected
        group_by(grp,hor,target) %>%
        dplyr::summarise(coefGrp=mean(coefBi)) %>%
        dplyr::filter(target==tar[j] & hor==paste("h",hChoises[h],sep="")) %>% 
        mutate(rank=ifelse(coefGrp==0, NA,as.character(grp))) %>%
        dplyr::arrange(desc(coefGrp))
      ranking <- cbind(ranking, baz = bar$rank) %>% set_colnames(c(names(ranking), 
                                                        paste(tar[j],foo[i],paste("h",hChoises[h],sep=""),sep="")))
    }
  }
}

saveRDS(ranking, "results/table6.rds")

rm(foo, bar,i,j,h,tar,met)


# Table 7: Factor models -----------------------------------------------------------

met <- list(DICVr2Long, DILASSOr2Long)
foo <- c("DICV", "DILASSO")
tar <- c("CPI","IPtotal","EmpInd")

rankingDI <- data.frame(row.names = 1:11)
for (h in 1:3) { 
  for (i in 1:2){
    for (j in 1:3){
      bar <- met[[i]] %>%  # Change horizon and approach accordingly (`LASSO`/`H1`)
        group_by(grp, hor, target) %>% 
        dplyr::summarise(r2=mean(coef)) %>% 
        dplyr::filter(target==tar[j] & hor==paste("h",hChoises[h],sep="")) %>% 
        mutate(rank=ifelse(r2==0, NA, as.character(grp))) %>%
        dplyr::arrange(desc(r2))
      rankingDI <- cbind(rankingDI, baz = bar$rank) %>% set_colnames(c(names(rankingDI), 
                                                                   paste(tar[j],foo[i],paste("h",hChoises[h],sep=""),sep="")))
    }
  }
}
saveRDS(rankingDI, "results/table7.rds")



# Fig 2 -------------------------------------------------------------------
p1 <- DICVr2Long %>% 
  dplyr::filter(target=="CPI", hor=="h1") %>% 
  group_by(var) %>% 
  dplyr::summarise(r2 = mean(coef)) %>% 
  ggplot(aes(var, r2)) +
  geom_bar(stat="identity") +
  labs(x="DICV", y=expression(R^2)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

p2 <- DILASSOr2Long %>% 
  dplyr::filter(target=="CPI", hor=="h1") %>% 
  group_by(var) %>% 
  dplyr::summarise(r2 = mean(coef)) %>% 
  ggplot(aes(var, r2)) +
  geom_bar(stat="identity") +
  labs(x="DILASSO", y="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

gridExtra::arrangeGrob(p1,p2, nrow=1) %>% 
  ggsave("results/fig2a.eps", .,  device = "eps")
