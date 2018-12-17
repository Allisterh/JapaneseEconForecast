# # MSFEs. First row in each block represents AR model's MSFE and the rest MSFEs relative to AR
for (k in 1:4) {
  if (k==1) foo <- rbind(ARabs=MSFEs[[k]]["AR",], MSFE2[[k]][-1,]) else
    foo <- rbind(foo, rbind(NA,NA,ARabs=MSFEs[[k]]["AR",], MSFE2[[k]][-1,]))
  if (k==4) {write.xlsx(foo, "results/excel/foo.xlsx", rowNames=T); rm(foo,k)}
}

# Model ranking by MSFE, returns model name (best model in first row, worst in bottom)
for (k in 1:4){
  if (k==1) foo <- MSFE3[[k]] else
    foo <- rbind(foo, rbind("","",MSFE3[[k]]))
  if (k==4) {write.xlsx(foo, "results/excel/modelRank.xlsx"); rm(foo,k)}
}

# Model ranking part 2. Aligned models in rows and standings in each cell. 
for (k in 1:4){
  if (k==1) foo <- MSFE4[[k]] else
    foo <- rbind(foo, rbind(NA,NA, MSFE4[[k]]))
  if (k==4) {write.xlsx(foo, "results/excel/rankings.xlsx"); rm(foo,k)}
}

# comparison table by models 
for (k in 1:4){
  if (k==1) foo <- compTable[[k]] else
    foo <- rbind(foo, rbind(NA,NA, compTable[[k]]))
  if (k==4) {write.xlsx(foo, "results/excel/compTable.xlsx", rowNames=T, keepNA=T); rm(foo,k)}
}

write.xlsx(nonzeros, "results/excel/nonzeros.xlsx", rowName=T)
write.xlsx(lambdas, "results/excel/lambdas.xlsx", rowName=T)