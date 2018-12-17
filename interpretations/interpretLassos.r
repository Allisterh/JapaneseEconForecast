nonzeros <- rbind(DILASSOnonzero, NA,NA,
                  LASSOnonzero, NA,NA,
                  ENETnonzero, NA,NA,
                  gLASSOnonzero, NA,NA, 
                  SCADnonzero) %>% 
  set_colnames(varNameShort)

lambdas <- rbind(DILASSOlambda, NA,NA,
                 LASSOlambda, NA,NA,
                 ENETlambda, NA,NA,
                 gLASSOlambda, NA,NA,
                 SCADlambda) %>% 
  set_colnames(varNameShort)