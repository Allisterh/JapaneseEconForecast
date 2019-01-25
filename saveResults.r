varNameShort <- scan("txt/targetVariablesShort.txt", character(), quiet=T)


# give names to lists -----------------------------------------------------

names(ARlags) <- varNameShort
names(DIlags) <- varNameShort
names(DIfactorList) <- c(paste("h", hChoises, sep=""))
names(DICVr2) <- varNameShort
names(DICVlags) <- varNameShort
names(DICVfactorList) <- c(paste("h", hChoises, sep=""))
names(DICVfactorCVlist) <- c(paste("h", hChoises, sep=""))
names(DILASSOcoefs) <- varNameShort
names(DILASSOr2) <- varNameShort
names(LASSOcoefs) <- varNameShort
names(ENETcoefs) <- varNameShort
names(ENETcv) <- varNameShort
names(gLASSOcoefs) <- varNameShort



saveRDS(ARlags, "results/ARlags.rds")

saveRDS(DIfactor, "results/DI/DIfactor.rds")
saveRDS(DIlags,"results/DI/DIlags.rds")
saveRDS(DIfactorList, "results/DI/DIfactorList.rds")

saveRDS(DICVlags, "results/DICV/DICVlags.rds")
saveRDS(DICVfactor, "results/DICV/DICVfactor.rds")
saveRDS(DICVfactorCVlist, "results/DICV/DICVfactorCVlist.rds")
saveRDS(DICVfactorList, "results/DICV/DICVfactorList.rds")
saveRDS(DICVr2, "results/DICV/DICVr2.rds")

saveRDS(DILASSOcoefs, "results/DILASSO/DILASSOcoefs.rds")
saveRDS(DILASSOlambda, "results/DILASSO/DILASSOlambda.rds")
saveRDS(DILASSOnonzero, "results/DILASSO/DILASSOnonzero.rds")
saveRDS(DILASSOr2, "results/DILASSO/DILASSOr2.rds")


saveRDS(LASSOcoefs, "results/LASSO/LASSOcoefs.rds")
saveRDS(LASSOlambda, "results/LASSO/LASSOlambda.rds")
saveRDS(LASSOnonzero, "results/LASSO/LASSOnonzero.rds")

saveRDS(ENETcoefs, "results/ENET/ENETcoefs.rds")
saveRDS(ENETalpha, "results/ENET/ENETalpha.rds")
saveRDS(ENETlambda, "results/ENET/ENETlambda.rds")
saveRDS(ENETcv, "results/ENET/ENETcv.rds")
saveRDS(ENETnonzero, "results/ENET/ENETnonzero.rds")

saveRDS(gLASSOcoefs, "results/gLASSO/gLASSOcoefs.rds")
saveRDS(gLASSOlambda, "results/gLASSO/gLASSOlambda.rds")
saveRDS(gLASSOnonzero, "results/gLASSO/gLASSOnonzero.rds")
