
ARlags <- readRDS("results/ARlags.rds")

DIfactor <- readRDS("results/DI/DIfactor.rds")
DIfactorDyn <- readRDS("results/DI/DIfactorDyn.rds")
DIfactorR2 <- readRDS("results/DI/DIfactorR2.rds")
DIlags <- readRDS("results/DI/DIlags.rds")
DIfactorList <- readRDS("results/DI/DIfactorList.rds")
DIinterpret <- readRDS("results/DI/DIinterpret.rds")

DILASSOcoefs <- readRDS("results/DILASSO/DILASSOcoefs.rds")
DILASSOlambda <- readRDS("results/DILASSO/DILASSOlambda.rds")
DILASSOnonzero <- readRDS("results/DILASSO/DILASSOnonzero.rds")
DILASSOsparsityRatio <- readRDS("results/DILASSO/DILASSOsparsityRatio.rds")

LASSOcoefs <- readRDS("results/LASSO/LASSOcoefs.rds")
LASSOlambda <- readRDS("results/LASSO/LASSOlambda.rds")
LASSOsparsityRatio <- readRDS("results/LASSO/LASSOsparsityRatio.rds")
LASSOnonzero <- readRDS("results/LASSO/LASSOnonzero.rds")

ENETcoefs <- readRDS("results/ENET/ENETcoefs.rds")
ENETalpha <- readRDS("results/ENET/ENETalpha.rds")
ENETsparsityRatio <- readRDS("results/ENET/ENETsparsityRatio.rds")
ENETcv <- readRDS("results/ENET/ENETcv.rds")
ENETlambda <- readRDS("results/ENET/ENETlambda.rds")
ENETnonzero <- readRDS("results/ENET/ENETnonzero.rds")

gLASSOcoefs <- readRDS("results/gLASSO/gLASSOcoefs.rds") # listed by variable and horizon, each list window x horizon
gLASSOlambda <- readRDS("results/gLASSO/gLASSOlambda.rds")
gLASSOsparsityRatio <- readRDS("results/gLASSO/gLASSOsparsityRatio.rds")
gLASSOnonzero <- readRDS("results/gLASSO/gLASSOnonzero.rds")

SCADcoefs <- readRDS("results/SCAD/SCADcoefs.rds")
SCADlambda <- readRDS("results/SCAD/SCADlambda.rds")
SCADsparsityRatio <- readRDS("results/SCAD/SCADsparsityRatio.rds")
SCADnonzero <- readRDS("results/SCAD/SCADnonzero.rds")



MSFEs <- readRDS("results/MSFEs.rds")
