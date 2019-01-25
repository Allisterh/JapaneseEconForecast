
ARlags <- readRDS("results/ARlags.rds")

DIfactor <- readRDS("results/DI/DIfactor.rds")
DIlags <- readRDS("results/DI/DIlags.rds")

DICVfactor <- readRDS("results/DICV/DICVfactor.rds")
DICVlags <- readRDS("results/DICV/DICVlags.rds")
DICVr2 <- readRDS("results/DICV/DICVr2.rds")

DILASSOcoefs <- readRDS("results/DILASSO/DILASSOcoefs.rds")
DILASSOlambda <- readRDS("results/DILASSO/DILASSOlambda.rds")
DILASSOnonzero <- readRDS("results/DILASSO/DILASSOnonzero.rds")
DILASSOr2 <- readRDS("results/DILASSO/DILASSOr2.rds")

LASSOcoefs <- readRDS("results/LASSO/LASSOcoefs.rds")
LASSOlambda <- readRDS("results/LASSO/LASSOlambda.rds")
LASSOnonzero <- readRDS("results/LASSO/LASSOnonzero.rds")

ENETcoefs <- readRDS("results/ENET/ENETcoefs.rds")
ENETalpha <- readRDS("results/ENET/ENETalpha.rds")
ENETlambda <- readRDS("results/ENET/ENETlambda.rds")
ENETnonzero <- readRDS("results/ENET/ENETnonzero.rds")

gLASSOcoefs <- readRDS("results/gLASSO/gLASSOcoefs.rds") # listed by variable and horizon, each list window x horizon
gLASSOlambda <- readRDS("results/gLASSO/gLASSOlambda.rds")
gLASSOnonzero <- readRDS("results/gLASSO/gLASSOnonzero.rds")

MSFEs <- readRDS("results/MSFE/MSFEs.rds")
