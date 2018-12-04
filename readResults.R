
ARlags <- readRDS("results/ARlags.rds")
VARlags <- readRDS("results/VARlags.rds")

DIfactor <- readRDS("results/DIfactor.rds")
DIfactorDyn <- readRDS("results/DIfactorDyn.rds")
DIfactorR2 <- readRDS("results/DIfactorR2.rds")
DIlags <- readRDS("results/DIlags.rds")
DIfactorList <- readRDS("results/DIfactorList.rds")

LASSOcoefs <- readRDS("results/LASSO/LASSOcoefs.rds")
LASSOlambda <- readRDS("results/LASSO/LASSOlambda.rds")
LASSOsparsityRatio <- readRDS("results/LASSO/LASSOsparsityRatio.rds")
LASSOnonzero <- readRDS("results/LASSO/LASSOnonzero.rds")

LASSO2coefs <- readRDS("results/LASSO2/LASSO2coefs.rds")
LASSO2lambda <- readRDS("results/LASSO2/LASSO2lambda.rds")
LASSO2sparsityRatio <- readRDS("results/LASSO2/LASSO2sparsityRatio.rds")
LASSO2nonzero <- readRDS("results/LASSO2/LASSO2nonzero.rds")

ENETcoefs <- readRDS("results/ENETcoefs.rds")
ENETalpha <- readRDS("results/ENETalpha.rds")
ENETsparsityRatio <- readRDS("results/ENETsparsityRatio.rds")

# gLASSOcoefs <- readRDS("results/gLASSOcoefs") # listed by variable and horizon, each list window x horizon
# gLASSOlambda <- readRDS("results/gLASSOlambda")
# gLASSOsparsityRatio <- readRDS("results/gLASSOsparsityRatio")

MSFEs <- readRDS("results/MSFEs.rds")
