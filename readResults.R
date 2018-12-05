
ARlags <- readRDS("results/ARlags.rds")
VARlags <- readRDS("results/VARlags.rds")

DIfactor <- readRDS("results/DI/DIfactor.rds")
DIfactorDyn <- readRDS("results/DI/DIfactorDyn.rds")
DIfactorR2 <- readRDS("results/DI/factorR2.rds")
DIlags <- readRDS("results/DI/DIlags.rds")
DIfactorList <- readRDS("results/DI/DIfactorList.rds")

LASSOcoefs <- readRDS("results/LASSO/LASSOcoefs.rds")
LASSOlambda <- readRDS("results/LASSO/LASSOlambda.rds")
LASSOsparsityRatio <- readRDS("results/LASSO/LASSOsparsityRatio.rds")
LASSOnonzero <- readRDS("results/LASSO/LASSOnonzero.rds")

LASSO2coefs <- readRDS("results/LASSO2/LASSO2coefs.rds")
LASSO2lambda <- readRDS("results/LASSO2/LASSO2lambda.rds")
LASSO2sparsityRatio <- readRDS("results/LASSO2/LASSO2sparsityRatio.rds")
LASSO2nonzero <- readRDS("results/LASSO2/LASSO2nonzero.rds")

LASSO3coefs <- readRDS("results/LASSO3/LASSO3coefs.rds")
LASSO3lambda <- readRDS("results/LASSO3/LASSO3lambda.rds")
LASSO3sparsityRatio <- readRDS("results/LASSO3/LASSO3sparsityRatio.rds")
LASSO3nonzero <- readRDS("results/LASSO3/LASSO3nonzero.rds")

LASSO4coefs <- readRDS("results/LASSO4/LASSO4coefs.rds")
LASSO4lambda <- readRDS("results/LASSO4/LASSO4lambda.rds")
LASSO4sparsityRatio <- readRDS("results/LASSO4/LASSO4sparsityRatio.rds")
LASSO4nonzero <- readRDS("results/LASSO4/LASSO4nonzero.rds")

ENETcoefs <- readRDS("results/ENET/ENETcoefs.rds")
ENETalpha <- readRDS("results/ENET/ENETalpha.rds")
ENETsparsityRatio <- readRDS("results/ENET/ENETsparsityRatio.rds")
ENETcv <- readRDS("results/ENET/ENETcv.rds")
ENETlambda <- readRDS("results/ENET/ENETlambda.rds")
ENETnonzero <- readRDS("results/ENET/ENETnonzero.rds")

ENET2coefs <- readRDS("results/ENET2/ENET2coefs.rds")
ENET2alpha <- readRDS("results/ENET2/ENET2alpha.rds")
ENET2sparsityRatio <- readRDS("results/ENET2/ENET2sparsityRatio.rds")
ENET2cv <- readRDS("results/ENET2/ENET2cv.rds")
ENET2lambda <- readRDS("results/ENET2/ENET2lambda.rds")
ENET2nonzero <- readRDS("results/ENET2/ENET2nonzero.rds")

gLASSOcoefs <- readRDS("results/gLASSO/gLASSOcoefs") # listed by variable and horizon, each list window x horizon
gLASSOlambda <- readRDS("results/gLASSO/gLASSOlambda")
gLASSOsparsityRatio <- readRDS("results/gLASSO/gLASSOsparsityRatio")
gLASSO2nonzero <- readRDS("results/gLASSO/gLASSOnonzero")

gLASSO2coefs <- readRDS("results/gLASSO2/gLASSO2coefs") # listed by variable and horizon, each list window x horizon
gLASSO2lambda <- readRDS("results/gLASSO2/gLASSO2lambda")
gLASSO2sparsityRatio <- readRDS("results/gLASSO2/gLASSO2sparsityRatio")
gLASSO2nonzero <- readRDS("results/gLASSO2/gLASSO2nonzero")

MSFEs <- readRDS("results/MSFEs.rds")
