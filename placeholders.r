
# create placeholders -----------------------------------------------------

models <- c("AR","DI","DICV","DILASSO","LASSO","ENET","gLASSO", "ER", "GR") # we compare nine approaches
msfe <- matrix(NA, nrow=length(models), ncol=length(targetVariables),
               dimnames = list(models, targetVariables)) # listed by horizon, each list model x variable
MSFEs <- list(h1=msfe, h3=msfe, h12=msfe)
# names(MSFEs) <- names(MSFEs) <- c(paste("h", hChoises, sep="")) ## idk why i put this line; going to delete if not needed
rm(models, msfe) # delete two intermediary products

ARlags <- list() # listed by variable, each list horizon x window

DIlags <- list()
DIfactor <- matrix(NA, nrow=length(hChoises), ncol=winSize, dimnames=list(c(paste("h=",hChoises,sep="")))) # number of factors
DIfactorList <- list()
DIeig <- list()

DICVfactorList <- list()
DICVfactorCVlist <- list()
DICVlags <-list()
DICVfactor <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                     dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
DICVr2<- list()

DILASSOcoefs <- list()
DILASSOlambda <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                        dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
DILASSOnonzero <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                        dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
DILASSOr2 <- list()

DIERlags <- list()
DIGRlags <- list()
DIfactorLag <- list()

MSFE_DIlag <- list()
LASSOkfcvLambda <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                          dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
# MSFE_DIlag[[1]] <- matrix(NA,6,20, dimnames=list(c("AR","F1","F2","F4","BN","CV"),targetVariables))

LASSOcoefs <- list() # listed by variable and horizon, each list window x horizon
LASSOlambda <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                      dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
LASSOnonzero <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                       dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))

ENETcoefs <- list() # listed by variable and horizon, each list window x horizon
ENETalpha <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                    dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
ENETlambda <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                    dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
ENETnonzero <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                      dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))

gLASSOcoefs <- list() # listed by variable and horizon, each list window x horizon
gLASSOcoefsLong <- list() # long matrix of glassocoefs
gLASSOlambda <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                      dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
gLASSOnonzero <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                        dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))


