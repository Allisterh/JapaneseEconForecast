
# create placeholders -----------------------------------------------------

models <- c("AR","DI","DILASSO","LASSO","ENET","gLASSO")
msfe <- matrix(NA, nrow=length(models), ncol=length(targetVariables),
               dimnames = list(models, targetVariables)) # listed by horizon, each list model x variable
MSFEs <- list(msfe, msfe, msfe, msfe)
names(MSFEs) <- names(MSFEs) <- c(paste("h", hChoises, sep=""))
rm(models, msfe)

ARlags <- list() # listed by variable, each list horizon x window
ARlagsCV <- list()

DIlags <- list()
DIfactor <- matrix(NA, nrow=length(hChoises), ncol=winSize, dimnames=list(c(paste("h=",hChoises,sep="")))) # number of factors
DIfactorDyn <- matrix(NA, nrow=length(hChoises), ncol=winSize, dimnames=list(c(paste("h=",hChoises,sep="")))) # nr. dynamic fac
DIfactorR2 <- matrix(NA, nrow=length(hChoises), ncol=winSize, dimnames=list(c(paste("h=",hChoises,sep="")))) # retained variance of X
DIinterpret <- matrix(NA, nrow=length(hChoises), ncol=ncol(dat), dimnames=list(paste("h=",hChoises,sep=""), names(dat))) # 
DIfactorList <- list()

DILASSOcoefs <- list()
DILASSOlambda <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                        dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
DILASSOsparsityRatio <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                               dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
DILASSOnonzero <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                        dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))

LASSOcoefs <- list() # listed by variable and horizon, each list window x horizon
LASSOlambda <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                      dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
LASSOsparsityRatio <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                       dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
LASSOnonzero <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                       dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))

ENETcoefs <- list() # listed by variable and horizon, each list window x horizon
ENETalpha <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                    dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
ENETlambda <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                    dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
ENETsparsityRatio <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                            dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
ENETcv <- list() # listed by variable and horizon, each list nr-of-lambda option and nr-of-alpha
ENETnonzero <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                      dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))

gLASSOcoefs <- list() # listed by variable and horizon, each list window x horizon
gLASSOlambda <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                      dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
gLASSOsparsityRatio <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                       dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
gLASSOnonzero <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                        dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))

