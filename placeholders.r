
# create placeholders -----------------------------------------------------

models <- c("AR","DI","DICV","DILASSO","LASSO","ENET","gLASSO")
msfe <- matrix(NA, nrow=length(models), ncol=length(targetVariables),
               dimnames = list(models, targetVariables)) # listed by horizon, each list model x variable
MSFEs <- list(h1=msfe, h3=msfe, h12=msfe)
names(MSFEs) <- names(MSFEs) <- c(paste("h", hChoises, sep=""))
rm(models, msfe)

ARlags <- list() # listed by variable, each list horizon x window

DIlags <- list()
DIfactor <- matrix(NA, nrow=length(hChoises), ncol=winSize, dimnames=list(c(paste("h=",hChoises,sep="")))) # number of factors
DIfactorList <- list()

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
ENETcv <- list() # listed by variable and horizon, each list nr-of-lambda option and nr-of-alpha
ENETnonzero <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                      dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))

gLASSOcoefs <- list() # listed by variable and horizon, each list window x horizon
gLASSOlambda <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                      dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
gLASSOnonzero <- matrix(NA, nrow=length(hChoises), ncol=length(targetVariables),
                        dimnames = list(c(paste("h=",hChoises,sep="")),targetVariables))
