#### Function to run Nimble model ##############################################

#' This script holds the function code to run the slidingWindowModel.
#' 
#' Inputs are:
#' dataInput (input data list) including biological data and temperature
#' constants = any constants that go into the model e.g. number of years
#' inits = initial values for all sampled parameters
#' niter = number of iterations
#' nchains = number of chains
#' nburnin = amount of burn in
#' parametersToMonitor = list of parameter names to be tracked and reported
#' nthin = thinning parameter
#' seed = needs to be length nchains
#' cMCMC = pass this compiled object to the function

#### Set up ####################################################################

#### load packages ####

library(nimble)
library(MCMCvis)
library(coda)

#### function code ####

runNimbleModel <- function(slidingWindowType = c("integer",
                                              "weighted"),
                        dataInput,
                        constants,
                        inits, 
                        niter, 
                        nchains, 
                        nburnin,
                        parametersToMonitor,
                        nthin,
                        seed){

  source("./Functions/nimbleModel.R")
  source("./Functions/nimbleSlidingWindow.R")
  
  slidingWindowModel <- defineNimbleModel(slidingWindowType = 
                                            slidingWindowType)
# compile the model
  
# then run in Nimble and return the output  
  
  rModel <- nimbleModel(code = slidingWindowModel, 
                      data = dataInput, 
                      constants = constants, 
                      inits = inits)
 
  cModel <- compileNimble(rModel) 

  conf <- configureMCMC(rModel, 
                       monitors = parametersToMonitor) 
 
  rMCMC <- buildMCMC(conf) 
 
  cMCMC <- compileNimble(rMCMC, 
                        project = rModel) 
 
  modelRun <- runMCMC(cMCMC, 
                      niter = niter, 
                      nburnin = nburnin, 
                      thin = nthin, 
                      nchains = nchains, 
                      setSeed = seed)
  
  # trying to clear the compiled objects to release memory
 
 rm(dataInput)
 rm(slidingWindowModel)
 rm(rModel)

 nimbleOptions(clearNimbleFunctionsAfterCompiling = TRUE)

 gc()
 
 # make a summary object to return to reduce memory load
 
 summaryModel <- MCMCsummary(modelRun)
 
 # then calculate geweke score for each chain and add
 
 for(i in 1:nchains){
 
 summaryModel[7+i] <- geweke.diag(coda::as.mcmc(modelRun[[i]], 
                                                       thin = nthin))$z
 colnames(summaryModel)[7+i] <- paste0("gewekeChain", i) 
 }
 
 
 
 return(summaryModel)
  
}
