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

#### Set up ####################################################################

#### load packages ####

library(nimble)
library(MCMCvis)

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
  
  #modelRun <- nimbleMCMC(code = slidingWindowModel, 
  #                     data = dataInput,
  #                     constants = constants,
  #                     inits = inits,
  #                     monitors = parametersToMonitor,
  #                     niter = niter,
  #                     nburnin = nburnin,
  #                     nchains = nchains,
  #                     thin = nthin,
  #                     setSeed = seed)

  
  return(modelRun)
  
}
