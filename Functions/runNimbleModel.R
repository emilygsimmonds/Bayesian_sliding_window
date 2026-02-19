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
#' 

#### Set up ####################################################################

#### load packages ####

library(nimble)

#### function code ####

nimbleModel <- function(slidingWindowType = c("integer",
                                              "weighted"),
                        dataInput,
                        constants,
                        inits, 
                        niter, 
                        nchains, 
                        nburnin,
                        parametersToMonitor,
                        nthin){

  
  source("./Functions/nimbleModel.R")
  source("./Functions/nimbleSlidingWindow.R")
  
  slidingWindowModel <- defineNimbleModel(slidingWindowType = 
                                            slidingWindowType)
  
# then run in Nimble and return the output  
  
  output <- nimbleMCMC(code = slidingWindowModel, 
                       data = dataInput,
                       constants = constants,
                       inits = inits,
                       monitors = parametersToMonitor,
                       niter = niter,
                       nburnin = nburnin,
                       nchains = nchains,
                       thin = nthin)
  
  return(output)
  
}
