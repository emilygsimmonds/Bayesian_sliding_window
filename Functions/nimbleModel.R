#### Function to run Nimble model #### 

#' This script holds the function code for the Nimble model.
#' 
#' Inputs are:
#' windowOpen = earliest day to search from
#' shortestWindow = shortest length to search
#' refDay = day you want to reference from should be the index that day falls on
#' intercept
#' slope
#' data_input (input data list)
#' constants, inits, n_iter, n_chains, n_burnin

nimbleModel <- function(data_input,
                        parametersToMonitor,
                        constants,
                        inits, n_iter, n_chains, n_burnin,
                        n_thin){
  
source("./Functions/nimbleSlidingWindow.R")

test_sliding_window <- nimbleCode({
  
#-------------------------------------------------------------------------------
## DEFINE PRIORS

open ~ dunif(windowOpen, refDay-1) 
#duration ~ dunif(shortestWindow, (refDay-windowOpen)-open+1)
duration ~ dunif(shortestWindow, 20)
intercept ~ dnorm(meanintercept, sd = 50)
slope ~ dnorm(meanslope, sd = 10)
#error ~ dgamma(2, 0.5)
error ~ dunif(0, 5)

#-------------------------------------------------------------------------------
## CALCULATE WINDOW

for(i in 1:length(years)){
  
#  temperature_window[i] <- nimbleSlidingWindow(open,
#                                             duration,
#                                             temperature[,i])
#temperature_window[i] <- mean(temperature[1:50,i])
  temperature_window[i] <- 5

#-------------------------------------------------------------------------------
## LIKELIHOOD FOR BIOLOGICAL VARIABLE

biological_variable[i] ~ dnorm((intercept 
                                + (temperature_window[i]*slope)), 
                                 sd = error)
  
}
  
#-------------------------------------------------------------------------------
## Calculate meaningful parameters for window

resultWindowClose <- refDay - open 
#resultWindowOpen <- 50 - (open + duration)
resultWindowOpen <- refDay - (open + duration)

})

output <- nimbleMCMC(code = test_sliding_window, 
           data = data_input,
           constants = constants,
           inits = inits,
           monitors = parametersToMonitor,
           niter = n_iter,
           nburnin = n_burnin,
           nchains = n_chains,
           thin = n_thin)

return(output)

}
