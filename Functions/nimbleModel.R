#### Code for Nimble model #####################################################

#' This script holds the code for the Nimble model. Want to be able to select
#' the different type of sliding window function. Therefore, have different
#' code for the integer or weighted mean options.
#' 
#' 
#' Inputs are:
#' slidingWindowType = either "integer" or "weighted"

#### Set up ####################################################################

#### load packages ####

library(nimble)

#### model code - as a function ####

defineNimbleModel <- function(slidingWindowType = c("integer",
                                              "weighted")){
  
if(slidingWindowType == "integer"){
slidingWindowModel <- nimbleCode({
  
#-------------------------------------------------------------------------------
## DEFINE PRIORS

open ~ dunif(windowStarts[1], windowStarts[2]-1) 
duration ~ dunif(windowDurations[1], windowDurations[2])
intercept ~ dnorm(50, sd = 100)
slope ~ dnorm(0, sd = 10)
error ~ dgamma(2, 1)

#-------------------------------------------------------------------------------
## CALCULATE WINDOW

for(i in 1:length(years)){
  
  temperatureWindow[i] <- nimbleSlidingWindow(open,
                                              duration,
                                              temperature[,i])

#-------------------------------------------------------------------------------
## LIKELIHOOD FOR BIOLOGICAL VARIABLE

  biological_variable[i] ~ dnorm((intercept 
                                + (temperatureWindow[i]*slope)), 
                                 sd = error)
  
}
  
#-------------------------------------------------------------------------------
## Calculate meaningful parameters for window

# have a think about this
#resultWindowClose <- windowStarts[2] - open 
#resultWindowOpen <- windowStarts[2] - (open + duration)

})}else{
slidingWindowModel <- nimbleCode({
  
  #-------------------------------------------------------------------------------
  ## DEFINE PRIORS
  
  open ~ dunif(windowStarts[1], windowStarts[2]-1) 
  duration ~ dunif(windowDurations[1], windowDurations[2])
  intercept ~ dnorm(50, sd = 100)
  slope ~ dnorm(0, sd = 10)
  error ~ dgamma(2, 1)
  
  #-------------------------------------------------------------------------------
  ## CALCULATE WINDOW
  
  for(i in 1:length(years)){
    
    temperatureWindow[i] <- nimbleWeightedSlidingWindow(open,
                                                duration,
                                                temperature[,i])
    
    #-------------------------------------------------------------------------------
    ## LIKELIHOOD FOR BIOLOGICAL VARIABLE
    
    biological_variable[i] ~ dnorm((intercept 
                                    + (temperatureWindow[i]*slope)), 
                                   sd = error)
    
  }
  
  #-------------------------------------------------------------------------------
  ## Calculate meaningful parameters for window
  
  # have a think about this
  #resultWindowClose <- windowStarts[2] - open 
  #resultWindowOpen <- windowStarts[2] - (open + duration)
  
})
}
}
