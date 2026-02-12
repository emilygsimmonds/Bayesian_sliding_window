#### Nimble function to run Nimble sliding window ############################## 

#' This script holds the function code for the sliding window Nimble function.

#### Set up ####################################################################

### Load packages ####

library(nimble)

#### Function code ####

# first need a nimble function for calculation of mean temperature in window
nimbleSlidingWindow <- nimbleFunction(
  run = function(open = double(0), 
                 duration = double(0),
                 temperature = double(1)) { # type declarations 1 = vector
    
    return(mean(temperature[round(open):round(open+duration)]))
    
    returnType(double(0))  # return type declaration
  } )

