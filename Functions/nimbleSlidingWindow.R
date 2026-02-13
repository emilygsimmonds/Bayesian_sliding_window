#### Nimble functions to run Nimble sliding window ############################## 

#' This script holds the function code for the sliding window Nimble function.
#' DEBUGGED USING ELM: 03 model
#' 
#' Has two possible functions to use for window. 
#' One uses integer indices for the window,
#' the other tapers the ends of window.
#' 
#' And a function to calculate a weighted mean

#### Set up ####################################################################

### Load packages ####

library(nimble)

#### Function code ####

### Window 1: integer indices

# using integer indices - so only whole days of temperature
nimbleSlidingWindow <- nimbleFunction(
  run = function(open = double(0), 
                 duration = double(0),
                 temperature = double(1)) { # type declarations 1 = vector
    
    return(mean(temperature[trunc(open):ceiling(open+duration)]))
    
    returnType(double(0))  # return type declaration
  } )

### Window 2: tapered first and last day

# weighting the first and last day in the window so it can include partial days
nimbleWeightedSlidingWindow <- nimbleFunction(
  run = function(open = double(0), 
                 duration = double(0),
                 temperature = double(1)) { # type declarations 1 = vector
    # set weight outside of function - first all to 1
    # subtract 1 as otherwise R creates a vector 1 longer than duration
    weight <- rep(1, length(trunc(open):ceiling(open+duration-1))) 
    # overwrite first and last entry to lower than 1
    weight[1] <- 1-(open-trunc(open))
    # need an if clause to deal with integers - only overwrite final weight if
    # the sum of open and duration is not an integer
    if((open+duration) - trunc(open+duration) != 0){
    weight[length(weight)] <- (open+duration) - trunc(open+duration)}
    windowMean <- weightedMeanNimble(values = c(temperature[trunc(open):ceiling(open+duration)]),
                                     weights = weight) # works
    return(windowMean)
    returnType(double(0))  # return type declaration
  } )

### Weighted mean function

weightedMeanNimble <- nimbleFunction(
  run = function(values = double(1),
                 weights = double(1)) {
    
    # take the variable values and make the numerator
    numerator <- sum(values*weights)
    
    # then the denominator
    denominator <- sum(weights)
    
    weightedMean <- numerator/denominator
    
    return(weightedMean)
    returnType(double(0))
  }
)
