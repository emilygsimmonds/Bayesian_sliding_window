#### Function to simulate the biological data ################################## 

#' This script holds the function code for simulating biological data
#' 
#' Single function that has different options
#' 
#' Inputs:
#' - seed to make repeatable, just a number
#' - bioNoise = numeric values from 1-10 indicating the standard deviation
#' in the regression
#' - intercept = intercept of the regression model
#' - slope = numeric values of the slope in the regression
#' - tempData = temperature data data frame
#' - windowOpen = index on when to open the window
#' - windowDuration = index on how long to open the window
#' 
#' Output:
#' - dataset of biological 'phenology' data

#### Set up ####################################################################

### Load packages ####

library(tidyverse)

#### Function code #############################################################

simulateBioData <- function(seed,
                            bioNoise,
                            slope,
                            intercept,
                            tempData,
                            windowOpen,
                            windowDuration){

windowTemp <- colMeans(tempData[windowOpen:(windowOpen + windowDuration),])

set.seed(seed)
biologicalVariable <- rnorm(ncol(tempData), 
                            mean = (intercept + 
                              (windowTemp*slope)),
                            sd = bioNoise)

return(biologicalVariable)

}