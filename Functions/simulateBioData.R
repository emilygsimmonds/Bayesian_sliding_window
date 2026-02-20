#### Function to simulate the biological data ################################## 

#' This script holds the function code for simulating biological data
#' 
#' Single function that has different options
#' 
#' Inputs:
#' - noise = numeric values from 1-10 indicating the standard deviation
#' in the regression
#' - intercept = intercept of the regression model
#' - slope = numeric values of the slope in the regression
#' - tempData = temperature data LIST
#' - windowOpen = index on when to open the window
#' - windowDuration = index on how long to open the window
#' 
#' Output:
#' - dataset of biological 'phenology' data

#### Set up ####################################################################

### Load packages ####

library(tidyverse)

#### Function code #############################################################

simulateBioData <- function(noise,
                            slope,
                            intercept,
                            tempData,
                            windowOpen,
                            windowDuration){
  
biologicalVariable <- map(.x = tempData, function(.x){
# create a biological variable that responds to part of the temperature series
  windowTemp <- mean(.x[windowOpen:(windowOpen + windowDuration),1])
  
  set.seed(2026)
  biologicalVariable <- rnorm(1, 
                              mean = intercept + 
                                (windowTemp*slope),
                              sd = noise)
  
})}