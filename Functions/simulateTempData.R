#### Function to simulate the temperature data ################################# 

#' This script holds the function code for simulating temperature data
#' 
#' Single function that has different options
#' 
#' Inputs:
#' - noise = numeric values from 1-10 indicating the standard deviation
#' in the non-window part of the time series
#' - mean = mean of the non-window days
#' - meanSignal = the mean of the 'window' 
#' - noiseSignal = standard deviation of the 'window'
#' - numYears = number of years to simulate for
#' - numDays = number of days to simulate for
#' - windowOpen = index on when to open the window
#' - windowDuration = index on how long to open the window
#' 
#' Output:
#' - list with each element being a single year and all entries within the
#' year being days of temperature

#### Set up ####################################################################

### Load packages ####

library(tidyverse)

#### Function code #############################################################

simulateTempData <- function(noise,
                             mean,
                             meanSignal,
                             noiseSignal,
                             numYears,
                             numDays,
                             windowOpen,
                             windowDuration){
  
# First, set up an index of years and days to iterate over
years <- as.list(1:numYears) # just an index of years
set.seed(2026)
# index of year effect - sd = 1/5 mean
yearEffect <- as.list(rnorm(numYears, 
                            mean = meanSignal, 
                            sd = meanSignal/5)) 

temperatureVariable <- map2(.x = years, .y = yearEffect, function(.x, .y){
  set.seed(.x)
  keyWindow <- rnorm(windowDuration, mean = .y,
                     sd = noiseSignal)
  set.seed(.x)
  preWindow <- rnorm(windowOpen-1, 
                     mean = mean,
                     sd = noise)
  set.seed(.x)
  postWindow <- rnorm((numDays-(windowOpen+windowDuration-1)), 
                      mean = mean, 
                      sd = noise)
  set.seed(.x)
  yearTemperature <- data.frame(year = c(preWindow,
                                            keyWindow,
                                            postWindow))
  colnames(yearTemperature) <- c(paste0("year", .x))
  return(yearTemperature)
}
)

return(temperatureVariable)

}
