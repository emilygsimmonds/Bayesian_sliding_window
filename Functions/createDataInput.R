#### Script to create data input list for nimble model #########################

#' This script holds a function to take simulated datasets and produce a data
#' input list

#### Set up ####################################################################

#### load packages ####

library(tidyverse)

#### Function code #############################################################

createDataInput <- function(temperatureData,
                            biologicalData){
  
  biologicalData <- unlist(biologicalData)
  
  # take the input dataframes and combine into a list
  dataInput <- list(temperature = temperatureData,
                    # mean centre the biological variable
                    biologicalVariable = biologicalData - mean(biologicalData))
  
  return(dataInput)
  
}