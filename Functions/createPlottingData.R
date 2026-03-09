#### Script to create datafile for plotting results ############################

#' This script holds a function to take nimble model chains and create a 
#' summary dataframe with one row per model
#' 
#' Inputs = model result file name and list of simulation inputs that created it
#' 
#' Output = dataframe with result summary linked to scenario

#### Set up ####################################################################

#### load packages ####

library(tidyverse)
library(MCMCvis)

#### Function code #############################################################

createPlottingData <- function(modelResult, inputs){
  
  # first read in the data
  modelResult <- readRDS(paste0("./Data/ModelResults/", modelResult))
  
  summary <- MCMCsummary(modelResult)
  
  # variables for plotting = posterior mean, lowerCI, upper CI, Rhat, n.eff,
  # parameter, scenario
  
  output <- data.frame(mean = summary$mean, 
             lowerCI = summary$`2.5%`,
             upperCI = summary$`97.5%`,
             rhat = summary$Rhat,
             n.eff = summary$n.eff,
             parameter = row.names(summary),
             tScenario = inputs$tScenario,
             bScenario = inputs$bScenario,
             trueBioNoise = inputs$bioNoise,
             trueSlope = inputs$slope,
             trueTNoise = inputs$noise,
             trueTMean = inputs$mean,
             trueOpen = inputs$windowOpen,
             trueDuration = inputs$windowDuration)
  
  return(output)
  
}