#### Script to create simulation datasets ######################################

#' This script runs functions simulateTempData and simulateBioData to produce
#' simulated datasets across a range of scenarios

#### Set up ####################################################################

#### load packages ####

library(tidyverse)

#### source code ####

source("./Functions/simulateTempData.R")
source("./Functions/simulateBioData.R")

#### Set up scenarios ##########################################################

### Inputs for Temperature Data 

# noise in non-window segment varies from 1 to 10
noise <- seq(1, 10, length.out = 5)

# mean temperature in non-window segment varies from 0.1 to 10
mean <- seq(0.1, 10, length.out = 5)

# mean of window segment held at 10
meanSignal <- 10

# noise in the window segment held at 0.5
noiseSignal <- 0.5

# number of years varies from 10 to 100
numYears <- seq(10, 100, 20)

# number of days fixed at 100
numDays <- 100

# windowOpen from 1 to 50
windowOpen <- c(1, 25, 50)

# windowDuration from 1-30
windowDuration <- c(1, 15, 30)

# seed from 1:100 but testing at 1 :10
seed <- seq(1, 100, 1)

### Want to run each combination 100 times initially

# expand a grid to all unique combinations - need to be in same order as in 
# function to make sure it can be used with pmap
temperatureInputs <- expand_grid(seed,
                                 noise,
                                 mean,
                                 meanSignal,
                                 noiseSignal,
                                 numYears,
                                 numDays,
                                 windowOpen,
                                 windowDuration) 

### run in pmap

simulatedTempData <- pmap(temperatureInputs, .f = simulateTempData)

saveRDS(simulatedTempData, file = "./Data/SimulatedTempData1.RDS")
write.csv(temperatureInputs, file = "./Data/TemperatureInputs1.csv")
