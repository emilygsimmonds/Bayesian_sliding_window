#### Check script: Model Running Functions #####################################

#' This script checks the model and running functions in scripts:
#' 'nimbleModel.R'
#' 'runNimbleModel.R'

#### Set up ####################################################################

#### load packages ####

library(nimble)
library(nimbleEcology)
library(tidyverse)
library(MCMCvis)
library(furrr)
library(tictoc)

#### source code ####

source("./Functions/nimbleSlidingWindow.R")
source("./Functions/nimbleModel.R")
source("./Functions/runNimbleModel.R")
source("./Functions/createDataInput.R")

#### load data ####

temperatureData <- readRDS("./Data/SimulatedTempData1.RDS")
biologicalData <- readRDS("./Data/SimulatedBioData1.RDS")

#### Parallel set up ###########################################################

# run defineNimbleModel function with integer selected

slidingWindowModel <- defineNimbleModel(slidingWindowType = "integer")

# combine the simulated datasets to create dataInput

dataInputs <- map2(.x = rep(temperatureData, 
                            (length(biologicalData)/
                                     length(temperatureData))), 
                   .y = biologicalData, .f = createDataInput)

## need to create an input dataframe which will feed in all necessary parameters
# to the model run: parameters need to be in the following order -
# code, data, constants, inits, niter, nburnin, nchains, monitors, thin

modelInputs <- data.frame(code = slidingWindowModel,
                          data = dataInput)

code = slidingWindowModel
data = dataInput
constants
inits
niter = niter
nburnin = nburnin
nchains = nchains 
monitors = parametersToMonitor
thin = nthin


#### Parallel running ##########################################################


# check time for 5 runs sequential
plan(sequential)

tic()
future_pmap()
toc()

# check time for 5 runs parallel - should be faster
plan(multisession, workers = 5)

tic()
future_pmap()
toc()
