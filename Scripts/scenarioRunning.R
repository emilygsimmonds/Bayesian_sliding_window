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

temperatureData <- readRDS("./Data/SimulatedTempDataBio1.RDS")
biologicalData <- readRDS("./Data/SimulatedBioData1.RDS")

#### Parallel set up ###########################################################

# run defineNimbleModel function with integer selected

slidingWindowModel <- defineNimbleModel(slidingWindowType = "integer")

# combine the simulated datasets to create dataInput

dataInputs <- map2(.x = temperatureData, 
                   .y = biologicalData, .f = createDataInput)

## need to create an input dataframe which will feed in all necessary parameters
# to the model run: parameters need to be in the following order -
# slidingWindowType, dataInput, constants, inits, niter, nburnin, 
# nchains, parametersToMonitor, nthin, seed

niter <- 50000
nburnin <- 5000
nchains <- 2
nthin <- 5
seed <- 1:nchains

constants <- list(numYears = 100,
                  windowStarts = c(1,50),
                  windowDurations = c(1,49))

set.seed(2026)
# sum of open and duration must be < numDays
inits <- list(open = round(runif(1,
                                 constants$windowStarts[1], 
                                 constants$windowStarts[2])),
              duration = round(runif(1, 
                                  constants$windowDurations[1], 
                                  constants$windowDurations[2])), 
              intercept = rnorm(1, 0, sd = 100),
              slope = rnorm(1, 0, sd = 10),
              error = rgamma(1, 2, 1))

parametersToMonitor = c("open",
                        "duration", 
                        "intercept",
                        "slope",
                        "error")

modelInputs <- data.frame(slidingWindowType = "weighted",
                          dataInputs,
                          constants,
                          inits,
                          niter,
                          nburnin,
                          nchains,
                          parametersToMonitor,
                          nthin,
                          seed) %>%
  rowid_to_column()


#### Parallel running ##########################################################


# check time for 5 runs sequential
plan(sequential)

tic()
testSequential <- future_pmap(modelInputs[1:5,], .f = runNimbleModel)
toc() # 

# check time for 5 runs parallel - should be faster
plan(multisession, workers = 5)

tic()
future_pmap()
toc()
