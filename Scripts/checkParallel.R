#### Check script: Parallelisation #############################################

#' This script checks that the parallel running produces the same outputs as 
#' non-parallel and that it runs correctly

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

# pull a list of file names of biological data and the temperature data
temperatureFileNames <- read.csv("./Data/BiologicalInputs1.csv") %>%
  select(tempDataNames) %>%
  unlist(recursive = FALSE) %>%
  as.vector()

# biological inputs
biologicalInputs <- read.csv("./Data/BiologicalInputs1.csv") 

biologicalFileNames <- paste0("bioData", 
                              biologicalInputs$bioMarker,
                              ".rds")

#### Testing parallel running ##################################################

# run defineNimbleModel function with integer selected

slidingWindowModel <- defineNimbleModel(slidingWindowType = "integer")

## need to create an input dataframe which will feed in all necessary parameters
# to the model run: parameters need to be in the following order -
# slidingWindowType, dataInput, constants, inits, niter, nburnin, 
# nchains, parametersToMonitor, nthin, seed

niter <- 50000
nburnin <- 5000
nchains <- 2
nthin <- 5
seed <- 1:nchains

constants <- list(numYears = 50,
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


modelInputs <- data.frame(constants = I(list(constants)),
                          inits = I(list(inits)),
                          parametersToMonitor = I(list(parametersToMonitor)),
                          seed = I(list(seed))) %>%
  .[rep(1, 6000),] %>%
  mutate(temperatureFileNames = temperatureFileNames,
         niter = niter,
         nburnin = nburnin,
         nchains = nchains,
         nthin = nthin,
         biologicalFileNames = biologicalFileNames,
         slidingWindowType = "weighted",)


#### Parallel running ##########################################################

#### manual run ####

# first do a single run of the model with all elements the same and check it 
# matches the pmap result

# first read in temperature and biological data
temperatureDataTest <- readRDS(paste0("./Data/TempData/", 
                                  modelInputs$temperatureFileNames[1]))    
biologicalDataTest <- readRDS(paste0("./Data/BioData/", 
                                 modelInputs$biologicalFileNames[1]))  

# combine the simulated datasets to create dataInput
dataInputs <- createDataInput(temperatureDataTest, biologicalDataTest)

test1Manual <- runNimbleModel(slidingWindowType = "weighted",
                              dataInput = dataInputs,
                              constants = modelInputs[1,]$constants[[1]],
                              inits = modelInputs[1,]$inits[[1]], 
                              niter = modelInputs[1,]$niter, 
                              nchains = modelInputs[1,]$nchains, 
                              nburnin = modelInputs[1,]$nburnin,
                              parametersToMonitor = modelInputs[1,]$parametersToMonitor[[1]],
                              nthin = modelInputs[1,]$nthin,
                              seed = modelInputs[1,]$seed[[1]])

# save out
saveRDS(test1Manual, file = "./Data/Checking/Manual1.rds")

# clear
gc()
rm(list = ls())

#### Run sequential ####

# re-run inputs: REMEMBER TO RE-RUN 'SET UP'

# run defineNimbleModel function with integer selected

slidingWindowModel <- defineNimbleModel(slidingWindowType = "integer")

## need to create an input dataframe which will feed in all necessary parameters
# to the model run: parameters need to be in the following order -
# slidingWindowType, dataInput, constants, inits, niter, nburnin, 
# nchains, parametersToMonitor, nthin, seed

niter <- 50000
nburnin <- 5000
nchains <- 2
nthin <- 5
seed <- 1:nchains

constants <- list(numYears = 50,
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


modelInputs <- data.frame(constants = I(list(constants)),
                          inits = I(list(inits)),
                          parametersToMonitor = I(list(parametersToMonitor)),
                          seed = I(list(seed))) %>%
  .[rep(1, 6000),] %>%
  mutate(temperatureFileNames = temperatureFileNames,
         niter = niter,
         nburnin = nburnin,
         nchains = nchains,
         nthin = nthin,
         biologicalFileNames = biologicalFileNames,
         slidingWindowType = "weighted",)

plan(sequential)

tic()
testSequential <- future_pmap(modelInputs[1:5,], 
                              function(slidingWindowType,
                                       biologicalFileNames,
                                       temperatureFileNames,
                                       constants,
                                       inits,
                                       niter,
                                       nburnin,
                                       nchains,
                                       parametersToMonitor,
                                       nthin,
                                       seed){
        
        # first read in temperature and biological data
        temperatureData <- readRDS(paste0("./Data/TempData/", 
                                          temperatureFileNames))    
        biologicalData <- readRDS(paste0("./Data/BioData/", 
                                         biologicalFileNames))  
        
        # combine the simulated datasets to create dataInput
        dataInputs <- createDataInput(temperatureData, biologicalData)
        
        # run the model and save the output
        modelResult <- runNimbleModel(slidingWindowType = slidingWindowType,
                                      dataInput = dataInputs,
                                      constants = constants,
                                      inits = inits, 
                                      niter = niter, 
                                      nchains = nchains, 
                                      nburnin = nburnin,
                                      parametersToMonitor = parametersToMonitor,
                                      nthin = nthin,
                                      seed = seed)
                                
          saveRDS(modelResult, file = paste0("./Data/Checking/Sequential",
                             str_sub(biologicalFileNames, 8, -5),
                             ".rds"))
                                
                              }, .options = furrr_options(seed = TRUE))

toc() # TIME = 80 seconds

#### run parallel ####

# check time for 5 runs parallel - should be faster
plan(multisession, workers = 5)

tic()
future_pmap(modelInputs[1:5,], 
            function(slidingWindowType,
                     biologicalFileNames,
                     temperatureFileNames,
                     constants,
                     inits,
                     niter,
                     nburnin,
                     nchains,
                     parametersToMonitor,
                     nthin,
                     seed){
              
              # first read in temperature and biological data
              temperatureData <- readRDS(paste0("./Data/TempData/", 
                                         temperatureFileNames))    
       biologicalData <- readRDS(paste0("./Data/BioData/", 
                                        biologicalFileNames))  
       
       # combine the simulated datasets to create dataInput
       dataInputs <- createDataInput(temperatureData, biologicalData)
       
       # run the model and save the output
       modelResult <- runNimbleModel(slidingWindowType = slidingWindowType,
                                     dataInput = dataInputs,
                                     constants = constants,
                                     inits = inits, 
                                     niter = niter, 
                                     nchains = nchains, 
                                     nburnin = nburnin,
                                     parametersToMonitor = parametersToMonitor,
                                     nthin = nthin,
                                     seed = seed)
       
       saveRDS(modelResult, file = paste0("./Data/Checking/Parallel",
                                          str_sub(biologicalFileNames, 8, -5),
                                          ".rds"))
              
            }, .options = furrr_options(seed = TRUE))

toc() # TIME = 21.32 seconds


testManual <- readRDS("./Data/Checking/Manual1.rds")

testSequential <- readRDS("./Data/Checking/Sequential1.rds")

testParallel <- readRDS("./Data/Checking/Parallel1.rds")

MCMCsummary(testManual)
MCMCsummary(testSequential)
MCMCsummary(testParallel)

# seed is working the same in the parallel but not matching the manual
# Results are VERY SIMILAR though