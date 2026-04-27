#### Running script: Model Running Functions ###################################

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

# remove lines that have already run
removalNumbers <- parse_number(list.files("./Data/ModelResults/"), na = ".")
# then match these to the number in the biologicalInputs dataframe
removalMarker <- which(biologicalInputs$bioMarker %in% removalNumbers)

# remove those entries from both sets of filenames
biologicalFileNames <- biologicalFileNames[-removalMarker]
temperatureFileNames <- temperatureFileNames[-removalMarker]

#### Parallel set up ###########################################################

# run defineNimbleModel function with integer selected

slidingWindowModel <- defineNimbleModel(slidingWindowType = "integer")

## need to create an input dataframe which will feed in all necessary parameters
# to the model run: parameters need to be in the following order -
# slidingWindowType, dataInput, constants, inits, niter, nburnin, 
# nchains, parametersToMonitor, nthin, seed

niter <- 500000
nburnin <- 50000
nchains <- 2
nthin <- 10
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
  .[rep(1, length(temperatureFileNames)),] %>%
  mutate(temperatureFileNames = temperatureFileNames,
         niter = niter,
         nburnin = nburnin,
         nchains = nchains,
         nthin = nthin,
         biologicalFileNames = biologicalFileNames,
         slidingWindowType = "weighted",)


#### Parallel running ##########################################################
#availableCores()

# check time for 5 runs parallel - should be faster
plan(multisession, workers = availableCores() - 5)

tic()
future_pmap(modelInputs, 
            safely(function(slidingWindowType,
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
              
              rModel <- nimbleModel(code = slidingWindowModel, 
                                    data = dataInputs, 
                                    constants = constants, 
                                    inits = inits)
              
              cModel <- compileNimble(rModel) 
              
              conf <- configureMCMC(rModel, 
                                    monitors = parametersToMonitor) 
              
              rMCMC <- buildMCMC(conf) 
              
              cMCMC <- compileNimble(rMCMC, 
                                     project = rModel) 
              
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
                                            seed = seed,
                                            cModel = cModel,
                                            cMCMC = cMCMC)
              
              saveRDS(modelResult, file = paste0("./Data/ModelResults/ModelResult",
                                                 str_sub(biologicalFileNames, 8, -5),
                                                 ".rds"))
              
              return(str_sub(biologicalFileNames, 8, -5))
              
            }), .options = furrr_options(seed = TRUE))
toc()


