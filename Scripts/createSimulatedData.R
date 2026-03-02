#### Script to create simulation datasets ######################################

#' This script runs functions simulateTempData and simulateBioData to produce
#' simulated datasets across a range of scenarios

#### Set up ####################################################################

#### load packages ####

library(tidyverse)

#### source code ####

source("./Functions/simulateTempData.R")
source("./Functions/simulateBioData.R")

#### Inputs for temperature data ###############################################

# noise in non-window segment varies from 1 to 5
noise <- seq(1, 5, length.out = 3)

# mean temperature in non-window segment varies from 0.1 to 5
mean <- seq(0.1, 5, length.out = 3)

# mean of window segment held at 10
meanSignal <- 10

# noise in the window segment held at 0.5
noiseSignal <- 0.5

# number of years 50
numYears <- 50

# number of days fixed at 100
numDays <- 100

# windowOpen from 1 to 50
windowOpen <- c(1, 25, 50)

# windowDuration from 1-30
windowDuration <- c(1, 15, 30)

# seed from 1:10 but testing at 1 :10
seed <- seq(1, 10, 1)

### Want to run each combination 100 times initially

# expand a grid to all unique combinations - need to be in same order as in 
# function to make sure it can be used with pmap
temperatureInputs <- expand_grid(seed, # noise scenario
                                 noise,
                                 mean = mean[2],
                                 meanSignal,
                                 noiseSignal,
                                 numYears,
                                 numDays,
                                 windowOpen = windowOpen[2],
                                 windowDuration = windowDuration[2],
                                 tScenario = "noise") %>%
  bind_rows(expand_grid(seed, # mean scenario
                        noise = noise[2],
                        mean,
                        meanSignal,
                        noiseSignal,
                        numYears,
                        numDays,
                        windowOpen = windowOpen[2],
                        windowDuration = windowDuration[2],
                        tScenario = "mean")) %>%
  bind_rows(expand_grid(seed, # window open scenario
                        noise = noise[2],
                        mean = mean[2],
                        meanSignal,
                        noiseSignal,
                        numYears,
                        numDays,
                        windowOpen,
                        windowDuration = windowDuration[2],
                        tScenario = "open")) %>%
  bind_rows(expand_grid(seed, # window duration scenario
                        noise = noise[2],
                        mean = mean[2],
                        meanSignal,
                        noiseSignal,
                        numYears,
                        numDays,
                        windowOpen = windowOpen[2],
                        windowDuration,
                        tScenario = "duration")) %>% # label the scenarios 
  rowid_to_column("marker")

#### Create temperature data ###################################################

simulatedTempData <- pmap(select(temperatureInputs, -tScenario), 
                          function(marker, seed, noise,
                                   mean, meanSignal, noiseSignal, 
                                   numYears, numDays, 
                                   windowOpen, windowDuration){
                            results <- simulateTempData(seed = seed,
                                                noise = noise,
                                                mean = mean,
                                                meanSignal = meanSignal,
                                                noiseSignal = noiseSignal,
                                                numYears = numYears,
                                                numDays = numDays,
                                                windowOpen = windowOpen,
                                                windowDuration = windowDuration)
                            saveRDS(results, paste0("./Data/TempData/tempData", 
                                                    marker,
                                                    ".rds"))
                          })

write.csv(temperatureInputs, file = "./Data/TemperatureInputs1.csv")


#### Inputs for biological data ################################################

# above created the temperature simulated datasets - these are basis of the
# biological data also want to add in an extra axes of noise and slope variation
# window open and close remain the same as the simulated data

# take the temperature inputs dataframe and reduce to columns I need
biologicalInputsA <- temperatureInputs %>% select(windowOpen, windowDuration)

# now try and expand to include the actual temperature data - now need as a file
# name rather than the actual data
tempDataNames <- list.files("./Data/TempData/")

biologicalInputsB <- biologicalInputsA %>%
  mutate(tempDataNames = tempDataNames) 

# noise in relationship between temp and biology 1 to 10
bioNoise <- c(2.5, 5)

# slope of temperature biology relationship 0 to 10
slope <- seq(0, 10, length.out = 3)

# intercept fixed at 20
intercept <- 20

# seed from 1:100 but testing at 1 :10
seed <- seq(1, 10, 1)

# now expand grid to include the extra axes

biologicalInputsC <- expand_grid(biologicalInputsB, 
                                 bioNoise, # bnoise scenario
                                 slope = slope[2], 
                                 intercept, 
                                 seed,
                                 bScenario = "bnoise") %>%
  bind_rows(expand_grid(biologicalInputsB, 
                        bioNoise = bioNoise[2], 
                        slope, # bslope scenario
                        intercept, 
                        seed,
                        bScenario = "slope"))

### reorder column names to be in order expected by pmap

biologicalInputs <- biologicalInputsC %>%
  select(seed, 
           bioNoise, 
           slope, 
           intercept, 
           tempDataNames, 
           windowOpen, 
           windowDuration,
        bScenario) %>%
  rowid_to_column("bioMarker")

#### Create biological data ####################################################

simulatedBioData <- pmap(select(biologicalInputs, -bScenario),
                         function(bioMarker, seed, bioNoise, 
                                  slope, intercept,
                                  tempDataNames, windowOpen, 
                                  windowDuration){
                           
                           # first import the temperature data
                           tempData <- readRDS(paste0("./Data/TempData/", 
                                                      tempDataNames))
                           
                           # then create the bioData
                           results <- simulateBioData(seed = seed,
                                              bioNoise = noise,
                                              slope = slope,
                                              intercept = intercept,
                                              tempData = tempData,
                                              windowOpen = windowOpen,
                                              windowDuration = windowDuration)
                           saveRDS(results, paste0("./Data/BioData/bioData", 
                                                   bioMarker,
                                                   ".rds"))  
                         })

# check three of them
test1 <- readRDS("./Data/BioData/bioData1.rds")
test2 <- readRDS("./Data/BioData/bioData2.rds")
test50 <- readRDS("./Data/BioData/bioData50.rds")

test1
test2
test50 # all different! That's what I wanted

# need to remove tempData column from the inputs to save
write.csv(biologicalInputs, file = "./Data/BiologicalInputs1.csv") 
