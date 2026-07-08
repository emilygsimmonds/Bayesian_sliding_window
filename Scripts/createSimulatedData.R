#### Script to create simulation datasets ######################################

#' This script runs functions simulateTempData and simulateBioData to produce
#' simulated datasets across a range of scenarios

#### Set up ####################################################################

#### load packages ####

library(tidyverse)
library(furrr)

#### source code ####

source("./Functions/simulateTempData.R")
source("./Functions/simulateBioData.R")

#### Inputs for temperature data ###############################################

# noise in non-window segment set at 5
noise <- 5

# mean temperature in non-window set at 5
mean <- 5

# mean of window segment held at 10
meanSignal <- 10

# noise in the window segment held at 0.5
noiseSignal <- 0.5

# number of years 50
numYears <- 50

# number of days fixed at 100
numDays <- 100

# windowOpen from 1 to 50
windowOpen <- round(seq(1, 50, length.out = 3))

# windowDuration from 1-30
windowDuration <- c(1, 15, 30)

# seed from 1:50
seed <- seq(1, 50, 1)

### Want to run each combination 50 times initially

# expand a grid to all unique combinations - need to be in same order as in 
# function to make sure it can be used with pmap
temperatureInputs <- expand_grid(seed, # open and duration scenarios
                                 noise,
                                 mean,
                                 meanSignal,
                                 noiseSignal,
                                 numYears,
                                 numDays,
                                 windowOpen = windowOpen[3],
                                 windowDuration = windowDuration,
                                 tScenario = "open") %>% 
  bind_rows(expand_grid(seed, # open and duration scenarios
              noise,
              mean,
              meanSignal,
              noiseSignal,
              numYears,
              numDays,
              windowOpen = windowOpen,
              windowDuration = windowDuration[3],
              tScenario = "duration")) %>%# label the scenarios 
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

temperatureInputs <- read.csv("./Data/TemperatureInputs1.csv")

# take the temperature inputs dataframe and reduce to columns I need
biologicalInputsA <- temperatureInputs %>% select(windowOpen, windowDuration)

# now try and expand to include the actual temperature data - now need as a file
# name rather than the actual data
tempDataNames <- list.files("./Data/TempData/", pattern = "rds")


biologicalInputsB <- biologicalInputsA %>%
  mutate(tempDataNames = tempDataNames) 

# noise in relationship between temp and biology 1 to 10
bioNoise <- c(2.5, 5, 10)

# slope of temperature biology relationship 0 to 10
slope <- seq(-6, 6, by = 3)

# intercept fixed at 20
intercept <- 20

# seed from 1:50
seed <- seq(1, 50, 1)

# now expand grid to include the extra axes

biologicalInputsC <- expand_grid(biologicalInputsB, 
                                 bioNoise, # bnoise scenario
                                 slope = slope[5], 
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

plan(multisession)

simulatedBioData <- future_pmap(select(biologicalInputs, -bScenario),
                         function(bioMarker, seed, bioNoise, 
                                  slope, intercept,
                                  tempDataNames, windowOpen, 
                                  windowDuration){
                           
                           # first import the temperature data
                           tempData <- readRDS(paste0("./Data/TempData/", 
                                                      tempDataNames))
                           
                           # then create the bioData
                           results <- simulateBioData(seed = seed,
                                              bioNoise = bioNoise,
                                              slope = slope,
                                              intercept = intercept,
                                              tempData = tempData,
                                              windowOpen = windowOpen,
                                              windowDuration = windowDuration)
                           saveRDS(results, paste0("./Data/BioData/bioData", 
                                                   bioMarker,
                                                   ".rds"))  
                         }, .options = furrr_options(seed = TRUE))

# check three of them
test0 <- readRDS("./Data/TempData/TempData1.rds")
test01 <- readRDS("./Data/TempData/TempData151.rds")
test1 <- readRDS("./Data/BioData/bioData1.rds")
test2 <- readRDS("./Data/BioData/bioData2.rds")
test50 <- readRDS("./Data/BioData/bioData100000.rds")

test1
test2
test50 # all different! That's what I wanted


# need to remove tempData column from the inputs to save
write.csv(biologicalInputs, file = "./Data/BiologicalInputs1.csv") 

#### REAL TEMPERATURE ##########################################################
#### Inputs for biological data (REAL) #########################################

# Using the same process as for the simulated data but this time for the real
# temperature data

tempDataNames <- list.files("./Data/TempDataReal/Editted/", pattern = "rds")

# now need to set up all of the simulation parameters including open and close 

# noise in relationship between temp and biology 1 to 10
bioNoise <- c(2.5, 5, 10)

# slope of temperature biology relationship 0 to 10
slope <- seq(-6, 6, by = 3)

# intercept fixed at 20
intercept <- 20

# seed from 1:50
seed <- seq(1, 50, 1)

# windowOpen from 1 to 50
windowOpen <- round(seq(1, 50, length.out = 3))

# windowDuration from 1-30
windowDuration <- c(1, 15, 30)

# now expand grid to include the extra axes

biologicalInputsReal <- data.frame(tempDataNames = 
                                     tempDataNames)

biologicalInputsRealB <- expand_grid(biologicalInputsReal, 
                                 bioNoise, # bnoise scenario
                                 slope = slope[5], 
                                 intercept, 
                                 seed,
                                 windowOpen = windowOpen[2],
                                 windowDuration = windowDuration[2],
                                 bScenario = "bnoise") %>%
  bind_rows(expand_grid(biologicalInputsReal, 
                        bioNoise = bioNoise[2], 
                        slope, # bslope scenario
                        intercept, 
                        seed,
                        windowOpen = windowOpen[2],
                        windowDuration = windowDuration[2],
                        bScenario = "slope")) %>%
  bind_rows(expand_grid(biologicalInputsReal, 
                        bioNoise = bioNoise[2], 
                        slope = slope[5], 
                        intercept, 
                        seed,
                        windowOpen,
                        windowDuration = windowDuration[2],
                        bScenario = "open")) %>%
  bind_rows(expand_grid(biologicalInputsReal, 
                        bioNoise = bioNoise[2], 
                        slope = slope[5], 
                        intercept, 
                        seed,
                        windowOpen = windowOpen[2],
                        windowDuration,
                        bScenario = "slope"))

### reorder column names to be in order expected by pmap

biologicalInputsRealB <- biologicalInputsRealB %>%
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

plan(multisession, workers = 3)

simulatedBioData <- future_pmap(select(biologicalInputsRealB, -bScenario),
                                function(bioMarker, seed, bioNoise, 
                                         slope, intercept,
                                         tempDataNames, windowOpen, 
                                         windowDuration){
                                  
                                  # first import the temperature data
                                  tempData <- readRDS(paste0("./Data/TempDataReal/Editted/", 
                                                             tempDataNames))
                                  
                                  # then create the bioData
                                  results <- simulateBioData(seed = seed,
                                                             bioNoise = bioNoise,
                                                             slope = slope,
                                                             intercept = intercept,
                                                             tempData = tempData,
                                                             windowOpen = windowOpen,
                                                             windowDuration = windowDuration)
                                  saveRDS(results, paste0("./Data/BioData2/bioData", 
                                                          bioMarker,
                                                          ".rds"))  
                                }, .options = furrr_options(seed = TRUE))

# check three of them
test1 <- readRDS("./Data/BioData2/bioData1.rds")
test2 <- readRDS("./Data/BioData2/bioData2.rds")
test50 <- readRDS("./Data/BioData2/bioData1000.rds")

test1
test2
test50 # all different! That's what I wanted


# need to remove tempData column from the inputs to save
write.csv(biologicalInputsRealB, file = "./Data/BiologicalInputsReal.csv") 



