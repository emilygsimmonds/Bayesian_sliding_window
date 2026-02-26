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
noise <- seq(1, 5, length.out = 2)

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
temperatureInputs <- expand_grid(seed,
                                 noise,
                                 mean,
                                 meanSignal,
                                 noiseSignal,
                                 numYears,
                                 numDays,
                                 windowOpen,
                                 windowDuration) 

#### Create temperature data ###################################################

simulatedTempData <- pmap(temperatureInputs, .f = simulateTempData)

saveRDS(simulatedTempData, file = "./Data/SimulatedTempData1.RDS")
write.csv(temperatureInputs, file = "./Data/TemperatureInputs1.csv")


#### Inputs for biological data ################################################

# above created the temperature simulated datasets - these are basis of the
# biological data also want to add in an extra axes of noise and slope variation
# window open and close remain the same as the simulated data

# take the temperature inputs dataframe and reduce to columns I need
biologicalInputsA <- temperatureInputs %>% select(windowOpen, windowDuration)

# now try and expand to include the actual temperature data
biologicalInputsB <- biologicalInputsA %>%
  mutate(tempData = simulatedTempData) 

# noise in relationship between temp and biology 1 to 10
bioNoise <- c(2.5, 5)

# slope of temperature biology relationship 0 to 10
slope <- seq(0, 10, length.out = 3)

# intercept fixed at 20
intercept <- 20

# seed from 1:100 but testing at 1 :10
seed <- seq(1, 10, 1)

# now expand grid to include the extra axes

biologicalInputsC <- expand_grid(biologicalInputsB, bioNoise, slope, 
                                 intercept, seed)

### reorder column names to be in order expected by pmap

biologicalInputs <- biologicalInputsC %>%
  select(seed, 
           bioNoise, 
           slope, 
           intercept, 
           tempData, 
           windowOpen, 
           windowDuration)

#### Create biological data ####################################################

simulatedBioData <- pmap(biologicalInputs, .f = simulateBioData)

saveRDS(simulatedBioData, file = "./Data/SimulatedBioData1.RDS")
# need to remove tempData column from the inputs to save
write.csv(biologicalInputs[,-5], file = "./Data/BiologicalInputs1.csv") 
