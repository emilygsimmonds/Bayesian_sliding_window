#### Check script: Simulation Functions ############################################

#' This script checks the simulation functions in scripts:
#' 'simulateTempData.R'
#' 'simulatedBioData.R'
#' 'runSimulations.R'

#### Set up ####################################################################

#### load packages ####

library(tidyverse)

#### source code ####

source("./Functions/simulateTempData.R")
source("./Functions/simulateBioData.R")

#### Testing simulateTempData ##################################################

### Test 1: can it produce the correct numbers? ####

## set up all the inputs

noise = 0.5
mean = 1
meanSignal = 10
noiseSignal = 0.5
numYears = 2
numDays = 10
windowOpen = 1
windowDuration = 1

test1 <- simulateTempData(noise, 
                          mean,
                          meanSignal, 
                          noiseSignal,
                          numYears,
                          numDays,
                          windowOpen, 
                          windowDuration) 
# Seems to work - 20.02.2026

test2 <- simulateTempData(noise, 
                          mean,
                          meanSignal, 
                          noiseSignal,
                          numYears,
                          numDays,
                          windowOpen = 2, 
                          windowDuration) 
# Seems to work - 20.02.2026

# test against year 1 manually
set.seed(2026)
# index of year effect - sd = 1/5 mean
yearEffect <- as.list(rnorm(numYears, 
                            mean = meanSignal, 
                            sd = meanSignal/5)) 
set.seed(1)
keyWindow <- rnorm(windowDuration, mean = yearEffect[[1]],
                                    sd = noiseSignal)
set.seed(1)
preWindow <- rnorm(0, 
                   mean = mean,
                   sd = noise)
set.seed(1)
postWindow <- rnorm(9, 
                    mean = mean, 
                    sd = noise)
manualTest1 <- data.frame(year = c(preWindow,
                                       keyWindow,
                                       postWindow))

test1[[1]] - manualTest1 # WORKS: 20.02.26


#### Testing simulateBioData ##################################################

### Test 1: can it produce the correct numbers? ####

## set up all the inputs

noise = 0.5
mean = 1
meanSignal = 10
noiseSignal = 0.5
numYears = 2
numDays = 10
windowOpen = 1
windowDuration = 1

# make the temperature data

tempData <- simulateTempData(noise, 
                          mean,
                          meanSignal, 
                          noiseSignal,
                          numYears,
                          numDays,
                          windowOpen, 
                          windowDuration) 

# make the bio data with the function


bioData <- simulateBioData(noise = 1,
                           slope = 5, 
                           intercept = 5,
                           tempData = tempData,
                           windowOpen = windowOpen,
                           windowDuration = windowDuration)

# try and simulate manually - year 2

windowTemp <- mean(tempData[[2]][windowOpen:(windowOpen + windowDuration),1])

set.seed(2026)
biologicalVariable <- rnorm(1, 
                            mean = 5 + 
                              (windowTemp*5),
                            sd = 1)

bioData[[2]] - biologicalVariable # WORKS 20.02.2026
