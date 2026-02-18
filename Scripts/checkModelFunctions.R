#### Check script: Model Running Functions #####################################

#' This script checks the model and running functions in scripts:
#' 'nimbleModel.R'
#' 'runNimbleModel.R'

#### Set up ####################################################################

#### load packages ####

library(nimble)

#### source code ####

source("./Functions/nimbleSlidingWindow.R")
source("./Functions/nimbleModel.R")
source("./Functions/runNimbleModel.R")

#### create test data ####

# need a vector of numbers to take the mean of

set.seed(2026)
testVector <- rnorm(100, mean = 6, sd = 4)

#### Testing nimbleSlidingWindow ###############################################