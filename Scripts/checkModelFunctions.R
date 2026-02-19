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

#### source code ####

source("./Functions/nimbleSlidingWindow.R")
source("./Functions/nimbleModel.R")
source("./Functions/runNimbleModel.R")

#### create test data ####

# will create some temperature data for 100 years
x <- as.list(1:100) # index of years
set.seed(2026)
y <- as.list(rnorm(100, 15, 5)) # index of year effect i.e. year mean

# now map over the year index and the year means to create individual years of
# of temperature with a key 'window' from day 20 to 21. Mean set to 5 for all 
# expect the window.

set.seed(2026)

temperatureVariable <- map2(x, y, ~ {

  # simulate the temperature for the key window and pre and post period
  keyWindow <- rnorm(2, mean = .y,
                     sd = 2)
  preWindow <- rnorm(19, mean = 5,
                     sd = 2)
  postWindow <- rnorm(79, mean = 5, 
                      sd = 2)
  
  # then combine into an annual time series
  yearlyTemperature <- data.frame(year = c(preWindow,
                                            keyWindow,
                                            postWindow))
  
  # label with the year index
  colnames(yearlyTemperature) <- c(paste0("year", .x))
  
  return(yearlyTemperature)
}
)

# test that the signal is strong enough
plottingTemperature <- temperatureVariable %>%
  pivot_longer(cols = everything(),
               names_to = "years",
               values_to = "temperature") %>%
  mutate(day = rep(1:100, each = 100))

ggplot(data = filter(plottingTemperature,
                     years == "year1"|
                       years == "year2" |
                       years == "year3"), aes(y = temperature,
                                              x = day)) +
  geom_line() +
  facet_wrap(~years)

# now create the biological variable from the temperature data

# combine the temperature into a single dataframe
temperatureVariableDF <- bind_cols(temperatureVariable)

# take the annual means of the key temperature window
temperatureMeansDF <- as.data.frame(colMeans(temperatureVariableDF[20:21,]))
colnames(temperatureMeansDF) <- c("meanTemperature")

# create a biological variable that responds to part of the temperature series
# intercept of 30, slope = 5

set.seed(2026)
biologicalVariable <- rnorm(100, 
                            mean = 30 + 
                                      (temperatureMeansDF$meanTemperature*5),
                            sd = 5)


# plot the relationship between the temperature and the biological variable
temperatureMeansDFcombined <- temperatureMeansDF %>%
  pivot_longer(cols = everything(),
               names_to = "years",
               values_to = "meanTemperature") %>%
  mutate(biologicalVariable = biologicalVariable)

ggplot(aes(y = biologicalVariable,
           x = meanTemperature), data = temperatureMeansDFcombined) +
  geom_smooth(method = "lm") +
  geom_point()

summary(lm(biologicalVariable ~ meanTemperature, 
           data = temperatureMeansDFcombined))

#### Testing nimbleModel #######################################################

# do manual and shared set up of model inputs

niter <- 50000
nburnin <- 5000
nchains <- 2
nthin <- 5

# set up initial values 

dataInput <- list(temperature = temperatureVariableDF,
                  # mean centre the biological variable
                  biologicalVariable = biologicalVariable - mean(biologicalVariable))

constants <- list(numYears = 100,
                  windowStarts = c(1,50),
                  windowDurations = c(1,49))

set.seed(2026)
# sum of open and duration must be < numDays
inits <- list(open = round(runif(1, 1, 50)),
              duration = round(runif(1, 1, 49)), 
              intercept = rnorm(1, 0, sd = 100),
              slope = rnorm(1, 0, sd = 10),
              error = rgamma(1, 2, 1))

parametersToMonitor = c("open",
             "duration", 
             "intercept",
             "slope",
             "error")

### Test 1: how do results compare to truth and lm? Integer ####

# run defineNimbleModel function with integer selected

slidingWindowModel <- defineNimbleModel(slidingWindowType = "integer")

# manual running straight from Nimble
set.seed(2026)
testModel1 <- nimbleMCMC(code = slidingWindowModel,
                         data = dataInput,
                         constants,
                         inits, 
                         niter = niter,
                         nburnin = nburnin,
                         nchains = nchains, 
                         monitors = parametersToMonitor,
                         thin = nthin) # RUNS AT LEAST 19.02.2026

centredVariables <- temperatureMeansDFcombined %>%
  mutate(centredBV = biologicalVariable - mean(biologicalVariable),
         centredT = meanTemperature - mean(meanTemperature))

testModel2 <- lm(centredBV ~ centredT, 
                    data = centredVariables)

MCMCsummary(testModel1) # SLOPE IS OVER ESTIMATED HERE BUT OPEN + DURATION 
# ARE BANG ON

summary(testModel2) # PRETTY MUCH EXACTLY RIGHT

### Test 2: does the weighted model function give same results as manual? ####

# run defineNimbleModel function with integer selected

slidingWindowModel <- defineNimbleModel(slidingWindowType = "weighted")

# manual running straight from Nimble
set.seed(2026)
testModel3 <- nimbleMCMC(code = slidingWindowModel,
                         data = dataInput,
                         constants,
                         inits, 
                         niter = niter,
                         nburnin = nburnin,
                         nchains = nchains, 
                         monitors = parametersToMonitor,
                         thin = nthin) # RUNS AT LEAST 19.02.2026


MCMCsummary(testModel3) # THIS ONE IS MUCH BETTER! EVERYTHING BANG ON. 
MCMCtrace(testModel3, pdf = FALSE)

#### Testing runNimbleModel ####################################################

### Test 1: does it give same answer as nimbleModel: integer? ####

# First step - run and use testModel1 above - saves time and code

# Then run with the runNimbleModel code
set.seed(2026)
testModel4 <- nimbleModel(slidingWindowType = "integer",
                          dataInput = dataInput,
                          constants = constants,
                          inits = inits, 
                          niter = niter, 
                          nchains = nchains, 
                          nburnin = nburnin,
                          parametersToMonitor = parametersToMonitor,
                          nthin = nthin) # RUNS AT LEAST 19.02.2026

MCMCsummary(testModel1)
MCMCsummary(testModel4) 

### Test 2: does it give same answer as nimbleModel: weighted? ####

# First step - run and use testModel3 above - saves time and code

# Then run with the runNimbleModel code
set.seed(2026)
testModel5 <- nimbleModel(slidingWindowType = "weighted",
                          dataInput = dataInput,
                          constants = constants,
                          inits = inits, 
                          niter = niter, 
                          nchains = nchains, 
                          nburnin = nburnin,
                          parametersToMonitor = parametersToMonitor,
                          nthin = nthin) # RUNS AT LEAST 19.02.2026

MCMCsummary(testModel3) 
MCMCsummary(testModel5) # CHECK PASSED INC with SEED
