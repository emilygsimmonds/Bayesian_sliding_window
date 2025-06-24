#### First test script #### 

#' This is the first script where I will try out creating a basic sliding
#' window search in Nimble with window 'open' and window 'duration' as 
#' parameters
#'

#### Set up ####

### Load packages

library(nimble)
library(nimbleEcology)
library(tidyverse)
library(MCMCvis)

### Create some dummy data

# temperature variable with some seasonality, need for multiple years (30)
set.seed(1)
x <- as.list(1:30)

temperature_variable <- map(x, ~ {
                     yearly_temperature <- data.frame(
                                             year = c(rnorm(50, 
                                             mean = seq(1, 10, length.out = 50),
                                             sd = 2),
                                             rnorm(50, mean = 10,
                                             sd = 2)))
                     colnames(yearly_temperature) <- c(paste0("year", .x))
                     return(yearly_temperature)
                        }
                        )

# create a biological variable that responds to part of the temperature series
# intercept of 30
create_bio_variable <- function(temperature, intercept, slope, 
                                standard_deviation){
  
  yearly_biological_variable <- rnorm(1, 
                                      mean = (intercept + 
                                             (mean(temperature[20:30])*slope)),
                                      sd = standard_deviation)
  
}

set.seed(1)
biological_variable <- map(temperature_variable, ~{
  yearly_biological_variable <- create_bio_variable(.x[,1],
                                                    intercept = 30,
                                                    slope = 2,
                                                    standard_deviation = 2)
}) %>%
  unlist()

temperature_variable <- bind_cols(temperature_variable)

### try to write a Nimble model

# first need a nimble function for calculation of mean temperature in window
window_calculator <- nimbleFunction(
    run = function(open = double(0), 
                   duration = double(0),
                   temperature = double(1)) { # type declarations 1 = vector
    return(mean(temperature[round(open):round(open+duration)]))
    returnType(double(0))  # return type declaration
  } )

test_sliding_window <- nimbleCode({
  
#-------------------------------------------------------------------------------
## DEFINE PRIORS

open ~ dunif(1, 50) # start with half of time series to avoid impossible combos
duration ~ dunif(1, 30) # start with shorter windows, again to avoid impossible
intercept ~ dnorm(30, sd = 5)
slope ~ dnorm(2, sd = 5)
error ~ dgamma(2, 1)

#-------------------------------------------------------------------------------
## CALCULATE WINDOW

for(i in 1:length(years)){

temperature_window[i] <- window_calculator(open,
                                           duration,
                                           temperature[,i])


#-------------------------------------------------------------------------------
## LIKELIHOOD FOR BIOLOGICAL VARIABLE

biological_variable[i] ~ dnorm((intercept + (temperature_window[i]*slope)), 
                            sd = error)
}

})


### try running the model

n_iter <- 50000
n_burnin <- 5000
n_chains <- 2

# set up initial values 

data_input <- list(temperature = temperature_variable,
                   biological_variable = biological_variable)

constants <- list(years = 30)

inits <- list(open = round(runif(1, 1, 50)),
                 duration = round(runif(1, 1, 30)),
                 intercept = rnorm(1, 30, sd = 5),
                 slope = rnorm(1, 0, sd = 10),
                 error = rgamma(1, 2, 1),
                 temperature_window = rep(1, 30))

model_result <- nimbleMCMC(code = test_sliding_window, 
                           data = data_input,
                           constants = constants,
                           inits = inits,
                           monitors = c("open",
                                        "duration", 
                                        "intercept",
                                        "slope",
                                        "error"),
                           niter = n_iter,
                           nburnin = n_burnin,
                           nchains = n_chains)

MCMCsummary(model_result)

