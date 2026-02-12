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

# then alter the temperature in the 'key window' to add strong interannual signal

temperature_variable2 <- map2(.x = temperature_variable,
                              .y = as.list(seq(5,30*5, by = 5)), ~{
  .x[20:30,1] <- rnorm(11, mean = .y, sd = 0.1)
  return(.x)
})

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
biological_variable <- map(temperature_variable2, ~{
  yearly_biological_variable <- create_bio_variable(.x[,1],
                                                    intercept = 30,
                                                    slope = 5,
                                                    standard_deviation = 0.5)
}) %>%
  unlist()

temperature_variable3 <- bind_cols(temperature_variable2)

# check by plotting
temperature_variable4 <- colMeans(temperature_variable3[20:30,])

plot(biological_variable~temperature_variable4)

temperature_variable5 <- colMeans(temperature_variable3[50:60,])

plot(biological_variable~temperature_variable5)

### try to write a Nimble model

# first need a nimble function for calculation of mean temperature in window
window_calculator <- nimbleFunction(
    run = function(open = double(0), 
                   duration = double(0),
                   x1 = double(1)) { # type declarations 1 = vector
    return(mean(x1[round(open):round(open+duration)]))
    returnType(double(0))  # return type declaration
  } )

test_sliding_window <- nimbleCode({

#-------------------------------------------------------------------------------
## DEFINE PRIORS

open ~ dunif(1, 70) # start with half of time series to avoid impossible combos
duration ~ dunif(1, 30) # start with shorter windows, again to avoid impossible
beta0 ~ dnorm(0, sd = 100)
beta1 ~ dnorm(0, sd = 100)
sigma ~ dunif(0, 100)  # prior for variance components based on Gelman (2006)
  
#-------------------------------------------------------------------------------
## CALCULATE WINDOW

for(i in 1:n) {

x2[i] <- window_calculator(open,
                           duration,
                           x1[,i])


#-------------------------------------------------------------------------------
## LIKELIHOOD FOR BIOLOGICAL VARIABLE

y[i] ~ dnorm(beta0 + beta1*x2[i], sd = sigma) # manual entry of linear predictors

}
  
})


### try running the model

n_iter <- 30000000
n_burnin <- 29995000
n_chains <- 2

# set up initial values 

#data_input <- list(temperature = temperature_variable4,
#                   biological_variable = biological_variable)
data_input <- list(y = biological_variable,
                   x1 = temperature_variable3)

constants <- list(n = 30)

inits <- list(open = round(runif(1, 1, 70)),
              duration = round(runif(1, 1, 30)),
#                 intercept = rnorm(1, 0, sd = 100),
#                 slope = rnorm(1, 0, sd = 100),
                  beta0 = rnorm(1, 0, sd = 100),
                  beta1 = rnorm(1, 0, sd = 100),
                  sigma = runif(1, 0, 5),
#                 error = runif(1, 0, 2))
                  x2 = rep(NA, 30))

model_result <- nimbleMCMC(code = test_sliding_window, 
                           data = data_input,
                           constants = constants,
                           inits = inits,
                           monitors = c("open",
                                        "duration", 
                                        "beta0",
                                        "beta1",
                                        "sigma",
                                        "x2"),
                           niter = n_iter,
                           nburnin = n_burnin,
                           nchains = n_chains,
                           thin = 50)

MCMCsummary(model_result)
MCMCtrace(model_result,
          params = c('beta0',
                     'beta1',
                     'open',
                     'duration'), 
          ISB = FALSE, 
          exact = TRUE,
          pdf = FALSE)

