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
x <- as.list(1:100) # just an index of years
set.seed(1)
y <- as.list(rnorm(100, 40, 25)) # index of year effect
temperature_variable <- map2(x, y, ~ {
  keyWindow <- rnorm(2, mean = .y,
                     sd = 0.5)
  preWindow <- rnorm(19, mean = 10,
                     sd = 0.1)
  postWindow <- rnorm(79, mean = 10, 
                     sd = 0.1)
  yearly_temperature <- data.frame(year = c(preWindow,
                                            keyWindow,
                                            postWindow))
  colnames(yearly_temperature) <- c(paste0("year", .x))
  return(yearly_temperature)
     }
     )
# create a biological variable that responds to part of the temperature series
# intercept of 30
create_bio_variable <- function(temperature, intercept, slope, 
                                standard_deviation){
  set.seed(1)
  yearly_biological_variable <- rnorm(1, 
                                      mean = (intercept + 
                                             (mean(temperature[20:21])*slope)),
                                      sd = standard_deviation)
  return(yearly_biological_variable)
  
}

set.seed(1)
biological_variable <- map(as.list(temperature_variable), ~{
  yearly_biological_variable <- create_bio_variable(temperature = .x[,1],
                                                    intercept = 30,
                                                    slope = 5,
                                                    standard_deviation = 2)
}) %>%
  unlist()

temperature_variable <- bind_cols(temperature_variable)

# test that the signal is strong enough
plottingTemperature <- temperature_variable %>%
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

tempMeans <- as.data.frame(colMeans(temperature_variable[14:15,])) %>%
  pivot_longer(cols = everything(),
               names_to = "years",
               values_to = "temperature") %>%
  mutate(biological_variable = biological_variable)

ggplot(aes(y = biological_variable,
           x = temperature), data = tempMeans) +
  geom_point()

#tempMeans <- colMeans(temperature_variable)
tempMeans <- colMeans(temperature_variable[14:15,])

summary(lm(biological_variable ~ tempMeans))

### try to write a Nimble model

# first need a nimble function for calculation of mean temperature in window
window_calculator_integer <- nimbleFunction(
  run = function(open = double(0), 
                 duration = double(0),
                 temperature = double(1)) { # type declarations 1 = vector
    # just take the mean of the integer values of open and duration
    windowMean <- mean(temperature[round(open):round(open)+round(duration)]) # works
    return(windowMean)
    returnType(double(0))  # return type declaration
  } )

# first need a nimble function for calculation of mean temperature in window
window_calculator_weighted <- nimbleFunction(
    run = function(open = double(0), 
                   duration = double(0),
                   temperature = double(1)) { # type declarations 1 = vector

      # set w outside of function
      w <- rep(1, length(trunc(open):ceiling(open+duration)))
      # overwrite first and last entry to lower than 1
      w[1] <- 1-(open-trunc(open))
      #if(ceiling(open+duration) != # ONLY overwrite 1s if duration not integer
      #   trunc(open+duration)){}
      w[length(w)] <- (open+duration) - trunc(open+duration)
      windowMean <- weighted_mean_nimble(x = c(temperature[trunc(open):ceiling(open+duration)]),
                                 w = w) # works
    return(windowMean)
    returnType(double(0))  # return type declaration
  } )

weighted_mean_nimble <- nimbleFunction(
  run = function(x = double(1),
                 w = double(1)) {
    
    # take the variable values and make the numerator
    numerator <- sum(x*w)
    
    # then the denominator
    denominator <- sum(w)
    
    weightedMean <- numerator/denominator
    return(weightedMean)
    returnType(double(0))
  }
)

test_sliding_window <- nimbleCode({
  
#-------------------------------------------------------------------------------
## DEFINE PRIORS

open ~ dunif(1, 50) # start with half of time series to avoid impossible combos
duration ~ dunif(1, 49) # start with shorter windows, again to avoid impossible
intercept ~ dnorm(50, sd = 100)
slope ~ dnorm(0, sd = 10)
error ~ dgamma(2, 1)

#-------------------------------------------------------------------------------
## CALCULATE WINDOW

for(i in 1:years){

temperature_window[i] <- window_calculator_integer(open,
                                           duration,
                                           temperature[,i])


#-------------------------------------------------------------------------------
## LIKELIHOOD FOR BIOLOGICAL VARIABLE

biological_variable[i] ~ dnorm((intercept + (temperature_window[i]*slope)), 
                            sd = error)
}

})


### try running the model

n_iter <- 50000000
n_burnin <- 4000000
n_chains <- 2

# set up initial values 

data_input <- list(temperature = temperature_variable,
                   biological_variable = biological_variable)

constants <- list(years = 30,
                  windowStarts = c(1,50),
                  windowDurations = c(1,49))

set.seed(12)
inits <- list(open = round(runif(1, 1, 50)),
                 duration = round(runif(1, 1, 49)),
                 intercept = rnorm(1, 50, sd = 100),
                 slope = rnorm(1, 0, sd = 10),
                 error = rgamma(1, 2, 1))
                 #temperature_window = rep(1, 30))

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

model_result <- nimbleModel(dataInput = data_input,
                            constants,
                            inits, 
                            niter = n_iter, 
                            nchains = n_chains, 
                            nburnin = n_burnin,
                            parametersToMonitor = c("open",
                                                    "duration", 
                                                    "intercept",
                                                    "slope",
                                                    "error"),
                            nthin = 50)

MCMCsummary(model_result)
MCMCtrace(model_result, pdf = FALSE)

# Trying to plot the results including true values 

# create a dataframe of the true values for each parameter
truth <- data.frame(parameter = c("open",
                                  "duration",
                                  "slope",
                                  "error", 
                                  "intercept"),
                    truth = c(20, 1, 5, 2, 30))

# create a dataframe of the posterior mead

meanResults <- data.frame(parameter = rownames(MCMCsummary(model_result)),
                          mean = MCMCsummary(model_result)[,"mean"])

# first bind all chain results

combinedResults <- bind_rows(as.data.frame(model_result$chain1), 
          as.data.frame(model_result$chain2)) %>%
  pivot_longer(cols = everything(), names_to = "parameter", 
               values_to = "value") %>% 
  left_join(truth, by = "parameter") %>%
  left_join(meanResults, by = "parameter")

ggplot(data = combinedResults, aes(x = value)) +
  geom_density(fill = "skyblue", alpha = 0.6, colour = NA) +
  geom_vline(aes(xintercept = truth),
             colour = "purple", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = mean),
             colour = "darkblue", linewidth = 0.5) +
  facet_wrap(~parameter, scales = "free") +
  labs(x = "Parameter value",
       y = "Posterior density",
       title = "Posterior distributions with generating values") +
  theme_bw()


