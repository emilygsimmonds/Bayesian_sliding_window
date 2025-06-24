#### First test script #### 

#' This is the first script where I will try out creating a basic sliding
#' window search in Stan with window 'open' and window 'duration' as 
#' parameters
#'

#### Set up ####

#### Load packages

library(rstan)

#### Set up the stan model

write("// Stan model for simple linear regression

data {
 int < lower = 1 > N; // Sample size MUST SET THIS EACH TIME FOR EACH VARIABLE
 int < lower = 1 > K; // Number of temperature obs
 matrix[K, N] x1; // Predictor - [SAMPLE SIZE]
 vector[N] y; // Outcome
 vector[N] x2; // new temperature
}

parameters {
 real beta0; // Intercept
 real beta1; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
 real  open; // open of window
 real < lower = open > close; // close of the window
}


model {

// specify window
 
 for(i in 1:N){
 x2 = mean(x1[round(open2):(round(close2)+1),i]);
 } 
  
}
 
 // specify prior
 beta0 ~ normal(0, 100);
 beta1 ~ normal(0, 100);
 
 //specify data likelihood
 
 y ~ normal(beta0 + x2 * beta1 , sigma);
}

generated quantities {
} // The posterior predictive distribution",

"stan_model1.stan")

# check it wrote ok and no errors
stanc("stan_model1.stan")

# save the file path
stan_model1 <- "stan_model1.stan"

#### Set up the data

# want to have x and y and N (sample size)

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

### Now store as x and y and N as a list

stan_data = list(N = 30,
                 x1 = temperature_variable4,
                 y = biological_variable)

#### Run the stan model

fit <- stan(file = stan_model1, 
            data = stan_data, 
            warmup = 500, 
            iter = 1000, 
            chains = 4, 
            cores = 2, 
            thin = 1)

MCMCsummary(fit)
MCMCtrace(fit,
          params = c('beta0',
                     'beta1'), 
          ISB = FALSE, 
          exact = TRUE,
          pdf = FALSE)
