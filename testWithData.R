#### Script to try this out with data #### 

#' This script tests out the Bayesian sliding window and climwin on real data
#'

#### Set up ####

### Load packages

library(nimble)
library(tidyverse)
library(climwin)
library(MCMCvis)

### Load functions

source("./Functions/nimbleModel.R")
source("./Functions/nimbleSlidingWindow.R")

### Load data

testTempData <- read.csv("./Data/temperatures.csv")

summary(testTempData) # it is hourly temperature

testBioData <- read.csv("./Data/Tree_Phenology.csv", na.strings = "")

summary(testBioData)

#### Edit data ####

# want to get daily mean temperature
# first - remove X from column names

# remove columns with NA
testTempData2 <- testTempData %>% 
  select(where(~!any(is.na(.)))) 
# this line says select columns where there are not (!) any NAs

colnames(testTempData2)[3:2450] <- substr(colnames(testTempData2[3:2450]),
                                                  2,5)

testTempData3 <- testTempData2 %>% pivot_longer(cols = -c("site", 
                                                          "year", 
                                                          "logger_id",
                                         "logger_res"),
                              values_to = "Temperature",
                              names_to = "DayTime")

# add new columns that splits day and time
testTempData3$Day <- unlist(map(as.list(1:2078352), ~{
  strsplit(testTempData3$DayTime[.x], 
           "[.]")[[1]][1]}))

# then group by day and year and site to get daily site means
testTempDataShort <- testTempData3 %>% 
  group_by(site, year, Day) %>%
  summarise(meanDayTemp = mean(Temperature))

# need to create a date column - CHATGPT HELP

testTempDataShort$date <- as.Date(as.numeric(testTempDataShort$Day) - 1, 
                                  origin = paste(testTempDataShort$year, 
                                                 "-01-01", sep=""))

testTempDataShort$dateFormatted <- format(testTempDataShort$date, 
                                          "%d/%m/%Y")

# sort by the date
testTempDataShort <- testTempDataShort %>% arrange(dateFormatted) %>%
  mutate(Day = as.numeric(Day))

# then do the bio data - want mean by site, species, year
testBioDataShort <- testBioData %>% 
  group_by(year, site) %>%
  summarise(meanFBB = round(mean(as.numeric(fbb), na.rm = TRUE))) %>%
  mutate(year = as.numeric(year),
         site = as.factor(site)) %>%
  drop_na()

summary(testBioDataShort)

# need to create a date column - CHATGPT HELP

testBioDataShort$date <- as.Date(as.numeric(testBioDataShort$meanFBB) - 1, 
                                  origin = paste(testBioDataShort$year, 
                                                 "-01-01", sep=""))

testBioDataShort$dateFormatted <- format(testBioDataShort$date, 
                                          "%d/%m/%Y")


#### climwin ####

# mean fbb as the reference day
mean(testBioDataShort$meanFBB) # day 112

# run sliding window for one species

climwinResult <- slidingwin(xvar = list(Temp = filter(testTempDataShort, 
                                     site == "DNC")$meanDayTemp),
           cdate = filter(testTempDataShort, 
                          site == "DNC")$dateFormatted,
           bdate = filter(testBioDataShort,
                          site == "DNC")$dateFormatted,
           baseline = glm(meanFBB ~ 1, data = filter(testBioDataShort,
                                                           site == "DNC")),
           range = c(50,0), type = "absolute", stat = "mean",
           cinterval = "day", refday = c(20,4)
           )


#### Bayesian sliding window ####

# format data, want a column per year for the temperature
testTempDataWide <- testTempDataShort %>% 
  pivot_wider(id_cols = c("site", "Day"),
              names_from = year,
              values_from = meanDayTemp) %>%
  drop_na() %>%
  arrange(Day)

# then need just one site and only the years
nimbleTemp <- filter(testTempDataWide, 
                     site == "PTH")[,3:13]
# for bio data reduce to year and date for one site
nimbleBio <- filter(testBioDataShort,
                    site == "PTH")[,"meanFBB"]


n_iter <- 10000000
n_burnin <- 10000
n_chains <- 2

# set up initial values 

data_input <- list(temperature = nimbleTemp,
                   biological_variable = nimbleBio$meanFBB)

constants <- list(years = length(nimbleBio$meanFBB),
                  windowOpen = 1,
                  refDay = 50,
                  shortestWindow = 1,
                  meanintercept = 100,
                  meanslope = -3)

inits <- list(open = round(runif(1, 1, 30)),
              duration = round(runif(1, 1, 20)),
              intercept = rnorm(1, 100, sd = 20),
              slope = rnorm(1, -3, sd = 5),
              error = rgamma(1, 2, 0.5),
              temperature_window = rep(1, 30),
              resultWindowOpen = runif(1,1,50),
              resultWindowClose = runif(1,1,50))

parametersToMonitor <- c("resultWindowOpen",
                         "resultWindowClose",
                         "slope",
                         "error",
                         "intercept",
                         "open",
                         "duration")

model_result <- nimbleModel(data_input = data_input,
                            constants = constants,
                            parametersToMonitor = parametersToMonitor,
                            inits = inits, 
                            n_iter = n_iter, 
                            n_chains = n_chains, 
                            n_burnin = n_burnin,
                            n_thin = 20)

MCMCsummary(model_result)
MCMCplot(model_result)
MCMCtrace(model_result, 
          params = c('slope',
                     'resultWindowOpen',
                     'resultWindowClose'), 
          ISB = FALSE, 
          exact = TRUE,
          pdf = FALSE)

#### Plots of results ####
