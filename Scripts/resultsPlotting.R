#### Plotting script: Result plots #############################################

#' This script runs plots of results from simulated scenarios

#### Set up ####################################################################

#### load packages ####

library(tidyverse)
library(MCMCvis)
library(patchwork)

#### source code ####

source('./Functions/createPlottingData.R')

#### load data ####

# biological inputs
biologicalInputs <- read.csv("./Data/BiologicalInputs1.csv") %>%
  mutate(tempDataNames = parse_number(tempDataNames))

# add in a column with the temperature scenarios
temperatureInputs <- read.csv("./Data/TemperatureInputs1.csv") %>%
  mutate(tempDataNames = marker) %>%
  select(tempDataNames, noise, mean, tScenario)
  
combinedInputs <- left_join(biologicalInputs, temperatureInputs)

# import all results for each scenario, summarise and combine into a single list

modelResults <- paste0("ModelResult", 
                       combinedInputs$bioMarker, ".rds")

# just check that works
#summary(parse_number(modelResults) - combinedInputs$bioMarker)

plottingData <- map2_df(.x = modelResults, 
                     .y = split(combinedInputs, 1:length(combinedInputs[,1])),
                     .f = createPlottingData) %>%
  mutate(error = case_when(parameter == "open" ~ mean - trueOpen,
                           parameter == "duration" ~ mean - trueDuration,
                           parameter == "slope" ~ mean - trueSlope,
                           parameter == "error" ~ mean - trueBioNoise,
                           parameter == "intercept" ~ 0,
                           TRUE ~ NA),
         trueInCI = case_when(parameter == "open" & between(trueOpen, 
                                           lowerCI, upperCI) ~ TRUE,
                              parameter == "duration" & between(trueDuration, 
                                                            lowerCI, upperCI) ~
                                                            TRUE,
                              parameter == "slope" & between(trueSlope, 
                                                             lowerCI, upperCI) ~
                                                            TRUE,
                              parameter == "error" & between(trueBioNoise, 
                                                             lowerCI, upperCI) ~
                                                            TRUE,
                              parameter == "intercept" & between(rep(0, n()), 
                                                             lowerCI, upperCI) ~
                                                            TRUE,
                              TRUE ~ FALSE))

summary(plottingData)

# save the plotting data
write.csv(plottingData, "./Data/plottingData.csv", row.names = FALSE)
#plottingData <- read.csv("./Data/plottingData.csv")

#### Plotting ##################################################################

#### Figure 1: accuracy of open and duration ###################################

# first plot this just for the baseline scenario: trueTNoise = 3, 
# trueTMean = 2.55, trueBioNoise = 2.5, trueSlope = 5

plottingDataBaseline = filter(plottingData,
                              trueTNoise == 3,
                              trueTMean == 2.55,
                              trueBioNoise == 2.5,
                              trueSlope == 5) 

summary(plottingDataBaseline)

# plot the error in open estimation by open and duration parameters
openPlot <- ggplot(aes(y = error, 
           x = as.factor(trueOpen)), data = filter(plottingDataBaseline,
                                        parameter == "open")) +
  labs(title = "Accuracy of open day", 
       y = "Error (est - truth)",
       x = "True open day") +
  geom_violin(aes(fill = as.factor(trueOpen))) +
  scale_fill_viridis_d()+
  facet_wrap(vars(as.factor(trueDuration)), labeller = 
               as_labeller(c('1' = "Duration = 1",
                             '15' = "Duration = 15",
                             '30' = "Duration = 30"))) +
  theme_minimal() +
  theme(legend.position = "none")

durationPlot <- ggplot(aes(y = error, 
                x = as.factor(trueDuration)), 
                data = filter(plottingDataBaseline,
                parameter == "duration")) +
  labs(title = "Accuracy of duration length", 
       y = "Error (est - truth)",
       x = "True duration") +
  geom_violin(aes(fill = as.factor(trueDuration))) +
  scale_fill_viridis_d()+
  facet_wrap(vars(as.factor(trueOpen)), labeller = 
               as_labeller(c('1' = "Open = 1",
                             '25' = "Open = 15",
                             '50' = "Open = 50"))) +
  theme_minimal() +
  theme(legend.position = "none")

openPlot + durationPlot

ggsave("./Plots/AccuracyOpen.png", last_plot())

# plot the % of results in CI by open and duration parameters

# first - summarise the data into a count of TRUE
summaryData <- plottingData %>%
  group_by(parameter, trueBioNoise,
           trueSlope, trueOpen, trueDuration) %>%
  summarise(percentTrue = (sum(trueInCI)/n())*100,
            percentFalse = (sum(!trueInCI)/n())*100) %>%
  ungroup()

#summary(summaryData)

openPlot2 <- ggplot() +
  labs(title = "Accuracy of open day", 
       y = "% with true in CI",
       x = "True open day") +
  geom_col(aes(y = percentTrue,
               x = as.factor(trueOpen),
               fill = as.factor(trueOpen)), 
           data = filter(summaryData,
           parameter == "open",
           trueBioNoise == 2.5)) +
  scale_fill_viridis_d() +
  facet_wrap(vars(as.factor(trueDuration)), labeller = 
               as_labeller(c('1' = "Duration = 1",
                             '15' = "Duration = 15",
                             '30' = "Duration = 30"))) +
  theme_minimal() +
  theme(legend.position = "none")

durationPlot2 <- ggplot() +
  labs(title = "Accuracy of duration", 
       y = "% with true in CI",
       x = "True duration") +
  geom_col(aes(y = percentTrue,
               x = as.factor(trueDuration),
               fill = as.factor(trueDuration)), 
           data = filter(summaryData,
                         parameter == "duration",
                         trueBioNoise == 2.5)) +
  scale_fill_viridis_d() +
  facet_wrap(vars(as.factor(trueOpen)), labeller = 
               as_labeller(c('1' = "Open = 1",
                             '25' = "Open = 15",
                             '50' = "Open = 50"))) +
  theme_minimal() +
  theme(legend.position = "none")

openPlot2 + durationPlot2

ggsave("./Plots/AccuracyOpen2.png", last_plot())
  
#### Figure 2: accuracy of slope ###############################################

plottingDataSlope = filter(plottingData,
                              trueTNoise == 3,
                              trueTMean == 2.55,
                              trueBioNoise == 5) 

summary(plottingDataSlope)

# plot the error in open estimation by open and duration parameters
slopePlot <- ggplot(aes(y = error, 
           x = as.factor(trueSlope)), data = filter(plottingDataSlope,
                                                   parameter == "slope")) +
  geom_violin(aes(fill = as.factor(trueOpen))) +
  labs(title = "Accuracy of slope estimate", 
       y = "Error (est - truth)",
       x = "True slope") +
  scale_fill_viridis_d()+
  facet_grid(rows = vars(as.factor(trueDuration)),
            cols = vars(as.factor(trueOpen)),
            labeller = 
              labeller(.rows = c('1' = "Duration = 1",
                                           '15' = "Duration = 15",
                                           '30' = "Duration = 30"),
                          .cols = c('1' = "Open = 1",
                            '25' = "Open = 15",
                            '50' = "Open = 50"))) +
  theme_minimal() +
  theme(legend.position = "none")

slopePlot

ggsave("./Plots/AccuracySlope.png", last_plot())

slopePlot2 <- ggplot() +
  labs(title = "Accuracy of slope", 
       y = "% with true in CI",
       x = "True slope") +
  geom_col(aes(y = percentTrue,
               x = as.factor(trueSlope),
               fill = as.factor(trueSlope)), 
           data = filter(summaryData,
                         parameter == "slope",
                         trueBioNoise == 5)) +
  scale_fill_viridis_d() +
  facet_grid(rows = vars(as.factor(trueDuration)),
             cols = vars(as.factor(trueOpen)),
             labeller = 
               labeller(.rows = c('1' = "Duration = 1",
                                  '15' = "Duration = 15",
                                  '30' = "Duration = 30"),
                        .cols = c('1' = "Open = 1",
                                  '25' = "Open = 15",
                                  '50' = "Open = 50"))) +
  theme_minimal() +
  theme(legend.position = "none")

slopePlot2 

ggsave("./Plots/AccuracySlope2.png", last_plot())

#### Figure 3: uncertainty #####################################################

plottingDataBaselineUncert <- plottingData %>%
  mutate(Uncertainty = upperCI - lowerCI)

# plot the error in open estimation by open and duration parameters
openPlotU <- ggplot(aes(y = Uncertainty, 
                       x = as.factor(trueOpen)), 
                   data = filter(plottingDataBaselineUncert,
                          parameter == "open")) +
  labs(title = "Uncertainty of open day", 
       y = "CI width",
       x = "True open day") +
  geom_violin(aes(fill = as.factor(trueOpen))) +
  scale_fill_viridis_d()+
  facet_wrap(vars(as.factor(trueDuration)), labeller = 
               as_labeller(c('1' = "Duration = 1",
                             '15' = "Duration = 15",
                             '30' = "Duration = 30"))) +
  theme_minimal() +
  theme(legend.position = "none")

durationPlotU <- ggplot(aes(y = Uncertainty, 
                        x = as.factor(trueDuration)), 
                    data = filter(plottingDataBaselineUncert,
                                  parameter == "duration")) +
  labs(title = "Uncertainty of duration", 
       y = "CI width",
       x = "True duration day") +
  geom_violin(aes(fill = as.factor(trueDuration))) +
  scale_fill_viridis_d()+
  facet_wrap(vars(as.factor(trueOpen)), labeller = 
               as_labeller(c('1' = "Open = 1",
                             '25' = "Open = 15",
                             '50' = "Open = 50"))) +
  theme_minimal() +
  theme(legend.position = "none")

openPlotU + durationPlotU

ggsave("./Plots/UncertaintyOpen.png", last_plot())

#### Figure 4: impact of noise #################################################

plottingDataNoise = filter(plottingData,
                              trueTNoise == 3,
                              trueTMean == 2.55,
                              trueSlope == 5,
                           trueOpen == 25,
                           trueDuration == 30) 

# plot the error in open estimation by open and duration parameters
openPlotNoise <- ggplot(aes(y = error, 
                       x = as.factor(trueOpen)), data = filter(plottingDataNoise,
                                                               parameter == "open")) +
  labs(title = "Accuracy of open day", 
       y = "Error (est - truth)",
       x = "True open day") +
  geom_violin(aes(fill = trueBioNoise)) +
  scale_fill_viridis_c() +
  facet_wrap(vars(trueBioNoise)) +
  theme_minimal() +
  theme(legend.position = "none")

durationPlotNoise <- ggplot(aes(y = error, 
                           x = as.factor(trueDuration)), 
                       data = filter(plottingDataNoise,
                                     parameter == "duration")) +
  labs(title = "Accuracy of duration length", 
       y = "Error (est - truth)",
       x = "True duration") +
  geom_violin(aes(fill = trueBioNoise)) +
  scale_fill_viridis_c() +
  facet_wrap(vars(trueBioNoise)) +
  theme_minimal() +
  theme(legend.position = "none")

slopePlotNoise <- ggplot(aes(y = error, 
                                x = trueSlope), 
                            data = filter(plottingDataNoise,
                                          parameter == "slope")) +
  labs(title = "Accuracy of slope", 
       y = "Error (est - truth)",
       x = "True slope") +
  geom_violin(aes(fill = trueBioNoise)) +
  scale_fill_viridis_c() +
  facet_wrap(vars(trueBioNoise)) +
  theme_minimal() +
  theme(legend.position = "none")

openPlotNoise + durationPlotNoise + slopePlotNoise

ggsave("./Plots/NoiseScenario.png", last_plot())

#### Figure 5: impact of signal strength #######################################

#### Supplementary: convergence ################################################