#### Script runs the the climwin models for the real temperature data ##########

#' This script runs the climwin models for the real temperature simulations

#### Set up ####################################################################

#### load packages ####

library(tidyverse)
library(furrr)
library(climwin)

#### source code ####

source("./Functions/reformatTempClimwin.R")
source("./Functions/reformatBioClimwin.R")

#### Load all data #############################################################

biologicalFiles <- as.list(list.files("./Data/bioData2/", pattern = "rds")) 
temperatureFiles <- as.list(list.files("./Data/TempDataReal/", pattern = "rds")) 

# set the folder paths

folderPathBio <- "./Data/bioData2/"
folderPathTemp <- "./Data/TempDataReal/"

# will be using the sliding win function first - might use randwin later
# want to output the actual model and the window selected

# first test of a single example

bioData <- reformatBioClimwin(filename = biologicalFiles[1],
                              folderPath = folderPathBio)

tempData <- reformatTempClimwin(filename = temperatureFiles[1],
                              folderPath = folderPathTemp)


climwinResult <- slidingwin(xvar = tempData$Temp,
                            cdate = tempData$Date,
                            bdate = bioData$Date,
                            baseline = lmer(Date ~ 1 + (1|Year)),
                            range = c(99, 0), 
                            type = "absolute",
                            stat = "mean",
                            func = "linear",
                            refday = c(1,1))

