#### Script to put real temp data in correct format ############################

#' This script imports the raw real temperature data, tidies and exports as .rds

#### Set up ####################################################################

#### load packages ####

library(tidyverse)
library(furrr)
library(ncdf4) # package for netcdf manipulation

#### source code ####

source("./Functions/formatMetOfficeData.R")

#### Reformat the temperature data #############################################

# list files

tempDataNames <- as.list(list.files("./Data/TempDataReal/", pattern = "nc")) 
# 1128 files

# finding the first 100 longitudes
ncData <- nc_open(paste0("./Data/TempDataReal/", tempDataNames[1]))
long <- ncvar_get(ncData, "longitude")
longMelted <- reshape2::melt(long, value.name = "longitude") %>%
  rename(x = Var1, y = Var2)
firstLongitudes <- longMelted[1:100, "longitude"]

# this function pulls out the key data needed and puts it into a dataframe
# should do this for all data so will be one long file for all days and 
# locations. Too big if do this for all data. 
# Instead, choose first 100 longitudes.
fullTempData <- map(.x = tempDataNames, .f = editMetOfficeData, 
                    folderPath = "./Data/TempDataReal/",
                    keyVariable = "tasmax",
                    longitudes = firstLongitudes) %>%
                bind_rows()

# now want to reduce to just 100 locations - will choose those with most
# complete data. Also reduce to 50 years. Then reformat so that each column is
# a year. 




