#### Script to put real temp data in correct format ############################

#' This script imports the raw real temperature data, tidies and exports as .rds

#### Set up ####################################################################

#### load packages ####

library(tidyverse)
library(furrr)
library(ncdf4) # package for netcdf manipulation

#### source code ####

source("./Functions/formatMetOfficeData.R")

#### Reformat the temperature data for Bayesian analysis #######################

# list files

tempDataNames <- as.list(list.files("./Data/TempDataReal/", pattern = "nc")) 
# 1128 files

# this function pulls out the key data needed and puts it into a dataframe
# should do this for all data so will be one long file for all days and 
# locations. Too big if do this for all data. 
fullTempData <- map(.x = tempDataNames, .f = editMetOfficeData, 
                    folderPath = "./Data/TempDataReal/",
                    keyVariable = "tasmax") 
#%>%
#                bind_rows()

# cannot form into single dataset, instead, split and save by location
# but for all time. Then try to select the locations with the most complete data

# create a list of all longitudes and latitudes - need to pull in one file to 
# do this

ncData <- nc_open(paste0("./Data/TempDataReal/", tempDataNames[1]))

# then pull longitude - each matches to unique latitude so only need one
long <- ncvar_get(ncData, "longitude")

# now melt each into a good format
longMelted <- reshape2::melt(long, value.name = "longitude") %>%
  rename(x = Var1, y = Var2)

# make a list of all longitudes
listLong <- as.list(longMelted$longitude)

map(.x = listLong, ~{
  
  # For each longitude want to map over all files and pull out just that
  # location. At the end, join and save out. 
  longitude <- .x
  
  output <- map(fullTempData, .f = splitSpace, 
                longitudeSplit = longitude) %>%
  bind_rows()
  
  if(length(output[,1] > 1000)){ # restrict saving to only those with data
  saveRDS(output, file = paste0("./Data/TempDataReal/spaceSplit", 
                                round(longitude, 3),
                                    ".rds"))}
  
  return(print(.x))
  
})

# then clear all files, re-import each spatial one and create a datafile of file
# names and file length to see which are most complete (if any)

# now reformat to be used in simulations and models

tempDataNames <- list.files("./Data/TempDataReal/", pattern = "rds")

map(tempDataNames[1:100], .f = reformatTempData, 
    folderPath = "./Data/TempDataReal/",
    outputPath = "./Data/TempDataReal/Editted/", 
    numDays = 100,
    reference = 100)

