#### Script to wrangle MET OFFICE temp data into correct format ################

#' This script holds the function code for manipulating the met office data into
#' the correct format for subsequent analyses. It imports the net cdf and then 
#' turns it into a data frame for latter use
#' 
#' Inputs: 
#' - folderPath = path to folder files are in
#' - filenames = path to net cdf file to import 
#' - key variable = name of key variable to extract as written in net cdf
#' 
#' Outputs:
#' - dataframe of year, fulldate, yDay, keyVariable, longitude, latitude

#### Set up ####################################################################

#### load packages ####

library(tidyverse)
library(ncdf4)
library(lubridate)

#### Function code #############################################################

editMetOfficeData <- function(folderPath,
                              filename,
                              keyVariable,
                              longitudes){
  
  # first, read in the net cdf file
  ncData <- nc_open(paste0(folderPath, filename))
  
  # then pull put the variables I want (tMax, longitude, latitude, day)
  keyVariable <- ncvar_get(ncData, keyVariable)
  long <- ncvar_get(ncData, "longitude")
  lat <- ncvar_get(ncData, "latitude")
  time <- ncvar_get(ncData, "time_bnds")
  
  # now melt each into a good format
  keyVariableMelted <- reshape2::melt(keyVariable, value.name = "keyVariable") %>%
    rename(x = Var1, y = Var2, day = Var3)
  longMelted <- reshape2::melt(long, value.name = "longitude") %>%
    rename(x = Var1, y = Var2)
  latMelted <- reshape2::melt(lat, value.name = "latitude") %>%
    rename(x = Var1, y = Var2)
  # time is a bit different as has start and end time rows, drop end one to keep
  # correct number of days
  timeMelted <- reshape2::melt(time[1,], value.name = "time") %>%
    rowid_to_column() %>%
    rename(day = rowid)
  
  # now want to try and join all of these
  fullMelted <- left_join(keyVariableMelted, longMelted, join_by(x == x, 
                                                          y == y)) %>%
    left_join(latMelted, join_by(x == x, y == y)) %>%
    left_join(timeMelted, join_by(day == day)) %>%
    # now add the date, need to pull this from the filename
    # add the day - 1 to give each day not just the start date from filename
    mutate(fullDate = as.numeric(substr(filename, 29, 36))+(day-1),
           year = substr(filename, 29, 32),
           yDay = yday(ymd(substr(filename, 29, 36)))+(day-1)) %>%
    # finally, remove columns I don't need and reorder
    select(c("fullDate", "year", "yDay","keyVariable", "latitude", "longitude")) %>%
    drop_na()
  
  gc()
  rm(ncData)
  
  return(fullMelted)
  
}