#### Script to reformat temperature data to be used in climwin #################

#' This script holds the function code for manipulating the temperature data
#' used for the Bayesian analyses into something that can be used by the
#' climwin package
#' 
#' Inputs: 
#' - folderPath = path to folder files are in
#' - filename = path to file to import 
#' 
#' Outputs:
#' - dataframe of Date, Temp
#' With data formatted correctly for climwin (dd/mm/yyyy)

#### Set up ####################################################################

#### load packages ####

library(tidyverse)
library(lubridate)

#### Function code #############################################################

reformatTempClimwin <- function(folderPath,
                                filename){
  
  # first, import the real temperature data in previous format
  temperatureFile <- readRDS(paste0(folderPath, filename)) %>%
  # now need to alter the format to get a Date column in dd/mm/yyyy
    mutate(Date_ymd = ymd(temperatureFile$fullDate)) %>%
    mutate(Date = format(Date_ymd, "%d/%m/%Y")) %>%
    select(Date, keyVariable)
  
  colnames(temperatureFile) <- c("Date", "Temp")
  
  return(temperatureFile)
  
}