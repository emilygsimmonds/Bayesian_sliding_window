#### Script to reformat simulated bio data to be used in climwin ###############

#' This script holds the function code for manipulating the biological data
#' used for the Bayesian analyses into something that can be used by the
#' climwin package
#' 
#' Inputs: 
#' - folderPath = path to folder files are in
#' - filename = path to file to import 
#' 
#' Outputs:
#' - dataframe of Date
#' With data formatted correctly for climwin (dd/mm/yyyy)

#### Set up ####################################################################

#### load packages ####

library(tidyverse)
library(lubridate)

#### Function code #############################################################

reformatBioClimwin <- function(folderPath,
                                filename){
  
  # first, import the simulated biological data in previous format
  bioFile <- readRDS(paste0(folderPath, filename))
  
  # need to make this a dataframe and make date integer
  bioDataFile <- data.frame(Date_yday = round(bioFile),
                            Year = seq(1974, 2024, 1)) %>%
    mutate(Date_mday   = lubridate::day(as.Date(str_c(Year, "-01-01")) + 
                                          (Date_yday - 1)),
           Date_m = lubridate::month(as.Date(paste0(Year, "-01-01")) + 
                                       (Date_yday - 1)),
           Date  = format(str_c(Date_mday, Date_m, Year, sep = "/"), 
                             format = "%d/%m/%Y"))
  
  return(bioDataFile)
  
}