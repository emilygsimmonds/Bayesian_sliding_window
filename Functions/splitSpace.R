#### Script to split reformatted MET office data by space not time #############

#' This script holds the function code for splitting the reformatting MET office
#' data by space. 
#' 
#' Inputs: 
#' - longitude
#' - fullData
#' 
#' Outputs:
#' - dataframe of year, fulldate, yDay, keyVariable, longitude, latitude

#### Set up ####################################################################

#### load packages ####

library(tidyverse)
library(ncdf4)

#### Function code #############################################################

splitSpace <- function(fullData,
                       longitudeSplit){
  
  fullDataSplit <- filter(fullData, round(longitude, 6) == 
                            round(longitudeSplit, 6))
  
  return(fullDataSplit)
  
}