#### Script to reformat temperature data into the same style as simulated ######

#' This script holds the function code for reformating temperature data into the 
#' same format as the simulated temperature data
#' 
#' Inputs: 
#' - folderPath = path to folder files are in
#' - filename = path to rds file to import 
#' - outputPath = folder to output into
#' - numDays = number of days to take data for
#' - reference = day to count back from
#' 
#' Outputs:
#' - dataframe of year, fulldate, yDay, keyVariable, longitude, latitude

#### Set up ####################################################################

#### load packages ####

library(tidyverse)

#### Function code #############################################################

reformatTempData <- function(folderPath,
                              filename,
                              outputPath, 
                              numDays,
                              reference){
  
  # want to turn the datafile into something that has X days beginning at 
  # 'reference' and each column is a year
  
  testTemp <- readRDS(paste0(folderPath, filename)) # temporary temperature file
  
  # first subset to just the day of year needed
  
  testTemp2 <- filter(testTemp, yDay <= reference & yDay >= reference - numDays)
  
  # now reformat to have column as year
  
  testTemp3 <- testTemp2 %>% 
    select(year, keyVariable) %>% # take just the columns we need 
    group_by(year) %>% # group by and split by year
    group_split() %>%
    bind_cols() %>%
    select(starts_with("keyVariable")) # now remove year columns
  
  saveRDS(testTemp3, paste0(outputPath, filename))
  
  return(1)


}