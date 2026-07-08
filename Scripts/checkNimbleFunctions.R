#### Check script: Nimble Functions ############################################

#' This script checks the Nimble functions in script:
#' 'nimbleSlidingWindow.R'

#### Set up ####################################################################

#### load packages ####

library(nimble)

#### source code ####

source("./Functions/nimbleSlidingWindow.R")

#### create test data ####

# need a vector of numbers to take the mean of

set.seed(2026)
testVector <- rnorm(100, mean = 6, sd = 4)

#### Testing nimbleSlidingWindow ###############################################

### Test 1: does it calculate the correct mean: integer input? ####

open <- 5
duration <- 5
close <- open - duration

# run the test using the function
test1 <- nimbleSlidingWindow(open = open,
                             close = close,
                             temperature = testVector)

# run it with the manual part pull out
manualCheck1 <- mean(testVector[close:ceiling(open)])

# run hard coded just to check 'ceiling' is doing what I expect
manualManualCheck1 <- mean(testVector[5:(5-5)])

test1 - manualCheck1 # 0: CHECK PASSED 08.07.26

test1 - manualManualCheck1 # 0: CHECK PASSED 08.07.26

### Test 2: correct mean: integer open but not duration? ####

open <- 5
duration <- 5.3 # one <0.5 over
duration2 <- 5.8 # one >0.5 over
close <- open - duration
close2 <- open - duration2

# run the test using the function
test2 <- nimbleSlidingWindow(open = open,
                             close = close,
                             temperature = testVector)
test2b <- nimbleSlidingWindow(open = open,
                             close = close2,
                             temperature = testVector)

# run it with the manual part pull out
manualCheck2 <- mean(testVector[trunc(close):ceiling(open)])
manualCheck2b <- mean(testVector[trunc(close):ceiling(open)])

test2 - manualCheck2 # 0: CHECK PASSED 08.07.26
test2b - manualCheck2b # 0: CHECK PASSED 08.07.26

### Test 3: correct mean: integer duration but not open? ####

duration <- 5
open <- 5.3 # one <0.5 over
open2 <- 5.8 # one >0.5 over
close <- open - duration
close2 <- open2 - duration

# run the test using the function
test3 <- nimbleSlidingWindow(open = open,
                             close = close,
                             temperature = testVector)
test3b <- nimbleSlidingWindow(open = open2,
                              close = close2,
                              temperature = testVector)

# run it with the manual part pull out
manualCheck3 <- mean(testVector[trunc(close):ceiling(open)])
manualCheck3b <- mean(testVector[trunc(close):ceiling(open)])

test3 - manualCheck3 # 0: CHECK PASSED 08.07.26
test3b - manualCheck3b # 0: CHECK PASSED 08.07.26

### Test 4: correct mean: both non integer? ####

duration <- 5.6
open <- 5.3 # one <0.5 over
# second pair sum to integer - see how that impacts things
duration2 <- 5.4
open2 <- 5.6 # one <0.5 over
close <- open - duration
close2 <- open2 - duration2

# run the test using the function
test4 <- nimbleSlidingWindow(open = open,
                             close = close,
                             temperature = testVector)
test4b <- nimbleSlidingWindow(open = open2,
                              close = close2,
                              temperature = testVector)

# run it with the manual part pull out
manualCheck4 <- mean(testVector[trunc(close):ceiling(open)])
manualCheck4b <- mean(testVector[trunc(close):ceiling(open)])

test4 - manualCheck4 # 0: CHECK PASSED 08.07.26
test4b - manualCheck4b # 0: CHECK PASSED 08.07.26

#### Testing weightedMeanNimble ################################################

### Test 1: does it calculate the correct mean: equal weights ####

values <- testVector[5:10]
weights <- rep(1, 6)

# run the test using the function
test1.1 <- weightedMeanNimble(values, weights)

# run it with base R weighted mean
manualCheck1.1 <- weighted.mean(x = values,
                              w = weights)

# run with manual code
adjustedValues <- sum(values*weights)
adjustedWeights <- sum(weights)
manualManualCheck1.1 <- adjustedValues/adjustedWeights

test1.1 - manualCheck1.1 # 0: CHECK PASSED 13.03.26

test1.1 - manualManualCheck1.1 # 0: CHECK PASSED 13.03.26

### Test 2: correct mean: different start weight ####

values <- testVector[5:10]
weights <- c(0.1, rep(1, 5)) # one above 0.5 and one below
weights2 <- c(0.7, rep(1, 5))

# run the test using the function
test2.1 <- weightedMeanNimble(values, weights)
test2.1b <- weightedMeanNimble(values, weights2)

# run it with base R weighted mean
manualCheck2.1 <- weighted.mean(x = values,
                                w = weights)
manualCheck2.1b <- weighted.mean(x = values,
                                w = weights2)

# run with manual code
adjustedValues <- sum(values*weights)
adjustedWeights <- sum(weights)
manualManualCheck2.1 <- adjustedValues/adjustedWeights

adjustedValues2 <- sum(values*weights2)
adjustedWeights2 <- sum(weights2)
manualManualCheck2.1b <- adjustedValues2/adjustedWeights2

test2.1 - manualCheck2.1 # 0: CHECK PASSED 13.03.26
test2.1b - manualCheck2.1b # 0: CHECK PASSED 13.03.26

test2.1 - manualManualCheck2.1 # 0: CHECK PASSED 13.03.26
test2.1b - manualManualCheck2.1b # 0: CHECK PASSED 13.03.26

### Test 3: correct mean: different start and end weight ####

values <- testVector[5:10]
weights <- c(0.1, rep(1, 4), 0.6) # one above 0.5 and one below
weights2 <- c(0.7, rep(1, 4), 0.4)

# run the test using the function
test3.1 <- weightedMeanNimble(values, weights)
test3.1b <- weightedMeanNimble(values, weights2)

# run it with base R weighted mean
manualCheck3.1 <- weighted.mean(x = values,
                                w = weights)
manualCheck3.1b <- weighted.mean(x = values,
                                 w = weights2)

# run with manual code
adjustedValues <- sum(values*weights)
adjustedWeights <- sum(weights)
manualManualCheck3.1 <- adjustedValues/adjustedWeights

adjustedValues2 <- sum(values*weights2)
adjustedWeights2 <- sum(weights2)
manualManualCheck3.1b <- adjustedValues2/adjustedWeights2

test3.1 - manualCheck3.1 # 0: CHECK PASSED 13.03.26
test3.1b - manualCheck3.1b # 0: CHECK PASSED 13.03.26

test3.1 - manualManualCheck3.1 # 0: CHECK PASSED 13.03.26
test3.1b - manualManualCheck3.1b # 0: CHECK PASSED 13.03.26

#### Testing nimbleWeightedSidingWindow ########################################

### Test 1: does it calculate the correct mean: only integers ####

open <- 10
duration <- 5
close <- open - duration

# run the test using the function
test1.3 <- nimbleWeightedSlidingWindow(open = open,
                             close = close,
                             temperature = testVector)

# run it with the manual part pull out
manualCheck1.3 <- mean(testVector[trunc(close):ceiling(open)])

test1.3 - manualCheck1.3 # Not 0: ROUNDING ERROR - NUMBERS LOOK SAME

### Test 2: correct mean: integer open but not duration? ####

open <- 10
duration <- 5.3 # one <0.5 over
duration2 <- 5.8 # one >0.5 over
close <- open - duration
close2 <- open - duration2

# run the test using the function
test2.3 <- nimbleWeightedSlidingWindow(open = open,
                             close = close,
                             temperature = testVector)
test2.3b <- nimbleWeightedSlidingWindow(open = open,
                              close = close2,
                              temperature = testVector)

# run it with the manual part pull out
manualCheck2.3 <- weighted.mean(x = testVector[trunc(close):ceiling(open)],
                                w = c(1-(close - trunc(close)), rep(1, 6)))
manualCheck2.3b <- weighted.mean(x = testVector[trunc(close2):ceiling(open)],
                                 w = c(1-(close2 - trunc(close2)), rep(1, 6)))

test2.3 - manualCheck2.3 # 0: CHECK PASSED 08.07.26
test2.3b - manualCheck2.3b # 0: CHECK PASSED 08.07.26

### Test 3: correct mean: integer duration but not open? ####

duration <- 5
open <- 6.3 # one <0.5 over
open2 <- 6.8 # one >0.5 over
close <- open - duration
close2 <- open2 - duration

# run the test using the function
test3.3 <- nimbleWeightedSlidingWindow(open = open,
                                       close = close,
                                       temperature = testVector)
test3.3b <- nimbleWeightedSlidingWindow(open = open2,
                                        close = close2,
                                        temperature = testVector)

# run it with the manual part pull out
manualCheck3.3 <- weighted.mean(x = testVector[trunc(close):ceiling(open)],
                                w = c(1-(close-trunc(close)),
                                      rep(1, 5), 
                                      ((open)-trunc(open))))
manualCheck3.3b <- weighted.mean(x = testVector[trunc(close2):ceiling(open2)],
                                 w = c(1-(close2-trunc(close2)),
                                       rep(1, 5), 
                                       ((open2)-trunc(open2))))


test3.3 - manualCheck3.3 # 0: CHECK PASSED 08.07.26
test3.3b - manualCheck3.3b # 0: CHECK PASSED 08.07.26

### Test 4: correct mean: both non integer? ####

duration <- 5.6
open <- 7.3 # one <0.5 over
# second pair sum to integer - see how that impacts things
duration2 <- 5.4
open2 <- 7.6 # one <0.5 over
close <- open - duration
close2 <- open2 - duration2

# run the test using the function
test4.3 <- nimbleWeightedSlidingWindow(open = open,
                                       close = close,
                                       temperature = testVector)
test4.3b <- nimbleWeightedSlidingWindow(open = open2,
                                        close = close2,
                                        temperature = testVector)

# run it with the manual part pull out
manualCheck4.3 <- weighted.mean(x = testVector[trunc(close):ceiling(open)],
                                w = c(1-(close-trunc(close)),
                                      rep(1, 6), 
                                      ((open)-trunc(open))))
manualCheck4.3b <- weighted.mean(x = testVector[trunc(close2):ceiling(open2)],
                                 w = c(1-(close2-trunc(close2)),
                                       rep(1, 5), ((open2)-trunc(open2))))

test4.3 - manualCheck4.3 # 0: CHECK PASSED 08.07.26
test4.3b - manualCheck4.3b # 0: CHECK PASSED 08.07.26
