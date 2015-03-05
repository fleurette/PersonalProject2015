# Data shape
# 1 is intertweetTime
# 2 is ids
# 3 is the DOBs
# 4 is the Tweet Times
# 5 is the Mean

# BinSize is a time length given in seconds, compute mean of intertweetTime
# for tweets falling in each bin
# Scale down
# 39 weeks before the endDate
# Assume there are some tweets between endDate and startDate
pregnancyLength <- 39*7*24*3600

getTimeIncrement <- function(timeLength,binSize) {
  numBins <- pregnancyLength/binSize
  timeInterval <- ceiling(numBins/(pregnancyLength/timeLength))
}

getIntertweetHistogamPregnancy <- function(tweetTimes, binSize, endDate) {
  numBins <- pregnancyLength/binSize
  total <- rep(0,numBins)
  count <- rep(0,numBins)
  startDate <- endDate - pregnancyLength
  result <- rep(0,numBins)

  for (i in seq(2,length(tweetTimes))) {
    if(endDate > tweetTimes[i] && startDate < tweetTimes[i]) {
      binIndex <- ceiling((tweetTimes[i] - startDate)/binSize)
      total[binIndex] <- tweetTimes[i-1]-tweetTimes[i]
      count[binIndex] <- count[binIndex] + 1
    }
  }

  # Compute average for each bin
  for (i in seq(1,numBins)) {
    if(count[i] > 0) {
      result[i] <- total[i]/count[i]
    }
  }

  # Scale down
  areaUnderTheCurve <- sum(result)*binSize
  result <- result/(areaUnderTheCurve)

  return (list(x=seq(1,numBins),y=result))
}

# Just count everything
getTweetCount <- function(tweetTimes, binSize, endDate) {
  pregnancyLength <- 39*7*24*3600
  numBins <- pregnancyLength/binSize
  total <- rep(0,numBins)
  count <- rep(0,numBins)
  startDate <- endDate - pregnancyLength
  result <- rep(0,numBins)

  for (i in seq(1,length(tweetTimes))) {
    if(endDate > tweetTimes[i] && startDate < tweetTimes[i]) {
      binIndex <- ceiling((tweetTimes[i] - startDate)/binSize)
      result[binIndex] <- result[binIndex] + 1
    }
  }

  # Scale down
  areaUnderTheCurve <- max(sum(result)*binSize,1)
  result <- result/(areaUnderTheCurve)

  return (list(x=seq(1,numBins),y=result))
}

getMean <- function(distributions) {
  numDistributions <- length(distributions)
  numElements <- length(distributions[[1]][["y"]])

  result <- rep(0,numElements)

  for (distribution in distributions) {
    result <- result + distribution[["y"]]
  }

  result <- result / numDistributions

  return (list(x=seq(1,numElements),y=result))
}
