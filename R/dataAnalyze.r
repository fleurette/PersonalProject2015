# Data shape
# 1 is intertweetTime
# 2 is ids
# 3 is the DOBs
# 4 is the Tweet Times
# 5 is the Mean

# Concatenate list of lists
concatenateArray <- function(ls) {
  result <- NULL
  for (l in ls) {
    result <- c(result,l)
  }
  return (result)
}

getApplyConvolution <- function() {
  # Concatenate all the intertweet times for male and female
  intertweet.maleDensity = density(concatenateArray(data.male[1]))
  intertweet.femaleDensity = density(concatenateArray(data.female[1]))
  return (intertweet)
}

# BinSize is a time length given in seconds, compute mean of intertweetTime
# for tweets falling in each bin
# Scale down
# 39 weeks before the endDate
getIntertweetHistogamPregnancy <- function(tweetTimes, binSize, endDate) {
  pregnancyLength <- 39*7*24*3600
  numBins <- pregnancyLength/binSize
  total <- rep(0,numBins)
  count <- rep(0,numBins)
  startDate <- endDate - pregnancyLength
  result <- rep(0,numBins)
 
  for (tweetTime in tweetTimes) {
    if(tweetTime < endDate & tweetTime > startDate) {
      binIndex <- ceiling((tweetTime - startDate)/binSize)
      total[binIndex] <- total[binIndex] + tweetTime
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

  return (result)
}


