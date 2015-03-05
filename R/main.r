source("dataPlot.r")
source("dataAnalyze.r")

# binSize and bandWidth are in seconds
extractInformation <- function(dataComplete,binSize,bandWidth) {
  numDistributions = length(dataComplete)
  histograms = list(numDistributions)
  smoothed = list(numDistributions)

  i = 1
  for (person in data) {
    histogram[[index]] = getTweetCount(person["tweetTimes"][[1]],binSize,person["dob"][[1]])
    smoothed[[index]] = ksmooth(histogram[[i]][['x']],histogram[[i]][['y']],"normal",bandwidth=bandWidth)
    i = i + 1
  }

  mean = getMean(smoothed)

  return (list(histograms=histograms,smoothed=smoothed,mean=mean))
}

# Model parameters
binSize = 3600
bandWidth = 3600*24

# Extract informations
processed.males = extractInformation(data.male,binSize,bandWidth)
processed.females = extractInformation(data.female,binSize,bandWidth)
