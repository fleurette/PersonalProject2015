source("dataPlot.r")
source("dataAnalyze.r")
source("dbImport.r")

# binSize and bandWidth are in seconds
extractInformation <- function(dataComplete,binSize,bandWidth) {
  numDistributions = length(dataComplete)
  histograms = list(numDistributions)
  smoothed = list(numDistributions)

  i = 1
  for (person in dataComplete) {
    histograms[[i]] = getTweetCount(person[["tweetTimes"]],binSize,person["dob"][[1]])
    smoothed[[i]] = ksmooth(histograms[[i]][['x']],histograms[[i]][['y']],"normal",bandwidth=bandWidth)
    i = i + 1
  }

  mean = getMean(smoothed)

  return (list(histograms=histograms,smoothed=smoothed,mean=mean))
}

reload <- function() {
  importData(raw.path,credentials.path)
  processData(processed.path)
}

processData <- function(filePath) {
  file.remove(filePath)
  processed.males = extractInformation(data.male.complete,binSize,bandWidth)
  processed.females = extractInformation(data.female.complete,binSize,bandWidth)
  save(processed.males,processed.females,file=filePath)
  load(filePath)
}

# Model parameters
binSize = 3600
bandWidth = 24*3600/binSize

# Adjust BandWidth
yAxisLength = 2*7*24*3600
yAxisIncrement = getTimeIncrement(yAxisLength,binSize)
pregnancyLength <- 39*7*24*3600
yAxisNumber = floor((pregnancyLength/binSize)/yAxisIncrement)

# File paths
credentials.path <- "../dbCredentials.dat"
raw.path <- ".rawData"
processed.path <- ".processedData"
figures.path <- "figures/"

# Load previous data
load(raw.path)
load(processed.path)
