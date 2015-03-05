library(rJava)
library(RMongo)
library(jsonlite)

parseData <- function(data) {
  dataPoints = nrow(data)
  result = list(dataPoints)
  for (i in seq(1,dataPoints)) {
    # Extract id and dob
    id = data[i,2]
    dob = as.numeric(as.POSIXct(data[i,3],format='%a %b %d %H:%M:%S GMT %Y'))
    if(is.na(dob)) {
      dob = as.numeric(as.POSIXct(data[i,3],format='%a %b %d %H:%M:%S BST %Y'))
    }
    # Extract all tweet dates
    tweets = fromJSON(data[i,4])
    numTweets = nrow(tweets)
    tweetTimes = rep(0,numTweets)
    for (j in seq(1,numTweets)) {
      tweetTimes[j] = as.numeric(as.POSIXct(tweets[j,1],format='%Y-%m-%dT%H:%M:%S'))
    }
    # Complete if DOB - 39 weeks is older than last Tweet
    complete = (dob-39*7*24*3600) > tweetTimes[numTweets]
    # Create list
    result[[i]] = list(id=id,dob=dob,tweetTimes=tweetTimes,complete=complete)
  }
  return (result)
}

isComplete <- function(person) {
  return (!is.na(person["complete"][[1]]) && person["complete"][[1]]==TRUE)
}

countComplete <- function(data) {
  numComplete = 0
  for (person in data) {
    if (isComplete(person)) {
      numComplete = numComplete + 1
    }
  }
  return (numComplete)
}

getComplete <- function(data) {
  complete = list(countComplete(data))
  i = 1
  for (person in data) {
    if (isComplete(person)) {
      complete[[i]] = person
      i = i + 1
    }
  }
  return (complete)
}

file.remove('.RData')
print("Removed previous image")

filePath <- "../dbCredentials.dat"
credentials <- scan(file=filePath,what="character",comment.char="#")
mongo <- mongoDbConnect(credentials[4],credentials[3],strtoi(credentials[2]))
print("Connected to database")

data.male = parseData(dbGetQuery(mongo, credentials[6], ''))
data.male.complete = getComplete(data.male)
print("Imported male data")
data.female = parseData(dbGetQuery(mongo, credentials[7], ''))
data.female.complete = getComplete(data.female)
print("Imported female data")

save(data.male, data.female, data.male.complete, data.female.complete, file='.RData')
