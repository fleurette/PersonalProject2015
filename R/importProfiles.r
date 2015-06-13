# The data is organized in the following way
# 1 is the tweet times
# 2 is the gender
# 3 is the dob
# 4 is the id

import.profile <- function(data) {
  result <- list(nrow(data))
  for (i in 1:nrow(data)) {
    # Extract id and dob
    id <- data[i,4]
    dob <- as.numeric(as.POSIXct(data[i,3],format='%m/%d/%Y'))
    # Extract all tweet dates
    tweets <- fromJSON(data[i,1])
    tweet.times <- rep(0,nrow(tweets))
    for (j in 1:nrow(tweets)) {
      date.parsed <- as.POSIXlt(tweets[j,1],format='%Y-%m-%dT%H:%M:%S')
      tweet.times[j] <- as.numeric(date.parsed)
      if(is.na(tweet.times[j])) {
        date.parsed$hour <- date.parsed$hour+1
        tweet.times[j] <- as.numeric(date.parsed)
      }
      if(is.na(tweet.times[j])) {
        print(tweets[j,1])
      }
    }
    # Create list
    result[[i]] <- list(id=id,dob=dob,tweet.times=tweet.times)
  }
  return (result)
}

import.profiles <- function(credentials.path) {
  # Extract credentials
  credentials <- scan(file=credentials.path,what="character",comment.char="#")
  mongo <- mongoDbConnect(credentials[4],credentials[3],strtoi(credentials[2]))
  print("Connected to database")
  # Get male data
  males <- import.profile(dbGetQuery(mongo, credentials[5], ''))
  print("Imported male data")
  # Get female data
  females <- import.profile(dbGetQuery(mongo, credentials[6], ''))
  print("Imported female data")
  
  return(list(males=males,females=females))
}
