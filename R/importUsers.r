import.users <- function(credentials.path) {
  # Extract credentials
  credentials <- scan(file=credentials.path,what="character",comment.char="#")
  mongo <- mongoDbConnect(credentials[4],credentials[3],strtoi(credentials[2]))
  print("Connected to database")
  # Get male data
  males <- extract.users(dbGetQuery(mongo, credentials[8], ''))
  print("Imported male data")
  # Get female data
  females <- extract.users(dbGetQuery(mongo, credentials[9], ''))
  print("Imported female data")
  return(list(males=males,females=females))
}

extract.users <- function(data) {
  data$gender <- NULL
  data$pregnant <- sapply(data$dob,nchar) > 0
  data$time_zone[sapply(data$time_zone,nchar)==0] <- "Unknown"
  data$lang[sapply(data$lang,nchar)==0] <- "Unknown"
  data$dob <- NULL
  data$statuses_count <- as.numeric(data$statuses_count)
  data$friends_count <- as.numeric(data$friends_count)
  data$followers_count <- as.numeric(data$followers_count)
  return(data)
}
