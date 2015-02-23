library(rJava)
library(RMongo)

filePath <- "../dbCredentials.dat"
credentials <- scan(file=filePath,what="character",comment.char="#")
mongo <- mongoDbConnect(credentials[4],credentials[3],strtoi(credentials[2]))

loadRawData <- function() {
  dbGetQuery(mongo, credentials[5], '')
}

loadFemaleData <- function() {
  dbGetQuery(mongo, credentials[6], '')
}

loadMaleData <- function() {
  dbGetQuery(mongo, credentials[7], '')
}
