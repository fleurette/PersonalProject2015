library(rJava)
library(RMongo)

filePath <- "../dbCredentials.dat"
credentials <- scan(file=filePath,what="character",comment.char="#")
mongo <- mongoDbConnect(credentials[4],credentials[3],strtoi(credentials[2]))

data.raw = dbGetQuery(mongo, credentials[5], '')
data.male = dbGetQuery(mongo, credentials[6], '')
data.female = dbGetQuery(mongo, credentials[7], '')

save(data.raw, data.male, data.female, file='.RData')
