library(rJava)
library(RMongo)

filePath <- "../DBCredentials.dat"

# Connect to database
connectDB <- function() {
  credentials <- scan(file=filePath,what="character",comment.char="#")
  mongo <- mongoDbConnect(credentials[4],credentials[3],strtoi(credentials[2]))
}
