library(rJava)
library(RMongo)

filePath <- "../DBCredentials.dat"

# Connect to database
connectDB <- function() {
  credentials <- scan(file=filePath,what="character",comment.char="#")
  mongo <- mongoDbConnect(credentials[4],credentials[3],strtoi(credentials[2]))
}

graphTest <- function () {
  Xs <- seq(1,10)
  Ys <- runif(10)
  Zs <- runif(10)
  plot(Xs, Ys)
  plot(Xs, Zs)
}

# Extract all data in suitable data structures

# Have functions to display informations about the statistical properties of both genders, maybe a series of graph

# Maybe start working on a basic Bayesian classifier
# Actually understand the underlying model
