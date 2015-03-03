# Data shape
# 1 is intertweetTime
# 2 is ids
# 3 is the timestamp
# 4 is means

# Concatenate list of lists
concatenateArray <- function(ls) {
  result <- NULL
  for (l in ls) {
    result <- c(result,l)
  }
  return (result)
}

getIntertweetDensity <- function() {
  # Concatenate all the intertweet times for male and female
  intertweet.maleDensity = density(concatenateArray(data.male[1]))
  intertweet.femaleDensity = density(concatenateArray(data.female[1]))
  return (intertweet)
}
