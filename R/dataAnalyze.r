getMeans <- function(data) {
  sort(t(data["mean"]))
}

getMeanDensity <- function(means) {
  density(means)
}
