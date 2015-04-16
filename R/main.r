source("dataPlot.r")
source("profilesAnalyze.r")
source("dbImport.r")

library(R.matlab)

# File paths
credentials.path <- "../dbCredentials.dat"
raw.path <- ".rawData"
processed.path <- ".processedData"
figures.path <- "figures/"
matlab.path <- "data.mat"

setup <- function() {
  deps <- c(
    "rJava"
    ,"RMongo"
    ,"jsonlite"
    ,"lubridate"
    ,"R.matlab"
  )

  install.packages(deps)
}

# Reload previously saved and processed data
data.load <- function() {
  if(!file.exists(raw.path)){
    db.data <- db.import(credentials.path)
    males <- db.data$males
    females <- db.data$females
    save(males,females,file=raw.path)
  }
  load(raw.path)
}

# Delete all previously created figures
data.reset <- function() {
  unlink(figures.path,recursive=TRUE)
  unlink(raw.path)
  unlink(processed.path)
  unlink(matlab.path)
  dir.create(figures.path)
}

# Process database data, aggregating tweets in bin of bin size (seconds), smoothing down with bandwidth
data.analyze <- function(bin.size,smoothing.bandwidth) {
  data.load()
  # Process data
  males.analyzed <- profiles.analyze(males,bin.size,smoothing.bandwidth)
  print("Processed male data")
  females.analyzed <- profiles.analyze(females,bin.size,smoothing.bandwidth)
  print("Processed female data")
  # Create new directories to vizualise the data
  dir.path <- strcat(c(figures.path,"bsize_",bin.size,"_smooth_",smoothing.bandwidth))
  dir.create(dir.path)
  dir.create(strcat(c(dir.path,"/males/")))
  dir.create(strcat(c(dir.path,"/females/")))
  # Plot and save data
  #plot.all(processed.males,dir.males)
  #plot.all(processed.females,dir.females)
  #save(processed.males,processed.females,file=processed.path)
}
