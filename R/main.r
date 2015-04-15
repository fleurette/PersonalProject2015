source("dataPlot.r")
source("dataAnalyze.r")
source("dbImport.r")
source("setup.r")

# File paths
credentials.path <- "../dbCredentials.dat"
raw.path <- ".rawData"
processed.path <- ".processedData"
figures.path <- "figures/"

# Load data from database and save it
data.reload <- function() {
  db.data <- db.import(credentials.path)

  data.male <- db.data[['data.male']]
  data.female <- db.data[['data.female']]

  save(data.male,data.female,file=raw.path)
}

install <- function() {
  setup()
}

# Reload previously saved and processed data
data.load <- function() {
  if(!file.exists(raw.path)){
    data.reload()
  }
}

clean.figures <- function() {
  unlink(figures.path,recursive=TRUE)
  dir.create(figures.path)
}

# Process database data, aggregating tweets in bin of bin size (seconds), smoothing down with bandwidth
process.complete <- function(bin.size,smoothing.bandwidth) {
  data.load()
  load(raw.path)
  # Process data
  processed.males <- data.complete.process(data.male,bin.size,smoothing.bandwidth)
  print("Processed male data")
  processed.females <- data.complete.process(data.female,bin.size,smoothing.bandwidth)
  print("Processed female data")

  # Create new directory to vizualise the data
  dir.name <- strcat(c("bsize_",bin.size,"_smooth_",smoothing.bandwidth))
  dir.path <- strcat(c(figures.path,dir.name))
  dir.males <- strcat(c(dir.path,"/males/"))
  dir.females <- strcat(c(dir.path,"/females/"))
  dir.create(dir.path)
  dir.create(dir.males)
  dir.create(dir.females)

  # Plot and save data
  plot.all(processed.males,dir.males)
  plot.all(processed.females,dir.females)

  return(list(processed.males=processed.males,processed.females=processed.females))
}
