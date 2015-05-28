source("plotProfiles.r")
source("analyzeProfiles.r")
source("importProfiles.r")
source("classification.r")

library(R.matlab)

# Dependencies
dependencies <- c(
  "rJava"
  ,"RMongo"
  ,"jsonlite"
  ,"lubridate"
  ,"R.matlab"
)

# File paths
credentials.path <- "../dbCredentials.dat"
raw.path <- ".rawData"
figures.path <- "figures/"
matlab.path <- "summary.mat"
r.path <- "profiles.data"
final.path <- "final/"
classified.path <- "classification/"

setup <- function() {
  install.packages(dependencies)
}

# Reload previously saved data
import <- function() {
  if(!file.exists(raw.path)){
    imported.profiles <- import.profiles(credentials.path)
    males <- imported.profiles$males
    females <- imported.profiles$females
    save(males,females,file=raw.path)
  }
}

# Delete all figures and data
reset <- function() {
  unlink(figures.path,recursive=TRUE)
  unlink(raw.path)
  dir.create(figures.path)
}

# Process database data, aggregating tweets in bin of bin size (seconds), smoothing down with bandwidth
analyze <- function(bin.size,smoothing.bandwidth,matlab.backup=FALSE) {
  import()
  load(raw.path)
  # Process data
  analyzed.males <- analyze.profiles(males,bin.size,smoothing.bandwidth)
  print("Processed male data")
  analyzed.females <- analyze.profiles(females,bin.size,smoothing.bandwidth)
  print("Processed female data")
  # Create new directories to vizualise the data
  dir.path <- paste(figures.path,"bsize_",bin.size,"_smooth_",smoothing.bandwidth,sep='')
  dir.create(dir.path)
  dir.create(paste(dir.path,"/males/",sep=''))
  dir.create(paste(dir.path,"/females/",sep=''))
  dir.create(paste(dir.path,"/data/",sep=''))
  dir.create(paste(dir.path,"/final/",sep=''))
  dir.create(paste(dir.path,"/classify/",sep=''))
  # Save processed data
  save(
    analyzed.males
    ,analyzed.females
    ,file=paste(dir.path,"/data/",r.path,sep='')
  )
  print("Saved data")
  if(matlab.backup) {
    writeMat(
      paste(dir.path,"/data/",matlab.path,sep='')
      # Males test
      ,males=analyzed.males
      ,females=analyzed.females
    )
    print("Saved data to Matlab binary")
  }
  # Plot data
  all.plot(analyzed.males,paste(dir.path,"/males/",sep=''))
  print("Plotted male data")
  all.plot(analyzed.females,paste(dir.path,"/females/",sep=''))
  print("Plotted female data")
  final.plot(
    analysed.males
    ,analysed.females
    ,matlab.path
    ,paste(dir.path,"/final/",sep='')
  )
  # Classify data
  classifications <- classify.data(analyzed.males,analyzed.females)
  classification.plot(
    classifications
    ,paste(dir.path,"/classify/",sep='')
  )
  # Return analyzed profiles
  return(list(analyzed.males,analyzed.females))
}
