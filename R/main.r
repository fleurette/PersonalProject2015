source("plotProfiles.r")
source("analyzeProfiles.r")
source("importProfiles.r")

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
  unlink(matlab.path)
  dir.create(figures.path)
}

classify.test <- function(analyzed.males,analyzed.females) {
  # Build data
  num.males <- length(analyzed.males$test)+length(analyzed.males$pregnant)
  num.females <- length(analyzed.females$test)+length(analyzed.females$pregnant)
  num.observations <- num.males+num.females
  num.features <- 5
  # All smoothed
  data.genres <- list(
    class=c(rep(0,num.males),rep(1,num.females))
    ,feats=rbind(
      t(sapply(analyzed.males$test,'[[','tweet.count.smoothed.adjusted'))
      ,t(sapply(analyzed.males$pregnant,'[[','tweet.count.smoothed.adjusted'))
      ,t(sapply(analyzed.females$test,'[[','tweet.count.smoothed.adjusted'))
      ,t(sapply(analyzed.females$pregnant,'[[','tweet.count.smoothed.adjusted'))
    )
  )
  # All acf
  data.acf <- list(
    class=c(rep(0,num.males),rep(1,num.females))
    ,feats=rbind(
      t(sapply(lapply(analyzed.males$test,'[[','acf'),'[[','acf'))
      ,t(sapply(lapply(analyzed.males$pregnant,'[[','acf'),'[[','acf'))
      ,t(sapply(lapply(analyzed.females$test,'[[','acf'),'[[','acf'))
      ,t(sapply(lapply(analyzed.females$pregnant,'[[','acf'),'[[','acf'))
    )
  )
  # Pregnant
  data.pregnant <- list(
    class=c(rep(0,length(analyzed.females$test)),rep(1,length(analyzed.females$pregnant)))
    ,feats=rbind(
      t(sapply(analyzed.females$test,'[[','tweet.count.smoothed.adjusted'))
      ,t(sapply(analyzed.females$pregnant,'[[','tweet.count.smoothed.adjusted'))
    )
  )
  # Acf smoothed versus 
}



# Process database data, aggregating tweets in bin of bin size (seconds), smoothing down with bandwidth
analyze <- function(bin.size,smoothing.bandwidth) {
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
  #dir.create(paste(dir.path,"/final/",sep=''))
  # Save processed data
  save(
    analyzed.males
    ,analyzed.females
    ,file=paste(dir.path,"/data/",r.path,sep='')
  )
  writeMat(
    # Males test
    paste(dir.path,"/data/",matlab.path,sep='')
    ,testMales=analyzed.males$summarized.test
    ,testAdjustedMales=analyzed.males$summarized.test.adjusted
    # Males pregnant
    ,pregnantMales=analyzed.males$summarized.pregnant
    ,pregnantAdjustedMales=analyzed.males$summarized.pregnant.adjusted
    # Females test
    ,testFemales=analyzed.females$summarized.test
    ,testAdjustedFemales=analyzed.females$summarized.test.adjusted
    # Females pregnant
    ,pregnantFemales=analyzed.females$summarized.pregnant
    ,pregnantAdjustedFemales=analyzed.females$summarized.pregnant.adjusted
  )
  print("Saved data")
  # Plot data
  all.plot(analyzed.males,paste(dir.path,"/males/",sep=''))
  print("Plotted male data")
  all.plot(analyzed.females,paste(dir.path,"/females/",sep=''))
  print("Plotted female data")
  # Plot summary
  #final.plot(analyzed.males,analyzed.females,paste(dir.path,"/final/",sep=''))
  #print("Plotted final plots")
}
