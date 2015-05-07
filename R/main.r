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
extracted.path <- "extracted.data"
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

extract.data <- function(analyzed.males,analyzed.females) {
  # Build data
  num.males <- length(analyzed.males$test)+length(analyzed.males$pregnant)
  num.females <- length(analyzed.females$test)+length(analyzed.females$pregnant)
  num.females.pregnant <- length(analyzed.females$pregnant)
  num.observations <- num.males+num.females
  num.features <- 5
  # All smoothed
  smoothed.adjusted <- list(
    class=c(rep(0,num.males),rep(1,num.females))
    ,feats=rbind(
      t(sapply(analyzed.males$test,'[[','tweet.count.smoothed.adjusted'))
      ,t(sapply(analyzed.males$pregnant,'[[','tweet.count.smoothed.adjusted'))
      ,t(sapply(analyzed.females$test,'[[','tweet.count.smoothed.adjusted'))
      ,t(sapply(analyzed.females$pregnant,'[[','tweet.count.smoothed.adjusted'))
    )
  )
  # All acf except those that are completely empty
  acf.genres <- list(
    class=c(rep(0,num.males),rep(1,num.females))
    ,feats=rbind(
      t(sapply(lapply(analyzed.males$test,'[[','acf'),'[[','acf'))
      ,t(sapply(lapply(analyzed.males$pregnant,'[[','acf'),'[[','acf'))
      ,t(sapply(lapply(analyzed.females$test,'[[','acf'),'[[','acf'))
      ,t(sapply(lapply(analyzed.females$pregnant,'[[','acf'),'[[','acf'))
    )
  )
  acf.processed <- acf.genres
  acf.processed$feats <- matrix(
    data=unlist(apply(
      acf.processed$feats
      ,1
      ,function(row) {
        row[is.na(row)] <- -Inf
        sort(row,index.return=TRUE,decreasing=TRUE)$ix
      }
    ))  
    ,nrow=nrow(acf.processed$feats)
    ,ncol=ncol(acf.processed$feats)
  )
  # Pregnant acf
  acf.pregnant <- list(
    class=c(rep(0,num.females.pregnant),rep(1,num.observations-num.females.pregnant))
    ,feats=rbind(
      t(sapply(lapply(analyzed.females$pregnant,'[[','acf'),'[[','acf'))
      ,t(sapply(lapply(analyzed.males$test,'[[','acf'),'[[','acf'))
      ,t(sapply(lapply(analyzed.males$pregnant,'[[','acf'),'[[','acf'))
      ,t(sapply(lapply(analyzed.females$test,'[[','acf'),'[[','acf'))
    )
  )
  acf.pregnant$feats <- matrix(
    data=unlist(apply(
      acf.pregnant$feats
      ,1
      ,function(row) {
        row[is.na(row)] <- -Inf
        sort(row,index.return=TRUE,decreasing=TRUE)$ix
      }
    ))  
    ,nrow=nrow(acf.pregnant$feats)
    ,ncol=ncol(acf.pregnant$feats)
  )
  return(list(
    smoothed.adjusted=smoothed.adjusted
    ,acf.genres=acf.genres
    ,acf.processed=acf.processed
    ,acf.pregnant=acf.pregnant
  ))
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
  extracted.data <- extract.data(analyzed.males,analyzed.females)
  save(extracted.data,file=paste(dir.path,"/data/",extracted.path,sep=''))
  print("Saved data")
  # Plot data
  all.plot(analyzed.males,paste(dir.path,"/males/",sep=''))
  print("Plotted male data")
  all.plot(analyzed.females,paste(dir.path,"/females/",sep=''))
  print("Plotted female data")
}
