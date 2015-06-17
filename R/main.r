source("plotProfiles.r")
source("analyzeProfiles.r")
source("importProfiles.r")
source("classification.r")
source("importUsers.r")

library(R.matlab)
library(class)
library(e1071)
library(rJava)
library(RMongo)
library(jsonlite)
library(TSA)

# Dependencies
dependencies <- c(
  "rJava"
  ,"RMongo"
  ,"jsonlite"
  ,"lubridate"
  ,"e1071"
  ,"R.matlab"
  ,"TSA"
)

# File paths
credentials.path <- "../dbCredentials.dat"
raw.path <- ".rawData"
figures.path <- "figures/"

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
treat.tweets <- function(bin.size,smoothing.bandwidth,matlab.backup=FALSE) {
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
  dir.create(paste(dir.path,"/summary/",sep=''))
  dir.create(paste(dir.path,"/classify/",sep=''))
  # Classify data
  classifications <- classify.data(analyzed.males,analyzed.females)
  print("Classified data")
  # Save processed data
  save(
    analyzed.males
    ,analyzed.females
    ,classifications
    ,file=paste(dir.path,"/data/profiles.data",sep='')
  )
  print("Saved data")
  if(matlab.backup) {
    writeMat(
      paste(dir.path,"/data/dump.mat",sep='')
      # Males test
      ,males=analyzed.males
      ,females=analyzed.females
    )
    print("Saved data to Matlab binary")
  }
  # Plot profiles information
  all.plot(analyzed.males,paste(dir.path,"/males/",sep=''))
  all.plot(analyzed.females,paste(dir.path,"/females/",sep=''))
  # Plot profiles summary
  final.plot(analyzed.males,analyzed.females,paste(dir.path,"/summary/",sep=""))
  # Plot summary of classification
  classification.plot(classifications,paste(dir.path,"/classify/",sep=''))
  print("Plotted data")
  # Return useful informations
  return(list(
    analyzed.males=analyzed.males
    ,analyzed.females=analyzed.females
    ,classifications=classifications
  ))
}

treat.users <- function() {
  users <- (import.users(credentials.path))
  dir.path <- (paste(figures.path,"users/",sep=""))
  unlink(dir.path)

  pdf(paste(dir.path,"summary.pdf",sep=""))

  females <- users$females
  pregnant.females <- females[females$pregnant,]
  test.females <- females[!females$pregnant,]
  males <- users$males
  pregnant.males <- males[males$pregnant,]
  test.males <- males[!males$pregnant,]

  pie(table(users$females$time_zone),radius=1.05,cex=0.40,main="Repartition of time zones for all female users")
  pie(table(users$females$lang),radius=0.7,cex=0.6,main="Repartition of languages for all female users")


  hist(log10(c(males$friends_count,females$friends_count)),breaks=20,xlab="Number of followers, scale in log10",ylab="Frequency",main="Number of followers of all users")


  pie(table(c(users$females$lang,users$males$lang)),radius=0.7,cex=0.6,main="Repartition of languages for all female users")

  pie(table(users$males$time_zone),radius=1.05,cex=0.40,main="Repartition of time zones for all male users")
  pie(table(users$males$lang),radius=0.7,cex=0.6,main="Repartition of languages for all male users")


  hist(log10(c(males$followers_count,females$followers_count)),breaks=20,xlab="Number of followers, scale in log10",ylab="Frequency",main="Number of followers of all female users")
  hist(log10(c(males$statuses,females$statuses)),breaks=20,xlab="Number of statuses, scale in log10",ylab="Frequency",main="Number of statuses for all users")

  hist(log10(females$followers_count),breaks=20,xlab="Number of followers, scale in log10",ylab="Frequency",main="Number of followers of all female users")
  hist(log10(pregnant.females$followers_count),breaks=20,xlab="Number of followers, scale in log10",ylab="Frequency",main="Number of followers of pregnant female users")
  hist(log10(test.females$followers_count),breaks=20,xlab="Number of followers, scale in log10",ylab="Frequency",main="Number of followers of test female users")

  hist(log10(males$followers_count),breaks=20,xlab="Number of followers, scale in log10",ylab="Frequency",main="Number of followers of all male users")
  hist(log10(pregnant.males$followers_count),breaks=20,xlab="Number of followers, scale in log10",ylab="Frequency",main="Number of followers of pregnant male users")
  hist(log10(test.males$followers_count),breaks=20,xlab="Number of followers, scale in log10",ylab="Frequency",main="Number of followers of test male users")

  hist(log10(females$friends_count),breaks=20,xlab="Number of friends, scale in log10",ylab="Frequency",main="Number of friends of all female users")
  hist(log10(pregnant.females$friends_count),breaks=20,xlab="Number of friends, scale in log10",ylab="Frequency",main="Number of friends of pregnant female users")
  hist(log10(test.females$friends_count),breaks=20,xlab="Number of friends, scale in log10",ylab="Frequency",main="Number of friends of test female users")

  hist(log10(males$friends_count),breaks=20,xlab="Number of friends, scale in log10",ylab="Frequency",main="Number of friends of all male users")
  hist(log10(pregnant.males$friends_count),breaks=20,xlab="Number of friends, scale in log10",ylab="Frequency",main="Number of friends of pregnant male users")
  hist(log10(test.males$friends_count),breaks=20,xlab="Number of friends, scale in log10",ylab="Frequency",main="Number of friends of test male users")

  hist(log10(females$statuses_count),breaks=20,xlab="Number of statuses, scale in log10",ylab="Frequency",main="Number of statuses of all female users")
  hist(log10(pregnant.females$statuses_count),breaks=20,xlab="Number of statuses, scale in log10",ylab="Frequency",main="Number of statuses of pregnant female users")
  hist(log10(test.females$statuses_count),breaks=20,xlab="Number of statuses, scale in log10",ylab="Frequency",main="Number of statuses of test female users")

  hist(log10(males$statuses_count),breaks=20,xlab="Number of statuses, scale in log10",ylab="Frequency",main="Number of statuses of all male users")
  hist(log10(pregnant.males$statuses_count),breaks=20,xlab="Number of statuses, scale in log10",ylab="Frequency",main="Number of statuses of pregnant male users")
  hist(log10(test.males$statuses_count),breaks=20,xlab="Number of statuses, scale in log10",ylab="Frequency",main="Number of statuses of test male users")

  dev.off()
}
