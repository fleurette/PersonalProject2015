# Time constants
pregnancy.length <- 40*7*24*3600
one.month <- 29.6*24*3600
one.day <- 24*3600

profiles.analyze <- function(data,bin.size,smoothing.bandwidth) {
  # Compute x time axis
  num.bins <- ceiling((pregnancy.length+2*one.month)/bin.size)
  num.bins.day <- one.day/bin.size
  time.axis <- seq(from=(-pregnancy.length-one.month)/one.day,by=num.bins.day,length=num.bins)
  
  profiles.analyzed <- (lapply(
    data
    ,profile.analyze
    ,time.axis
    ,bin.size
    ,smoothing.bandwidth
  ))
  
  return(profiles.analyzed)
}

# Mark random profiles, compute histogram and smoothed curves
profile.analyze <- function(profile,time.axis,bin.size,smoothing.bandwidth) {
  # For random profiles, assume dob is one month before oldest tweet, set new random field
  if(is.na(profile$dob)) {
    profile$dob <- profile$tweet.times[1]-one.month
    profile$random <- TRUE
  } else {
    profile$random <- FALSE
  }
  # Compute start and end dates
  profile$start.date <- profile$dob - pregnancy.length - one.month
  profile$end.date <- profile$dob + one.month
  indices <- (profile$tweet.times<=profile$end.date) & (profile$tweet.times>=profile$start.date)
  # Compute indices of relevant tweets
  profile$selected.tweet.times <- profile$tweet.times[indices]
  profile$tweet.times.indices <- ceiling((profile$selected.tweet.times-profile$start.date)/bin.size)
  # Add time axis
  profile$time.axis <- time.axis
  # Build histogram
  profile$tweet.count <- rep(0,length(time.axis))
  for(tweet.index in profile$tweet.times.indices) {
    profile$tweet.count[tweet.index] <- profile$tweet.count[tweet.index]+1
  }
  # Compute tweet count smoothed curve and normalize it
  tweet.count.smoothed <- ksmooth(
    profile$time.axis
    ,profile$tweet.count
    ,"normal"
    ,bandwidth=smoothing.bandwidth
  )[[2]]
  profile$tweet.count.smoothed <- tweet.count.smoothed/(sum(tweet.count.smoothed)*bin.size)

  return(profile)
}

#get.info <- function(data) {
#  # Compute mean
#  total <- data[[1]][['intertweet.smoothed']]
#  for(index in 2:length(data)) {
#    total <- total + data[[index]][['intertweet.smoothed']]
#  }
#  mean <- total/length(data)
#  # Compute variance and standard deviation
#  var <- rep(0,length(mean))
#  for(index in 1:length(data)) {
#    var <- var + (mean-data[[index]][['intertweet.smoothed']])^2
#  }
#  var <- var/length(data)
#  sd <- sqrt(var)
#
#  return(list(intertweet.axis=data[[1]][['intertweet.axis']],mean=mean,var=var,sd=sd,data=data))
#} 
