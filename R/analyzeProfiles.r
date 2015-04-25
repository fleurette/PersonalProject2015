# Time constants
pregnancy.length <- 40*7*24*3600
one.month <- 29.6*24*3600
one.day <- 24*3600

analyze.profiles <- function(data,bin.size,smoothing.bandwidth) {
  # Compute x taxis
  num.bins <- ceiling((pregnancy.length+2*one.month)/bin.size)
  num.bins.day <- one.day/bin.size
  taxis <- seq(from=(-pregnancy.length-one.month)/one.day,to=one.month/one.day,length=num.bins)
  # Analyze profiles
  analyzed.profiles <- (lapply(
    data
    ,analyze.profile
    ,taxis
    ,bin.size
    ,smoothing.bandwidth
  ))
  # Extract test, pregnant profiles
  test.indices <- sapply(analyzed.profiles,'[[','test')
  test <- analyzed.profiles[test.indices]
  pregnant <- analyzed.profiles[!test.indices]
  # Summarize all
  return(list(
    test=test
    ,summarized.test=summarize.profiles(
      sapply(test,'[[','tweet.count.smoothed')
      ,taxis
    )
    ,summarized.test.adjusted=summarize.profiles(
      sapply(test,'[[','tweet.count.smoothed.adjusted')
      ,taxis
    )
    ,pregnant=pregnant
    ,summarized.pregnant=summarize.profiles(
      sapply(pregnant,'[[','tweet.count.smoothed')
      ,taxis
    )
    ,summarized.pregnant.adjusted=summarize.profiles(
      sapply(pregnant,'[[','tweet.count.smoothed.adjusted')
      ,taxis
    )
  ))
}

summarize.profiles <- function(profiles,taxis) {
  # Compute mean, sd, variance
  profiles.mean <- apply(profiles,1,mean,na.rm=TRUE)
  profiles.var <- apply(profiles,1,var,na.rm=TRUE)
  profiles.sd <- sqrt(profiles.var)
  count <- colSums(rbind(apply(
    profiles
    ,1
    ,function(profile) as.numeric(!is.na(profile))
  )))
  return(list(
    mean=profiles.mean
    ,var=profiles.var
    ,sd=profiles.sd
    ,taxis=taxis
    ,pcount=ncol(profiles)
    ,count=count
  ))
}

# Mark test profiles, compute histogram and smoothed curves
analyze.profile <- function(profile,taxis,bin.size,smoothing.bandwidth) {
  # For test profiles, assume dob is one month before oldest tweet, set new test field
  if(is.na(profile$dob)) {
    profile$dob <- profile$tweet.times[1]-one.month
    profile$test <- TRUE
  } else {
    profile$test <- FALSE
  }
  # Compute start and end dates
  profile$start.date <- profile$dob - pregnancy.length - one.month
  profile$end.date <- profile$dob + one.month
  indices <- (profile$tweet.times<=profile$end.date) & (profile$tweet.times>=profile$start.date)
  # Compute indices of relevant tweets
  profile$selected.tweet.times <- profile$tweet.times[indices]
  profile$tweet.times.indices <- ceiling((profile$selected.tweet.times-profile$start.date)/bin.size)
  # Add taxis
  profile$taxis <- taxis
  profile$start.dob <- profile$taxis[which.min(abs(profile$taxis+279))]
  profile$end.dob <- profile$taxis[which.min(abs(profile$taxis))]
  # Build histogram
  profile$tweet.count <- rep(0,length(taxis))
  for(tweet.index in profile$tweet.times.indices) {
    profile$tweet.count[tweet.index] <- profile$tweet.count[tweet.index]+1
  }
  # Compute tweet count smoothed curve and normalize it
  tweet.count.smoothed <- ksmooth(
    profile$taxis
    ,profile$tweet.count
    ,"normal"
    ,bandwidth=smoothing.bandwidth
  )[[2]]
  # If some values are NA warn user about it and restore histogram instead
  while(any(is.nan(tweet.count.smoothed))) {
    smoothing.bandwidth <- smoothing.bandwidth + 0.1
    print(paste(
      "Smoothing coefficient too low, smoothing with higher smoothing bandwidth"
      ,smoothing.bandwidth
    ))
    tweet.count.smoothed <- ksmooth(
      profile$taxis
      ,profile$tweet.count
      ,"normal"
      ,bandwidth=smoothing.bandwidth
    )[[2]]
  }
  # Normalize smoothing count
  normalizer <- sum(tweet.count.smoothed)*bin.size
  if(normalizer>0) {
    profile$tweet.count.smoothed <- tweet.count.smoothed/normalizer
  } else {
    profile$tweet.count.smoothed <- tweet.count.smoothed
  }
  # Compute adjusted tweet count smoothed curve, normalize it
  zero.indices <- profile$tweet.count.smoothed==0
  normalizer.coefficient <- (length(zero.indices)-sum(as.numeric(zero.indices)))/length(zero.indices)
  profile$tweet.count.smoothed.adjusted <- profile$tweet.count.smoothed*normalizer.coefficient
  profile$tweet.count.smoothed.adjusted[zero.indices] <- NA
  # Add acf information
  profile$acf <- acf(
    profile$tweet.count.smoothed.adjusted
    ,plot=FALSE
    ,na.action=na.pass
    #,max.lag=length(which(taxis>0))
    ,lag.max=60
  )

  return(profile)
}
