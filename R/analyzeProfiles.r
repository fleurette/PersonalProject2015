# Time constants
pregnancy.length <- 40*7*24*3600
one.month <- 29.6*24*3600
one.day <- 24*3600

analyze.profiles <- function(data,bin.size,smoothing.bandwidth) {
  result <- list()
  # Set a boolean to indicate whether or not the profile is a test profile or a pregnant profile
  result$all <- lapply(
    data
    ,function(profile) {
      if(is.na(profile$dob)) {
        profile$dob <- profile$tweet.times[1]
        profile$test <- TRUE
      } else {
        profile$test <- FALSE
      }
      return(profile)
    }
  )
  # Compute num bins and time axis
  num.bins <- ceiling(pregnancy.length/bin.size)
  result$taxis <- seq(from=-pregnancy.length/one.day,to=0,length=num.bins)
  # Analyze profiles with different start and end dates
  result$all <- lapply(
    result$all
    ,function(profile) {
      during <- analyze.profile(profile$tweet.times,bin.size,smoothing.bandwidth,profile$dob-pregnancy.length,profile$dob,num.bins)
      before <- analyze.profile(profile$tweet.times,bin.size,smoothing.bandwidth,profile$dob-2*pregnancy.length,profile$dob-pregnancy.length,num.bins)
      if(length(during)) {
        during$id <- profile$id
      }
      if(length(before)) {
        before$id <- profile$id
      }
      profile$analysis <- list(
        during=during
        ,before=before
      )
      profile$classify <- during
      return(profile)
    }
  )
  # Extract test and pregnant profiles
  result$test <- Filter(function(x) x$test,result$all)
  result$pregnant <- Filter(function(x) !x$test,result$all)
  # Compute summary of data
  result$summary <- list(
    test=summarize.profiles(result$test)
    ,pregnant=summarize.profiles(result$pregnant)
    ,all=summarize.profiles(result$all)
  )
  # Summarize all
  return(result)
}

summarize.profiles <- function(profiles) {
  # Compute mean, sd, variance, and sde for tweet times and acf values
  summarize <- function(data) {
    result <- (lapply(list(
        list(data=t(sapply(data,'[[','pdf')),type="pdf")
        ,list(data=t(sapply(data,'[[','acf')),type="acf")
      )
      ,function(observations) {
        observation.count <- colSums(apply(observations$data,2,function(observation) (!is.na(observation))))
        return(list(
          mean=colMeans(observations$data,na.rm=TRUE)
          ,sde=sqrt(apply(observations$data,2,function(col) {var(col,na.rm=TRUE)}))/sqrt(observation.count)
          ,type=observations$type
          ,count=observation.count
        ))
      }
    ))
    return(list(
      pdf=result[[1]]
      ,acf=result[[2]]
    ))
  }
  # Extract analysis for pregnancy period and before pregnancy period
  profiles.analysis <- lapply(profiles,'[[','analysis')
  return(list(
    before=summarize(Filter(function(x) length(x),lapply(profiles.analysis,'[[','before')))
    ,during=summarize(Filter(function(x) length(x),lapply(profiles.analysis,'[[','during')))
  ))
}

analyze.profile <- function(tweet.times,bin.size,smoothing.bandwidth,start.date,end.date,num.bins) {
  result <- list()
  # Compute indices of relevant tweet for selected bin.size
  relevant.tweet.times <- tweet.times[(tweet.times<=end.date) & (tweet.times>=start.date)]
  tweet.indices <- ceiling((relevant.tweet.times-start.date)/bin.size)
  # If there are no relevant tweets return NULL
  if(length(tweet.indices)==0) return(logical(0))
  # Build tweet histogram
  result$histogram <- rep(0,num.bins)
  for(tweet.index in tweet.indices) {
    result$histogram[tweet.index] <- result$histogram[tweet.index]+1
  }
  # Build tweet pdf, normalize it and assign NA to zero values to account for missing information
  result$pdf <- ksmooth(
    1:num.bins
    ,result$histogram
    ,"normal"
    ,bandwidth=smoothing.bandwidth
  )[[2]]
  zero.indices <- result$pdf==0
  result$pdf <- result$pdf*(num.bins-sum(zero.indices))/(sum(result$pdf)*num.bins^2)
  result$pdf[zero.indices] <- NA
  # Extract acf information
  result$acf <- as.numeric(acf(
    result$pdf
    ,plot=FALSE
    ,na.action=na.pass
    ,lag.max=one.month*2/bin.size
  )$acf)
  # Extract acf indices, complete with zero values to get the same number of values as in the original acf curve
  result$acf.indices <- sort(result$acf,index.return=TRUE,decreasing=TRUE)$ix
  result$acf.indices <- c(result$acf.indices,rep(0,length(result$acf)-length(result$acf.indices)))

  return(result)
}
