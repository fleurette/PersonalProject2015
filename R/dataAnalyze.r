pregnancy.length <- 40*7*24*3600

get.complete <- function(data) {
  data.complete <- data[lapply(
    data
    ,function(l) { 
      tweet.times <- l[['tweet.times']]
      dob <- l[['dob']]
      start.date <- dob-pregnancy.length
      l[['start.date']] <- start.date
      l[['treated.tweet.times']] <- tweet.times[tweet.times<dob&tweet.times>start.date]-start.date
      return(length(l[['treated.tweet.times']]) > 0)
    }) == TRUE
  ]
  return(data.complete)
}

# Only keep tweet times between dob and start of pregnancy, express them as time spent since start of pregnancy
get.treated <- function(data) {
  result <- lapply(
    data
    ,function(l) { 
      tweet.times <- l[['tweet.times']]
      dob <- l[['dob']]
      start.date <- dob-pregnancy.length
      l[['start.date']] <- start.date
      l[['treated.tweet.times']] <- tweet.times[tweet.times<dob&tweet.times>start.date]-start.date
      return(l)
    }
 )
  return(result)
}

get.histograms <- function(data,bin.size) {
  bin.nums <- pregnancy.length/bin.size
  # Fraction of a month represented by bin size
  bin.num.month <- bin.size/(4.3*7*24*3600)
  # Time axis, main unit is month
  intertweet.time.axis <- seq(from=0,by=bin.num.month,length=bin.nums)

  result <- lapply(
    data
    ,function(l) {
      count <- rep(0,bin.nums)
      for(treated.tweet.time in l[['treated.tweet.times']]) {
        bin.index <- ceiling(treated.tweet.time/bin.size)
        count[bin.index] <- count[bin.index]+1
      }
      # Normalize result
      l[['intertweet.axis']] <- intertweet.time.axis
      l[['intertweet.histogram']] <- count
      l[['bin.size']] <- bin.size
      l[['bin.nums']] <- bin.nums
      return(l)
    }
  )

  return(result)
}

get.smoothed <- function(data,smoothing.bandwidth) {
  result <- lapply(
    data
    ,function(l) {
      intertweet.smoothed <- ksmooth(
        l[['intertweet.axis']]
        ,l[['intertweet.histogram']]
        ,"normal"
        ,bandwidth=smoothing.bandwidth
      )[[2]]
      l[['intertweet.smoothed']] <- intertweet.smoothed/(sum(intertweet.smoothed)*l[['bin.size']])
      return(l)
    }
  )

  return(result)
}

get.info <- function(data) {
  # Compute mean
  total <- data[[1]][['intertweet.smoothed']]
  for(index in 2:length(data)) {
    total <- total + data[[index]][['intertweet.smoothed']]
  }
  mean <- total/length(data)
  # Compute variance and standard deviation
  var <- rep(0,length(mean))
  for(index in 1:length(data)) {
    var <- var + (mean-data[[index]][['intertweet.smoothed']])^2
  }
  var <- var/length(data)
  sd <- sqrt(var)

  return(list(intertweet.axis=data[[1]][['intertweet.axis']],mean=mean,var=var,sd=sd,data=data))
} 

get.pca <- function(data) {
  #max.data <- data[['data']][['intertweet.smoothed']][[1]]trix.data <- data[['data']][['intertweet.smoothed']][[1]]
  #for(index in 2:length(data[['data']])) {
  #  matrix.data <- rbind(matrix.data,data[['data']][['intertweet.smoothed']][[index]])
  #}
  #return(matrix.data)
  return(data)
}

# Extract smoothed pdf of data with bin size using smoothing bandwidth, compute mean and standard deviation
data.complete.process <- function(data,bin.size,smoothing.bandwidth) {
  data.complete <- get.complete(data)
  data.treated <- get.treated(data.complete)
  data.histograms <- get.histograms(data.treated,bin.size)
  data.smoothed <- get.smoothed(data.histograms,smoothing.bandwidth)
  data.info <- get.info(data.smoothed)
  data.pca <- get.pca(data.info)

  return(data.pca)
}
