strcat <- function(strings) {
  paste(strings,collapse="")
}

plot.all <- function(data,path) {
  # Extract mean, sd, time.axis
  time.axis <- data[["intertweet.axis"]]
  mean <- data[["mean"]]
  var <- data[["var"]]

  # Plot moments
  pdf(strcat(c(path,"moments.pdf")))
  plot(x=time.axis,y=mean,xlab="Time in months", ylab="Probability of tweet",type='l')
  title(main="Mean of tweet time distribution")
  plot(x=time.axis,y=var,xlab="Time in months", ylab="Probability of tweet",type='l')
  title(main="Variance of tweet time distribution")
  dev.off()

  # Plot histograms
  pdf(strcat(c(path,"histograms.pdf")))
  for(d in data[['data']]) {
    plot(x=time.axis,y=d[['intertweet.histogram']],ylim=c(0.9,max(d[['intertweet.histogram']])),xlab="time in months", ylab="number of tweets",pch=18,col='red')
    title(main=strcat(c("Tweet-time histogram for user ",d[["id"]])))
  }
  dev.off()

  # Plot smoothed curved with mean
  pdf(strcat(c(path,"smoothed.pdf")))
  for(d in data[['data']]) {
    plot(x=time.axis,y=d[['intertweet.smoothed']],xlab="Time in months", ylab="Pdf of a tweet event",type='l')
    lines(x=time.axis,y=mean,col="red",lty=2)
    legend(0.3,0.95*max(d[['intertweet.smoothed']]),c("Mean","pdf"),lty=c(2,1),lwd=c(1,1),col=c("red","black"))
    title(main=strcat(c("Intertweet distribution for user ",d[["id"]])))
  }
  dev.off()
}
