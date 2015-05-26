all.plot <- function(analyzed.profiles,path) {
  # Extract moments
  summarized.test <- analyzed.profiles$summarized.test
  summarized.pregnant <- analyzed.profiles$summarized.pregnant
  summarized.test.adjusted <- analyzed.profiles$summarized.test.adjusted
  summarized.pregnant.adjusted <- analyzed.profiles$summarized.pregnant.adjusted

  # Extract profiles
  test <- analyzed.profiles$test
  pregnant <- analyzed.profiles$pregnant

  # Plot summary
  pdf(paste(path,"summary_test.pdf",sep=''))
  summary.plot(summarized.test,"test")
  summary.plot(summarized.test.adjusted,"adjusted test")
  dev.off()
  pdf(paste(path,"summary_pregnant.pdf",sep=''))
  summary.plot(summarized.pregnant,"pregnant")
  summary.plot(summarized.pregnant.adjusted,"pregnant adjusted")
  dev.off()

  # Plot histogram of tweet counts
  pdf(paste(path,"histograms_test.pdf",sep=''))
  histograms.plot(test,'test')
  dev.off()
  pdf(paste(path,"histograms_pregnant.pdf",sep=''))
  histograms.plot(pregnant,'pregnant')
  dev.off()

  # Plot acf
  pdf(paste(path,"acf_test.pdf",sep=''))
  acf.plot(test,'test')
  dev.off()
  pdf(paste(path,"acf_pregnant.pdf",sep=''))
  acf.plot(pregnant,'pregnant')
  dev.off()

  # Plot smoothed tweet count with mean
  pdf(paste(path,"smoothed_test.pdf",sep=''))
  smoothed.plot(test,'test',summarized.test$mean)
  dev.off()
  pdf(paste(path,"smoothed_pregnant.pdf",sep=''))
  smoothed.plot(pregnant,'pregnant',summarized.pregnant$mean)
  dev.off()

  # Plot smoothed tweet count with adjusted mean
  pdf(paste(path,"smoothed_adjusted_test.pdf",sep=''))
  smoothed.plot(test,'test',summarized.test.adjusted$mean)
  dev.off()
  pdf(paste(path,"smoothed_adjusted_pregnant.pdf",sep=''))
  smoothed.plot(pregnant,'pregnant',summarized.pregnant.adjusted$mean)
  dev.off()
}

acf.plot <- function(profiles,type) {
  for(profile in profiles) {
    if(!any(is.na(profile$acf$acf))) {
      plot(profile$acf,main=paste("ACF for",type,"user",profile$id))
    }
  }
}

smoothed.plot <- function(profiles,type,profiles.mean) {
  for(profile in profiles) {
    plot(
      x=profile$taxis
      ,y=profile$tweet.count.smoothed
      ,xlab="Time before date of birth in days"
      ,ylab="Probability distribution function of tweet"
      ,type='l'
    )
    lines(
      x=profile$taxis
      ,y=profiles.mean
      ,col="red"
      ,lty=2
    )
    abline(
      v=c(profile$start.dob,profile$end.dob)
      ,col="blue"
    )
    legend(
      "topleft"
      ,c("Mean","pdf")
      ,lty=c(2,1)
      ,lwd=c(1,1)
      ,col=c("red","black")
      ,inset=0.05
    )
    title(main=paste("Probability of tweet event for",type,"user",profile$id))
  }
}

histograms.plot <- function(profiles,type) {
  for(profile in profiles) {
    plot(
      x=profile$taxis
      ,y=profile$tweet.count
      ,xlab="Time before date of birth in days"
      ,ylab="Number of tweets"
      ,type="h"
    )
    abline(
      v=c(profile$start.dob,profile$end.dob)
      ,col="blue"
    )
    title(main=paste("Tweet count histogram for",type,"user",profile$id))
  }
}

summary.plot <- function(summary,type) {
  # Plot mean
  plot(
    x=summary$taxis
    ,y=summary$mean
    ,xlab="Time in days"
    ,ylab="Probability of tweet"
    ,type='l'
  )
  abline(
    v=c(summary$start.dob,summary$end.dob)
    ,col="blue"
  )
  title(main=paste("Mean of tweet time distribution for ",type,"profiles"))
  # Plot variance
  plot(
    x=summary$taxis
    ,y=summary$var
    ,xlab="Time in days"
    ,ylab="Probability of tweet"
    ,type='l'
  )
  abline(
    v=c(summary$start.dob,summary$end.dob)
    ,col="blue"
  )
  title(main=paste("Var of tweet time distribution for ",type,"profiles"))
  # Mean vs Sd
  plot(
    x=summary$mean
    ,y=summary$sd
    ,xlab="Mean of tweet probability density"
    ,ylab="SD of tweet probability density"
    ,type="p"
  )
}

final.plot <- function(analyzed.males,analyzed.females,matlab.path,save.path) {
  # Start matlab
  cmd <- paste(
    "./callMatlab.sh "
    ,paste("'../R/",matlab.path,"' ",sep='')
    ,paste("'../R/",save.path,"' ",sep='')
  )
  system(cmd)
}

classification.plot <- function(classifications,save.path) {
  pdf(paste(save.path,"classification_outcomes.pdf",sep=""))
  lapply(
    classifications
    ,function(classification) {
      plot(
        type="l"
        ,col="red"
        ,x=classification$knn.euclidean$ks
        ,y=classification$knn.euclidean$accuracies
        ,ylim=c(0,1)
        ,xlab="Number of Neighbors"
        ,ylab="Accuracy"
        ,main=paste(
          "Accuracy with k neighbors when classifying between"
          ,classification$class1
          ,"and\n"
          ,classification$class2
          ,"using"
          ,classification$type
          ,sep=" "
        )
        ,cex.main=0.9
      )
      abline(h=classification$qda,col="black")
      abline(h=classification$svm,col="green")
      legend(
        "bottomleft"
        ,c("knn","qda","svm")
        ,lty=c(1,1,1)
        ,lwd=c(1,1,1)
        ,col=c("red","black","green")
        ,inset=0.05
      )
      return(classification)
    }
  )
  dev.off()
}
