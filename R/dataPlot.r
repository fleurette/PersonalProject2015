filePath <- "figures/"

strcat <- function(str1, str2) {
  paste(c(str1,str2),collapse=" ")
}

plotMeanDensities <- function(maleDensities, femaleDensities) {
  pdf(strcat(filePath,"meanDensities.pdf"))
  plot(
    x=femaleDensities, 
    type="l", 
    lty=2,
    col="red"
  )
  lines(maleDensities,col="blue",type="l",lty=3)
  dev.off()
}

plotMeans <- function(sortedMaleMeans, sortedFemaleMeans) {
  pdf(strcat(filePath,"maleMeans.pdf"))
  plot(
    x=sortedMaleMeans, 
    type="l", 
    xlab="", 
    ylab="Intertweet time mean in seconds",
    main="Intertweet time mean for male users"
  )
  dev.off()

  pdf(strcat(filePath,"maleHistogramMeans.pdf"))
  hist(
    x=sortedMaleMeans, 
    breaks=20,
    xlab="Intertweet time mean in seconds", 
    ylab="Number of users",
    main="Intertweet time mean for male users"
  )
  dev.off()

  pdf(strcat(filePath,"femaleMeans.pdf"))
  plot(
    x=sortedFemaleMeans, 
    type="l", 
    xlab="", 
    ylab="Intertweet time mean in seconds",
    main="Intertweet time mean for female users"
  )
  dev.off()

  pdf(strcat(filePath,"femaleHistogramMeans.pdf"))
  hist(
    x=sortedFemaleMeans, 
    breaks=20,
    xlab="Intertweet time mean in seconds", 
    ylab="Number of users",
    main="Intertweet time mean for male users"
  )
  dev.off()
}
