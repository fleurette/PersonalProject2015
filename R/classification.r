library(class)
library(e1071)
source('qda.r')


# Perform all analysis 
classify.data <- function(analyzed.males,analyzed.females) {
  return(lapply(
    extract.problems(analyzed.males,analyzed.females)
    ,function(profile) {
      profile$knn.euclidean <- knn.euclidean(profile)
      profile$qda <- qda.execute(profile)
      profile$svm <- 1-svm(profile$feats,profile$class,cross=40)$tot.MSE
      # Clean up profile
      profile$class <- NULL
      profile$feats <- NULL
      return(profile)
    }
  ))
}

extract.problems <- function(analyzed.males,analyzed.females) {
  # Extract data information
  num.test.males <- length(analyzed.males$test)
  num.pregnant.males <- length(analyzed.males$pregnant)
  num.males <- num.test.males+num.pregnant.males

  num.test.females <- length(analyzed.females$test)
  num.pregnant.females <- length(analyzed.females$pregnant)
  num.females <- num.test.females+num.pregnant.females

  num.observations <- num.males+num.females

  # Prepare all different feature matrices
  feature.matrices <- list()

  tweet.counts <- rbind(
    t(sapply(analyzed.males$test,'[[','tweet.count.smoothed.adjusted'))
    ,t(sapply(analyzed.males$pregnant,'[[','tweet.count.smoothed.adjusted'))
    ,t(sapply(analyzed.females$test,'[[','tweet.count.smoothed.adjusted'))
    ,t(sapply(analyzed.females$pregnant,'[[','tweet.count.smoothed.adjusted'))
  )
  tweet.counts[is.na(tweet.counts)] <- 0
  tweet.counts <- scale(tweet.counts)
  feature.matrices[[length(feature.matrices)+1]] <- list(
    data=tweet.counts
    ,type="tweet counts"
  )

  acf.values <- rbind(
    t(sapply(lapply(analyzed.males$test,'[[','acf'),'[[','acf'))
    ,t(sapply(lapply(analyzed.males$pregnant,'[[','acf'),'[[','acf'))
    ,t(sapply(lapply(analyzed.females$test,'[[','acf'),'[[','acf'))
    ,t(sapply(lapply(analyzed.females$pregnant,'[[','acf'),'[[','acf'))
  )
  acf.values[is.na(acf.values)] <- 0
  acf.values <- scale(acf.values)
  feature.matrices[[length(feature.matrices)+1]] <- list(
    data=acf.values
    ,type="acf values"
  )

  acf.indices <- scale(rbind(
    t(sapply(analyzed.males$test,'[[','acf.indices'))
    ,t(sapply(analyzed.males$pregnant,'[[','acf.indices'))
    ,t(sapply(analyzed.females$test,'[[','acf.indices'))
    ,t(sapply(analyzed.females$pregnant,'[[','acf.indices'))
  ),center=FALSE)
  feature.matrices[[length(feature.matrices)+1]] <- list(
    data=acf.indices
    ,type="acf indices"
  )

  feature.matrices[[length(feature.matrices)+1]] <- list(
    data=cbind(acf.indices,acf.values,tweet.counts)
    ,type="all features"
  )

  # Prepare all different classification problems
  classification.problems <- list()

  classification.problems[[length(classification.problems)+1]] <- list(
    class1="males"
    ,class2="females"
    ,class=c(rep(0,num.males),rep(1,num.females))
    ,range=1:num.observations
  )

  classification.problems[[length(classification.problems)+1]] <- list(
    class1="test females"
    ,class2="pregnant females"
    ,class=c(rep(0,num.test.females),rep(1,num.pregnant.females))
    ,range=(num.males+1):num.observations
  )

  classification.problems[[length(classification.problems)+1]] <- list(
    class1="all observations"
    ,class2="pregnant females"
    ,class=c(rep(0,num.observations-num.pregnant.females),rep(1,num.pregnant.females))
    ,range=1:num.observations
  )

  classification.problems[[length(classification.problems)+1]] <- list(
    class1="males"
    ,class2="females"
    ,class3="pregnant females"
    ,class=c(rep(0,num.males),rep(1,num.observations-num.pregnant.females),rep(2,num.pregnant.females))
    ,range=1:num.observations
  )

  # Extract problems
  extracted.problems <- list() 

  for(classification.problem in classification.problems) {
    for(feature.matrix in feature.matrices) {
      extracted.problems[[length(extracted.problems)+1]] <- list(
        class1=classification.problem$class1
        ,class2=classification.problem$class2
        ,feats=feature.matrix$data[classification.problem$range,]
        ,class=classification.problem$class
        ,type="tweet counts"
      )
    }
  }


  return(extracted.problems)
}

# Wrapper for knn algorithm
knn.euclidean <- function(data,from=2,to=60,by=2) {
  accuracies <- c()
  num.observations <- length(data$class)
  ks <- seq(from=from,to=to,by=by)
  for(k in ks) {
    # Knn probably return an error rate already check this
    pred.classes <- knn.cv(data$feats,data$class,k)
    accuracies <- c(
      accuracies
      ,sum(as.numeric(pred.classes==data$class))/num.observations
    )
  }
  return(list(
    ks=ks
    ,accuracies=accuracies
  ))
}
