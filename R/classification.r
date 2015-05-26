library(class)
library(e1071)
source('qda.r')


# Perform all analysis 
classify.data <- function(analyzed.males,analyzed.females) {
  return(lapply(
    extract.data(analyzed.males,analyzed.females)
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

extract.data <- function(analyzed.males,analyzed.females) {
  # Extract data information
  num.males <- length(analyzed.males$test)+length(analyzed.males$pregnant)
  num.females <- length(analyzed.females$test)+length(analyzed.females$pregnant)
  num.females.pregnant <- length(analyzed.females$pregnant)
  num.observations <- num.males+num.females

  # Build acf indices matrix
  tweet.counts <- scale(rbind(
    t(sapply(analyzed.males$test,'[[','tweet.count.smoothed.adjusted'))
    ,t(sapply(analyzed.males$pregnant,'[[','tweet.count.smoothed.adjusted'))
    ,t(sapply(analyzed.females$test,'[[','tweet.count.smoothed.adjusted'))
    ,t(sapply(analyzed.females$pregnant,'[[','tweet.count.smoothed.adjusted'))
  ),center=FALSE)
  tweet.counts[is.na(tweet.counts)] <- 0
  tweet.counts <- scale(tweet.counts)

  acf.values <- rbind(
    t(sapply(lapply(analyzed.males$test,'[[','acf'),'[[','acf'))
    ,t(sapply(lapply(analyzed.males$pregnant,'[[','acf'),'[[','acf'))
    ,t(sapply(lapply(analyzed.females$test,'[[','acf'),'[[','acf'))
    ,t(sapply(lapply(analyzed.females$pregnant,'[[','acf'),'[[','acf'))
  )
  acf.values[is.na(acf.values)] <- 0
  acf.values <- scale(acf.values)

  acf.values.pregnant <- rbind(
    t(sapply(lapply(analyzed.females$test,'[[','acf'),'[[','acf'))
    ,t(sapply(lapply(analyzed.females$pregnant,'[[','acf'),'[[','acf'))
  )
  acf.values.pregnant[is.na(acf.values.pregnant)] <- 0
  acf.values.pregnant <- scale(acf.values.pregnant)

  acf.indices <- scale(rbind(
    t(sapply(analyzed.males$test,'[[','acf.indices'))
    ,t(sapply(analyzed.males$pregnant,'[[','acf.indices'))
    ,t(sapply(analyzed.females$test,'[[','acf.indices'))
    ,t(sapply(analyzed.females$pregnant,'[[','acf.indices'))
  ),center=FALSE)

  acf.indices.pregnant <- scale(rbind(
    t(sapply(analyzed.females$test,'[[','acf.indices'))
    ,t(sapply(analyzed.females$pregnant,'[[','acf.indices'))
  ),center=FALSE)

  profiles <- list()

  # Tweet counts
  profiles[[length(profiles)+1]] <- list(
    class=c(rep(0,num.males),rep(1,num.females))
    ,feats=tweet.counts
    ,class1="males"
    ,class2="females"
    ,type="tweet pdfs"
  )
  profiles[[length(profiles)+1]] <- list(
    class=c(rep(0,length(analyzed.females$test)),rep(1,length(analyzed.females$pregnant)))
    ,feats=acf.values.pregnant
    ,class1="females"
    ,class2="pregnant females"
    ,type="acf values"
  )
  profiles[[length(profiles)+1]] <- list(
    class=c(rep(0,length(analyzed.females$test)),rep(1,length(analyzed.females$pregnant)))
    ,feats=acf.indices.pregnant
    ,class1="females"
    ,class2="pregnant females"
    ,type="acf indices"
  )
  # Genre acf
  profiles[[length(profiles)+1]] <- list(
    class=c(rep(0,num.males),rep(1,num.females))
    ,feats=acf.values
    ,class1="males"
    ,class2="females"
    ,type="acf values"
  )
  # Genre acf indices
  profiles[[length(profiles)+1]] <- list(
    class=c(rep(0,num.males),rep(1,num.females))
    ,feats=acf.indices
    ,class1="males"
    ,class2="females"
    ,type="acf indices"
  )
  # Pregnant acf
  profiles[[length(profiles)+1]] <- list(
    class=c(rep(0,num.observations-num.females.pregnant),rep(1,num.females.pregnant))
    ,feats=acf.values
    ,class1="rest of the observations"
    ,class2="pregnant females"
    ,type="acf values"
  )
  # Pregnant acf indices
  profiles[[length(profiles)+1]] <- list(
    class=c(rep(0,num.observations-num.females.pregnant),rep(1,num.females.pregnant))
    ,feats=acf.indices
    ,class1="rest of the observations"
    ,class2="pregnant females"
    ,type="acf indices"
  )

  return(profiles)
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
