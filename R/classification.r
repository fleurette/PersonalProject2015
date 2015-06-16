source('qda.r')


# Classify data using a number of different classifiers on the during pregnancy features
classify.data <- function(analyzed.males,analyzed.females) {
  return(lapply(
    extract.problems(analyzed.males,analyzed.females)
    ,function(problem) {
      problem$knn.euclidean <- knn.euclidean(problem)
      problem$qda <- qda.execute(problem)
      problem$svm <- svm.custom(problem)
      print("Done Problem")
      return(problem)
    }
  ))
}

extract.problems <- function(analyzed.males,analyzed.females) {
  # Prepare all different feature matrices
  test.males <- Filter(function(x) length(x),lapply(analyzed.males$test,'[[','classify'))
  test.females <- Filter(function(x) length(x),lapply(analyzed.females$test,'[[','classify'))
  pregnant.males <- Filter(function(x) length(x),lapply(analyzed.males$pregnant,'[[','classify'))
  pregnant.females <- Filter(function(x) length(x),lapply(analyzed.females$pregnant,'[[','classify'))

  # Extract data information
  num.test.males <- length(test.males)
  num.pregnant.males <- length(pregnant.males)
  num.males <- num.test.males+num.pregnant.males

  num.test.females <- length(test.females)
  num.pregnant.females <- length(pregnant.females)
  num.females <- num.test.females+num.pregnant.females

  num.observations <- num.males+num.females

  extract.observations <- function(feature.name) {
    observations <- rbind(
      t(sapply(test.males,'[[',feature.name))
      ,t(sapply(pregnant.males,'[[',feature.name))
      ,t(sapply(test.females,'[[',feature.name))
      ,t(sapply(pregnant.females,'[[',feature.name))
    )
    observations[is.na(observations)] <- 0
    return(observations)
  }

  pdfs <- extract.observations("pdf")
  acfs <- extract.observations("acf")
  all.features <- cbind(pdfs,acfs)

  feature.matrices <- list(
    list(feats=pdfs,type="pdf")
    ,list(feats=acfs,type="acf")
    list(feats=all.features,type="all features")
  )

  # Prepare all different classification problems
  classification.problems <- list(
    list(
      class1="males"
      ,class2="females"
      ,num.classes=2
      ,class=c(rep(0,num.males),rep(1,num.females))
      ,range=1:num.observations
    )
    ,list(
      class1="test females"
      ,class2="pregnant females"
      ,num.classes=2
      ,class=c(rep(0,num.test.females),rep(1,num.pregnant.females))
      ,range=(num.males+1):num.observations
    )
    list(
      class1="rest of observations"
      ,class2="pregnant females"
      ,num.classes=2
      ,class=c(rep(0,num.observations-num.pregnant.females),rep(1,num.pregnant.females))
      ,range=1:num.observations
    )
    ,list(
      class1="males"
      ,class2="females"
      ,class3="pregnant females"
      ,num.classes=3
      ,class=c(rep(0,num.males),rep(1,num.observations-num.males-num.pregnant.females),rep(2,num.pregnant.females))
      ,range=1:num.observations
    )
  )

  # Extract problems
  extracted.problems <- list() 

  for(classification.problem in classification.problems) {
    for(feature.matrix in feature.matrices) {
      problem <- classification.problem
      problem$type <- feature.matrix$type
      problem$feats <- feature.matrix$feats[classification.problem$range,]
      extracted.problems[[length(extracted.problems)+1]] <- problem
    }
  }

  return(extracted.problems)
}

# Wrapper for svm 
svm.custom <- function(problem) {
  num.folds <- 10
  return(svm(problem$feats,factor(problem$class),cross=num.folds,kernel="radial",cost=10,epsilon=0.2)$accuracies/100)
}

knn.euclidean <- function(problem) {
  # Ks configuration
  k.from <- 2
  k.to <- 60
  k.by <- 2
  accuracies <- c()
  num.observations <- length(problem$class)
  ks <- seq(from=k.from,to=k.to,by=k.by)
  for(k in ks) {
    pred.classes <- knn.cv(problem$feats,problem$class,k)
    accuracies <- c(
      accuracies
      ,mean(pred.classes==problem$class)
    )
  }
  return(list(
    ks=ks
    ,accuracies=accuracies
  ))
}
