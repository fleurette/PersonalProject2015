library(MASS)

get.fold.idx <- function(data,num.folds) {
  num.obs <- length(data$class)
  # Generate random indices
  random.idx <- sample.int(num.obs)
  obs.by.fold <- max(floor(num.obs/num.folds),1)
  fold.idx <- list()
  for(idx in 1:num.folds) {
    if(idx!=num.folds) {
      remaining.idx <- random.idx[1:obs.by.fold]
      random.idx <- random.idx[-(1:length(remaining.idx))]
    } else {
      remaining.idx <- random.idx
    }
    fold.idx[[idx]] <- remaining.idx
  }
  return(fold.idx)
}

get.fold.data <- function(data,fold.idx) {
  train.data <- list(
    class=data$class[-fold.idx]
    ,feats=data$feats[-fold.idx,]
  )
  if(length(fold.idx)==1) {
    test.data <- list(
      class=data$class[fold.idx]
      ,feats=t(as.matrix(data$feats[fold.idx,]))
    )
  } else {
    test.data <- list(
      class=data$class[fold.idx]
      ,feats=data$feats[fold.idx,]
    )
  }
  return(list(
    train.data=train.data
    ,test.data=test.data
  ))
}

qda.train <- function(train.data) {
  data.class1 <- train.data$feats[train.data$class==0,]
  data.class2 <- train.data$feats[train.data$class==1,]
  # Calculate mean and covar
  mean1 <- apply(data.class1,2,mean,na.rm=TRUE)
  covar1 <- cov(data.class1,use="complete")
  mean2 <- apply(data.class2,2,mean,na.rm=TRUE)
  covar2 <- cov(data.class2,use="complete")
  prior1 <- 1 - sum(train.data$class)/length(train.data$class)
  prior2 <- 1 - prior1

  return(list(
      mean1=mean1,mean2=mean2
      ,covar1=covar1,covar2=covar2
      ,prior1=prior1,prior2=prior2
      ,covar1.inverse=ginv(covar1),covar2.inverse=ginv(covar2)
      ,covar1.norm=norm(covar1),covar2.norm=norm(covar2)
    )
  )
}

qda.discriminant <- function(obs,mean,covar,prior,covar.inv,covar.norm) {
  missing.feats <- which(is.na(obs))
  if(length(missing.feats)) {
    centered.obs <- obs[-missing.feats]-mean[-missing.feats]
    adjusted.cov <- covar.inv[-missing.feats,-missing.feats]
    term1 <- -0.5*t(centered.obs)%*%adjusted.cov%*%(centered.obs)
  } else {
    centered.obs <- obs-mean
    adjusted.cov <- covar.inv
    term1 <- -0.5*t(centered.obs)%*%adjusted.cov%*%(centered.obs)
  }

  term2 <- -0.5*length(obs)*log(2*pi)
  term3 <- -0.5*log(covar.norm)+log(prior)

  return(term1+term2+term3)
}

qda.pred <- function(qda.model,train.data,test.data) {
  num.test.obs <- nrow(test.data$feats)
  num.feats <- ncol(test.data$feats)
  # Classify test data
  scores1 <- apply(
    test.data$feats
    ,1
    ,qda.discriminant
    ,qda.model$mean1
    ,qda.model$covar1
    ,qda.model$prior1
    ,qda.model$covar1.inverse
    ,qda.model$covar1.norm
  )
  scores2 <- apply(
    test.data$feats
    ,1
    ,qda.discriminant
    ,qda.model$mean2
    ,qda.model$covar2
    ,qda.model$prior2
    ,qda.model$covar2.inverse
    ,qda.model$covar2.norm
  )
  # Predict classes
  pred.classes <- as.numeric(scores2>scores1)
  # Calculate err rate
  correct.count <- sum(as.numeric(pred.classes==test.data$class))
  err.rate <- 1 - correct.count/num.test.obs

  return(list(
    scores1=scores1
    ,scores2=scores2
    ,pred.classes=pred.classes
    ,err.rate=err.rate
  ))
}

qda.classify <- function(train.data,test.data) {
  train.time <- as.numeric(system.time(qda.model <- qda.train(train.data))[3])
  pred.time <- as.numeric(system.time(qda.result <- qda.pred(qda.model,train.data,test.data))[3])

  qda.result$pred.time <- pred.time
  qda.result$train.time <- train.time
  qda.result$total.time <- pred.time+train.time

  return(qda.result)
}

qda.execute <- function(train.data,num.folds=2) {
  num.obs <- length(train.data$class)
  # Get fold indices
  fold.idx <- get.fold.idx(train.data,num.folds)
  # Compute error rate
  qda.result <- sapply(
    1:num.folds
    ,function(idx) {
      temp.data <- get.fold.data(train.data,fold.idx[[idx]])
      qda.classify(temp.data$train.data,temp.data$test.data)
    }
  )
  train.time <- mean(apply(qda.result,2,'[[','train.time'))
  pred.time <- mean(apply(qda.result,2,'[[','pred.time'))
  err.rate <- mean(apply(qda.result,2,'[[','err.rate'))
  return(1-err.rate)
}
