# Implementation of five different classifiers
library(MASS)

######
# EM #
######

em.lhood <- function(obs,means,covars,mix.prop,covars.invs,covars.norms) {
  dim <- ncol(obs)
  num.dists <- length(mix.prop)
  return(cbind(sapply(
    1:num.dists
    ,function(dist.idx) {
      centered.data <- t(t(obs)-means[dist.idx,])
      temp <- exp(-0.5*apply(
        centered.data
        ,1
        ,function(row) (t(row)%*%covars.invs[[dist.idx]]%*%row)
      ))
      temp <- mix.prop[dist.idx]*temp/(((2*pi)^(0.5*dim))*sqrt(covars.norms[dist.idx]))
      return(temp)
    }
  )))
}

# Compute initial parameters for the em algorithm
em.initialize <- function(obs,num.dists) {
  num.feats <- ncol(obs)
  num.obs <- nrow(obs)
  # Initialize means and mix prop
  means <- cbind(apply(
    obs
    ,2
    ,function(col)(runif(num.dists,min=min(col),max=max(col)))
  ))
  mix.prop <- rep(1/num.dists,num.dists)
  # Covariances
  covars <- lapply(
    1:num.dists
    ,function(dist.idx) {
      centered.data <- t(t(obs)-means[dist.idx,])
      return (t(centered.data)%*%(centered.data)/(num.obs-1))
    }
  )
  covars.invs <- lapply(covars,inv.shrinkage)
  covars.norms <- sapply(covars,norm)

  lhoods <- em.lhood(obs,means,covars,mix.prop,covars.invs,covars.norms)

  return(list(
    mix.prop=mix.prop
    ,covars=covars
    ,means=means
    ,covars.invs=covars.invs
    ,covars.norms=covars.norms
    ,lhoods=lhoods
  ))
}

# Compute inverse of a covar matrix using shrinkage 
inv.shrinkage <- function(covar,shrinkage.parameter=0.5) {
  return(tryCatch(
    ginv(covar),
    err=ginv(shrinkage.parameter*covar+shrinkage.parameter*diag(diag(covar)))
  ))
}

# Calculate fitting mixture of gaussians
em.fit <- function(obs,num.dists,eps=0.01,max.iters=50) {
  num.feats <- ncol(obs)
  num.obs <- nrow(obs)
  # Extract initial parameters
  initial.params <- em.initialize(obs,num.dists)
  mix.prop <- initial.params$mix.prop
  means <- initial.params$means
  covars <- initial.params$covars
  covars.invs <- initial.params$covars.invs
  covars.norms <- initial.params$covars.norms
  lhoods <- initial.params$lhoods
  # Termination parameters
  iter.idx <- 0
  lhood.delta <- Inf
  # Calculate likelihood
  lhood <- sum(apply(
    lhoods
    ,1
    ,function(row) log(sum(row))
  ))
  # Iterate until termination
  while (lhood.delta > eps & iter.idx < max.iters) {
    # E-Step: Evaluate Responsibilites
    resps <- t(apply(
      lhoods
      ,1
      ,function(row) row/sum(row)
    ))
    # M-Step: Update parameters using new responsibilites
    distrib.resps <- colSums(resps)
    means <- t(rbind(sapply(
      1:num.dists
      ,function(dist.idx) colSums(resps[,dist.idx]*obs)/distrib.resps[dist.idx]
    )))
    covars <- lapply(
      1:num.dists
      ,function(dist.idx) {
        centered.data <- t(t(obs)-means[dist.idx,])
        return(t(centered.data)%*%(centered.data)/(distrib.resps[dist.idx]))
      }
    )
    covars.invs <- lapply(covars,inv.shrinkage)
    covars.norms <- sapply(covars,norm)
    # Update lhood
    lhood.prev <- lhood
    lhoods <- em.lhood(obs,means,covars,mix.prop,covars.invs,covars.norms)
    lhood <- round(sum(apply(
      lhoods
      ,1
      ,function(row) log(sum(row))
    )))
    lhood.delta.prev <- lhood.delta
    lhood.delta <- abs(lhood^2-lhood.prev^2)
    if(lhood.delta==lhood.delta.prev & lhood>lhood.prev) {
      lhood.delta <- 0
    }
    iter.idx <- iter.idx + 1
  }

  return(list(
    mix.prop=mix.prop
    ,covars=covars
    ,means=means
    ,covars.invs=covars.invs
    ,covars.norms=covars.norms
  ))
}

em.classify <- function(train.data,test.data,num.dists) {
  # Extract data class
  obs.class1 <- train.data$feats[train.data$class==0,]
  obs.class2 <- train.data$feats[train.data$class==1,]
  # Find fitting gausian
  em.model1 <- em.fit(obs.class1,num.dists)
  em.model2 <- em.fit(obs.class2,num.dists)
  # Calculate data scores
  scores1 <- apply(
      em.lhood(
        test.data$feats
        ,em.model1$means
        ,em.model1$covars
        ,em.model1$mix.prop
        ,em.model1$covars.invs
        ,em.model1$covars.norms
      )
      ,1
      ,function(row) sum(row)
  )
  scores2 <- apply(
      em.lhood(
        test.data$feats
        ,em.model2$means
        ,em.model2$covars
        ,em.model2$mix.prop
        ,em.model2$covars.invs
        ,em.model2$covars.norms
      )
      ,1
      ,function(row) sum(row)
  )

  pred.classes <- as.numeric(scores2>scores1)
  err.rate <- 1-sum(as.numeric((pred.classes==test.data$class)))/length(test.data$class)

  return(list(
    scores1=scores1
    ,scores2=scores2
    ,pred.classes=pred.classes
    ,err.rate=err.rate
  ))
}

em.calibrate <- function(num.dists) {
  return(function(train.data,test.data) {
    em.classify(train.data,test.data,num.dists)
  })
}

em.execute <- function(train.data,num.folds=2,min.k=2,max.k=18,by.k=2) {
  num.obs <- length(train.data$class)
  optimal.err.rate <- Inf
  optimal.k <- min.k
  fold.idx <- get.fold.idx(train.data,num.folds)
  # For each k
  for(k in seq(from=min.k,to=max.k,by=by.k)) {
    # Compute error rate
    err.rate <- mean(sapply(
      1:num.folds
      ,function(idx) {
        temp.data <- get.fold.data(train.data,fold.idx[[idx]])
        return(em.classify(temp.data$train.data,temp.data$test.data,k)$err.rate)
      }
    ))
    print(paste("Em with",k,"distributions. Error rate was",err.rate))
    # Update k and error rates
    if(err.rate < optimal.err.rate) {
      optimal.err.rate <- err.rate
      optimal.k <- k
    }
  }
  print(paste(
    "Best mean error rate for em is"
    ,optimal.err.rate
    ,". Optimal number of distributions is"
    ,optimal.k
  ))
  # Return classifier set with parameter
  return(list(
    err.rate=optimal.err.rate
    ,num.distributions=optimal.k
    ,classifier=em.calibrate(optimal.k)
    ,name="em"
  ))
}

#################
# KNN Algorithm #
#################

# Compute tricube function
tricube <- function(x) {
  return ((1-abs(x)^3)^3)
}

# Return the classification scores for test matrix
knn.classify <- function(train.data,test.data,k) {
  num.feats <- ncol(test.data$feats)
  num.test.obs <- nrow(test.data$feats)
  num.train.obs <- nrow(train.data$feats)
  # Hold class scores for every test point and every class
  scores1 <- rep(0,num.test.obs)
  scores2 <- rep(0,num.test.obs)
  # For every test data point
  for (obs.idx in 1:num.test.obs) {
    # Compute the dists to all the train points
    dists <-  apply(train.data$feats,1,function(row) sqrt(sum((row-test.data$feats[obs.idx,])^2))) 
    # Get idx, and dists of k closest elements
    k.closest <- (sort.int(dists,index.return=TRUE))
    k.closest.idx <- k.closest$ix[1:k]
    k.closest.dists <- k.closest$x[1:k]
    # Rescale dists by dividing by the largest element ie last element of sorted array of dists
    k.closest.dists <- k.closest.dists/k.closest.dists[k]
    # Compute class scores for the k closest elements
    for (idx in 1:k) {
      # Extract class and distance of neighbor
      neighbor.idx <- k.closest.idx[idx]
      neighbor.class <- train.data$class[neighbor.idx]
      neighbor.distance <- k.closest.dists[idx]
      # Add tricube of neighbor.distance to class scores
      if(neighbor.class==0){
       scores1[obs.idx] <- scores1[obs.idx] + tricube(neighbor.distance)
      } else {
       scores2[obs.idx] <- scores2[obs.idx] + tricube(neighbor.distance)
      }
    }
  }

  pred.classes <- as.numeric(scores2>scores1)
  correct.count <- sum(as.numeric(pred.classes==test.data$class))

  # Calculate err rate
  err.rate <- 1 - correct.count/num.test.obs

  return(list(
    scores1=scores1
    ,scores2=scores2
    ,pred.classes=pred.classes
    ,err.rate=err.rate
  ))
}

knn.calibrate <- function(k) {
  return(function(train.data,test.data) {
    knn.classify(train.data,test.data,k)
  })
}

knn.execute <- function(train.data,num.folds=10,min.k=5,max.k=50,by.k=5) {
  num.obs <- length(train.data$class)
  optimal.err.rate <- Inf
  optimal.k <- min.k
  fold.idx <- get.fold.idx(train.data,num.folds)
  # For each k
  for(k in seq(from=min.k,to=max.k,by=by.k)) {
    # Compute error rate
    err.rate <- mean(sapply(
      1:num.folds
      ,function(idx) {
        temp.data <- get.fold.data(train.data,fold.idx[[idx]])
        return(knn.classify(temp.data$train.data,temp.data$test.data,k)$err.rate)
      }
    ))
    print(paste("knn with",k,"neighbors. Error rate was",err.rate))
    # Update k and error rates
    if(err.rate < optimal.err.rate) {
      optimal.err.rate <- err.rate
      optimal.k <- k
    }
  }
  print(paste(
    "Best mean error rate for knn is"
    ,optimal.err.rate
    ,". Optimal number of neighbors is"
    ,optimal.k
  ))
  # Return classifier set with parameter
  return(list(
    err.rate=optimal.err.rate
    ,neighbors=optimal.k
    ,classifier=knn.calibrate(optimal.k)
    ,name="knn"
  ))
}

#################
# QDA Algorithn #
#################

qda.train <- function(train.data) {
  data.class1 <- train.data$feats[train.data$class==0,]
  data.class2 <- train.data$feats[train.data$class==1,]
  # Calculate mean and covar
  mean1 <- apply(data.class1,2,mean)
  covar1 <- cov(data.class1)
  mean2 <- apply(data.class2,2,mean)
  covar2 <- cov(data.class2)
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

qda.discriminant <- function(obs,mean,covar,prior,covar.inverse,covar.norm) {
  term1 <- -0.5*t(obs-mean)%*%covar.inverse%*%(obs-mean)
  term2 <- -0.5*length(obs)*log(2*pi)
  term3 <- -0.5*log(covar.norm)+log(prior)

  return(term1+term2+term3)
}

qda.classify <- function(train.data,test.data) {
  # Train
  qda.model <- qda.train(train.data)
  num.test.obs <- nrow(test.data$feats)
  scores1 <- rep(0,num.test.obs)
  scores2 <- rep(0,num.test.obs)
  pred.classes <- rep(0,num.test.obs)
  num.feats <- ncol(test.data$feats)
  correct.count <- 0
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

qda.execute <- function(train.data,num.folds=2) {
  num.obs <- length(train.data$class)
  # Get fold indices
  fold.idx <- get.fold.idx(train.data,num.folds)
  # Compute error rate
  err.rate <- mean(sapply(
    1:num.folds
    ,function(idx) {
      temp.data <- get.fold.data(train.data,fold.idx[[idx]])
      return(qda.classify(temp.data$train.data,temp.data$test.data)$err.rate)
    }
  ))
  print(paste("Mean error rate for qda is",err.rate))
  return(list(
    err.rate=err.rate
    ,classifier=qda.classify
    ,name="qda"
  ))
}

################################
# Fisher Discriminant Analysis #
################################

fda.train <- function(train.data) {
  data.class1 <- train.data$feats[train.data$class==0,]
  data.class2 <- train.data$feats[train.data$class==1,]
  num.obs <- length(train.data$feats)
  num.obs.class1 <- nrow(data.class1)
  num.obs.class2 <- nrow(data.class2)

  mean.class1 <- apply(data.class1,2,mean)
  mean.class2 <- apply(data.class2,2,mean)
  grand.mean <- (num.obs.class1*mean.class1+num.obs.class2*mean.class2)/num.obs

  covar.class1 <- cov(data.class1)
  covar.class2 <- cov(data.class2)

  between.class.scatter.matrix <- num.obs.class1*(mean.class1-grand.mean)%*%t(mean.class1-grand.mean) + num.obs.class2*(mean.class2-grand.mean)%*%t(mean.class2-grand.mean)
  within.class.scatter.matrix <- (num.obs.class1-1)*covar.class1 + (num.obs.class2-1)*covar.class2

  # Take n-1 first eigenvectors
  eigenvectors <- (eigen(ginv(within.class.scatter.matrix)%*%between.class.scatter.matrix))$vectors

  return(list(
    projection=eigenvectors
    ,proj.mean1=mean.class1%*%eigenvectors
    ,proj.mean2=mean.class2%*%eigenvectors
    ,proj.inv.covar1=ginv(t(eigenvectors)%*%covar.class1%*%eigenvectors)
    ,proj.inv.covar2=ginv(t(eigenvectors)%*%covar.class2%*%eigenvectors)
  ))
}

mahalanobis.distance <- function(obs,mean,covar.inv) {
  return (sqrt((obs-mean)%*%covar.inv%*%t(obs-mean)))
}

fda.classify <- function(train.data,test.data) {
  # Build model
  fda.model <- fda.train(train.data)
  # Classify test data
  num.test.obs <- length(test.data$class)
  num.train.obs <- length(train.data$class)
  scores1 <- rep(0,num.test.obs)
  scores2 <- rep(0,num.test.obs)
  pred.classes <- rep(0,num.test.obs)
  correct.count <- 0
  # Compute priors
  prior2 <- sum(train.data$class)/num.train.obs
  prior1 <- 1-prior2
  # Project obs
  proj.obs <- test.data$feats%*%fda.model$projection
  scores1 <- prior1*apply(
    proj.obs
    ,1
    ,mahalanobis.distance
    ,fda.model$proj.mean1
    ,fda.model$proj.inv.covar1
  )
  scores2 <- prior2*apply(
    proj.obs
    ,1
    ,mahalanobis.distance
    ,fda.model$proj.mean2
    ,fda.model$proj.inv.covar2
  )

  pred.classes <- as.numeric(abs(scores2)<abs(scores1))
  correct.count <- sum(as.numeric(pred.classes==test.data$class))

  # Calculate err rate
  err.rate <- 1 - correct.count/num.test.obs

  return(list(
    scores1=scores1
    ,scores2=scores2
    ,pred.classes=pred.classes
    ,err.rate=err.rate
  ))
}

fda.execute <- function(train.data,num.folds=2) {
  num.obs <- length(train.data$class)
  # Get fold indices
  fold.idx <- get.fold.idx(train.data,num.folds)
  # Compute error rate
  err.rate <- mean(sapply(
    1:num.folds
    ,function(idx) {
      temp.data <- get.fold.data(train.data,fold.idx[[idx]])
      return(fda.classify(temp.data$train.data,temp.data$test.data)$err.rate)
    }
  ))
  print(paste("Mean error rate for fda is",err.rate))
  return(list(
    err.rate=err.rate
    ,classifier=fda.classify
    ,name="fda"
  ))
}

####################
# Weak classifiers #
###################

# Implementation of Perceptron Classifier with weights
simple.perceptron <- function(train.data,test.data,final.data=NULL,learning.rate=0.1,fit.weights=NULL) {
  # Configure weights and epsilon
  num.train.obs <- length(train.data$class)
  num.feats <- ncol(train.data$feats)
  num.test.obs <- length(test.data$class)
  if(is.null(fit.weights)) {
    fit.weights <- rep(1/num.train.obs,num.train.obs)
  }
  max.iters <- 50
  # Select v
  v <- rep(0,num.feats+1)
  # Compute extended train set
  extended.train.data <- cbind(rep(1,num.train.obs),train.data$feats)
  extended.train.data[train.data$class==0,] <- -extended.train.data[train.data$class==0]
  # Perform gradient descent until convergence
  for(i in 1:max.iters) {
    # Calculate misclassified elements
    misclassified.idx <- which(extended.train.data%*%v<=0)
    v <- v+learning.rate*length(misclassified.idx)*colSums(rbind(
      rep(0,num.feats+1)
      ,fit.weights[misclassified.idx]*extended.train.data[misclassified.idx,]
    ))
  }
  # Classify test data
  pred.classes <- as.numeric((test.data$feats%*%v[-1]+v[1])>0)
  correct.count <- sum(as.numeric(pred.classes==test.data$class))
  # Calculate err rate
  err.rate <- 1 - correct.count/num.test.obs
  # Classify final data
  if(!is.null(final.data)) {
    num.final.obs <- length(final.data$class)
    final.pred.classes <- as.numeric((final.data$feats%*%v[-1]+v[1])>0)
    final.correct.count <- sum(as.numeric(final.pred.classes==final.data$class))
    final.err.rate <- 1 - final.correct.count/num.final.obs
    return(list(
      pred.classes=pred.classes
      ,err.rate=err.rate
      ,final.pred.classes=final.pred.classes
      ,final.err.rate=final.err.rate
    ))
  } else {
    return(list(
      pred.classes=pred.classes
      ,err.rate=err.rate
    ))
  }
}

decision.stump <- function(train.data,test.data,final.data=NULL,fit.weights=NULL) {
  # Configure weights and epsilon
  num.train.obs <- length(train.data$class)
  num.test.obs <- length(test.data$class)
  num.feats <- ncol(train.data$feats)
  if(is.null(fit.weights)) {
    fit.weights <- rep(1/num.train.obs,num.train.obs)
  }
  # Find most discriminant feat
  min.err.rate <- Inf
  err.rate <- function(threshold) {
    -sum(fit.weights*as.numeric(as.numeric(thresholds<threshold)==train.data$class))
  }
  for(feat.idx in 1:num.feats) {
    thresholds <- train.data$feats[,feat.idx]
    err.rates <- sapply(thresholds,err.rate)
    min.idx <- which.min(err.rates)
    if(err.rates[min.idx] < min.err.rate) {
      min.err.rate <- err.rates[min.idx]
      min.feat <- feat.idx
      min.threshold <- thresholds[min.idx]
    }
  }
  # Classify data
  pred.classes <- as.numeric(test.data$feats[,min.feat]<min.threshold)
  correct.count <- sum(as.numeric(pred.classes==test.data$class))
  # Calculate err rate
  err.rate <- 1 - correct.count/num.test.obs

  if(!is.null(final.data)) {
    num.final.obs <- length(final.data$class)
    final.pred.classes <- as.numeric(final.data$feats[,min.feat]<min.threshold)
    final.correct.count <- sum(as.numeric(final.pred.classes==final.data$class))
    final.err.rate <- 1 - final.correct.count/num.final.obs
    return(list(
      pred.classes=pred.classes
      ,err.rate=err.rate
      ,final.pred.classes=final.pred.classes
      ,final.err.rate=final.err.rate
    ))
  } else {
    return(list(
      pred.classes=pred.classes
      ,err.rate=err.rate
    ))
  }
}

##############
# Techniques #
##############

# Implementation of Bagging algorithm
bagging.classify <- function(train.data,test.data,num.iter,base.learner) {
  train.sample.size <- length(train.data$class)
  test.sample.size <- length(test.data$class)

  votes.count <- rep(0,test.sample.size)

  for(i in 1:num.iter) {
    # Create new data set
    idx <- sample.int(train.sample.size,size=train.sample.size,replace=TRUE) 
    bootstrapped.data <- list(
      class = train.data$class[idx]
      ,feats = train.data$feats[idx,]
    )
    # Update votes
    base.model <- base.learner(bootstrapped.data,test.data)
    votes.count <- votes.count + base.model$pred.classes
  }

  # Compute final scores
  pred.classes <- round(votes.count/num.iter)
  correct.count <- sum(as.numeric(pred.classes==test.data$class))

  # Calculate err rate
  err.rate <- 1 - correct.count/test.sample.size

  return(list(
    pred.classes=pred.classes
    ,err.rate=err.rate
  ))
}

# Implementation of Adaboost algorithm
adaboost.classify <- function(train.data,test.data,num.layers,base.learners=list(simple.perceptron,decision.stump)) {
  train.sample.size <- length(train.data$class)
  test.sample.size <- length(test.data$class)
  num.learners <- length(base.learners)

  boosting.weights <- rep(1/train.sample.size,train.sample.size)

  classification.outcome <- rep(0,test.sample.size)

  alpha <- function(err.rate) {
    return (0.5*log(abs(1-err.rate)/err.rate))
  }

  for(layer.idx in 1:num.layers) {
    min.err.rate <- Inf
    for(learner.idx in 1:num.learners) {
      # Compute base learner scores on this data
      base.learner.scores <- base.learners[[learner.idx]](
        train.data
        ,train.data
        ,fit.weights=boosting.weights
        ,final.data=test.data
      )
      err.rate <- 0.5-0.5*sum(boosting.weights*(2*base.learner.scores$pred.classes-1)*(2*train.data$class-1))
      if(err.rate < min.err.rate) {
        min.err.rate <- err.rate
        min.scores <- base.learner.scores
      }
    }
    alpha.err <- alpha(min.err.rate)
    # Update classification scores using alpha value
    classification.outcome <- classification.outcome+alpha.err*(2*min.scores$final.pred.classes-1)
    # Update weights
    boosting.weights <- boosting.weights*exp(-alpha.err*(2*train.data$class-1)*(2*min.scores$pred.classes-1))
    boosting.weights <- boosting.weights/sum(boosting.weights)
  }

  # Compute final scores
  pred.classes <- 0.5*(sign(classification.outcome)+1)
  correct.count <- sum(as.numeric(pred.classes==test.data$class))

  # Calculate err rate
  err.rate <- 1 - correct.count/test.sample.size

  return(list(
    pred.classes=pred.classes
    ,err.rate=err.rate
  ))
}

adaboost.calibrate <- function(num.layers) {
  return(function(train.data,test.data) {
    adaboost.classify(train.data,test.data,num.layers)
  })
}

adaboost.execute <- function(train.data,num.folds=2,min.k=5,max.k=50,by.k=5) {
  num.obs <- length(train.data$class)
  optimal.err.rate <- Inf
  optimal.k <- min.k
  fold.idx <- get.fold.idx(train.data,num.folds)
  # For each k
  for(k in seq(from=min.k,to=max.k,by=by.k)) {
    # Compute error rate
    err.rate <- mean(sapply(
      1:num.folds
      ,function(idx) {
        temp.data <- get.fold.data(train.data,fold.idx[[idx]])
        return(adaboost.classify(temp.data$train.data,temp.data$test.data,k)$err.rate)
      }
    ))
    print(paste("Adaboost with",k,"layers. Error rate was",err.rate))
    # Update k and error rates
    if(err.rate < optimal.err.rate) {
      optimal.err.rate <- err.rate
      optimal.k <- k
    }
  }
  print(paste(
    "Best mean error rate for adaboost is"
    ,optimal.err.rate
    ,". Optimal number of layers is"
    ,optimal.k
  ))
  # Return classifier set with parameter
  return(list(
    err.rate=optimal.err.rate
    ,layers=optimal.k
    ,classifier=adaboost.calibrate(optimal.k)
    ,name="adaboost"
  ))
}

##############
# Data Clean #
##############
  
data.clean.train <- function(train.data) {
  mean1 <- colMeans(train.data$feats[train.data$class==0,],na.rm=TRUE)
  mean2 <- colMeans(train.data$feats[train.data$class==1,],na.rm=TRUE)
  faulty <- c()
  for(obs.idx in 1:nrow(train.data$feats)) {
    if(all(is.na(train.data$feats[obs.idx,]))) {
      faulty <- c(faulty,obs.idx)
    } else {
      for(feat.idx in 1:ncol(train.data$feats)) {
        if(is.na(train.data$feats[obs.idx,feat.idx])) {
          if(train.data$class[obs.idx]==0) {
            train.data$feats[obs.idx,feat.idx] <- mean1[feat.idx]
          } else {
            train.data$feats[obs.idx,feat.idx] <- mean2[feat.idx]
          }
        }
      }
    }
  }
  if(length(faulty)>0){
    # Remove observations that are only NA's
    train.data$feats <- train.data$feats[-faulty,]
    train.data$class <- train.data$class[-faulty]
  }
  return(train.data)
}

data.clean.test <- function(test.data) {
  meanG <- colMeans(test.data$feats,na.rm=TRUE)
  faulty <- c()
  for(obs.idx in 1:nrow(test.data$feats)) {
    if(all(is.na(test.data$feats[obs.idx,]))) {
      faulty <- c(faulty,obs.idx)
    } else {
      for(feat.idx in 1:ncol(test.data$feats)) {
        if(is.na(test.data$feats[obs.idx,feat.idx])) {
          test.data$feats[obs.idx,feat.idx] <- meanG[feat.idx]
        }
      }
    }
  }
  if(length(faulty)>0){
    # Remove observations that are only NA's
    test.data$feats <- test.data$feats[-faulty,]
    test.data$class <- test.data$class[-faulty]
  }
  return(test.data)
}
####################
# Comparison Tools #
####################

# Significiance level given in percentage
mc.nemar <- function(classifier1,classifier2,test.data,significiance.level) {
  pred.classes1 <- classifier1$pred.classes
  pred.classes2 <- classifier2$pred.classes
  classes <- test.data$class
  n01 <- sum(as.numeric(pred.classes1!=classes&pred.classes2==classes))
  n10 <- sum(as.numeric(pred.classes2!=classes&pred.classes1==classes))

  if(n10==n01&n10==0) {
    test.score <- 0
  } else {
    test.score <- (abs(n01-n10)-1)/sqrt(n10+n01)
  }

  threshold <- sqrt(qchisq((100-significiance.level)/100,1))

  print(paste(
    "Mc Nemar test score is"
    ,test.score
    ,"threshold is"
    ,threshold
  ))

  if(test.score>threshold) {
    print(paste(
      "Null hypothesis rejected with confidence"
      ,100-significiance.level
      ,"%"
    ))
    print("The classifiers are not equivelent")
  } else {
    print(paste(
      "The two classifiers appear to be equivalent"
    ))
  }
}

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

#################
# Third project #
#################

analyze.data <- function(verbose=FALSE) {
  # Load data and analyze it
  data <- read.table("data14.dat")
  
  # Extract class information and feats
  num.feats <- ncol(data)-1
  feats <- data[,2:(num.feats+1)]
  class <- data[,1]

  if(verbose) {
    # Number of data points
    cat('Number of data points is:',nrow(feats),'\n')
    # Information about class
    print('Classes and number of elements of each class:')
    print(table(data[,1]))
    # Mean and variance for each class
    print('Mean of feats class 1')
    print(colMeans(feats[class==0,],na.rm=TRUE))
    print('Mean of feats class 2')
    print(colMeans(feats[class==1,],na.rm=TRUE))
    print(sapply(feats[class==1,],mean,na.rm=TRUE))
    print('Var of feats class 1')
    print(sapply(feats[class==0,],var,na.rm=TRUE))
    print('Var of feats class 2')
    print(sapply(feats[class==1,],var,na.rm=TRUE))
    # Plot densities for each feat
    pdf('feats_densities.pdf')
    for(feat.idx in 1:num.feats) {
      data.class1 <- feats[,feat.idx][class==0]
      data.class2 <- feats[,feat.idx][class==1]
      plot(
        density(data.class1,na.rm=TRUE)
        ,lwd=2
        ,main=paste('pdf of feat',feat.idx,'for class 1')
        ,xlab=paste('range of feat',feat.idx)
        ,ylab='probability'
      )
      plot(
        density(data.class2,na.rm=TRUE)
        ,lwd=2
        ,main=paste('pdf of feat',feat.idx,'for class 2')
        ,xlab=paste('range of feat',feat.idx)
        ,ylab='probability'
      )
    }
    dev.off()
    print("Saved pdf in feats_densities.pdf")
  }
  
  # Split data into a test and train set
  test.idx <- sample.int(nrow(feats),size=100)
  test.data <- list(
    class=class[test.idx]
    ,feats=as.matrix(feats[test.idx,])
  )
  train.data <- list(
    class=class[-test.idx]
    ,feats=as.matrix(feats[-test.idx,])
  )

  return(list(
    test.data=data.clean.test(test.data)
    ,train.data=data.clean.train(train.data)
  ))
}

compare.classifiers <- function(train.data,test.data,num.folds=2) {
  results <- list(
    em.execute(train.data,num.folds)
    ,fda.execute(train.data,num.folds)
    ,qda.execute(train.data,num.folds)
    ,adaboost.execute(train.data,num.folds)
    ,knn.execute(train.data,num.folds)
  )
  # Extract results
  err.rates <- sapply(results,'[[','err.rate')
  two.closest <- results[sort.int(err.rates,index.return=TRUE)$ix[1:2]]
  print(paste(
    "The two most accurate classifiers are"
    ,two.closest[[1]]$name
    ,"with error rate"
    ,two.closest[[1]]$err.rate
    ,"and"
    ,two.closest[[2]]$name
    ,"with error rate"
    ,two.closest[[2]]$err.rate
  ))
  # Test on actual test data 
  first.classifier <- two.closest[[1]]$classifier(train.data,test.data)
  second.classifier <- two.closest[[2]]$classifier(train.data,test.data)
  print(paste(
    "On actual test data set accuracies were"
    ,first.classifier$err.rate
    ,"for"
    ,two.closest[[1]]$name
    ,"and"
    ,second.classifier$err.rate
    ,"for"
    ,two.closest[[2]]$name
  ))
  mc.nemar(first.classifier,second.classifier,test.data,1)
}

project <- function(num.folds=2) {
  # Get data
  data <- analyze.data(verbose=TRUE)
  # Analyze it
  compare.classifiers(data$train.data,data$test.data,num.folds)
  # Do the mastery part of the project
  mastery(data)
}

###################
# Mastery project #
###################

mastery <- function(data,k.start=2,k.end=20,k.by=2) {
  ks <- seq(from=k.start,to=k.end,by=k.by)
  bagging.simple <- c()
  bagging.decision <- c()
  adaboost.simple <- c()
  adaboost.decision <- c()
  adaboost.all <- c()
  for(k in ks) {
    bagging.simple <- c(
      bagging.simple
      ,bagging.classify(data$train.data,data$test.data,k,simple.perceptron)$err.rate
    )
    print("Bagging simple")
    bagging.decision <- c(
      bagging.decision
      ,bagging.classify(data$train.data,data$test.data,k,decision.stump)$err.rate
    )
    print("Bagging decision")
    adaboost.simple <- c(
      adaboost.simple
      ,adaboost.classify(data$train.data,data$test.data,k,list(simple.perceptron))$err.rate
    )
    print("Adaboost simple")
    adaboost.decision <- c(
      adaboost.decision
      ,adaboost.classify(data$train.data,data$test.data,k,list(decision.stump))$err.rate
    )
    print("Adaboost decision")
    adaboost.all <- c(
      adaboost.all
      ,adaboost.classify(data$train.data,data$test.data,k)$err.rate
    )
    print("Adaboost double")
    print(paste("Completed iteration for number of layers",k))
  }
  # Plot results
  pdf("bagging_boosting.pdf")
  all.err.rates <- c(bagging.simple,bagging.decision,adaboost.simple,adaboost.decision,adaboost.all)
  ylim = c(0,1.7*max(all.err.rates))
  plot(
    ks
    ,bagging.simple
    ,ylim=ylim
    ,col='red'
    ,type='b'
    ,ylab='Error rate'
    ,xlab='Number of layers'
    ,pch=1
    ,lty=6
    ,main="Accuracy for different bagging/boosting classifiers"
  )
  lines(
    bagging.decision
    ,x=ks
    ,col='blue'
    ,type='b'
    ,pch=2
    ,lty=2
  )
  lines(
    adaboost.simple
    ,x=ks
    ,col='black'
    ,type='b'
    ,pch=3
    ,lty=3
  )
  lines(
    adaboost.decision
    ,x=ks
    ,col='green'
    ,type='b'
    ,pch=4
    ,lty=4
  )
  lines(
    adaboost.all
    ,x=ks
    ,col='pink'
    ,type='b'
    ,pch=5
    ,lty=5
  )
  legend(
    "topright"
    ,c("Bagging simple perceptron","Bagging decision stump","Adaboost simple perceptron","Adaboost decision stump","Adaboost simple and decision stump")
    ,lty=c(6,2,3,4,5)
    ,lwd=c(1)
    ,col=c("red","blue","black","green","pink")
    ,inset=0.05
  )
  dev.off()
}
