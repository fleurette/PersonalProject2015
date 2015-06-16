library(MASS)

qda.execute <- function(problem) {
  num.folds <- nrow(problem$feats)
  num.folds <- 10
  fold.idx <- get.fold.idx(problem,num.folds)
  return(sapply(
    1:num.folds
    ,function(idx) {
      data <- get.fold.data(problem,fold.idx[[idx]])
      models <- qda.modelize(data$train,problem$num.classes)
      scores <- rbind(sapply(models,function(model) apply(data$test$feats,1,qda.discriminant,model)))
      pred.classes <- apply(scores,1,which.max)-1
      accuracy <- mean(data$test$class==pred.classes)
      return(accuracy)
    }
  ))
}

qda.modelize <- function(data,num.classes) {
  num.observations <- length(data$class)
  return(lapply(
    0:(num.classes-1)
    ,function(class.label) {
      observations <- data$feats[data$class==class.label,]
      if(length(observations)==0) {
        # Throw an error
      }
      covar <- cov(observations,use="complete")
      return(list(
        covar=covar
        ,prior=nrow(observations)/num.observations
        ,covar.inv=ginv(covar)
        ,covar.norm=norm(covar)
        ,mean=colMeans(observations,na.rm=TRUE)
      ))
    }
  ))
}

qda.discriminant <- function(observation,model) {
  missing.feats <- which(is.na(observation))
  if(any(missing.feats)) {
    centered.observation <- observation[-missing.feats]-model$mean[-missing.feats]
    adjusted.cov <- model$covar.inv[-missing.feats,-missing.feats]
  } else {
    centered.observation <- observation-model$mean
    adjusted.cov <- model$covar.inv
  }
  term1 <- -0.5*t(centered.observation)%*%adjusted.cov%*%(centered.observation)
  term2 <- -0.5*length(observation)*log(2*pi)
  term3 <- -0.5*log(model$covar.norm)+log(model$prior)

  return(as.numeric(term1+term2+term3))
}

get.fold.idx <- function(data,num.folds) {
  num.obs <- length(data$class)
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
  return(list(
    train=list(
      feats=data$feats[-fold.idx,]
      ,class=data$class[-fold.idx]
    )
    ,test=list(
      class=data$class[fold.idx]
      ,feats=as.matrix(data$feats[fold.idx,])
    )
  ))
}
