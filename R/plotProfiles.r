classification.plot <- function(classifications,save.path) {
  pdf(paste(save.path,"classification_outcomes.pdf",sep=""))
  lapply(
    classifications
    ,function(classification) {
      main <- paste("Accuracy when classifying between",classification$class1,"and\n",classification$class2," ")
      if(classification$num.classes==3) {
        main <- paste(main,"and",classification$class3,sep=" ")
      }
      main <- paste(main,"using",classification$type,sep=" ")
      # Plot comparison of all classifiers and chance
      best.k <- max(classification$knn.euclidean$accuracies)
      average.svm <- mean(classification$svm)
      average.qda <- mean(classification$qda)
      chance <- 1/classification$num.classes
      plot(type="l",col="white",x=1:2,y=rep(0,2),ylim=c(0,1),ylab="Accuracy",xlab="",main=paste(main,""),cex.main=0.9,xaxt='n')
      abline(h=best.k,col="red",lty=4)
      abline(h=average.qda,col="blue",lty=3)
      abline(h=average.svm,col="green",lty=2)
      abline(h=chance,col="black",lty=1)
      legend(
        "bottomleft"
        ,c(paste("knn with accuracy",round(best.k,2)),paste("qda with accuracy",round(average.qda,2)),paste("svm with accuracy",round(average.svm,2)),"chance")
        ,lty=c(4,3,2,1)
        ,col=c("red","blue","green","black")
        ,inset=0.05
      )
      # Plot knn scores depending on different ks
      plot(type="l",col="blue",x=classification$knn.euclidean$ks,y=classification$knn.euclidean$accuracies
           ,xlab="Number of neighbors",ylab="Accuracy",main=paste(main,", for different number of neighbors",sep=""),cex.main=0.9)
    }
  )
  dev.off()
}

all.plot <- function(profiles,path) {
  # Extract test and pregnant profiles
  test <- lapply(profiles$test,'[[','analysis')
  pregnant <- lapply(profiles$pregnant,'[[','analysis')
  taxis <- profiles$taxis
  # Start basic plot for all different profiles
  basic.plot(
    Filter(function(x) length(x),lapply(test,'[[','before'))
    ,profiles$summary$test$before
    ,taxis
    ,'test_before'
    ,path
  )
  basic.plot(
    Filter(function(x) length(x),lapply(pregnant,'[[','before'))
    ,profiles$summary$pregnant$before
    ,taxis
    ,'pregnant_before'
    ,path
  )
  basic.plot(
    Filter(function(x) length(x),lapply(test,'[[','during'))
    ,profiles$summary$test$during
    ,taxis
    ,'test_during'
    ,path
  )
  basic.plot(
    Filter(function(x) length(x),lapply(pregnant,'[[','during'))
    ,profiles$summary$pregnant$during
    ,taxis
    ,'pregnant_during'
    ,path
  )
}

basic.plot <- function(analysis,summary,taxis,type,path) {
  summary.plot(summary,taxis,type,path)
  histogram.plot(analysis,taxis,type,path)
  acf.plot(analysis,type,path)
  pdf.plot(analysis,taxis,type,path)
}

summary.plot <- function(summary,taxis,type,path) {
  pdf(paste(path,type,"_summary.pdf",sep=""))
  plot(
    x=taxis
    ,y=summary$pdf$mean
    ,xlab="Time in days"
    ,ylab="Probability of tweet"
    ,type="l"
    ,main="Mean pdf"
  )
  arrows(
    taxis,summary$pdf$mean+summary$pdf$sde
    ,taxis,summary$pdf$mean-summary$pdf$sde
    ,code=1,length=0,lwd=0.5,angle=90,col='blue'
  )
  plot(
    x=(1:length(summary$acf$mean))
    ,y=summary$acf$mean
    ,xlab="Time in days"
    ,ylab="ACF"
    ,type="l"
    ,main="Mean ACF"
  )
  arrows(
    x0=1:length(summary$acf$mean),y1=summary$acf$mean+summary$acf$sde
    ,y0=summary$acf$mean-summary$acf$sde
    ,code=1,length=0,angle=90,col='blue',lwd=2
  )
  dev.off()
}

acf.plot <- function(analysis,type,path) {
  pdf(paste(path,type,'_acf.pdf',sep=''))
  for(a in analysis) {
    plot(y=a$acf,x=1:length(a$acf),main=paste("ACF of user",a$id),xlab="lag",ylab="acf")
  }
  dev.off()
}

pdf.plot <- function(analysis,taxis,type,path) {
  pdf(paste(path,type,'_pdf.pdf',sep=''))
  for(a in analysis) {
    plot(
      x=taxis
      ,y=a$pdf
      ,xlab="Time before date of birth in days"
      ,ylab="Probability distribution function of tweet"
      ,type='l'
      ,main=paste("Probability of tweet event for user",a$id)
    )
  }
  dev.off()
}

histogram.plot <- function(analysis,taxis,type,path) {
  pdf(paste(path,type,'_histograms.pdf',sep=''))
  for(a in analysis) {
    plot(
      x=taxis
      ,y=a$histogram
      ,xlab="Time before date of birth in days"
      ,ylab="Number of tweets"
      ,type="h"
      ,main=paste("Tweet count histogram for user",a$id)
    )
  }
  dev.off()
}

final.plot <- function(analyzed.males,analyzed.females,path) {
  msummary <- analyzed.males$summary
  fsummary <- analyzed.females$summary
  taxis <- analyzed.males$taxis
  comparison.plot(
    taxis
    ,msummary$all$before
    ,fsummary$all$before
    ,path,"mabfab","all males","all females"
  )
  comparison.plot(
    taxis
    ,msummary$all$during
    ,fsummary$all$during
    ,path,"madfad","all males","all females"
  )
  comparison.plot(
    taxis
    ,msummary$pregnant$during
    ,fsummary$pregnant$during
    ,path,"mpdfpd","males pregnant","females pregnant"
  )
  comparison.plot(
    taxis
    ,msummary$pregnant$during
    ,fsummary$pregnant$during
    ,path,"mpdfpd","males pregnant","females pregnant"
  )
  comparison.plot(
    taxis
    ,msummary$all$during
    ,fsummary$pregnant$during
    ,path,"madfpd","all males","females pregnant"
  )
  comparison.plot(
    taxis
    ,fsummary$pregnant$before
    ,fsummary$pregnant$during
    ,path,"fpbfpd","females pregnant","females pregnant"
  )
  comparison.plot(
    taxis
    ,msummary$pregnant$before
    ,msummary$pregnant$during
    ,path,"mpbmpd","males pregnant","males pregnant"
  )
  comparison.plot(
    taxis
    ,fsummary$test$during
    ,fsummary$pregnant$during
    ,path,"ftdfpd","females test","females pregnant"
  )
  comparison.plot(
    taxis
    ,msummary$test$during
    ,msummary$pregnant$during
    ,path,"mtdmpd","males test","males pregnant"
  )
  comparison.plot(
    taxis
    ,msummary$all$during
    ,fsummary$pregnant$during
    ,path,"madfpd","rest of observations","females pregnant"
  )
}

comparison.plot <- function(taxis,summary.1,summary.2,path,type,type.1,type.2) {
  pdf(paste(path,type,"_comparison.pdf",sep=""))
  # Pdf comparison
  plot(
    x=taxis
    ,y=summary.1$pdf$mean
    ,xlab="Time in days"
    ,ylab="Probability of tweet"
    ,type="l"
    ,ylim=c(0,max(summary.2$pdf$mean+summary.2$pdf$mean,summary.1$pdf$mean+summary.1$pdf$mean))
    ,col="black"
    ,main=paste("Mean and sde of pdf for\n",type.1,"and",type.2)
  )
  lines(
    x=taxis
    ,y=summary.2$pdf$mean
    ,col="red"
  )
  arrows(
    x0=taxis,y0=summary.2$pdf$mean-summary.2$pdf$sde
    ,y1=summary.2$pdf$mean+summary.2$pdf$sde
    ,code=1,length=0,angle=90,col='pink'
  )
  arrows(
    x0=taxis,y0=summary.1$pdf$mean-summary.1$pdf$sde
    ,y1=summary.1$pdf$mean+summary.1$pdf$sde
    ,code=1,length=0,angle=90,col='blue'
  )
  legend(
    "topleft"
    ,legend=c(type.1,paste(type.1,"sde"),type.2,paste(type.2,"sde"))
    ,col=c("black","blue","red","pink")
    ,lty=c(1,1,1,1)
    ,lwd=c(1,5,1,5)
    ,inset=0.05
  )
  # Acf comparison
  plot(
    x=1:length(summary.1$acf$mean)
    ,y=summary.1$acf$mean
    ,xlab="Time in days"
    ,ylab="Correlation"
    ,type="l"
    ,ylim=c(
      min(summary.2$acf$mean-summary.2$acf$sde,summary.1$acf$mean-summary.1$acf$sde)
      ,max(summary.2$acf$mean+summary.2$acf$sde,summary.1$acf$mean+summary.1$acf$sde)
    )
    ,col="black"
    ,main=paste("Mean and sde of acf for\n",type.1,"and",type.2)
  )
  lines(
    x=1:length(summary.1$acf$mean)
    ,y=summary.2$acf$mean
    ,col="red"
  )
  arrows(
    x0=1:length(summary.1$acf$mean),y0=summary.1$acf$mean-summary.1$acf$sde
    ,y1=summary.1$acf$mean+summary.1$acf$sde
    ,code=1,length=0,angle=90,col='blue'
  )
  arrows(
    x0=(1:length(summary.2$acf$mean))+0.5,y0=summary.2$acf$mean-summary.2$acf$sde
    ,y1=summary.2$acf$mean+summary.2$acf$sde
    ,code=1,length=0,angle=90,col='pink'
  )
  legend(
    "topright"
    ,legend=c(type.1,paste(type.1,"sde"),type.2,paste(type.2,"sde"))
    ,col=c("black","blue","red","pink")
    ,lty=c(1,1,1,1)
    ,lwd=c(1,5,1,5)
    ,inset=0.05
  )
  dev.off()
}
