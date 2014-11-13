library(dplyr)
source("crf_classifier.R")
source("crf_classifier_cv.R")
source("elcm_qda_classifier.R")

##k-fold cross-validation on a binary classifier
#Print the error rate on the screen.
CVfold <- function (dt, fold){
  #Break the data
  dt$fold = cut(1:nrow(dt), breaks=fold, labels=F)
  cl.accuracies = c()
  
  #Compute the accuracies of prediction for each choice of fold
  for(i in 1:fold){
    label.pre <- as.numeric(crf_classifier_cv(dt[dt$fold != i,],dt[dt$fold == i,],5,0.5))
    #numcorrect = sum(label.pre == dt[dt$fold ==i,]$label)
    #cl.accuracies = append(numcorrect / nrow(dt[dt$fold == i,]), cl.accuracies)
  }
}

## Functions to compute ROC and AUC
# Algorithm 1 in Fawcett (2005)
GenerateROC <- function(preds, truth){
  dt <- data.frame(preds = preds, truth = truth)
  dt <- arrange(dt, desc(preds))
  print(dt)
  num.positive <- sum(truth)
  num.negative <- length(truth)-num.positive
  true.positive <- 0
  false.positive <- 0
  prev.score <- -10000
  ROC<-data.frame(fpr=c(),tpr=c())

  for (i in 1:length(preds)){
    if(dt$preds[i]!=prev.score){
      newRow <- data.frame(fpr = false.positive/num.negative, tpr = true.positive/num.positive)
      ROC <- rbind(ROC, newRow)
      prev.score <- dt$preds[i]
    }
    if(dt$truth[i]){
      #cat("Truth label at i :",i, "\n")
      true.positive <- true.positive + 1
    }else{
      false.positive <- false.positive + 1
    }
  }
  newRow <- data.frame(fpr = false.positive/num.negative, tpr = true.positive/num.positive) # This is (1,1)
  ROC <- rbind(ROC, newRow)
  return (ROC)
}

## From classify.R in lab on Oct 28
CalculateTPR <- function(thresh, preds, truth) {
  as.numeric(sum(preds[truth] > thresh) / sum(truth))
}

CalculateFPR <- function(thresh, preds, truth) {
  as.numeric(sum(preds[!truth] > thresh) / sum(!truth))
}

# Show summary statistics, ROC curve, and AUC for model
EvaluateModel <- function(preds, truth) {
  # Print an ROC curve and return the AUC for a fitted model.
  # Args:
  #  fitted.model: A model from a call to glm() that can be used with
  #                predict() on <newdata>
  #  preds: The numeric predictions on the held out data
  #  truth: The true classifications of the held out data (same length as preds)
  
  # Plot an ROC curve
  res <- 1000
  
  ROC <- GenerateROC(preds, truth)
  p <-  ggplot(data=ROC) +
          geom_line(aes(x=fpr, y=tpr), color="blue") +
          geom_abline(aes(slope=1, intercept=0))
  print(p)
  
  # Calculate the AUC
  positive.classifications <-
    sapply(preds[!truth],
           FUN = function(threshold) { CalculateFPR(threshold, preds, !truth) })
  negative.auc <- sum(positive.classifications) / sum(!truth)
  cat("AUC based on negative examples: ", negative.auc, "\n")
  
  return(p)
}



