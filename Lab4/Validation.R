library(ggplot2)
library(class)
library(MASS)
library(hmeasure)

source("ValidationLib.R")

##Practice (http://cran.r-project.org/web/packages/hmeasure/vignettes/hmeasure.pdf) 
data(Pima.te)
n <- dim(Pima.te)[1]; ntrain <- floor(2*n/3)
ntest <- n-ntrain
pima.train <- Pima.te[seq(1,n,3),]
pima.test <- Pima.te[-seq(1,n,3),]
true.class<-pima.test[,8]
str(true.class)

#LDA
pima.lda <- lda(formula=type~., data=pima.train)
out.lda <- predict(pima.lda,newdata=pima.test)
class.lda <- out.lda$class;

#Check output: posterior probability 
out.lda$posterior[1:5,]
scores.lda <- out.lda$posterior[,2]
scores.lda[1:3]

#default threshold of 0.5
all((scores.lda > 0.5)== (class.lda=="Yes"))

#KNN
class.knn <- knn(train=pima.train[,-8], test=pima.test[,-8],
                    cl=pima.train$type, k=9, prob=TRUE, use.all=TRUE)
scores.knn <- attr(class.knn,"prob")
scores.knn[class.knn=="No"] <- 1-scores.knn[class.knn=="No"]
scores <- data.frame(LDA=scores.lda,kNN=scores.knn)
results <- HMeasure(true.class,scores)
class(results)
plotROC(results)

##End of practice


