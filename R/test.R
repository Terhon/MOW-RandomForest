library(pryr)
library(rpart)
library(doParallel)
library(caret)

registerDoParallel(cores = 6)

results <- foreach(i=1:15, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  forest <- randomForest(V1~V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17, 
                       letter.recognition, numberOfTrees=i, bootstrap = TRUE, method="class")
  list(caret::confusionMatrix(letter.recognition[,1], factor(predict(forest, letter.recognition, "class"), 
                                  levels = levels(letter.recognition[,1]))))
}

resultsNoBootrstrap <- foreach(i=1:15, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  forest <- randomForest(V1~V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17, 
                         letter.recognition, numberOfTrees=i, bootstrap = FALSE, method="class")
  list(caret::confusionMatrix(letter.recognition[,1], factor(predict(forest, letter.recognition, "class"), 
                                                             levels = levels(letter.recognition[,1]))))
}

winequality_white$quality <- factor(winequality_white$quality)
resultsWhiteWine <- foreach(i=1:15, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  forest <- MOWRandomForest::randomForest(quality~., 
                         winequality_white, numberOfTrees=i, bootstrap = TRUE, method="class")
  list(caret::confusionMatrix(winequality_white$quality, factor(predict(forest, winequality_white, "class"), 
                                                             levels = levels(winequality_white$quality))))
}

resultsWhiteWineNoBootstrap <- foreach(i=1:15, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  forest <- MOWRandomForest::randomForest(quality~., 
                         winequality_white, numberOfTrees=i, bootstrap = FALSE, method="class")
  list(caret::confusionMatrix(winequality_white$quality, factor(predict(forest, winequality_white, "class"), 
                                                             levels = levels(winequality_white$quality))))
}

acc <- list()
for(i in 1:15)acc<-append(acc, results[[i]]$overall[['Accuracy']])
for(i in 1:15)acc<-append(acc, resultsNoBootrstrap[[i]]$overall[['Accuracy']])
plot(1:30, acc)

accWine <- list()
for(i in 1:15)accWine<-append(accWine, resultsWhiteWine[[i]]$overall[['Accuracy']])
for(i in 1:15)accWine<-append(accWine, resultsNoBootrstrap[[i]]$overall[['Accuracy']])
plot(1:30, accWine)


