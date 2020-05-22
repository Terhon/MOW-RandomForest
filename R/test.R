test <- function(){
library(pryr)
library(rpart)
library(doParallel)
library(caret)
library(dplyr)

registerDoParallel(cores = 6)

results <- foreach(i=1:15, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  forest[[i]] <- randomForest(V1~V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17,
                       letter.recognition, numberOfTrees=i, bootstrap = TRUE, method="class")
  list(caret::confusionMatrix(letter.recognition[,1], factor(predict(forest[[i]], letter.recognition, "class"),
                                  levels = levels(letter.recognition[,1]))))
}

predict(rpart(V1~V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17, letter.recognition, method = "class"))

resultsNoBootrstrap <- foreach(i=1:15, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  forest[[i]] <- randomForest(V1~V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17,
                         letter.recognition, numberOfTrees=i, bootstrap = FALSE, method="class")
  list(caret::confusionMatrix(letter.recognition[,1], factor(predict(forest[[i]], letter.recognition, "class"),
                                                             levels = levels(letter.recognition[,1]))))
}

resultsAttr <- foreach(i=1:5, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  forest[[i]] <- randomForest(V1~V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17,
                              letter.recognition, numberOfTrees=6, bootstrap = TRUE, method="class", attributesToChooseCount = i)
  list(caret::confusionMatrix(letter.recognition[,1], factor(predict(forest[[i]], letter.recognition, "class"),
                                                             levels = levels(letter.recognition[,1]))))
}

resultsNoBootrstrapAttr <- foreach(i=1:5, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  forest[[i]] <- randomForest(V1~V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17,
                              letter.recognition, numberOfTrees=6, bootstrap = FALSE, method="class", attributesToChooseCount = i)
  list(caret::confusionMatrix(letter.recognition[,1], factor(predict(forest[[i]], letter.recognition, "class"),
                                                             levels = levels(letter.recognition[,1]))))
}

forest <- MOWRandomForest::randomForest(stabf~.,Data_for_UCI_named, numberOfTrees=1, bootstrap = TRUE, method="class")
prediction <- predict(forest, Data_for_UCI_named, "class")

resultsWhiteWine <- foreach(i=1:15, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  forest <- MOWRandomForest::randomForest(quality~.,
                         winequality.white, numberOfTrees=i, bootstrap = TRUE, method="anova")
  prediction <- predict(forest, winequality.white, "vector")
  RMSE(prediction, winequality.white$quality)
}

resultsWhiteWineAttr <- foreach(i=1:10, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  forest <- MOWRandomForest::randomForest(quality~.,
                                          winequality.white, numberOfTrees=5, bootstrap = TRUE, method="anova", attributesToChooseCount = i)
  prediction <- predict(forest, winequality.white, "vector")
  RMSE(prediction, winequality.white$quality)
}

resultsWhiteWineNoBootstrap <- foreach(i=1:15, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  forest <- MOWRandomForest::randomForest(quality~.,
                         winequality_white, numberOfTrees=i, bootstrap = FALSE, method="class")
  list(caret::confusionMatrix(winequality_white$quality, factor(predict(forest, winequality_white, "class"),
                                                             levels = levels(winequality_white$quality))))
}

resEnergy <- foreach(i=1:15, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  forest <- MOWRandomForest::randomForest(Appliances~.,
                                          energydata_complete, numberOfTrees=i, bootstrap = TRUE, method="anova")
  prediction <- predict(forest, energydata_complete, "vector")
  RMSE(prediction, energydata_complete$Appliances)
}

resEnergyAttr <- foreach(i=1:10, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  forest <- MOWRandomForest::randomForest(quality~.,
                                          winequality.white, numberOfTrees=5, bootstrap = TRUE, method="anova", attributesToChooseCount = i)
  prediction <- predict(forest, winequality.white, "vector")
  RMSE(prediction, winequality.white$quality)
}

acc <- list()
for(i in 1:15)acc<-append(acc, results[[i]]$overall[['Accuracy']])
for(i in 1:15)acc<-append(acc, resultsNoBootrstrap[[i]]$overall[['Accuracy']])
plot(1:30, acc)

accWine <- list()
for(i in 1:15)accWine<-append(accWine, resultsWhiteWine[[i]]$overall[['Accuracy']])
for(i in 1:15)accWine<-append(accWine, resultsNoBootrstrap[[i]]$overall[['Accuracy']])
plot(1:30, accWine)


accAttr <- list()
for(i in 1:5)accAttr<-append(accAttr, resultsAttr[[i]]$overall[['Accuracy']])
for(i in 1:5)accAttr<-append(accAttr, resultsNoBootrstrapAttr[[i]]$overall[['Accuracy']])
plot(1:5, accAttr)


forest <- MOWRandomForest::randomForest(stabf~.,Data_for_UCI_named, numberOfTrees=1, bootstrap = TRUE, method="anova")
prediction <- predict(forest, Data_for_UCI_named, "vector")


aaa <- apply(prediction, 1, function (pred) {
  uniq <- unique(pred)
  uniq[which.max(tabulate(match(pred, uniq)))]
})

crossValidation(5, quality~., 'quality', winequality.white, floor(sqrt(11)), 1, TRUE)

getConfusionMatrixMetricsClass <- function(prediction, real){
  real <- factor(real, levels=levels(real))
  prediction <- factor(prediction, levels=levels(prediction))

  confMatrix <- caret::confusionMatrix(prediction, real, mode="prec_recall")
  overall <- confMatrix$overall
  classInfo <- confMatrix$byClass

  accuracyMetric <- overall[['Accuracy']]
  precisionMetric <- classInfo[,'Precision']
  recallMetric <- classInfo[,'Recall']

  list(list(accuracy = accuracyMetric, precision = precisionMetric, recall = recallMetric))
}

collectCrossValidationClass <- function(crossMetricsList){
  accuracy <- crossMetricsList[[1]]$accuracy
  precision <- crossMetricsList[[1]]$precision
  recall <- crossMetricsList[[1]]$recall

  accuracy[is.na(accuracy)] <- 0
  precision[is.na(precision)] <- 1
  recall[is.na(recall)] <- 1

  for (i in 2:length(crossMetricsList)){
    metrics <- crossMetricsList[[i]]

    metrics$accuracy[is.na(metrics$accuracy)] <- 0
    metrics$precision[is.na(metrics$precision)] <- 1
    metrics$recall[is.na(metrics$recall)] <- 1

    accuracy <- accuracy + metrics$accuracy
    precision <- precision + metrics$precision
    recall <- recall + metrics$recall
  }

  n <- length(crossMetricsList)

  accuracy <- accuracy / n
  precision <- precision / n
  recall <- recall / n
  list(list(accuracy = accuracy, precision = precision, recall = recall))
}

resultsCrossTreesLetters <- foreach(i=1:15, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=floor(sqrt(16)), numberOfTrees=i,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)
}

}
