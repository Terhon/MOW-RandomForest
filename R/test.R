test <- function(){
library(pryr)
library(rpart)
library(doParallel)
library(caret)
library(dplyr)
library(BBmisc)

registerDoParallel(cores = 5)

results <- foreach(i=1:15, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  forest[[i]] <- randomForest(V1~V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17,
                       letter.recognition, numberOfTrees=i, bootstrap = TRUE, method="class")
  list(caret::confusionMatrix(letter.recognition[,1], factor(predict(forest[[i]], letter.recognition, "class"),
                                  levels = levels(letter.recognition[,1]))))
}
predict(rpart(V1~., letter.recognition, method = "class"))

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


resEnergy <- foreach(i=3:3, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  forest <- MOWRandomForest::randomForest(Appliances~.,
                                          energydata_complete[2:29], numberOfTrees=3, attributesToChooseCount = 19, bootstrap = TRUE, method="anova")
  prediction <- predict(forest, energydata_complete[2:29], "vector")
  RMSE(prediction, energydata_complete[2:29]$Appliances)
}
energyNormalized <- energydata_complete
energyNormalized$Appliances <- normalize(energyNormalized$Appliances)
RMSE(predict(rpart(Appliances~., energyNormalized, method = "anova")), energyNormalized$Appliances)


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
}
