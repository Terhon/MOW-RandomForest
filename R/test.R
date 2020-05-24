library(pryr)
library(rpart)
library(doParallel)
library(caret)
library(dplyr)

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

#tests with validation------------------------
#letters
#tree number
resultsCrossTreesLetters <- foreach(i=1:10, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=floor(sqrt(16)), numberOfTrees=i,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)
}

resultsCrossTreesLettersNoBootstrap <- foreach(i=1:10, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=floor(sqrt(16)), numberOfTrees=i,
                  bootstrap = FALSE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)
}

#attributes
treeNumber <- 6
resultsCrossTreesLettersAttr <- foreach(i=1:6, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=i, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)
}

resultsCrossTreesLettersAttrNoBootstrap <- foreach(i=1:6, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=i, numberOfTrees=treeNumber,
                  bootstrap = FALSE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)
}

#white wine--------------------------------
#trees
resultsCrossTreesWhiteWine <- foreach(i=1:20, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, quality~., "quality", winequality.white, numberOfAttributes=floor(sqrt(12)), numberOfTrees=i,
                  bootstrap = TRUE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE)
}

resultsCrossTreesWhiteWineNoBootstrap <- foreach(i=1:20, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, quality~., "quality", winequality.white, numberOfAttributes=floor(sqrt(12)), numberOfTrees=i,
                  bootstrap = FALSE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE)
}

#attributes
treeNumber <- 4
resultsCrossTreesWhiteWineAttr <- foreach(i=1:11, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, quality~., "quality", winequality.white, numberOfAttributes=i, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE)
}

resultsCrossTreesWhiteWineAttrNoBootstrap <- foreach(i=1:11, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, quality~., "quality", winequality.white, numberOfAttributes=i, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE)
}

par(mfrow=c(2,2))
plot(1:20, resultsCrossTreesWhiteWine, xlab = "Number of trees", ylab = "Root Mean Squared Error")
plot(1:20, resultsCrossTreesWhiteWineNoBootstrap, xlab = "Number of trees", ylab = "Root Mean Squared Error")
plot(1:11, resultsCrossTreesWhiteWineAttr, xlab = "Number of attributes", ylab = "Root Mean Squared Error")
plot(1:11, resultsCrossTreesWhiteWineAttrNoBootstrap, xlab = "Number of attributes", ylab = "Root Mean Squared Error")

#appliances--------------------------
#trees
resultsCrossTreesAppliances <- foreach(i=1:20, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, Appliances~., "Appliances", energydata_complete, numberOfAttributes=floor(sqrt(29)), numberOfTrees=i,
                  bootstrap = TRUE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE)
}

resultsCrossTreesAppliancesNoBootstrap <- foreach(i=1:20, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, Appliances~., "Appliances", energydata_complete, numberOfAttributes=floor(sqrt(29)), numberOfTrees=i,
                  bootstrap = FALSE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE)
}

#attributes
treeNumber <- 4
resultsCrossTreesAppliancesAttr <- foreach(i=1:20, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, Appliances~., "Appliances", energydata_complete, numberOfAttributes=i, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE)
}

resultsCrossTreesAppliancesAttrNoBootstrap <- foreach(i=1:20, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, Appliances~., "Appliances", energydata_complete, numberOfAttributes=i, numberOfTrees=treeNumber,
                  bootstrap = FALSE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE)
}

par(mfrow=c(2,2))
plot(1:20, resultsCrossTreesAppliances, xlab = "Number of trees", ylab = "Root Mean Squared Error")
plot(1:20, resultsCrossTreesAppliancesNoBootstrap, xlab = "Number of trees", ylab = "Root Mean Squared Error")
plot(1:20, resultsCrossTreesAppliancesAttr, xlab = "Number of attributes", ylab = "Root Mean Squared Error")
plot(1:20, resultsCrossTreesAppliancesAttrNoBootstrap, xlab = "Number of attributes", ylab = "Root Mean Squared Error")

