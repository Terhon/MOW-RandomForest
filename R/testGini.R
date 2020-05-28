testGini <- function(){
library(pryr)
library(rpart)
library(doParallel)
library(caret)
library(dplyr)
library(BBmisc)
library(randomForest)

registerDoParallel(cores = 6)

#letters
#tree number
resultsCrossTreesLetters <- foreach(i=1:15, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=floor(sqrt(16)), numberOfTrees=i,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)
}

confusionMatrix(letter.recognition$V1,
  factor(apply(predict(rpart(V1~., letter.recognition, method = "class"), letter.recognition), 1, function (pred) {
    uniq <- unique(pred)
    uniq[which.max(tabulate(match(pred, uniq)))]
  }), levels = levels(letter.recognition$V1)))

resultsCrossTreesLettersNoBootstrap <- foreach(i=1:15, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=floor(sqrt(16)), numberOfTrees=i,
                  bootstrap = FALSE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)
}

#attributes
treeNumber <- 6
resultsCrossTreesLettersAttr <- foreach(i=seq(1,15,2), .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=i, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)
}

#complexity
attributeNumber <- 7
cpX <- 7:-1:1
cpX <- 10^-cpX

resultsCrossTreesLettersComplex <- foreach(i=cpX, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE,
                  control = rpart.control(cp = i))
}

#minSplit
resultsCrossTreesLettersMinSplit <- foreach(i=seq(0,50,5), .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE,
                  control = rpart.control(minsplit = i))
}

#minBucket
resultsCrossTreesLettersMinBucket <- foreach(i=seq(0,30,3), .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE,
                  control = rpart.control(minbucket =  i))
}

par(mfrow=c(3,2))
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {resultsCrossTreesLetters[[i]]$accuracy}, xlab = "Number of trees", ylab = "Accuracy")
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {resultsCrossTreesLettersNoBootstrap[[i]]$accuracy}, xlab = "Number of trees", ylab = "Accuracy")
plot(type="b",seq(1,15,2), foreach(i=1:8, .combine = c) %do% {resultsCrossTreesLettersAttr[[i]]$accuracy}, xlab = "Number of attributes", ylab = "Accuracy")
plot(type="b",cpX, foreach(i=1:7, .combine = c) %do% {resultsCrossTreesLettersComplex[[i]]$accuracy}, xlab = "Minimal increase in fit for attempting split", ylab = "Accuracy", log = "x")
plot(type="b",seq(0,50,5), foreach(i=1:11, .combine = c) %do% {resultsCrossTreesLettersMinSplit[[i]]$accuracy}, xlab = "Minimal observations for attempting split", ylab = "Accuracy")
plot(type="b",seq(0,30,3), foreach(i=1:11, .combine = c) %do% {resultsCrossTreesLettersMinBucket[[i]]$accuracy}, xlab = "Minimal observations in terminal node", ylab = "Accuracy")

par(mfrow=c(3,2))
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {mean(resultsCrossTreesLetters[[i]]$precision)}, xlab = "Number of trees", ylab = "Precision")
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {mean(resultsCrossTreesLettersNoBootstrap[[i]]$precision)}, xlab = "Number of trees", ylab = "Precision")
plot(type="b",seq(1,15,2), foreach(i=1:8, .combine = c) %do% {mean(resultsCrossTreesLettersAttr[[i]]$precision)}, xlab = "Number of attributes", ylab = "Precision")
plot(type="b",cpX, foreach(i=1:7, .combine = c) %do% {mean(resultsCrossTreesLettersComplex[[i]]$precision)}, xlab = "Minimal increase in fit for attempting split", ylab = "Precision", log = "x")
plot(type="b",seq(0,50,5), foreach(i=1:11, .combine = c) %do% {mean(resultsCrossTreesLettersMinSplit[[i]]$precision)}, xlab = "Minimal observations for attempting split", ylab = "Precision")
plot(type="b",seq(0,30,3), foreach(i=1:11, .combine = c) %do% {mean(resultsCrossTreesLettersMinBucket[[i]]$precision)}, xlab = "Minimal observations in terminal node", ylab = "Precision")

par(mfrow=c(3,2))
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {mean(resultsCrossTreesLetters[[i]]$recall)}, xlab = "Number of trees", ylab = "Recall")
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {mean(resultsCrossTreesLettersNoBootstrap[[i]]$recall)}, xlab = "Number of trees", ylab = "Recall")
plot(type="b",seq(1,15,2), foreach(i=1:8, .combine = c) %do% {mean(resultsCrossTreesLettersAttr[[i]]$recall)}, xlab = "Number of attributes", ylab = "Recall")
plot(type="b", cpX, foreach(i=1:7, .combine = c) %do% {mean(resultsCrossTreesLettersComplex[[i]]$recall)}, xlab = "Minimal increase in fit for attempting split", ylab = "Recall", log = "x")
plot(type="b",seq(0,50,5), foreach(i=1:11, .combine = c) %do% {mean(resultsCrossTreesLettersMinSplit[[i]]$recall)}, xlab = "Minimal observations for attempting split", ylab = "Recall")
plot(type="b",seq(0,30,3), foreach(i=1:11, .combine = c) %do% {mean(resultsCrossTreesLettersMinBucket[[i]]$recall)}, xlab = "Minimal observations in terminal node", ylab = "Recall")

treeNumber <- 6
attributeNumber <- 7
control = rpart.control(minsplit = 50, minbucket = 30, cp=10^-7)
resultsCrossTreesLettersBootstrapRpart <- crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                               bootstrap = TRUE, control = control, method="class", predictionType = "class",
                               metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)

resultsCrossTreesLettersBootstrapRpartSingle <- crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=attributeNumber, numberOfTrees=1,
                               bootstrap = TRUE, control = control, method="class", predictionType = "class",
                               metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)

resultsCrossTreesLettersRpartSingleNoAttrBootstrap <- crossValidation(5, V1~., "V1", letter.recognition,  numberOfAttributes=11, numberOfTrees=1,
                               bootstrap = TRUE, control = control, method="class", predictionType = "class",
                               metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)

resultsCrossRpartLetters <- crossValidation(5, V1~., "V1", letter.recognition,
                               control = control, method="class", predictionType = "class",
                               metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = FALSE, predictor = predictorRpart)

resultsCrossRandomForestLettersBootstrapSingle <- crossValidation(5, V1~., "V1", letter.recognition,  numberOfTrees=1,
                               bootstrap = TRUE, control = control, method="class", predictionType = "class",
                               metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = FALSE, predictor = predictorRandomForest)

resultsCrossRandomForestLettersBootstrap <- crossValidation(5, V1~., "V1", letter.recognition, numberOfTrees=treeNumber,
                               bootstrap = TRUE, control = control, method="class", predictionType = "class",
                               metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = FALSE, predictor = predictorRandomForest)


#Electrical grid
ElectricalGrid <- ElectricalGrid[, -which(names(ElectricalGrid) %in% c("p1", "stab"))]

#tree number
resultsCrossTreesElectricalGrid <- foreach(i=1:20, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, stabf~., "stabf", ElectricalGrid, numberOfAttributes=floor(sqrt(11)), numberOfTrees=i,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)
}

resultsCrossTreesElectricalGridNoBootstrap <- foreach(i=1:20, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, stabf~., "stabf", ElectricalGrid, numberOfAttributes=floor(sqrt(11)), numberOfTrees=i,
                  bootstrap = FALSE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)
}

#attributes
treeNumber <- 18

resultsCrossTreesElectricalGridAttr <- foreach(i=1:11, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, stabf~., "stabf", ElectricalGrid, numberOfAttributes=i, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)
}

#complexity
attributeNumber <- 5
resultsCrossTreesElectricalGridComplex <- foreach(i=cpX, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, stabf~., "stabf", letter.recognition, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE,
                  control = rpart.control(cp = i))
}

#minSplit
resultsCrossTreesElectricalGridMinSplit <- foreach(i=seq(0,50,5), .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, stabf~., "stabf", letter.recognition, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE,
                  control = rpart.control(minsplit =  i + 1))
}

#minBucket
resultsCrossTreesElectricalGridMinBucket <- foreach(i=seq(0,30,3), .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, stabf~., "stabf", letter.recognition, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE,
                  control = rpart.control(minbucket =  i + 1))
}


par(mfrow=c(3,2))
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {resultsCrossTreesElectricalGrid[[i]]$accuracy}, xlab = "Number of trees", ylab = "Accuracy")
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {resultsCrossTreesElectricalGridNoBootstrap[[i]]$accuracy}, xlab = "Number of trees", ylab = "Accuracy")
plot(type="b",seq(1,15,2), foreach(i=1:8, .combine = c) %do% {resultsCrossTreesElectricalGridAttr[[i]]$accuracy}, xlab = "Number of attributes", ylab = "Accuracy")
plot(type="b",cpX, foreach(i=1:7, .combine = c) %do% {resultsCrossTreesElectricalGridComplex[[i]]$accuracy}, xlab = "Minimal increase in fit for attempting split", ylab = "Accuracy", log = "x")
plot(type="b",seq(0,50,5), foreach(i=1:11, .combine = c) %do% {resultsCrossTreesElectricalGridMinSplit[[i]]$accuracy}, xlab = "Minimal observations for attempting split", ylab = "Accuracy")
plot(type="b",seq(0,30,3), foreach(i=1:11, .combine = c) %do% {resultsCrossTreesElectricalGridMinBucket[[i]]$accuracy}, xlab = "Minimal observations in terminal node", ylab = "Accuracy")

par(mfrow=c(3,2))
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {mean(resultsCrossTreesElectricalGrid[[i]]$precision)}, xlab = "Number of trees", ylab = "Precision")
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {mean(resultsCrossTreesElectricalGridNoBootstrap[[i]]$precision)}, xlab = "Number of trees", ylab = "Precision")
plot(type="b",seq(1,15,2), foreach(i=1:8, .combine = c) %do% {mean(resultsCrossTreesElectricalGridAttr[[i]]$precision)}, xlab = "Number of attributes", ylab = "Precision")
plot(type="b",cpX, foreach(i=1:7, .combine = c) %do% {mean(resultsCrossTreesElectricalGridComplex[[i]]$precision)}, xlab = "Minimal increase in fit for attempting split", ylab = "Precision", log = "x")
plot(type="b",seq(0,50,5), foreach(i=1:11, .combine = c) %do% {mean(resultsCrossTreesElectricalGridMinSplit[[i]]$precision)}, xlab = "Minimal observations for attempting split", ylab = "Precision")
plot(type="b",seq(0,30,3), foreach(i=1:11, .combine = c) %do% {mean(resultsCrossTreesElectricalGridMinBucket[[i]]$precision)}, xlab = "Minimal observations in terminal node", ylab = "Precision")

par(mfrow=c(3,2))
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {mean(resultsCrossTreesElectricalGrid[[i]]$recall)}, xlab = "Number of trees", ylab = "Recall")
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {mean(resultsCrossTreesElectricalGridNoBootstrap[[i]]$recall)}, xlab = "Number of trees", ylab = "Recall")
plot(type="b",seq(1,15,2), foreach(i=1:8, .combine = c) %do% {mean(resultsCrossTreesElectricalGridAttr[[i]]$recall)}, xlab = "Number of attributes", ylab = "Recall")
plot(type="b", cpX, foreach(i=1:7, .combine = c) %do% {mean(resultsCrossTreesElectricalGridComplex[[i]]$recall)}, xlab = "Minimal increase in fit for attempting split", ylab = "Recall", log = "x")
plot(type="b",seq(0,50,5), foreach(i=1:11, .combine = c) %do% {mean(resultsCrossTreesElectricalGridMinSplit[[i]]$recall)}, xlab = "Minimal observations for attempting split", ylab = "Recall")
plot(type="b",seq(0,30,3), foreach(i=1:11, .combine = c) %do% {mean(resultsCrossTreesElectricalGridMinBucket[[i]]$recall)}, xlab = "Minimal observations in terminal node", ylab = "Recall")

treeNumber <- 18
attributeNumber <- 5
control = rpart.control(minsplit=0, minbucket=0)
resultsCrossTreesElectricalGridBootstrapRpart <- crossValidation(5, stabf~., "stabf", ElectricalGrid, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, control = control, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)

resultsCrossTreesElectricalGridBootstrapRpartSingle <- crossValidation(5, stabf~., "stabf", ElectricalGrid, numberOfAttributes=attributeNumber, numberOfTrees=1,
                  bootstrap = TRUE, control = control, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)

resultsCrossTreesElectricalGridRpartSingleNoAttrBootstrap <- crossValidation(5, stabf~., "stabf", ElectricalGrid, numberOfAttributes=11, numberOfTrees=1,
                  bootstrap = TRUE, control = control, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)

resultsCrossRpartElectricalGridBootstrap <- crossValidation(5, stabf~., "stabf", ElectricalGrid,
                  control = control, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = FALSE, predictor = predictorRpart)

resultsCrossRandomForestElectricalGridBootstrapSingle <- crossValidation(5, stabf~., "stabf", ElectricalGrid, numberOfTrees=1,
                  bootstrap = TRUE, control = control, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = FALSE, predictor = predictorRandomForest)

resultsCrossRandomForestElectricalGridBootstrap <- crossValidation(5, stabf~., "stabf", ElectricalGrid, numberOfTrees=treeNumber,
                  bootstrap = TRUE, control = control, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = FALSE, predictor = predictorRandomForest)

resultsCrossTreesElectricalGridBootstrapFull <- crossValidation(5, stabf~., "stabf", ElectricalGrid, numberOfAttributes=attributeNumber, numberOfTrees=500,
                  bootstrap = TRUE, control = control, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE)

resultsCrossRandomForestElectricalGridBootstrapFull <- crossValidation(5, stabf~., "stabf", ElectricalGrid, numberOfTrees=500,
                  bootstrap = TRUE, control = control, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = FALSE, predictor = predictorRandomForest)
}
