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
resultsCrossTreesLettersComplex <- foreach(i=1:7, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE, 
                  control = rpart.control(cp = i/10^i))
}

#minSplit
resultsCrossTreesLettersMinSplit <- foreach(i=seq(0,50,5), .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE, 
                  control = rpart.control(minsplit =  i + 1))
}

#minBucket
resultsCrossTreesLettersMinBucket <- foreach(i=seq(0,30,3), .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, V1~., "V1", letter.recognition, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="class", predictionType = "class",
                  metric = getConfusionMatrixMetricsClass, collect = collectCrossValidationClass, changeFactor = TRUE,
                  control = rpart.control(minbucket =  i + 1))
}

par(mfrow=c(3,2))
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {resultsCrossTreesLetters[[i]]$accuracy}, xlab = "Number of trees", ylab = "Accuracy")
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {resultsCrossTreesLettersNoBootstrap[[i]]$accuracy}, xlab = "Number of trees", ylab = "Accuracy")
plot(type="b",seq(1,15,2), foreach(i=1:8, .combine = c) %do% {resultsCrossTreesLettersAttr[[i]]$accuracy}, xlab = "Number of attributes", ylab = "Accuracy")
plot(type="b",(foreach(i=1:7, .combine = c) %do% {1/10^i}), foreach(i=1:7, .combine = c) %do% {resultsCrossTreesLettersComplex[[i]]$accuracy}, xlab = "Minimal increase in fit for attempting split", ylab = "Accuracy", log = "x")
plot(type="b",seq(0,50,5), foreach(i=1:11, .combine = c) %do% {resultsCrossTreesLettersMinSplit[[i]]$accuracy}, xlab = "Minimal observations for attempting split", ylab = "Accuracy")
plot(type="b",seq(0,30,3), foreach(i=1:11, .combine = c) %do% {resultsCrossTreesLettersMinBucket[[i]]$accuracy}, xlab = "Minimal observations in terminal node", ylab = "Accuracy")

par(mfrow=c(3,2))
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {mean(resultsCrossTreesLetters[[i]]$precision)}, xlab = "Number of trees", ylab = "Precision")
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {mean(resultsCrossTreesLettersNoBootstrap[[i]]$precision)}, xlab = "Number of trees", ylab = "Precision")
plot(type="b",seq(1,15,2), foreach(i=1:8, .combine = c) %do% {mean(resultsCrossTreesLettersAttr[[i]]$precision)}, xlab = "Number of attributes", ylab = "Precision")
plot(type="b",(foreach(i=1:7, .combine = c) %do% {1/10^i}), foreach(i=1:7, .combine = c) %do% {mean(resultsCrossTreesLettersComplex[[i]]$precision)}, xlab = "Minimal increase in fit for attempting split", ylab = "Precision", log = "x")
plot(type="b",seq(0,50,5), foreach(i=1:11, .combine = c) %do% {mean(resultsCrossTreesLettersMinSplit[[i]]$precision)}, xlab = "Minimal observations for attempting split", ylab = "Precision")
plot(type="b",seq(0,30,3), foreach(i=1:11, .combine = c) %do% {mean(resultsCrossTreesLettersMinBucket[[i]]$precision)}, xlab = "Minimal observations in terminal node", ylab = "Precision")

par(mfrow=c(3,2))
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {mean(resultsCrossTreesLetters[[i]]$recall)}, xlab = "Number of trees", ylab = "Recall")
plot(type="b",1:15, foreach(i=1:15, .combine = c) %do% {mean(resultsCrossTreesLettersNoBootstrap[[i]]$recall)}, xlab = "Number of trees", ylab = "Recall")
plot(type="b",seq(1,15,2), foreach(i=1:8, .combine = c) %do% {mean(resultsCrossTreesLettersAttr[[i]]$recall)}, xlab = "Number of attributes", ylab = "Recall")
plot(type="b",(foreach(i=1:7, .combine = c) %do% {1/10^i}), foreach(i=1:7, .combine = c) %do% {mean(resultsCrossTreesLettersComplex[[i]]$recall)}, xlab = "Minimal increase in fit for attempting split", ylab = "Recall", log = "x")
plot(type="b",seq(0,50,5), foreach(i=1:11, .combine = c) %do% {mean(resultsCrossTreesLettersMinSplit[[i]]$recall)}, xlab = "Minimal observations for attempting split", ylab = "Recall")
plot(type="b",seq(0,30,3), foreach(i=1:11, .combine = c) %do% {mean(resultsCrossTreesLettersMinBucket[[i]]$recall)}, xlab = "Minimal observations in terminal node", ylab = "Recall")
