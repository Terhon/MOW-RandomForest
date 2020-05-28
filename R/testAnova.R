library(pryr)
library(rpart)
library(doParallel)
library(caret)
library(dplyr)
library(BBmisc)

registerDoParallel(cores = 6)

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
                  bootstrap = FALSE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE)
}

#complexity
attributeNumber <- 6
resultsCrossTreesWhiteWineComplex <- foreach(i=1:7, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, quality~., "quality", winequality.white, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE, control = rpart.control(cp = i/10^i))
}

resultsCrossTreesWhiteWineComplexNoBootstrap <- foreach(i=1:7, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, quality~., "quality", winequality.white, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = FALSE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE, control = rpart.control(cp = i/10^i))
}

#minsplit
resultsCrossTreesWhiteWineMinSplit <- foreach(i=seq(0,50,5), .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, quality~., "quality", winequality.white, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE, control = rpart.control(minsplit =  i + 1))
}

#minbucket
resultsCrossTreesWhiteWineMinBucket <- foreach(i=seq(0,30,3), .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, quality~., "quality", winequality.white, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE, control = rpart.control(minbucket =  i + 1))
}


par(mfrow=c(3,2))
plot(type = "b", 1:20, resultsCrossTreesWhiteWine, xlab = "Number of trees", ylab = "RMSE")
plot(type = "b", 1:20, resultsCrossTreesWhiteWineNoBootstrap, xlab = "Number of trees without bootstrap", ylab = "RMSE")
plot(type = "b", 1:11, resultsCrossTreesWhiteWineAttr, xlab = "Number of attributes", ylab = "RMSE")
#plot(1:11, resultsCrossTreesWhiteWineAttrNoBootstrap, xlab = "Number of attributes", ylab = "RMSE")
#par(mfrow=c(2,2))
#x <- foreach(i=1:7, .combine = c) %do% {1/10^i}
plot(type = "b", (foreach(i=1:7, .combine = c) %do% {1/10^i}), resultsCrossTreesWhiteWineComplex, xlab = "Minimal increase in fit for attempting split", ylab = "RMSE", log = 'x')
#plot(x[7:1], resultsCrossTreesWhiteWineComplexNoBootstrap, xlab = "Minimal increase in fit for attempting split", ylab = "RMSE", log = 'x')
plot(type = "b", seq(0,50,5), resultsCrossTreesWhiteWineMinSplit, xlab = "Minimal observations for attempting split", ylab = "RMSE")
plot(type = "b", seq(0,30,3), resultsCrossTreesWhiteWineMinBucket, xlab = "Minimal observations in terminal node", ylab = "RMSE")

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

resultsCrossTreesAppliancesAttrNoBootstrap <- foreach(i=1:1, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, Appliances~., "Appliances", energydata_complete, numberOfAttributes=i, numberOfTrees=treeNumber,
                  bootstrap = FALSE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE)
}

#complexity
attributeNumber <- 6
resultsCrossTreesAppliancesComplex <- foreach(i=1:7, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, Appliances~., "Appliances", energydata_complete, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE, control = rpart.control(cp = i/10^i))
}

resultsCrossTreesAppliancesComplexNoBootstrap <- foreach(i=1:7, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, Appliances~., "Appliances", energydata_complete, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = FALSE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE, control = rpart.control(cp = i/10^i))
}

#minsplit
resultsCrossTreesAppliancesMinSplit <- foreach(i=seq(0,50,5), .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, Appliances~., "Appliances", energydata_complete, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE, control = rpart.control(minsplit =  i + 1))
}

#minbucket
resultsCrossTreesAppliancesMinBucket <- foreach(i=seq(0,30,3), .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
  crossValidation(5, Appliances~., "Appliances", energydata_complete, numberOfAttributes=attributeNumber, numberOfTrees=treeNumber,
                  bootstrap = TRUE, method="anova", predictionType = "vector",
                  metric = RMSE, collect = mean, changeFactor = FALSE, control = rpart.control(minbucket =  i + 1))
}

par(mfrow=c(3,2))
plot(type = "b", 1:20, resultsCrossTreesAppliances, xlab = "Number of trees", ylab = "RMSE")
plot(type = "b", 1:20, resultsCrossTreesAppliancesNoBootstrap, xlab = "Number of trees without bootstrap", ylab = "RMSE")
plot(type = "b", 1:20, resultsCrossTreesAppliancesAttr, xlab = "Number of attributes", ylab = "RMSE")
#plot(type = "b", 1:20, resultsCrossTreesAppliancesAttrNoBootstrap, xlab = "Number of attributes", ylab = "RMSE")
plot(type = "b",(foreach(i=1:7, .combine = c) %do% {1/10^i}), resultsCrossTreesAppliancesComplex, xlab = "Minimal increase in fit for attempting split", ylab = "RMSE", log = 'x')
#plot(x[7:1], resultsCrossTreesAppliancesComplexNoBootstrap, xlab = "Complexity", ylab = "RMSE", log = 'x')
plot(type = "b", seq(0,50,5), resultsCrossTreesAppliancesMinSplit, xlab = "Minimal observations for attempting split", ylab = "RMSE")
plot(type = "b", seq(0,30,3), resultsCrossTreesAppliancesMinBucket, xlab = "Minimal observations in terminal node", ylab = "RMSE")

