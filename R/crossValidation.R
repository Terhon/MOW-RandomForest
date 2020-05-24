#' Cross validation prediction test function
#'
#' @examples
#' crossValidation(5, quality~., 'quality', winequality.white, 30, TRUE)
#'
#' @export
crossValidation <- function(k, formula, targetAttr, data, numberOfAttributes, numberOfTrees, bootstrap, method = "anova", predictionType = "vector", metric = RMSE, collect = mean, changeFactor = FALSE) {
  lev <- levels(data[[targetAttr]])

  size <- nrow(data)/k
  subsets <- list()
  for(i in 1:(k-1)) {
    sample <- sample(seq_len(nrow(data)),size)
    subsets[[i]] <- data[sample,]
    data <- data[-sample,]
  }

  subsets[[k]] <- data

  results <- foreach(i=1:k, .combine = c, .multicombine = TRUE, .packages = c(loadedNamespaces())) %dopar% {
    trainSet <- bind_rows(subsets[(1:k)[-i]])
    forest <- MOWRandomForest::randomForest(formula, trainSet, attributesToChooseCount = numberOfAttributes, numberOfTrees = numberOfTrees, bootstrap = bootstrap, method = method)
    prediction <- predict(forest, subsets[[i]], predictionType)

    if (changeFactor){
      prediction <- unname(prediction)

      prediction <- factor(prediction, levels = lev)
      real <- factor(subsets[[i]][[targetAttr]], levels = lev)
    } else {
      real <- subsets[[i]][[targetAttr]]
    }

    metric(prediction, real)
  }

  collect(results)
}

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
