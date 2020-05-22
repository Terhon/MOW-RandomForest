#' Cross validation prediction test function
#'
#' @examples
#' crossValidation(5, quality~., 'quality', winequality.white, 30, TRUE)
#'
#' @export
crossValidation <- function(k, formula, targetAttr, data, numberOfAttributes, numberOfTrees, bootstrap, method = "anova", predictionType = "vector", metric = RMSE) {
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
    metric(prediction, subsets[[i]][[targetAttr]])
  }
  mean(results)
}
