#' Cross validation function
#'
#' @examples
#' crossValidation(5, quality~., 'quality', winequality.white, 30, TRUE)
#'
#' @export
crossValidation <- function (k, formula, attr, data, numberOfTrees, bootstrap) {
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
    forest <- MOWRandomForest::randomForest(formula, trainSet, numberOfTrees = numberOfTrees, bootstrap = bootstrap, method="anova")
    prediction <- predict(forest, subsets[[i]], "vector")
    RMSE(prediction, subsets[[i]][[attr]])
  }
  mean(results)
}
