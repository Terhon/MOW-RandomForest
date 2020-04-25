#' Random Forest train algorithm.
#'
#' Creates a random forest and trains it.
#'
#' Creates a random forest trees with random selecting attributes before splitting node and with bootstrapping method.
#' To build trees uses rpart package.
#'
#' @param formula Random forest formula
#' @param data Data table
#' @param attributesToChooseCount Number of attributes to choose while splitting node - default sqrt(n) where n number of attributes.
#' @param bootstrap Use bootstrap method - default=FALSE.
#' @param numberOfTrees Number of trees in random forest.
#' @param weights Tree examples weights
#' @param subset Expression saying that only a subset of the rows of the data should be used in the fit.
#' @param na.action The default action deletes all observations for which y is missing, but keeps those in which one or more predictors are missing.
#' @param method User split methods.
#' @param model Keep a copy of the model frame in the result.
#' @param x Keep a copy of the x matrix in the result.
#' @param y Keep a copy of the dependent variable in the result.
#' @param parms Optional parameters for the splitting function.
#' @param control A list of options that control details of the rpart algorithm.
#' @param cost A vector of non-negative costs.
#'
#' @return Random forest construction containg trained trees
#'
#' @examples
#' method <- list(eval=evalNode, split=splitNode, init=initTree)
#' data <- data.frame(y = numeric(10), a = numeric(10), b = numeric(10))
#' forest <- randomForest(y~., data, 2, numberOfTrees=3, method=method)
#'
#' @export
randomForest <- function (formula, data, attributesToChooseCount=sqrt(ncol(data)-1), bootstrap=FALSE, numberOfTrees=1, method, ...) {
  treeArgs <- list(...)
  treeArgs$formula <- formula
  treeArgs$data <- data
  treeArgs$attributesToChooseCount <- attributesToChooseCount
  treeArgs$bootstrap <- bootstrap
  treeArgs$method <- method

  forest <- list()

  for (i in 1:numberOfTrees){
    forest[[i]] <- do.call(tree, treeArgs)
  }

  class(forest) <- "randomForest"
  forest
}

#' Random Forest predict algorithm.
#'
#' Predicts with use of random forest algorithm.
#'
#' Predicts on single tress of random forest and average results.
#' Function is associated with predict method.
#'
#' @param randomForestObject Random forest object
#' @param data Data table on which to predict
#' @param type Type of prediction - "class" or "vector" (regression)
#'
#' @return Random forest prediction results
#'
#' @examples
#' method <- list(eval=evalNode, split=splitNode, init=initTree)
#' data <- data.frame(y = numeric(10), a = numeric(10), b = numeric(10))
#' forest <- randomForest(y~., data, 2, numberOfTrees=3, method=method)
#' prediction <- predict(forest, data)
#'
#' @export
predict.randomForest <- function (randomForestObject, data, type = c("class", "vector"), ...) {
  match.arg(type)

  predictions <- sapply(randomForestObject, function (tree) {
    predict(tree, data)
  })

  if (type == "vector") {
    result <- apply(predictions, 2, mean)
  } else if (type == "class") {
    result <- apply(predictions, 2, function (pred) {
      uniq <- unique(pred)
      uniq[which.max(tabulate(match(pred, uniq)))]
    })
  }

  result
}
