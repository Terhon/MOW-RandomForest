#' Random Forest algorithm.
#'
#' Creates a random forest.
#'
#' Creates a single random forest trees with random selecting attributes before splitting node and with bootstrapping method.
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
#' forest <- randomForest(y~., data, 2, method=method)
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
