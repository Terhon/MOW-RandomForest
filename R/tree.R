nodeAttributesChoiceInfo <- createEnvironment()

#' Creates a single random forest tree.
#'
#' Creates a single random forest tree with random selecting attributes before splitting node.
#' To build a tree uses rpart package.
#'
#' @param formula Tree formula
#' @param data Data table
#' @param attributesToChooseCount Number of attributes to choose while splitting node - default sqrt(n) where n number of attributes.
#' @param bootstrap Use bootstrap method - default=FALSE.
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
#' @return Rpart tree build using wrapped function that selects attributes.
#'
#' @examples
#' method <- list(eval=evalNode, split=splitNode, init=initTree)
#' data <- data.frame(y = numeric(10), a = numeric(10), b = numeric(10))
#' t <- tree(y~., data, 2, method=method)
#'
#' @export
tree <- function(formula, data, attributesToChooseCount=floor(sqrt(ncol(data)-1)), bootstrap=FALSE, method, parms=list(), ...) {
  if (bootstrap){
    bootstrapIndexes <- sample(nrow(data), replace=TRUE)
    data <- data[bootstrapIndexes,]
  }

  rpartArgs <- list(...)
  rpartArgs$formula <- formula
  rpartArgs$data <- data

  allAttributesCount <- ncol(data) - 1

  nodeAttributesChoice.init(nodeAttributesChoiceInfo, attributesToChooseCount, allAttributesCount)

  evalFunction <- partial(treeEvalNode, method$eval)
  splitFunction <- partial(treeSplitNode, method$split)
  wrappedMethods <- list(eval=evalFunction, split=splitFunction, init=method$init)

  rpartArgs$method <- wrappedMethods
  rpartArgs$parms <- parms
  do.call(rpart, rpartArgs)
}

#' Rpart eval wrapper function for selecting atributes in tree.
#'
#' Generates indexes of attributes that has to be selected while splitting tree.
#'
#' @param evalMethod User eval method - it will be executed
#' @param y Destination attribute to predict
#' @param wt Examples weights in tree
#' @param parms Tree user parameters
#'
#' @return Result of user evalMethod
#'
treeEvalNode <- function (evalMethod, y, wt, parms) {
  nodeAttributesChoice.chooseAttributes(nodeAttributesChoiceInfo)
  evalMethod(y, wt, parms)
}

#' Rpart split wrapper function for checking if attribute is chosen and spliting attribute.
#'
#' Checks if current index of attribute in NodeAttributesChoiceInfo structure is selected.
#' If attribute is chosen executes split function in argument.
#' Otherwise it returns no split - goodness with zero values for all splits.
#'
#' @param evalMethod User eval method - it will be executed
#' @param y Destination attribute to predict
#' @param wt Examples weights in tree
#' @param x Attribute that will be splitted
#' @param parms Tree user parameters
#' @param continuous Is attribute continuous
#'
#' @return Result of user splitFunction or zero goodness vector of splits.
#'
treeSplitNode <- function (splitFunction, y, wt, x, parms, continuous) {
  if (nodeAttributesChoice.isAttributeChosen(nodeAttributesChoiceInfo)) {
    result <- splitFunction(y, wt, x, parms, continuous)
  } else {
    if (continuous) {
      n <- length(y) - 1
      result <- list(goodness=rep(0, n), direction=rep(1, n))
    } else {
      n <- length(unique(x)) - 1
      result <- list(goodness=rep(0, n), direction=rep(1, n))
    }
  }
  result
}
