#' @importFrom graphics text
#' @importFrom stats IQR model.response predict smooth.spline
#' @import rpart
#' @importFrom pryr partial

nodeAttributesChoiceInfo <- createEnvironment()

#' Creates a single random forest tree.
#'
#' Creates a single random forest tree with random selecting attributes before splitting node.
#' To build a tree uses rpart package.
#'
#' @param formula Tree formula
#' @param data Data table
#' @param attributesToChooseCount Number of attributes to choose while splitting node - default sqrt(n) where n number of attributes.
#' @param bootstrap Use bootstrap method - default=TRUE.
#' @param weights Tree examples weights
#' @param subset Expression saying that only a subset of the rows of the data should be used in the fit.
#' @param na.action The default action deletes all observations for which y is missing, but keeps those in which one or more predictors are missing.
#' @param method User split methods or specific split functions ("anova", "class", "default").
#' @param model Keep a copy of the model frame in the result.
#' @param x Keep a copy of the x matrix in the result.
#' @param y Keep a copy of the dependent variable in the result.
#' @param parms Optional parameters for the splitting function.
#' @param control A list of options that control details of the rpart algorithm.
#' @param cost A vector of non-negative costs.
#'
#' @return Tree structure containing rpart tree build using wrapped function that selects attributes.
#'
#' @examples
#' method <- list(eval=evalNode, split=splitNode, init=initTree)
#' data <- data.frame(y = numeric(10), a = numeric(10), b = numeric(10))
#' t <- tree(y~., data, 2, method=method)
#'
#' @export
tree <- function(formula, data, attributesToChooseCount=floor(sqrt(ncol(data)-1)), bootstrap=TRUE, na.action=na.rpart, method="default", model=FALSE, parms=NULL, control=rpart.control(), ...) {
  if (bootstrap){
    bootstrapIndexes <- sample(nrow(data), replace=TRUE)
    data <- data[bootstrapIndexes,]
  }

  rpartArgs <- list(...)
  rpartArgs$formula <- formula
  rpartArgs$data <- data

  allAttributesCount <- ncol(data) - 1

  nodeAttributesChoice.init(nodeAttributesChoiceInfo, attributesToChooseCount, allAttributesCount)

  userModel <- model

  if (is.data.frame(model)) {
    m <- model
    model <- FALSE
  } else {
    m <- stats::model.frame(formula=formula, data=data, na.action=na.action)
  }

  Y <- model.response(m)

  if (method == "default") {
    method <- if (is.factor(Y) || is.character(Y)) "class"
             else "anova"
  }

  model <- userModel
  ylevels <- NULL

  if (is.list(method)){
    treeMethod <- "user"
  } else if (method == "anova"){
    method <- list()
    method$init <- initAnova
    method$eval <- evalAnova
    method$split <- splitAnova
    treeMethod <- "anova"
  } else if (method == "class") {
    fy <- as.factor(Y)
    ylevels <- levels(fy)

    if (is.null(parms)){
      parms <- list()
    }

    parms$minbucket <- control$minbucket

    method <- list()
    method$init <- initGini
    method$eval <- evalGini
    method$split <- splitGini
    treeMethod <- "class"
  } else {
    stop('Unknown split method')
  }

  evalFunction <- partial(treeEvalNode, method$eval)
  splitFunction <- partial(treeSplitNode, method$split)
  wrappedMethods <- list(eval=evalFunction, split=splitFunction, init=method$init)

  rpartArgs$method <- wrappedMethods
  rpartArgs$parms <- parms
  rpartArgs$control <- control
  r <- do.call(rpart, rpartArgs)

  tree <- list(tree = r, method = treeMethod, ylevels = ylevels)
  class(tree) <- "tree"
  tree
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
      n <- length(unique(x))
      result <- list(goodness=rep(0, n-1), direction=rep(1, n))
    }
  }
  result
}

#' Tree predict algorithm.
#'
#' Predicts with use of rpart prediction algorithm.
#'
#' @param treeObject Tree object containing fitted rpart tree
#' @param data Data table on which to predict
#' @param type Type of prediction - "class" or "vector" (regression)
#'
#' @return rpart tree prediction results
#'
#' @examples
#' method <- list(eval=evalNode, split=splitNode, init=initTree)
#' data <- data.frame(y = numeric(10), a = numeric(10), b = numeric(10))
#' forest <- tree(y~., data, 2, method=method)
#' predictions <- predict(forest, data)
#'
#' @export
predict.tree <- function (treeObject, ...) {
  rpartTree <- treeObject$tree
  predictions <- predict(rpartTree, ...)

  if (treeObject$method == "class"){
    ylevels <- treeObject$ylevels

    predictions <- sapply(predictions, function(pred){
      ylevels[[pred]]
    })
  }

  predictions
}

#' Tree summary.
#'
#' Shows rpart summary for tree.
#'
#' @param treeObject Tree object containing fitted tree structure
#'
#' @return rpart tree summary
#'
#' @export
summary.tree <- function (treeObject, ...) {
  summary(treeObject$tree, ...)
}

#' Tree text.
#'
#' Shows rpart text plot for tree.
#'
#' @param treeObject Tree object containing fitted tree structure
#'
#' @return rpart tree text
#'
#' @export
text.tree <- function (treeObject, ...) {
  text(treeObject$tree, ...)
}

