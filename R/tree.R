#' Creates a single random forest tree.
#'
#' Creates a single random forest tree with random selecting attributes before splitting node.
#' To build a tree uses rpart package.
#'
#' @param e example
#'
#' @return Rpart tree build using wrapped function that selects attributes.
#'
#' @examples
#' randomForest()
#'
#' @export
tree <- function(formula, data, attributesToChooseCount, weights, subset, na.action = na.rpart, method,
                      model = FALSE, x = FALSE, y = TRUE, parms, control, costr) {

  allAttributesCount <- ncol(data) - 1

  treeParameters <- createEnvironment()
  treeParameters$nodeAttributesChoiceInfo <- nodeAttributesChoice.init(attributesToChooseCount, allAttributesCount)
  treeParameters$userParms <- parms

  evalFunction <- partial(treeEvalNode, method$eval)
  splitFunction <- partial(treeSplitNode, method$split)
  wrappedMethods <- list(eval=evalFunction, split=splitFunction, init=method['init'])

  rpart(formula, data, weights, subset, na.action, wrappedMethods,
       model, x, y, treeParameters, control, cost)
}

#' Rpart eval wrapper function for selecting atributes in tree.
#'
#' Generates indexes of attributes that has to be selected while splitting tree.
#'
#' @param evalMethod User eval method - it will be executed
#' @param y Destination attribute to predict
#' @param wt Attribute weights in tree
#' @param parms Tree parameters - contains NodeAttributesChoiceInfo structure and user additional parameters
#'
#' @return Result of user evalMethod
#'
treeEvalNode <- function (evalMethod, y, wt, parms) {
  nodeAttributesChoice.chooseAttributes(parms$nodeAttributesChoiceInfo)
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
#' @param wt Attribute weights in tree
#' @param x Attribute that will be splitted
#' @param parms Tree parameters - contains NodeAttributesChoiceInfo and user additional parameters
#' @param continuous Is attribute continuous
#'
#' @return Result of user splitFunction or zero goodness vector of splits.
#'
treeSplitNode <- function (splitFunction, y, wt, x, parms, continuous) {
  if (nodeAttributesChoice.isAttributeChosen(parms$nodeAttributesChoiceInfo)) {
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
