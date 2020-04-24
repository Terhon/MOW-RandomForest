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

treeEvalNode <- function (evalMethod, y, wt, x, parms, continuous) {
  nodeAttributesChoice.chooseAttributes(parms$nodeAttributesChoiceInfo)
  evalMethod(y, wt, x, parms, continuous)
}

treeSplitNode <- function (splitFunction, y, offset, parms, wt) {
  if (nodeAttributesChoice.isAttributeChosen(parms$nodeAttributesChoiceInfo)) {
    splitFunction(y, offset, parms, wt)
  }
}
