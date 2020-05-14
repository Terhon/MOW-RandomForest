initGini <- function(y, offset, parms, wt) {
  if (is.matrix(y) && ncol(y) > 1)
    stop("Matrix response not allowed")
  if (!missing(parms) && length(parms) > 0)
    warning("parameter argument ignored")
  if (length(offset)) y <- y - offset
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste(" label=", format(signif(yval, digits)),
          ", dev=" , format(signif(dev/wt, digits)),
          sep = '')
  }
  environment(sfun) <- .GlobalEnv

  list(y = c(y), parms = NULL, numresp = 1, numy = 1, summary = sfun)
}

giniImpurity <- function(prob) {
  1 - sum(prob ** 2)
}

getClassWeightsInfo <- function(y, wt, parms){
  weightsSum <- 0
  classes <- unique(y)
  classWeights <- table(classes) - 1

  for (i in 1:length(y)){
    weightsSum <- weightsSum + wt[[i]]
    class <- toString(y[[i]])

    classWeights[[class]] <- classWeights[[class]] + wt[[i]]
  }

  list(classWeights=classWeights, weightsSum=weightsSum, classes=classes)
}

evalGini <- function(y, wt, parms) {
  weightsInfo <- getClassWeightsInfo(y, wt, parms)

  prob <- weightsInfo$classWeights / weightsInfo$weightsSum

  gini <- giniImpurity(prob)
  lab <- names(which.max(prob))[[1]]

  list(label = lab, deviance = gini)
}

getGiniGoodness <- function (maxImpurity, leftProb, rightProb, leftCount, allCount){
  leftImpurity <- giniImpurity(leftProb)
  rightImpurity <- giniImpurity(rightProb)
  maxImpurity - ((leftCount * leftImpurity + (allCount - leftCount) * rightImpurity) / allCount)

}

splitUniqueGini <- function(y, wt, x, ux, n, maxImpurity, nodeWeightsInfo){
  mask <- x == ux

  y <- y[mask]
  wt <- wt[mask]

  leftCount <- sum(mask)

  left <- table(nodeWeightsInfo$classes) - 1
  right <- nodeWeightsInfo$classWeights

  leftSum <- 0
  rightSum <- nodeWeightsInfo$weightsSum

  for (i in 1:leftCount){
    class <- toString(y[i])
    left[[class]] <- left[[class]] + wt[i]
    right[[class]] <- right[[class]] - wt[i]

    leftSum <- leftSum + wt[i]
    rightSum <- rightSum - wt[i]
  }

  getGiniGoodness(maxImpurity, left / leftSum, right / rightSum, leftCount, n)

}

splitGini <- function(y, wt, x, parms, continuous)
{
  nodeWeightsInfo <- getClassWeightsInfo(y, wt, parms)
  maxImpurity <- giniImpurity(nodeWeightsInfo$classWeights / nodeWeightsInfo$weightsSum)

  n <- length(y)

  if (continuous) {
    left <- table(nodeWeightsInfo$classes) - 1
    right <- nodeWeightsInfo$classWeights

    leftSum <- 0
    rightSum <- nodeWeightsInfo$weightsSum

    goodness <- rep(0, n-1)
    for (i in 1:(n-1)){
      class <- toString(y[[i]])

      left[[class]] <- left[[class]] + wt[[i]]
      right[[class]] <- right[[class]] - wt[[i]]

      leftSum <- leftSum + wt[i]
      rightSum <- rightSum - wt[i]

      goodness[[i]] <- getGiniGoodness(maxImpurity, left / leftSum, right / rightSum, i, n)
    }

    list(goodness=goodness, direction=rep(1, n-1))
  } else {
    ux <- unique(x)
    giniGoodness <- sapply(ux, function(val){
      splitUniqueGini(y, wt, x, val,
                      n, maxImpurity, nodeWeightsInfo)
    })

    ord <- order(giniGoodness)
    goodness <- giniGoodness[ord]
    n <- length(ord)

    list(goodness=goodness[-n],direction = ux[ord])
  }
}
