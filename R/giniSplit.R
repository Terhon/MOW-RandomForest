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
  classes <- unique(y)
  classWeights <- tapply(wt, y, sum)
  weightsSum <- sum(classWeights)

  list(classWeights=classWeights, weightsSum=weightsSum, classes=classes)
}

evalGini <- function(y, wt, parms) {
  weightsInfo <- getClassWeightsInfo(y, wt, parms)

  prob <- weightsInfo$classWeights / weightsInfo$weightsSum

  gini <- giniImpurity(prob)
  lab <- names(which.max(prob))[[1]]

  list(label = lab, deviance = gini)
}

getGiniGoodness <- function (maxImpurity, leftClassWeights, leftWeights, rightClassWeights, rightWeights, allWeights){
  leftImpurity <- giniImpurity(leftClassWeights / leftWeights)
  rightImpurity <- giniImpurity(rightClassWeights / rightWeights)
  leftPart <- leftWeights * leftImpurity
  rightPart <- rightWeights * rightImpurity

  goodness <- maxImpurity - ((leftPart + rightPart) / allWeights)
  direction <- 2 * (rightPart < leftPart) - 1

  list(goodness=goodness, direction=direction)
}

splitUniqueGini <- function(y, wt, x, ux, maxImpurity, allWeights){
  mask <- x == ux

  left <- tapply(wt[mask], y[mask], sum)
  right <- tapply(wt[!mask], y[!mask], sum)

  leftSum <- sum(left)
  rightSum <- sum(right)

  giniGoodnesInfo <- getGiniGoodness(maxImpurity, left, leftSum, right, rightSum, allWeights)
  giniGoodnesInfo$goodness
}

splitGini <- function(y, wt, x, parms, continuous)
{
  nodeWeightsInfo <- getClassWeightsInfo(y, wt, parms)
  maxImpurity <- giniImpurity(nodeWeightsInfo$classWeights / nodeWeightsInfo$weightsSum)

  if (continuous) {
    n <- length(y)

    left <- table(nodeWeightsInfo$classes) - 1
    right <- nodeWeightsInfo$classWeights

    leftSum <- 0
    rightSum <- nodeWeightsInfo$weightsSum

    goodness <- rep(0, n-1)
    direction <- rep(0, n-1)
    for (i in 1:(n-1)){
      class <- toString(y[[i]])

      left[[class]] <- left[[class]] + wt[[i]]
      right[[class]] <- right[[class]] - wt[[i]]

      leftSum <- leftSum + wt[[i]]
      rightSum <- rightSum - wt[[i]]

      giniGoodnessInfo <- getGiniGoodness(maxImpurity, left, leftSum, right, rightSum, nodeWeightsInfo$weightsSum)
      goodness[[i]] <- giniGoodnessInfo$goodness
      direction[[i]] <- giniGoodnessInfo$direction
    }

    list(goodness=goodness, direction=direction)
  } else {
    ux <- unique(x)
    giniGoodness <- sapply(ux, function(val){
      splitUniqueGini(y, wt, x, val, maxImpurity, nodeWeightsInfo$weightsSum)
    })

    ord <- order(giniGoodness)
    goodness <- giniGoodness[ord]
    n <- length(ord)

    list(goodness=goodness[-n], direction = ux[ord])
  }
}
