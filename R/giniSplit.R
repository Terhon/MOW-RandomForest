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

  parms <- list()
  parms$classes <- unique(y)
  parms$classesCount <- length(parms$classes)

  list(y = c(y), parms = parms, numresp = 1, numy = 1, summary = sfun)
}

c <- function(prob) {
  1 - sum(prob ** 2)
}

getClassWeightsInfo <- function(y, wt, parms){
  weightsSum <- 0
  classWeights <- table(parms$classes) - 1

  for (i in 1:length(y)){
    weightsSum <- weightsSum + wt[i]
    class <- toString(y[i])
    classWeights[[class]] <- classWeights[[class]] + wt[i]
  }

  list(classWeights=classWeights, weightsSum=weightSum)
}

evalGini <- function(y, wt, parms) {
  weightsInfo <- getClassWeightsInfo(y, wt, parms)

  prob <- weightsInfo$classWeights / weightsInfo$weightsSum

  gini <- giniImpurity(prob)
  lab <- names(which.max(prob))[[1]]

  list(label = lab, deviance = gini)
}

splitGini <- function(y, wt, x, parms, continuous)
{
  if (continuous) {
    nodeWeightsInfo <- getClassWeightsInfo(y, wt, parms)
    maxImpurity <- giniImpurity(nodeWeightsInfo$classWeights / nodeWeightsInfo$weightsSum)

    left <- nodeWeightsInfo$classWeights
    right <- table(parms$classes) - 1

    leftSum <- nodeWeightsInfo$weightsSum
    rightSum <- 0

    n <- length(y)
    goodness <- table(1:n-1)
    for (i in 1:(n-1)){
      class <- toString(y[i])
      left[[class]] <- left[[class]] - wt[i]
      right[[class]] <- right[[class]] + wt[i]

      leftSum <- leftSum - y[i]
      rightSum <- rightSum + y[i]

      leftImpurity <- giniImpurity(left / leftSum)
      rightImpurity <- giniImpurity(right / rightSum)
      goodness[[i]] <- maxImpurity - ((i * leftImpurity + (n - i) * rightImpurity) / n)
    }
  } else {

  }

  list(goodness=goodness, direction=rep(1, n))
}
