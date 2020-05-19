#' Anova init split function
#'
#' Initializes variables for anova split function
#'
#' @param y Target variable to predict
#' @param offset Data offset
#' @param wt Tree examples weights
#' @param parms Optional parameters for the splitting function.
#'
#' @return Rpart init structure containing basic algorithm information.
#'
initAnova <- function(y, offset, parms, wt) {
  if (is.matrix(y) && ncol(y) > 1)
    stop("Matrix response not allowed")
  if (!missing(parms) && length(parms) > 0)
    warning("parameter argument ignored")
  if (length(offset)) y <- y - offset
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste(" mean=", format(signif(yval, digits)),
          ", MSE=" , format(signif(dev/wt, digits)),
          sep = '')
  }
  environment(sfun) <- .GlobalEnv
  list(y = c(y), parms = NULL, numresp = 1, numy = 1, summary = sfun)
}

#' Anova eval split function
#'
#' Used for printing node information by rpart.
#'
#' @param y Target variable to predict
#' @param wt Tree examples weights
#' @param parms Optional parameters for the splitting function.
#'
#' @return Rpart init structure containing basic node information.
#'
evalAnova <- function(y, wt, parms) {
  wmean <- sum(y*wt)/sum(wt)
  rss <- sum(wt*(y-wmean)^2)
  list(label = wmean, deviance = rss)
}

#' Anova split node function
#'
#' Used for calculating split goodness.
#'
#' @param y Target variable to predict
#' @param wt Tree examples weights
#' @param x Attribute to split.
#' @param parms Optional parameters for the splitting function.
#' @param continuous Is x attribute continous flag.
#'
#' @return Rpart init structure containing basic split information.
#'
splitAnova <- function(y, wt, x, parms, continuous)
{
  # Center y
  n <- length(y)
  y <- y- sum(y*wt)/sum(wt)
  if (continuous) {
    # continuous x variable
    temp <- cumsum(y*wt)[-n]
    left.wt <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    goodness <- (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2)
    rx <- rank(x[-1])
    if(IQR(goodness) > 0 & IQR(rx) > 0 & length(sort(rx[!duplicated(rx)])) > 3L){
      fit <- smooth.spline(rx, goodness, df=4)
      goodness =  predict(fit, rx)$y
    }
    list(goodness = goodness, direction = sign(lmean))
  } else {
    # Categorical X variable
    ux <- sort(unique(x))
    wtsum <- tapply(wt, x, sum)
    ysum <- tapply(y*wt, x, sum)
    means <- ysum/wtsum
    # For anova splits, we can order the categories by their means
    # then use the same code as for a non-categorical
    ord <- order(means)
    n <- length(ord)
    temp <- cumsum(ysum[ord])[-n]
    left.wt <- cumsum(wtsum[ord])[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    list(goodness= (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2),
         direction = ux[ord])
  }
}
