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

giniImpurity <- function(prob) {
  1 - sum(prob ** 2)
}

evalGini <- function(y, wt, parms) {
  probSum <- 0
  prob <- table(parms$classes) - 1

  for (i in 1:length(y)){
    probSum <- probSum + wt[i]
    class <- toString(y[i])
    prob[[class]] <- prob[[class]] + wt[i]
  }

  prob <- prob / probSum

  gini <- giniImpurity(prob)
  lab <- names(which.max(prob))[[1]]

  list(label = lab, deviance = gini)
}

splitGini <- function(y, wt, x, parms, continuous)
{

}
