#' Formatting of data for node information.
#'
#' Formats digists and matrixes for node information printing.
#'
#' @param x Data to format
#' @param digit Number of digits to print
#' @param format Formatting of data.
#'
#' @return Formatted data.
#'
formatg <- function(x, digits = getOption("digits"),
                    format = paste0("%.", digits, "g"))
{
  if (!is.numeric(x)) stop("'x' must be a numeric vector")

  temp <- sprintf(format, x)
  if (is.matrix(x)) matrix(temp, nrow = nrow(x)) else temp
}

#' Gini init split function
#'
#' Initializes variables for anova split function.
#' Code was written based on rpart source code for gini.
#'
#' @param y Target variable to predict
#' @param offset Data offset
#' @param wt Tree examples weights
#' @param parms Optional parameters for the splitting function.
#'
#' @return Rpart init structure containing basic algorithm information.
#'
initGini <- function(y, offset, parms, wt) {
  # Print, summary and text function and part of init function were taken from rpart source code.
  # Credits of fragments of code to authors of rpart package.

  fy <- as.factor(y)
  y <- as.integer(fy)
  ylev <- levels(fy)

  numclass <- max(y[!is.na(y)])

  classWeights <- getClassWeights(y, wt, list(numclass = numclass))
  weightsSum <- sum(classWeights)
  freq <- classWeights / weightsSum

  if (missing(parms) || is.null(parms))
    parms <- list(prior = freq,
                  loss = matrix(rep(1, numclass^2) - diag(numclass),
                                numclass))
  else if (is.list(parms)) {
    if (is.null(names(parms))) stop("The parms list must have names")
    temp <- pmatch(names(parms), c("prior", "loss"), 0L)
    if (any(temp == 0L))
      stop(gettextf("'parms' component not matched: %s",
                    names(parms)[temp == 0L]), domain = NA)
    names(parms) <- c("prior", "loss")[temp]

    if (is.null(parms$prior)) temp <- c(freq)
    else {
      temp <- parms$prior
      if (sum(temp) != 1) stop("Priors must sum to 1")
      if (any(temp < 0)) stop("Priors must be >= 0")
      if (length(temp) != numclass) stop("Wrong length for priors")
    }

    if (is.null(parms$loss)) temp2 <- 1 - diag(numclass)
    else {
      temp2 <- parms$loss
      if (length(temp2) != numclass^2)
        stop("Wrong length for loss matrix")
      temp2 <- matrix(temp2, ncol = numclass)
      if (any(diag(temp2) != 0))
        stop("Loss matrix must have zero on diagonals")
      if (any(temp2 < 0))
        stop("Loss matrix cannot have negative elements")
      if (any(rowSums(temp2) == 0))
        stop("Loss matrix has a row of zeros")
    }

    parms <- list(prior = temp, loss = matrix(temp2, numclass))
  }
  else stop("Parameter argument must be a list")

  parms$numclass <- numclass
  #parms$ylevels <- ylevels

  parms$aprior <- rowSums(parms$loss * c(parms$prior))
  temp <- sum(parms$aprior)

  for (i in 1:numclass){
    if (freq[[i]] > 0){
      parms$prior[[i]] <- parms$prior[[i]] / freq[[i]]
      parms$aprior[[i]] <- parms$aprior[[i]] / (temp * freq[[i]])
    }
  }

  list(y = y, parms = parms, numresp = 2 * numclass + 2L, counts = classWeights, ylevels = levels(fy), numy = 1L,
       print = function(yval, ylevel, digits) {
         ylevel <- ylev

         temp <- if (is.null(ylevel)) as.character(yval[, 1L])
         else ylevel[yval[, 1L]]

         nclass <- (ncol(yval) - 2L)/2L
         yprob <- if (nclass < 5L)
           format(yval[, 1L + nclass + 1L:nclass],
                  digits = digits, nsmall = digits)
         else formatg(yval[, 1L + nclass + 1L:nclass], digits = 2L)
         if (!is.matrix(yprob)) #this case only occurs for no split trees
           yprob <- matrix(yprob, nrow = 1L)

         temp <- paste0(temp, " (", yprob[, 1L])
         for (i in 2L:ncol(yprob)) temp <- paste(temp, yprob[, i], sep = " ")
         temp <- paste0(temp, ")")
         temp
       },
       summary = function(yval, dev, wt, ylevel, digits) {
         ylevel <- ylev

         nclass <- (ncol(yval) - 2L) /2L
         group <- yval[, 1L]
         counts <- yval[, 1L + (1L:nclass)]
         yprob <- yval[, 1L + nclass + 1L:nclass]
         nodeprob <- yval[, 2L * nclass + 2L]
         if (!is.null(ylevel)) group <- ylevel[group]

         temp1 <- formatg(counts, format = "%5g")
         temp2 <- formatg(yprob, format = "%5.3f")
         if (nclass >1) {
           temp1 <- apply(matrix(temp1, ncol = nclass), 1L,
                          paste, collapse = " ")
           temp2 <- apply(matrix(temp2, ncol = nclass), 1L,
                          paste, collapse = " ")
         }
         dev <- dev/(wt[1L] * nodeprob)
         paste0("  predicted class=", format(group, justify = "left"),
                "  expected loss=", formatg(dev, digits),
                "  P(node) =", formatg(nodeprob, digits), "\n",
                "    class counts: ", temp1, "\n",
                "   probabilities: ", temp2)
       },
       text = function(yval, dev, wt, ylevel, digits, n, use.n) {
         ylevel <- ylev

         nclass <- (ncol(yval) - 2L)/2L
         group <- yval[, 1L]
         counts <- yval[, 1L+ (1L:nclass)]
         if (!is.null(ylevel)) group <- ylevel[group]

         temp1 <- formatg(counts, digits)
         if (nclass > 1L)
           temp1 <- apply(matrix(temp1, ncol = nclass), 1L,
                          paste, collapse = "/")
         if (use.n)  paste0(format(group, justify = "left"), "\n", temp1)
         else format(group, justify = "left")
       })
}

#' Calculates gini impurity
#'
#' Used for calculating split goodness.
#'
#' @param prob Vector of probabilites of classes.
#'
#' @return Gini impurity
#'
giniImpurity <- function(prob) {
  1 - sum(prob ** 2)
}

#' Calculates class weights for target attribute.
#'
#' Used to calculate likelihood for classes.
#'
#' @param y Target variable to predict
#' @param wt Tree examples weights
#' @param parms Optional parameters for the splitting function.
#'
#' @return Class weights
#'
getClassWeights <- function(y, wt, parms){
  classWeights <- tapply(wt, factor(y, levels = 1:parms$numclass), sum)
  ifelse(is.na(classWeights), 0, classWeights)
}

#' Gini eval split function
#'
#' Used for printing node information by rpart.
#'
#' @param y Target variable to predict
#' @param wt Tree examples weights
#' @param parms Optional parameters for the splitting function.
#'
#' @return Rpart init structure containing basic node information for printing.
#'
evalGini <- function(y, wt, parms) {
  classWeights <- getClassWeights(y, wt, parms)
  counts <- getClassWeights(y, rep(1, length(y)), parms)

  prob <- sum(classWeights)

  realClassWeights <- classWeights * parms$prior
  choiceLoss <- colSums(parms$loss * c(realClassWeights))

  lab <- which.min(choiceLoss)[[1]]

  classFreq <- as.vector(realClassWeights / sum(realClassWeights))
  counts <- as.vector(counts)

  ret <- c(lab, counts, classFreq, prob)

  list(label = ret, deviance = min(choiceLoss))
}

#' Calculates gini split function.
#'
#' Used for calculating split goodness.
#'
#' @param maxImpurity Max impurity of node.
#' @param leftClassWeights Class weights for left part of split
#' @param leftWeights Summed class weight for left part of split
#' @param rightClassWeights Class weights for right part of split
#' @param rightWeights Summed class weight for right part of split
#'
#' @return Rpart init structure containing basic split information.
#'
getGiniGoodness <- function (maxImpurity, leftClassWeights, leftWeights, rightClassWeights, rightWeights){
  left <- leftClassWeights / leftWeights
  right <- rightClassWeights / rightWeights

  leftImpurity <- giniImpurity(left)
  rightImpurity <- giniImpurity(right)
  leftPart <- leftWeights * leftImpurity
  rightPart <- rightWeights * rightImpurity

  goodness <- maxImpurity - (leftPart + rightPart)

  d <- c(0:(length(left)-1))
  lmean <- sum(left * d)
  rmean <- sum(right * d)
  direction <- 2 * (lmean > rmean) - 1

  list(goodness=goodness, direction=direction)
}

#' Calculates groups using graycode.
#'
#' Used for splitting unique attributes.
#'
#' @param gray Graycode structure containing graycode vector.
#'
#' @return Graycode structure containing next graycode number and updated graycode vector.
#'
graycode <- function(gray){
  gray <- gray$gray
  len <- length(gray)
  num <- len

  for (i in 1:(len-1)){
    if (gray[[i]] == 1){
      gray[[i]] <- 2
      num <- i
      break
    } else if (gray[[i]] == 2){
      gray[[i]] <- 1
    }
  }

  list(number = num, gray = gray)
}

#' Returns next number from order vector.
#'
#' Used for splitting unique attributes.
#'
#' @param num Number structure with order from which to take next number.
#'
#' @return Structure containing next number.
#'
nextOrderNumber <- function(num){
  idx <- num$idx
  list(number = num$order[[idx]], order = num$order, idx = idx + 1)
}

#' Gini split node function
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
splitGini <- function(y, wt, x, parms, continuous)
{
  classWeights <- getClassWeights(y, wt, parms) * c(parms$aprior)
  weightsSum <- sum(classWeights)

  maxImpurity <- giniImpurity(classWeights / weightsSum) * weightsSum

  left <- table(1:parms$numclass) - 1
  right <- classWeights

  leftSum <- 0
  rightSum <- weightsSum

  if (continuous) {
    n <- length(y)

    goodness <- rep(1, n-1)
    direction <- rep(1, n-1)
    for (i in 1:(n-1)){
      class <- y[[i]]

      left[[class]] <- left[[class]] + wt[[i]]
      right[[class]] <- right[[class]] - wt[[i]]

      leftSum <- leftSum + wt[[i]]
      rightSum <- rightSum - wt[[i]]

      giniGoodnessInfo <- getGiniGoodness(maxImpurity, left, leftSum, right, rightSum)

      goodness[[i]] <- giniGoodnessInfo$goodness
      direction[[i]] <- giniGoodnessInfo$direction
    }

    list(goodness=goodness, direction=direction)
  } else {
    categoricalClassWeights = tapply(1:length(y), x, function(idx){
      getClassWeights(y[idx], wt[idx], parms) * c(parms$aprior)
    })

    categoricalWeightsSum = sapply(categoricalClassWeights, sum)

    if (parms$numclass == 2){
      rate <- sapply(categoricalClassWeights, function(weights){
        weights[[0]]
      })

      rate <- rate / categoricalWeightsSum
      ord <- order(rate)
      iteratorInfo <- list(order = ord, idx = 1)
      iterator <- nextOrderNumber
    } else {
      iteratorInfo <- list(gray = 1 * categoricalWeightsSum > 0)
      iterator <- graycode
    }

    ux <- names(categoricalWeightsSum)
    n <- length(ux)

    dir <- rep(1, n)
    bestGoodness <- 0
    while(TRUE) {
      iteratorInfo <- iterator(iteratorInfo)
      i <- iteratorInfo$number

      if (i >= n){
        break
      }

      left <- left + dir[[i]] * categoricalClassWeights[[i]]
      leftSum <- leftSum + dir[[i]] * categoricalWeightsSum[[i]]

      right <- right - dir[[i]] * categoricalClassWeights[[i]]
      rightSum <- rightSum - dir[[i]] * categoricalWeightsSum[[i]]

      dir[[i]] <- -dir[[i]]

      giniGoodnesInfo <- getGiniGoodness(maxImpurity, left, leftSum, right, rightSum)
      if (giniGoodnesInfo$goodness > bestGoodness){
        bestGoodness <- giniGoodnesInfo$goodness
        bestDir <- -1 * giniGoodnesInfo$direction * dir
      }
    }

    direction <- ux[bestDir == -1]
    i <- length(direction)
    direction <- c(direction, ux[bestDir == 1])

    goodness <- rep(0, n-1)
    goodness[[i]] <- bestGoodness

    list(goodness=goodness, direction = direction)
  }
}
