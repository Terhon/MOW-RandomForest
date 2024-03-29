% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree.R
\name{predict.tree}
\alias{predict.tree}
\title{Tree predict algorithm.}
\usage{
\method{predict}{tree}(treeObject, ...)
}
\arguments{
\item{treeObject}{Tree object containing fitted rpart tree}

\item{data}{Data table on which to predict}

\item{type}{Type of prediction - "class" or "vector" (regression)}
}
\value{
rpart tree prediction results
}
\description{
Predicts with use of rpart prediction algorithm.
}
\examples{
method <- list(eval=evalNode, split=splitNode, init=initTree)
data <- data.frame(y = numeric(10), a = numeric(10), b = numeric(10))
forest <- tree(y~., data, 2, method=method)
predictions <- predict(forest, data)

}
