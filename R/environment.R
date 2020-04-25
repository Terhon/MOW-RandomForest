#' Creates environment to save data across functions without loss.
#'
#' @return New Environment to save data.
#'
#' @examples
#' createEnvironment()
createEnvironment <- function() {
  new.env(parent = emptyenv())
}
