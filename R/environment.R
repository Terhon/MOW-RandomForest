
createEnvironment <- function() {
  new.env(parent = emptyenv())
}

saveToEnvironment <- function(env, name, value) {
  oldValue <- env$name
  env[[name]] <- value
  invisible(oldValue)
}

getFromEnvironment <- function(env, name) {
  env[[name]]
}
