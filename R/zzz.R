.pkgenv <- new.env(parent = emptyenv())
.onLoad <- function(libname, pkgname) {
  has_data <- requireNamespace("zdat", quietly = TRUE)
  .pkgenv[["has_data"]] <- has_data
}

.onAttach <- function(libname, pkgname) {
  if (!.pkgenv$has_data) {
    msg <- paste("To use this package, you must install the",
                 "zdat package. To install that ",
                 "package, run `install.packages('zdat',",
                 "repos='https://jlehtoma.github.io/drat/', type='source')`.",
                 "See the `zdat` vignette for more details.")
    msg <- paste(strwrap(msg), collapse = "\n")
    packageStartupMessage(msg)
  }
}

hasData <- function(has_data = .pkgenv$has_data) {
  if (!has_data) {
    msg <- paste("To use this function, you must have the",
                 "`zdat` package installed. See the",
                 "`zdat` package vignette for more details.")
    msg <- paste(strwrap(msg), collapse = "\n")
    stop(msg)
  }
}
