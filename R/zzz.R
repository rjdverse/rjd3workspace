#' @include utils.R
NULL

.onLoad <- function(libname, pkgname) {

  if (! requireNamespace("rjd3tramoseats", quietly = T)) stop("Loading rjd3 libraries failed")
  if (! requireNamespace("rjd3x13", quietly = T)) stop("Loading rjd3 libraries failed")

  result <- rJava::.jpackage(pkgname, lib.loc=libname)
  if (!result) stop("Loading java packages failed")
}

