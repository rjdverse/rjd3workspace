#' @include utils.R
NULL

#' @title Java Utility Functions
#'
#' @description
#' These functions are used in all JDemetra+ 3.0 packages to easily interact between R and Java objects.
#' @name jd3_utilities
NULL

.onAttach <- function(libname, pkgname) {
    current_java_version <- rjd3toolkit::get_java_version()
    if (current_java_version < rjd3toolkit::minimal_java_version) {
        packageStartupMessage(sprintf(
            "Your java version is %s. %s or higher is needed.",
            current_java_version,
            rjd3toolkit::minimal_java_version
        ))
    }
}

#' @importFrom rJava .jpackage
.onLoad <- function(libname, pkgname) {
    result <- rJava::.jpackage(pkgname, lib.loc = libname)
    if (!result) stop("Loading java packages failed", call. = FALSE)
}
