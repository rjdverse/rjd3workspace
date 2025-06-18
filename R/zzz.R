#' @include utils.R
NULL

#' @rdname jd3_utilities
#' @export
jversion <- NULL

.onAttach <- function(libname, pkgname) {
    # what's your java  version?  Need >= 17
    if (jversion < 17) {
        packageStartupMessage(sprintf("Your java version is %s. 17 or higher is needed.", jversion))
    }
}

.onLoad <- function(libname, pkgname) {
    if (!requireNamespace("rjd3tramoseats", quietly = TRUE)) stop("Loading rjd3 libraries failed")
    if (!requireNamespace("rjd3x13", quietly = TRUE)) stop("Loading rjd3 libraries failed")
    if (!requireNamespace("rjd3providers", quietly = TRUE)) stop("Loading rjd3 libraries failed")

    jversion <<- .jcall("java.lang.System", "S", "getProperty", "java.version")
    jversion <<- as.integer(regmatches(jversion, regexpr(pattern = "^(\\d+)", text = jversion)))

    result <- rJava::.jpackage(pkgname, lib.loc = libname)
    if (!result) stop("Loading java packages failed")
}
