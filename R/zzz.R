#' @include utils.R
#' @importFrom rjd3toolkit get_java_version minimal_java_version
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

#' @importFrom rJava .jpackage .jaddClassPath
.onLoad <- function(libname, pkgname) {
    result <- rJava::.jpackage(pkgname, lib.loc = libname)
    if (!result) stop("Loading java packages failed", call. = FALSE)
    # reload sa managers (tramoseats, x13)
    # necessary because we don't master the way java classes are loaded in R
    try({
        .jcall(
            "jdplus/sa/base/api/SaManager",
            "V",
            "reload"
        )
    })
}
