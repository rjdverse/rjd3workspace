#' @include utils.R
#' @importFrom rjd3jars reload_safactories reload_tsproviders check_java_version
#' @importFrom rjd3tramoseats tramoseats_dictionary
#' @importFrom rjd3x13 x13_dictionary
#' @importFrom rjd3providers spreadsheet_name
#' @importFrom rjd3toolkit dictionary
NULL

#' @importFrom rJava .jpackage .jaddClassPath
.onLoad <- function(libname, pkgname) {
    result <- rJava::.jpackage(pkgname, lib.loc = libname)
    if (!result) stop("Loading java packages failed", call. = FALSE)

    if (rjd3jars::check_java_version()){
        rjd3jars::reload_tsproviders()
        rjd3jars::reload_safactories()
    }
}
