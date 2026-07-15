#' @include utils.R
#' @importFrom rjd3jars reload_safactories reload_tsproviders check_java_version
#' @importFrom rjd3tramoseats tramoseats_dictionary
#' @importFrom rjd3x13 x13_dictionary
#' @importFrom rjd3providers spreadsheet_name
#' @importFrom rjd3toolkit dictionary
NULL

#' @importFrom rjd3jars check_java_version
.onAttach <- function(libname, pkgname) {
    # Check java version
    rjd3jars::check_java_version(silent = FALSE, startup = TRUE)
}

#' @importFrom rJava .jpackage .jaddClassPath
#' @importFrom rjd3jars reload_dictionaries reload_tsproviders
#' @importFrom rjd3jars reload_safactories check_java_version
.onLoad <- function(libname, pkgname) {
    # Loading dependencies
    if (!requireNamespace("rjd3jars", quietly = TRUE)) {
        stop("Loading {rjd3jars} failed", call. = FALSE)
    }
    if (!requireNamespace("rjd3toolkit", quietly = TRUE)) {
        stop("Loading {rjd3toolkit} failed", call. = FALSE)
    }
    if (!requireNamespace("rjd3x13", quietly = TRUE)) {
        stop("Loading {rjd3x13} failed", call. = FALSE)
    }
    if (!requireNamespace("rjd3tramoseats", quietly = TRUE)) {
        stop("Loading {rjd3tramoseats} failed", call. = FALSE)
    }
    if (!requireNamespace("rjd3providers", quietly = TRUE)) {
        stop("Loading {rjd3providers} failed", call. = FALSE)
    }

    # Loading Java class
    jar_dir <- file.path(libname, pkgname, "inst", "java")
    jars_inst <- list.files(
        jar_dir,
        pattern = "\\.jar$",
        full.names = TRUE,
        all.files = TRUE
    )
    result <- rJava::.jpackage(
        pkgname,
        lib.loc = libname,
        morePaths = jars_inst
    )
    if (!result) {
        stop("Loading java packages failed")
    }

    # If java >= 21, then reload dictionnaries
    has_java <- rjd3jars::check_java_version(silent = TRUE)
    if (has_java) {
        rjd3jars::reload_dictionaries()
        rjd3jars::reload_tsproviders()
        rjd3jars::reload_safactories()
    }
}
