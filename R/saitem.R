#' @include utils.R
NULL

#' @title Read an SA-item
#'
#' @description
#' `read_sai()` extracts all the information of a SA-item (see details).
#'
#' @param jsai Java SA-item object.
#'
#' @returns a list
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#'
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml",
#'                     package = "rjd3workspace")
#' \donttest{
#' jws <- jws_open(file)
#'
#' # Select SAProcessing
#' jsap1 <- jws_sap(jws, 1)
#'
#' # Select SA-item (as java object)
#' jsai1 <- jsap_sai(jsap1, 3)
#'
#'  # read SA-item
#' read_sai(jsai = jsai1)
#' }
#'
#'
#'
#' @details
#' A SA-item contains more information than just the results of an estimation.
#' Full information is extracted with the `read_sai()` function that
#' returns a list of 5 objects:
#' - `ts`: raw time series.
#' - `referenceSpec`: initial specification. Reference when refreshing and
#'   relaxing constraints.
#' - `estimationSpec`: specification used for the current estimation.
#' - `resultSpec`: specification containing all parameters stemming from
#'   `estimationSpec` (fully identified model).
#' - `results`: results of the estimation.
#'
#' @export
#'
read_sai <- function(jsai) {
    sai <- list(
        ts = get_ts(jsai),
        referenceSpec = get_reference_specification(jsai),
        estimationSpec = get_estimation_specification(jsai),
        resultSpec = get_result_specification(jsai),
        results = get_results(jsai)
    )
    return(sai)
}

#' @title Extract results from a SA-item
#'
#' @description
#' `get_results()` extracts the results of a SA-item.
#' `.jsai_results()` extracts specific output of the model of the SA-item.
#' `.jsai_jresults()` extracts the Java object of the results of a SA-item.
#'
#' @inheritParams read_sai
#' @param items vector of characters containing the variables to extract.
#' See [rjd3x13::x13_dictionary()] or [rjd3tramoseats::tramoseats_dictionary()].
#' By default, extracts all the possible variables.
#'
#' @returns List with all the results of the adjustment.
#'
#' @name get-results
#' @export
#'
get_results <- function(jsai) {
    jestimation <- .jcall(
        jsai,
        "Ljdplus/sa/base/api/SaEstimation;",
        "getEstimation"
    )
    if (is.jnull(jestimation) || is.null(jestimation)) {
        return(NULL)
    }
    jrslt <- .jcall(
        obj = jestimation,
        returnSig = "Ljdplus/toolkit/base/api/information/GenericExplorable;",
        method = "getResults"
    )
    if (is.jnull(jrslt)) {
        return(NULL)
    }

    if (
        .jinstanceof(
            jrslt,
            "jdplus/tramoseats/base/core/tramoseats/TramoSeatsResults"
        )
    ) {
        rslt <- jrslt |>
            .jcast(
                "jdplus/tramoseats/base/core/tramoseats/TramoSeatsResults"
            ) |>
            rjd3tramoseats::.tramoseats_rslts()
    } else if (.jinstanceof(jrslt, "jdplus.x13.base.core.x13.X13Results")) {
        rslt <- jrslt |>
            .jcast("jdplus/x13/base/core/x13/X13Results") |>
            rjd3x13::.x13_rslts()
    }
    return(rslt)
}

#' @rdname get-results
#' @export
.jsai_results <- function(jsai, items = NULL) {
    jestimation <- .jcall(
        jsai,
        "Ljdplus/sa/base/api/SaEstimation;",
        "getEstimation"
    )
    if (is.jnull(jestimation)) {
        return(NULL)
    }
    jrslt <- .jcall(
        jestimation,
        "Ljdplus/toolkit/base/api/information/GenericExplorable;",
        "getResults"
    )
    if (is.null(items)) {
        items <- rjd3toolkit::.proc_dictionary2(jrslt)
    }
    r <- lapply(items, function(t) {
        rjd3toolkit::.proc_data(jrslt, t)
    })
    names(r) <- items
    return(r)
}

#' @rdname get-results
#' @export
.jsai_jresults <- function(jsai) {
    jestimation <- .jcall(
        jsai,
        "Ljdplus/sa/base/api/SaEstimation;",
        "getEstimation"
    )
    if (is.jnull(jestimation)) {
        return(NULL)
    }
    jrslt <- .jcall(
        jestimation,
        "Ljdplus/toolkit/base/api/information/GenericExplorable;",
        "getResults"
    )
    res <- rjd3toolkit::.jd3_object(jrslt, result = TRUE)
    return(res)
}

#' @rdname sap_name
#' @export
sai_name <- function(jsai) {
    return(.jcall(jsai, "S", "getName"))
}

#' @title Extract Metadata from a SA-Item
#'
#' @description
#' Extract specific metadata or time series metadata of a SA-item.
#'
#' @inheritParams read_sai
#' @param key key of the metadata.
#' @export
#'
#' @returns The corresponding metadata (character, numeric...)
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#'
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml",
#'                     package = "rjd3workspace")
#' \donttest{
#' jws <- jws_open(file)
#'
#' # Select SAProcessing
#' jsap1 <- jws_sap(jws, 1)
#'
#' # Select SA-item (as java object)
#' jsai1 <- jsap_sai(jsap1, 3)
#'
#' # Extract the comment as metadata
#' get_metadata(jsai1, "comment")
#'
#' # Extract the ts metadata
#' get_metadata(jsai1, "@id")
#' get_metadata(jsai1, "@source")
#' get_metadata(jsai1, "@timestamp")
#' }
#'
get_metadata <- function(jsai, key) {
    val <- .jcall(
        obj = "jdplus/sa/base/workspace/Utility",
        returnSig = "S",
        method = "getSingleMetaData",
        jsai,
        as.character(key)
    )
    return(val)
}

#' @rdname get_metadata
#' @export
get_ts_metadata <- function(jsai, key) {
    val <- .jcall(
        obj = "jdplus/sa/base/workspace/Utility",
        returnSig = "S",
        method = "getSingleTsMetaData",
        jsai,
        as.character(key)
    )
    return(val)
}
