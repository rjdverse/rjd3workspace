#' @include utils.R
NULL

#' @title Read an SA-item
#'
#' @description
#' `read_sai()` extracts all the information of a SA-item (see details).
#'
#' @param jsai Java SA-item object.
#'
#' @return a list
#'
#' @examplesIf jversion >= 17
#'
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' jws <- jws_open(file)
#'
#' # Select SAProcessing
#' jsap1 <- jws_sap(jws, 1)
#'
#' # Select SA-item (as java object)
#' jsai1 <- jsap_sai(jsap1, 3)
#'
#' @details A SA-item contains more information than just the results of an estimation.
#' Full information is extracted with the `read_sai()` function that
#' returns a list of 5 objects:
#' - `ts`: raw time series.
#' - `domainSpec`: initial specification. Reference when refreshing and relaxing constraints.
#' - `estimationSpec`: specification used for the current estimation.
#' - `pointSpec`: specification corresponding to the results of the current
#' estimation (fully identified model).
#' - `results`: results of the estimation.
#'
#' @export
#'
read_sai <- function(jsai) {
    #  if (! .jcall(jsai, "Z", "isProcessed"))
    #    stop("You must run 'jws_compute()' on your workspace.")

    jdef <- .jcall(jsai, "Ljdplus/sa/base/api/SaDefinition;", "getDefinition")

    jestimation <- .jcall(jsai, "Ljdplus/sa/base/api/SaEstimation;", "getEstimation")
    jrslt <- .jnull()
    if (!is.jnull(jestimation)) {
        jrslt <- .jcall(
            obj = jestimation,
            returnSig = "Ljdplus/toolkit/base/api/information/GenericExplorable;",
            method = "getResults"
        )
    }
    # ts
    jts <- .jcall(jdef, "Ljdplus/toolkit/base/api/timeseries/Ts;", "getTs")
    rts <- rjd3toolkit::.jd2r_ts(jts)

    jdspec <- .jcall(jdef, "Ljdplus/sa/base/api/SaSpecification;", "getDomainSpec")
    jspec <- .jcall(jdef, "Ljdplus/sa/base/api/SaSpecification;", "activeSpecification")
    spec <- NULL
    dspec <- NULL
    pspec <- NULL
    rslt <- NULL

    if (.jinstanceof(jspec, "jdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec")) {
        spec <- rjd3tramoseats::.jd2r_spec_tramoseats(.jcast(
            jspec,
            "jdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec"
        ))
        dspec <- rjd3tramoseats::.jd2r_spec_tramoseats(.jcast(
            jdspec,
            "jdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec"
        ))
        if (!is.jnull(jrslt)) {
            rslt <- rjd3tramoseats::.tramoseats_rslts(.jcast(
                jrslt,
                "jdplus/tramoseats/base/core/tramoseats/TramoSeatsResults"
            ))
            jpspec <- .jcall(
                obj = jestimation,
                returnSig = "Ljdplus/sa/base/api/SaSpecification;",
                method = "getPointSpec"
            )
            pspec <- rjd3tramoseats::.jd2r_spec_tramoseats(.jcast(
                jpspec,
                "jdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec"
            ))
        }
    } else if (.jinstanceof(jspec, "jdplus/x13/base/api/x13/X13Spec")) {
        spec <- rjd3x13::.jd2r_spec_x13(.jcast(jspec, "jdplus/x13/base/api/x13/X13Spec"))
        dspec <- rjd3x13::.jd2r_spec_x13(.jcast(jdspec, "jdplus/x13/base/api/x13/X13Spec"))
        if (!is.jnull(jrslt)) {
            rslt <- rjd3x13::.x13_rslts(.jcast(jrslt, "jdplus/x13/base/core/x13/X13Results"))
            jpspec <- .jcall(jestimation, "Ljdplus/sa/base/api/SaSpecification;", "getPointSpec")
            pspec <- rjd3x13::.jd2r_spec_x13(.jcast(jpspec, "jdplus/x13/base/api/x13/X13Spec"))
        }
    }
    return(list(
        ts = rts,
        domainSpec = dspec,
        estimationSpec = spec,
        pointSpec = pspec,
        results = rslt
    ))
}

#' @title Extract results from a SA-item
#'
#' @description
#' `.jsai_results()` extracts specific variables of the model of the SA-item while
#' `.jsai_jresults()` extracts the Java object of the results of a SA-item.
#'
#' @param jsai Java SA-item object.
#' @param items vector of characters containing the variables to extract.
#' See [rjd3x13::x13_dictionary()] or [rjd3tramoseats::tramoseats_dictionary()].
#' By default, extracts all the possible variables.
#'
#' @export
#'
.jsai_results <- function(jsai, items = NULL) {
    jestimation <- .jcall(jsai, "Ljdplus/sa/base/api/SaEstimation;", "getEstimation")
    if (is.jnull(jestimation)) {
        return(NULL)
    }
    jrslt <- .jcall(jestimation, "Ljdplus/toolkit/base/api/information/GenericExplorable;", "getResults")
    if (is.null(items)) {
        items <- rjd3toolkit::.proc_dictionary2(jrslt)
    }
    r <- lapply(items, function(t) {
        rjd3toolkit::.proc_data(jrslt, t)
    })
    names(r) <- items
    return(r)
}

#' @name .jsai_results
#' @export
.jsai_jresults <- function(jsai) {
    jestimation <- .jcall(jsai, "Ljdplus/sa/base/api/SaEstimation;", "getEstimation")
    if (is.jnull(jestimation)) {
        return(NULL)
    }
    jrslt <- .jcall(jestimation, "Ljdplus/toolkit/base/api/information/GenericExplorable;", "getResults")
    res <- rjd3toolkit::.jd3_object(jrslt, result = TRUE)
    return(res)
}


#' @name sap_name
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
#' @examplesIf jversion >= 17
#'
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
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
#'
get_metadata <- function(jsai, key) {
    val <- .jcall(
        obj = "jdplus/sa/base/workspace/Utility",
        returnSig = "S",
        method = "getSingleMetaData",
        jsai, as.character(key)
    )
    return(val)
}

#' @name get_metadata
#' @export
get_ts_metadata <- function(jsai, key) {
    val <- .jcall(
        obj = "jdplus/sa/base/workspace/Utility",
        returnSig = "S",
        method = "getSingleTsMetaData",
        jsai, as.character(key)
    )
    return(val)
}
