#' @include utils.R
NULL

#' Read SAItem
#'
#' `.jsa_results()` extracts specific variables of the model of the SAItem while
#' `.jsa_read()` extracts all the informations of a SAItem (see details).
#'  `.jsa_jresults()` extracts the Java object of the results of a SAItem.
#'
#' @param jsa Java SAItem object.
#' @param items vector of characters containing the variables to extract.
#' See [rjd3x13::x13_dictionary()] or [rjd3tramoseats::tramoseats_dictionary()].
#' By default, extracts all the possible variables.
#'
#' @details A SAItem contains more information than just the results of a model.
#' All those informations are extracted with the `.jsa_read()` function that
#' returns a list with 5 objects:
#'
#' - `ts`: the raw time series.
#' - `domainSpec`: initial specification. Reference for any relaxing of some
#' elements of the specification.
#' - `estimationSpec`: specification used for the current estimation.
#' - `pointSpec`: specification corresponding to the results of the current
#' estimation (fully identified model).
#' - `results`: the result of the model.
#' @export
.jsa_read <- function(jsa) {
    #  if (! .jcall(jsa, "Z", "isProcessed"))
    #    stop("You must run '.jws_compute()' on your workspace.")

    jdef <- .jcall(jsa, "Ljdplus/sa/base/api/SaDefinition;", "getDefinition")

    jestimation <- .jcall(jsa, "Ljdplus/sa/base/api/SaEstimation;", "getEstimation")
    jrslt <- .jnull()
    if (!is.jnull(jestimation)) {
        jrslt <- .jcall(jestimation, "Ljdplus/toolkit/base/api/information/Explorable;", "getResults")
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

#' @name .jsa_read
#' @export
.jsa_results <- function(jsa, items = NULL) {
    jestimation <- .jcall(jsa, "Ljdplus/sa/base/api/SaEstimation;", "getEstimation")
    if (is.jnull(jestimation)) {
        return(NULL)
    }
    jrslt <- .jcall(jestimation, "Ljdplus/toolkit/base/api/information/Explorable;", "getResults")
    if (is.null(items)) {
        items <- rjd3toolkit::.proc_dictionary2(jrslt)
    }
    r <- lapply(items, function(t) {
        rjd3toolkit::.proc_data(jrslt, t)
    })
    names(r) <- items
    return(r)
}

#' @name .jsa_read
#' @export
.jsa_jresults <- function(jsa) {
    jestimation <- .jcall(jsa, "Ljdplus/sa/base/api/SaEstimation;", "getEstimation")
    if (is.jnull(jestimation)) {
        return(NULL)
    }
    jrslt <- .jcall(jestimation, "Ljdplus/toolkit/base/api/information/Explorable;", "getResults")
    res <- rjd3toolkit::.jd3_object(jrslt, result = TRUE)
    return(res)
}


#' @name .jsap_name
#' @export
.jsa_name <- function(jsa) {
    return(.jcall(jsa, "S", "getName"))
}

#' Extract Java Metadata
#'
#' Extract specific metadata or time series metadata of a SAItem.
#'
#' @inheritParams .jsa_read
#' @param key key of the metadata.
#' @export
.jsa_metadata <- function(jsa, key) {
    val <- .jcall("jdplus/sa/base/workspace/Utility", "S", "getSingleMetaData", jsa, as.character(key))
    return(val)
}

#' @name .jsa_metadata
#' @export
.jsa_ts_metadata <- function(jsa, key) {
    val <- .jcall("jdplus/sa/base/workspace/Utility", "S", "getSingleTsMetaData", jsa, as.character(key))
    return(val)
}
