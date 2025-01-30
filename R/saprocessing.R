#' @include saitem.R
NULL

#' @name .jws_sap_count
#' @export
.jsap_sa_count <- function(jsap) {
    return(.jcall(jsap, "I", "size"))
}

#' @title Get the name of a SAProcessing or a SaItem
#'
#' @description
#' Functions to retrieve the name of a SAProcessing (`.jsap_name()`) or SaItem (`.jsai_name()`).
#'
#' @param jsap,jsa the object to retrieve the name from.
#'
#' @export
.jsap_name <- function(jsap) {
    return(.jcall(jsap, "S", "getName"))
}


#' @name make_copy
#' @export
.jsap_make_copy <- function(jsap) {
    return(.jcall(jsap, "Ljdplus/sa/base/workspace/MultiProcessing;", "makeCopy"))
}


#' @name .jws_sap
#' @export
.jsap_sai <- function(jsap, idx) {
    if (is.jnull(jsap) || idx < 1) {
        return(NULL)
    }
    return(.jcall(jsap, "Ljdplus/sa/base/api/SaItem;", "get", as.integer(idx - 1)))
}

#' @title Get the Java name of sa_items
#'
#' @description
#' This function is used to retrieve the Java names of all the \code{sa_items}
#' contained in a \code{SA-Processing}.
#'
#' @param jsap the java object representing the \code{SA-Processing}
#'
#' @return A vector \code{character}.
#'
#' @seealso Other functions to retrieve the name of JDemetra+ objects
#' (\code{workspace}, \code{SA-Processing} or \code{sa-item}):
#' \code{\link{.jsai_name}}, \code{\link{.jsap_name}}.
#'
#' @examples \donttest{
#'
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#'
#' jws <- .jws_new()
#' jsap1 <- .jws_sap_new(jws, "sa1")
#'
#' add_sa_item(jsap1, name = "x13", x = rjd3x13::x13(y))
#' add_sa_item(jsap1, name = "tramo", x = rjd3tramoseats::tramoseats(y))
#' add_sa_item(jsap1, name = "x13-2", x = y, rjd3x13::x13_spec())
#' add_sa_item(jsap1, name = "tramo-2", x = y, rjd3tramoseats::tramoseats_spec())
#'
#' print(.jsap_sa_name(jsap1))
#' }
#'
#' @name .jsap_name
#' @export
.jsap_sa_name <- function(jsap) {
    if (is.jnull(jsap)) {
        return(NULL)
    }

    n <- .jcall(obj = jsap, returnSig = "I", method = "size")
    if (n == 0) {
        return(NULL)
    }

    names_sa <- vapply(
        X = seq_len(n),
        FUN = function(i) {
            .jsai_name(.jsap_sai(jsap, i))
        },
        FUN.VALUE = character(1)
    )

    return(names_sa)
}

#' @name read_workspace
#' @export
read_sap <- function(jsap) {
    n <- .jcall(jsap, "I", "size")
    if (n == 0) {
        return(NULL)
    }
    all <- lapply(seq_len(n), function(i) {
        read_sai(.jsap_sai(jsap, i))
    })
    names <- lapply(seq_len(n), function(i) {
        .jsai_name(.jsap_sai(jsap, i))
    })
    names(all) <- names
    return(all)
}

#' @name read_workspace
#' @export
.jread_sap <- function(jsap) {
    n <- .jcall(jsap, "I", "size")
    if (n == 0) {
        return(NULL)
    }
    all <- lapply(seq_len(n), function(i) {
        .jsa_jresults(.jsap_sai(jsap, i))
    })
    names <- lapply(seq_len(n), function(i) {
        .jsai_name(.jsap_sai(jsap, i))
    })
    names(all) <- names
    return(all)
}

#' @name refresh
#' @export
.jsap_refresh <- function(jsap,
                          policy = c("FreeParameters", "Complete",
                                     "Outliers_StochasticComponent",
                                     "Outliers", "FixedParameters",
                                     "FixedAutoRegressiveParameters", "Fixed"),
                          period = 0,
                          start = NULL,
                          end = NULL,
                          info = c("All", "Data", "None")) {
    policy <- match.arg(policy)
    info <- match.arg(info)
    jdom <- rjd3toolkit::.jdomain(period, start, end)
    output <- .jcall(
        obj = jsap,
        returnSig = "Ljdplus/sa/base/workspace/MultiProcessing;",
        method = "refresh", policy, jdom, info
    )
    return(output)
}


#' @title Add SAItem to SAProcessing
#'
#' @param jsap the SAProcessing.
#' @param name the name of SAItem.
#' @param x either a seasonal adjustment model (from [rjd3x13::x13()] or
#' [rjd3tramoseats::tramoseats()]), a SaItem or a `"ts"` object.
#' @param spec the specification to use when `x` is a `"ts"` object.
#' @param ... other unused parameters.
#'
#' @examples
#' dir <- tempdir()
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' jws <- .jws_new()
#' jsap1 <- .jws_sap_new(jws, "sa1")
#' add_sa_item(jsap1, name = "x13", x = rjd3x13::x13(y))
#' add_sa_item(jsap1, name = "tramo", x = rjd3tramoseats::tramoseats(y))
#' add_sa_item(jsap1, name = "x13-2", x = y, rjd3x13::x13_spec())
#' add_sa_item(jsap1, name = "tramo-2", x = y, rjd3tramoseats::tramoseats_spec())
#' save_workspace(jws, file.path(dir, "workspace.xml"))
#' @export
add_sa_item <- function(jsap, name, x, spec, ...) {
    UseMethod("add_sa_item", x)
}
#' @export
add_sa_item.ts <- function(jsap, name, x, spec, ...) {
    jts <- rjd3toolkit::.r2jd_tsdata(x)
    if (inherits(spec, "JD3_X13_SPEC")) {
        jspec <- rjd3x13::.r2jd_spec_x13(spec)
    } else if (inherits(spec, "JD3_TRAMOSEATS_SPEC")) {
        jspec <- rjd3tramoseats::.r2jd_spec_tramoseats(spec)
    } else {
        stop("wrong type of spec")
    }
    .jcall(
        jsap, "V", "add",
        name,
        jts,
        .jcast(jspec, "jdplus/sa/base/api/SaSpecification")
    )
}
#' @export
add_sa_item.default <- function(jsap, name, x, spec, ...) {
    if (inherits(x, "JD3_X13_OUTPUT")) {
        y <- x$result$preadjust$a1
        spec <- x$estimation_spec
    } else if (inherits(x, "JD3_TRAMOSEATS_OUTPUT")) {
        y <- x$result$final$series$data
        spec <- x$estimation_spec
    } else if (inherits(x$estimationSpec, c("JD3_X13_SPEC", "JD3_TRAMOSEATS_SPEC"))) {
        y <- x$ts
        spec <- x$estimationSpec
    } else {
        stop("wrong type of spec")
    }
    add_sa_item.ts(
        jsap = jsap,
        x = y,
        spec = spec,
        name = name,
        ...
    )
}

#' @export
add_sa_item.jobjRef <- function(jsap, name, x, spec, ...) {
    if (.jinstanceof(x, "jdplus/sa/base/api/SaItem")) {
        .jcall(jsap, "V", "add", x)
        if (!missing(name)) {
            set_name(jsap, name = name, idx = .jsap_sa_count(jsap))
        }
    } else {
        stop("x is not SaItem")
    }
    invisible(TRUE)
}

#' @title Replace or Remove a SaItem
#'
#' @description
#' `replace_sa_item()` replaces a SaItem of a SAProcessing and `remove_sa_item()` removes a SaItem from a SAProcessing
#'
#' @param jsap the SAProcessing to modify.
#' @param jsa the new SaItem.
#' @param idx index of the target SaItem.
#' @export
replace_sa_item <- function(jsap, idx, jsa) {
    .jcall(
        obj = jsap, returnSig = "V", method = "set",
        as.integer(idx - 1), jsa
    )
}

#' @name replace_sa_item
#' @export
remove_sa_item <- function(jsap, idx) {
    .jcall(
        obj = jsap, returnSig = "V", method = "remove",
        as.integer(idx - 1)
    )
}

#' Remove all sa-item from a \code{SA-Processing}
#'
#' @description
#' This functions clear a \code{SA-Processing} by removing all the sa-item contained.
#'
#' @param jsap the SAProcessing to modify.
#'
#' @return \code{NULL} returned invisibly
#'
#' @name replace_sa_item
#' @export
remove_all_sa_item <- function(jsap) {
    .jcall(obj = jsap, returnSig = "V", method = "removeAll")
    return(invisible(TRUE))
}

#'
#' Copy & paste series from one \code{SA-Processing} to another
#'
#' @param jsap_from The SA-Processing from which to take the series
#' @param jsap_to The SA-Processing in which to paste the series
#' @param selected_series The vector containing the series-to-update's names.
#' @param print_indications A boolean to print indications on the processing status (optional)
#'
#' @return \code{NULL} returned invisibly
#'
#' @details
#' If \code{selected_series} is missing, all series from \code{jsap_from} will be copied.
#' In this context, the word series refers to \code{sa-item}.
#'
#' @name replace_sa_item
#' @export
transfer_series <- function(jsap_from, jsap_to, selected_series,
                            print_indications = TRUE) {
    sap_from_sa_name <- .jsap_sa_name(jsap_from)
    sap_to_sa_name <- .jsap_sa_name(jsap_to)

    if (missing(selected_series) || is.null(selected_series)) {
        selected_series <- sap_from_sa_name
    }

    if (!all(selected_series %in% sap_from_sa_name)) {
        missing_series <- selected_series[!selected_series %in% sap_from_sa_name]
        stop("The series ",
             toString(missing_series),
             " are missing from the first SA Processing. ",
             "The replacement wasn't performed.")
    }

    for (serie_name in selected_series) {
        index_from <- which(serie_name == sap_from_sa_name)
        if (length(index_from) > 1) {
            stop("Several series from first SA Processing have the same name : ", serie_name)
        }
        jsa1 <- .jsap_sai(jsap_from, idx = index_from)

        index_to <- which(serie_name == sap_to_sa_name)
        if (length(index_to) > 1) {
            stop("Several series from second SA Processing have the same name : ", serie_name)
        } else if (length(index_to) == 0) {
            add_sa_item(jsap = jsap_to, name = serie_name, x = jsa1)
        } else {
            replace_sa_item(jsap = jsap_to, jsa = jsa1, idx = index_to)
        }

        if (print_indications) {
            print(paste0("Serie ", serie_name, ": Transfered."))
        }
    }

    if (print_indications) {
        print(paste0("Done"))
    }

    return(invisible(NULL))
}


#' Set Specification or Data of a SaItem
#'
#' @inheritParams replace_sa_item
#' @param spec the new specification.
#' @export
set_specification <- function(jsap, idx, spec) {
    if (inherits(spec, "JD3_X13_SPEC")) {
        jspec <- rjd3x13::.r2jd_spec_x13(spec)
    } else if (inherits(spec, "JD3_TRAMOSEATS_SPEC")) {
        jspec <- rjd3tramoseats::.r2jd_spec_tramoseats(spec)
    } else {
        stop("wrong type of spec")
    }
    jspec <- .jcast(jspec, "jdplus/sa/base/api/SaSpecification")
    jsai <- .jsap_sai(jsap, idx = idx)
    jsai <- .jcall(
        jsai,
        "Ljdplus/sa/base/api/SaItem;",
        "withSpecification",
        jspec
    )
    replace_sa_item(jsap, jsa = jsai, idx = idx)
}
#' @name set_specification
#' @export
set_domain_specification <- function(jsap, idx, spec) {
    if (inherits(spec, "JD3_X13_SPEC")) {
        jspec <- rjd3x13::.r2jd_spec_x13(spec)
    } else if (inherits(spec, "JD3_TRAMOSEATS_SPEC")) {
        jspec <- rjd3tramoseats::.r2jd_spec_tramoseats(spec)
    } else {
        stop("wrong type of spec")
    }
    jspec <- .jcast(jspec, "jdplus/sa/base/api/SaSpecification")
    jsai <- .jsap_sai(jsap, idx = idx)
    jsai <- .jcall(
        jsai,
        "Ljdplus/sa/base/api/SaItem;",
        "withDomainSpecification",
        jspec
    )
    replace_sa_item(jsap, jsa = jsai, idx = idx)
}
#' Get/Set the Raw Data of a SaItem
#'
#' @inheritParams replace_sa_item
#' @param y the new raw time serie.
#' @param jsa a SaItem.
#' @export
set_raw_data <- function(jsap, idx, y) {
    .jcall(jsap, "V", "setData", as.integer(idx - 1), rjd3toolkit::.r2jd_tsdata(y))
}

#' @name set_raw_data
#' @export
get_raw_data <- function(jsa) {
    jts <- .jcall(
        .jcall(jsa, "Ljdplus/sa/base/api/SaDefinition;", "getDefinition"),
        "Ljdplus/toolkit/base/api/timeseries/Ts;", "getTs"
    )
    rjd3toolkit::.jd2r_tsdata(.jcall(jts, "Ljdplus/toolkit/base/api/timeseries/TsData;", "getData"))
}

#' Get/Set the time series of a SaItem
#'
#' @inheritParams set_raw_data
#' @param y a "full" time series (jd3-like).
#' @export
set_ts <- function(jsap, idx, y) {
    jsai <- .jsap_sai(jsap, idx = idx)
    jsai <- .jcall(
        jsai,
        "Ljdplus/sa/base/api/SaItem;",
        "withTs",
        rjd3toolkit::.r2jd_ts(y)
    )
    replace_sa_item(jsap, jsa = jsai, idx = idx)
}

#' @name set_ts
#' @export
get_ts <- function(jsa) {
    jts <- .jcall(
        .jcall(jsa, "Ljdplus/sa/base/api/SaDefinition;", "getDefinition"),
        "Ljdplus/toolkit/base/api/timeseries/Ts;", "getTs"
    )
    return(rjd3toolkit::.jd2r_ts(jts))
}
#' Get/Set SaItem Comment
#'
#' @inheritParams set_raw_data
#' @param comment character containing the comment.
#' @export
set_comment <- function(jsap, idx, comment) {
    jsai <- .jsap_sai(jsap, idx = idx)
    jsai <- .jcall(
        jsai,
        "Ljdplus/sa/base/api/SaItem;",
        "withComment",
        comment
    )
    replace_sa_item(jsap, jsa = jsai, idx = idx)
}


#' @name set_comment
#' @export
get_comment <- function(jsa) {
    .jcall(jsa, "S", "getComment")
}

#' Set the name associated to a SaItem Comment
#'
#' @inheritParams set_raw_data
#' @param name character containing the name of the SAItem.
#' @seealso [.jsai_name()]
#' @export
set_name <- function(jsap, idx, name) {
    jsai <- .jsap_sai(jsap, idx = idx)
    jsai <- .jcall(
        jsai,
        "Ljdplus/sa/base/api/SaItem;",
        "withName",
        name
    )
    replace_sa_item(jsap, jsa = jsai, idx = idx)
}

# set_metadata <- function(jsap, ref_jsa, idx) {
#   jsa <- .jsap_sai(jsap, idx = idx)
#   jsa <- jsa$withInformations(ref_jsa$getMeta())
#   replace_sa_item(jsap, jsa = jsa, idx = idx)
# }
#' Set Time Series Metadata of a SaItem
#'
#' Function to set the time series metadata of a SaItem (provider, source of the data...).
#' `set_ts_metadata()` uses the metadata of another SaItem while `put_ts_metadata()`
#' allows to update a specific key with a new information.
#'
#' @inheritParams set_raw_data
#' @param ref_jsa a reference SaItem containing the metadata.
#' @param key key of the metadata.
#' @param value value of the metadata.
#'
#' @export
#' @examples
#' # Change the file of a given item
#' file <- system.file("workspaces", "test.xml", package = "rjd3workspace")
#' jws <- .jws_load(file)
#' jsap <- .jws_sap(jws, 1)
#' jsa <- .jsap_sai(jsap, 1)
#' nid <- rjd3providers::spreadsheet_change_file(.jsa_ts_metadata(jsa, "@id"), "test.xlsx")
#' put_ts_metadata(jsap, 1, "@id", nid)
#' jsa <- .jsap_sai(jsap, 1)
#' .jsa_ts_metadata(jsa, "@id")
set_ts_metadata <- function(jsap, idx, ref_jsa) {

    jsai <- .jsap_sai(jsap, idx = idx)
    jts <- .jcall(
        .jcall(jsai, "Ljdplus/sa/base/api/SaDefinition;", "getDefinition"),
        "Ljdplus/toolkit/base/api/timeseries/Ts;", "getTs"
    )
    jts_ref <- .jcall(
        .jcall(ref_jsa, "Ljdplus/sa/base/api/SaDefinition;", "getDefinition"),
        "Ljdplus/toolkit/base/api/timeseries/Ts;", "getTs"
    )
    jtsbuilder <- .jcall(
        jts, "Ljdplus/toolkit/base/api/timeseries/Ts$Builder;",
        "toBuilder"
    )
    # jts_ref$getMeta()$getClass()$descriptorString()
    # .jcall(jtsbuilder,  "Ljdplus/toolkit/base/api/timeseries/Ts$Builder;",
    #        "meta",
    #        .jcall(jts_ref, "Ljava/util/Map;", "getMeta"))
    jts <- jtsbuilder$
        meta(.jcall(jts_ref, "Ljava/util/Map;", "getMeta"))$
        moniker(.jcall(jts_ref, "Ljdplus/toolkit/base/api/timeseries/TsMoniker;", "getMoniker"))$
        build()
    jsai <- .jcall(
        obj = jsai,
        returnSig = "Ljdplus/sa/base/api/SaItem;",
        method = "withTs", jts
    )
    replace_sa_item(jsap, jsa = jsai, idx = idx)
}

#' @name set_ts_metadata
#' @export
put_ts_metadata <- function(jsap, idx, key, value) {
    jsai <- .jsap_sai(jsap, idx = idx)
    jsai <- .jcall(
        "jdplus/sa/base/workspace/Utility",
        "Ljdplus/sa/base/api/SaItem;",
        "withTsMetaData",
        jsai, key, value
    )
    replace_sa_item(jsap, jsa = jsai, idx = idx)
}


#' Get/Set SaItem Priority
#'
#' @inheritParams set_raw_data
#' @param priority integer containing the priority.
#' @export
set_priority <- function(jsap, idx, priority = 0) {
    jsai <- .jsap_sai(jsap, idx = idx)
    jsai <- .jcall(
        jsai,
        "Ljdplus/sa/base/api/SaItem;",
        "withPriority",
        as.integer(priority)
    )
    replace_sa_item(jsap, jsa = jsai, idx = idx)
}
#' @name set_priority
#' @export
get_priority <- function(jsa) {
    .jcall(jsa, "I", "getPriority")
}
