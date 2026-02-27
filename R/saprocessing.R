#' @include saitem.R
NULL

#' @name ws_sap_count
#' @export
sap_sai_count <- function(jsap) {
    return(.jcall(jsap, "I", "size"))
}

#' @title Get the name of a SAProcessing or one (or all) Sa-item
#'
#' @description
#' Functions to retrieve the name of a SAProcessing (`sap_name()`) or Sa-item
#' (`sai_name()`) or all SA-item (`sap_sai_names()`) .
#'
#' @param jsap,jsai the object to retrieve the name from.
#' @returns A vector \code{character}.
#'
#' @examplesIf current_java_version >= minimal_java_version
#'
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml",
#'                     package = "rjd3workspace")
#' jws <- jws_open(file)
#'
#' # Extract 2nd SA-Processing
#' jsap_2 <- jws_sap(jws, 2)
#'
#' # Retrieve the name
#' sap_name(jsap_2)
#'
#' # Retrieve all the SA-items names
#' sap_sai_names(jsap_2)
#'
#' @export
sap_name <- function(jsap) {
    return(.jcall(jsap, "S", "getName"))
}

#' @name make_copy
#' @export
jsap_make_copy <- function(jsap) {
    return(.jcall(jsap, "Ljdplus/sa/base/workspace/MultiProcessing;", "makeCopy"))
}

#' @name jws_sap
#' @export
jsap_sai <- function(jsap, idx) {
    if (is.jnull(jsap) || idx < 1L) {
        return(NULL)
    }
    return(.jcall(jsap, "Ljdplus/sa/base/api/SaItem;", "get", as.integer(idx - 1L)))
}
#' @name sap_name
#' @export
sap_sai_names <- function(jsap) {
    if (is.jnull(jsap)) {
        return(NULL)
    }

    n <- .jcall(obj = jsap, returnSig = "I", method = "size")
    if (n == 0L) {
        return(NULL)
    }

    sai_names <- vapply(
        X = seq_len(n),
        FUN = function(i) {
            sai_name(jsap_sai(jsap, i))
        },
        FUN.VALUE = character(1L)
    )

    return(sai_names)
}

#' @name read_workspace
#' @export
read_sap <- function(jsap) {
    n <- .jcall(jsap, "I", "size")
    if (n == 0L) {
        return(NULL)
    }
    all <- lapply(seq_len(n), function(i) {
        read_sai(jsap_sai(jsap, i))
    })
    names <- lapply(seq_len(n), function(i) {
        sai_name(jsap_sai(jsap, i))
    })
    names(all) <- names
    return(all)
}

#' @name read_workspace
#' @export
jread_sap <- function(jsap) {
    n <- .jcall(jsap, "I", "size")
    if (n == 0L) {
        return(NULL)
    }
    all <- lapply(seq_len(n), function(i) {
        .jsai_jresults(jsap_sai(jsap, i))
    })
    names <- lapply(seq_len(n), function(i) {
        sai_name(jsap_sai(jsap, i))
    })
    names(all) <- names
    return(all)
}

#' @name refresh
#' @export
jsap_refresh <- function(jsap,
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


#' @title Add a SA-item to a SAProcessing
#'
#' @param jsap SAProcessing.
#' @param name name of the SA-item to be added.
#' @param x either a seasonal adjustment model (from [rjd3x13::x13()] or
#' [rjd3tramoseats::tramoseats()]), a SA-item object, `"ts"` object.
#' @param spec specification to use when `x` is a `"ts"` object.
#'
#' @returns \code{NULL} returned invisibly
#'
#' @examplesIf current_java_version >= minimal_java_version
#'
#' dir <- tempdir()
#'
#' # Raw series
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#'
#' # Creating an empty workspace and SAProcessing
#' jws <- jws_new()
#' jsap1 <- jws_sap_new(jws, "sap1")
#'
#' # Adding SA-item as estimation result
#' # Estimation with rjd313
#' add_sa_item(jsap1, name = "series_1", x = rjd3x13::x13(y))
#'
#' # Estimation with rjd3tramoseats
#' add_sa_item(jsap1, name = "series_2", x = rjd3tramoseats::tramoseats(y))
#'
#' # Adding SA-item as raw series + specification
#' add_sa_item(jsap1, name = "series_3", x = y, rjd3x13::x13_spec("RSA3"))
#' add_sa_item(jsap1, name = "series_4", x = y, rjd3tramoseats::tramoseats_spec("RSAFull"))
#' rws <- read_workspace(jws)
#' rws$processing$sap1$series_4
#'
#' # Writing the workspace
#' save_workspace(jws, file.path(dir, "workspace.xml"))
#'
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
            set_name(jsap, name = name, idx = sap_sai_count(jsap))
        }
    } else {
        stop("x is not SaItem")
    }
    invisible(TRUE)
}

#' @title Replace or Remove a SA-item
#'
#' @description
#' `replace_sa_item()` replaces a SA-item in a SAProcessing and
#' `remove_sa_item()` removes a SA-item from a SAProcessing
#' `remove_all_sa_item()` removes all SA-item from a SAProcessing
#'
#' @param jsap SAProcessing to be modified.
#' @param jsai new SA-item (for replacement).
#' @param idx index of the target SA-item.
#' @returns \code{NULL} returned invisibly
#' @export
replace_sa_item <- function(jsap, idx, jsai) {
    .jcall(
        obj = jsap, returnSig = "V", method = "set",
        as.integer(idx - 1L), jsai
    )
}
#' @name replace_sa_item
#' @export
remove_sa_item <- function(jsap, idx) {
    .jcall(
        obj = jsap, returnSig = "V", method = "remove",
        as.integer(idx - 1L)
    )
}
#' @name replace_sa_item
#' @export
remove_all_sa_item <- function(jsap) {
    .jcall(obj = jsap, returnSig = "V", method = "removeAll")
    return(invisible(TRUE))
}

#' Copy & paste SA-items from one \code{SA-Processing} to another
#'
#' @param jsap_from SA-Processing from which to take the SA-items
#' @param jsap_to SA-Processing to which paste the SA-items
#' @param selected_sa_items vector containing the SA-items names to be updated.
#' @param print_indications A boolean to print indications on the processing status (optional)
#'
#' @returns \code{NULL} returned invisibly
#'
#' @details
#' If \code{selected_sa_items} is missing, all SA-items from \code{jsap_from} will be copied.
#' @export
transfer_sa_item <- function(jsap_from, jsap_to, selected_sa_items,
                             print_indications = TRUE) {
    sap_from_sai_name <- sap_sai_names(jsap_from)
    sap_to_sai_name <- sap_sai_names(jsap_to)

    if (missing(selected_sa_items) || is.null(selected_sa_items)) {
        selected_sa_items <- sap_from_sai_name
    }

    if (!all(selected_sa_items %in% sap_from_sai_name)) {
        missing_series <- selected_sa_items[!selected_sa_items %in% sap_from_sai_name]
        stop("The SA-items ",
             toString(missing_series),
             " are missing from the first SA Processing. ",
             "The replacement wasn't performed.")
    }

    for (serie_name in selected_sa_items) {
        index_from <- which(serie_name == sap_from_sai_name)
        if (length(index_from) > 1L) {
            stop("Several SA-items from first SAProcessing have the same name : ", serie_name)
        }
        jsai1 <- jsap_sai(jsap_from, idx = index_from)

        index_to <- which(serie_name == sap_to_sai_name)
        if (length(index_to) > 1L) {
            stop("Several SA-items from second SA Processing have the same name : ", serie_name)
        } else if (length(index_to) == 0L) {
            add_sa_item(jsap = jsap_to, name = serie_name, x = jsai1)
        } else {
            replace_sa_item(jsap = jsap_to, jsai = jsai1, idx = index_to)
        }

        if (print_indications) {
            cat("Serie ", serie_name, ": Transfered.\n")
        }
    }

    if (print_indications) {
        cat("Done.\n")
    }

    return(invisible(NULL))
}
#' Set Specification in a Sa-Item
#'
#' @inheritParams replace_sa_item
#' @param spec new specification generated with [rjd3x13::x13_spec()] or [rjd3tramoseats::tramoseats_spec()]
#' @returns \code{NULL} returned invisibly
#' @examplesIf current_java_version >= minimal_java_version
#'
#' # Create a (customized) spec) spec
#' library(rjd3x13)
#'
#' spec <- rjd3x13::x13_spec("rsa3") |>
#'     rjd3toolkit::set_basic(type = "From", d0 = "2012-01-01")
#'
#' # Load a Workspace to modify
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' jws <- jws_open(file)
#'
#' # Select SAProcessing with the target SA-item
#' sap1 <- jws_sap(jws, 1)
#'
#' # Set specification in targeted SA-item
#' set_specification(sap1, 2, spec)
#'
#' # Set domain specification in selected SA-item
#' set_domain_specification(sap1, 3, spec)
#'
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
    jsai <- jsap_sai(jsap, idx = idx)
    jsai <- .jcall(
        jsai,
        "Ljdplus/sa/base/api/SaItem;",
        "withSpecification",
        jspec
    )
    replace_sa_item(jsap, jsai = jsai, idx = idx)
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
    jsai <- jsap_sai(jsap, idx = idx)
    jsai <- .jcall(
        jsai,
        "Ljdplus/sa/base/api/SaItem;",
        "withDomainSpecification",
        jspec
    )
    replace_sa_item(jsap, jsai = jsai, idx = idx)
}
#' @title Get/Set Raw Data in a SA-item
#'
#' @inheritParams replace_sa_item
#' @param y new raw time series.
#' @param jsai a SA-item.
#' @returns \code{NULL} returned invisibly (set) or TS object (get)
#'
#' @examplesIf current_java_version >= minimal_java_version
#'
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' jws <- jws_open(file)
#'
#' # Select SAProcessing
#' sap1 <- jws_sap(jws, 1)
#'
#' # Select SA-item
#' sai1 <- jsap_sai(sap1, 3) # java object sai
#' tail(get_raw_data(sai1))
#'
#' new_raw_data <- rjd3toolkit::ABS$X0.2.15.10.M
#' set_raw_data(sap1,3,new_raw_data)
#'
#' sai1 <- jsap_sai(sap1,3) # reload SA-item
#' tail(get_raw_data(sai1)) # get raw data
#'
#' @export
set_raw_data <- function(jsap, idx, y) {
    .jcall(jsap, "V", "setData", as.integer(idx - 1L), rjd3toolkit::.r2jd_tsdata(y))
}

#' @name set_raw_data
#' @export
get_raw_data <- function(jsai) {
    jts <- .jcall(
        .jcall(jsai, "Ljdplus/sa/base/api/SaDefinition;", "getDefinition"),
        "Ljdplus/toolkit/base/api/timeseries/Ts;", "getTs"
    )
    rjd3toolkit::.jd2r_tsdata(.jcall(jts, "Ljdplus/toolkit/base/api/timeseries/TsData;", "getData"))
}

#' @title Get/Set the (JDemetra+) time series of a SA-item
#'
#' @description
#' (JDemetra+) time series contains more information than raw data,
#' which can be manipulated with `set_raw_data()` and `get_raw_data()`
#'
#' @inheritParams set_raw_data
#' @param y a "full" time series (jd3-like).
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' # Load a workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' my_jws <- jws_open(file)
#'
#' library("rjd3providers")
#' data_path <- system.file("data", "IPI_nace4.csv", package = "rjd3workspace")
#'
#' ts_object <- txt_series(
#'     file = data_path,
#'     series = 1L,
#'     delimiter = "SEMICOLON",
#'     fmt.date = "dd/MM/yyyy"
#' )
#'
#' # Select the first SA-Processing
#' jsap <- jws_sap(my_jws, 1L)
#'
#' # Change the ts object
#' set_ts(jsap = jsap, idx = 1L, ts_object)
#'
#' jsai1 <- jsap_sai(jsap, 1L)
#' jsai2 <- jsap_sai(jsap, 2L)
#' jsai3 <- jsap_sai(jsap, 3L)
#'
#' # Get the ts object
#' get_ts(jsai1)
#' get_ts(jsai2)
#' get_ts(jsai3)
#'
set_ts <- function(jsap, idx, y) {
    jsai <- jsap_sai(jsap, idx = idx)
    jsai <- .jcall(
        jsai,
        "Ljdplus/sa/base/api/SaItem;",
        "withTs",
        rjd3toolkit::.r2jd_ts(y)
    )
    replace_sa_item(jsap, jsai = jsai, idx = idx)
}

#' @name set_ts
#' @export
get_ts <- function(jsai) {
    jts <- .jcall(
        .jcall(jsai, "Ljdplus/sa/base/api/SaDefinition;", "getDefinition"),
        "Ljdplus/toolkit/base/api/timeseries/Ts;", "getTs"
    )
    return(rjd3toolkit::.jd2r_ts(jts))
}
#' Get/Set Comment from a SA-item
#'
#' @inheritParams set_raw_data
#' @param comment character containing the comment.
#' @returns \code{NULL} returned invisibly
#'
#' @examplesIf current_java_version >= minimal_java_version
#'
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' jws <- jws_open(file)
#'
#' # Select SAProcessing
#' jsap1 <- jws_sap(jws, 1L)
#'
#' # Add a comment
#' set_comment(jsap1, 2L, "data collection changed in 2012")
#'
#' jsai2 <- jsap_sai(jsap1, 2L)
#' get_comment(jsai2)
#'
#' @export
set_comment <- function(jsap, idx, comment) {
    jsai <- jsap_sai(jsap, idx = idx)
    jsai <- .jcall(
        jsai,
        "Ljdplus/sa/base/api/SaItem;",
        "withComment",
        comment
    )
    replace_sa_item(jsap, jsai = jsai, idx = idx)
}


#' @name set_comment
#' @export
get_comment <- function(jsai) {
    .jcall(jsai, "S", "getComment")
}

#' Set the name of a SA-item
#'
#' @inheritParams set_raw_data
#' @param name character corresponding to the new name
#' @returns \code{NULL} returned invisibly
#' @seealso [sai_name()]
#' @examplesIf current_java_version >= minimal_java_version
#'
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' jws <- jws_open(file)
#'
#' # Select SAProcessing
#' sap1 <- jws_sap(jws,1)
#'
#' # Select SA-item
#' sai1 <- jsap_sai(sap1,3) # java object sai
#'
#' # set name
#' set_name(sap1,3,"RF1011_1")
#'
#' # check
#' sai1 <- jsap_sai(sap1,3) # reload sai
#' sai_name(sai1) #get name
#'
#' @export
set_name <- function(jsap, idx, name) {
    jsai <- jsap_sai(jsap, idx = idx)
    jsai <- .jcall(
        jsai,
        "Ljdplus/sa/base/api/SaItem;",
        "withName",
        name
    )
    replace_sa_item(jsap, jsai = jsai, idx = idx)
}

#' @title Set (JDemetra+) Metadata of a SA-item
#'
#' @description
#' Function to set the metadata of a SA-item.
#'
#' `XXX_ts_metadata()` set the time series metadata of a SA-item (provider,
#' source of the data...). `XXX_metadata()` set any metadata to a SA-Item.
#'
#' `set_XXX()` uses the metadata of another SA-item while `put_XXX()`
#' allows to update a specific key with a new information.
#'
#' @inheritParams set_raw_data
#' @param ref_jsai a reference SA-item containing the metadata.
#' @param key key of the metadata.
#' @param value value of the metadata.
#'
#' @export
#' @examplesIf current_java_version >= minimal_java_version
#'
#' # Change the file of a given item
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' jws <- jws_open(file)
#' jsap <- jws_sap(jws, 1)
#' jsai <- jsap_sai(jsap, 1)
#' nid <- rjd3providers::txt_change_file(get_ts_metadata(jsai, "@id"), "test.csv")
#' put_ts_metadata(jsap, 1, "@id", nid)
#'
#' jsai <- jsap_sai(jsap, 1)
#' get_ts_metadata(jsai, "@id")
#'
set_ts_metadata <- function(jsap, idx, ref_jsai) {

    jsai <- jsap_sai(jsap, idx = idx)
    jts <- .jcall(
        .jcall(jsai, "Ljdplus/sa/base/api/SaDefinition;", "getDefinition"),
        "Ljdplus/toolkit/base/api/timeseries/Ts;", "getTs"
    )
    jts_ref <- .jcall(
        .jcall(ref_jsai, "Ljdplus/sa/base/api/SaDefinition;", "getDefinition"),
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
    replace_sa_item(jsap, jsai = jsai, idx = idx)
}

#' @name set_ts_metadata
#' @export
put_ts_metadata <- function(jsap, idx, key, value) {
    jsai <- jsap_sai(jsap, idx = idx)
    jsai <- .jcall(
        "jdplus/sa/base/workspace/Utility",
        "Ljdplus/sa/base/api/SaItem;",
        "withTsMetaData",
        jsai, key, value
    )
    replace_sa_item(jsap, jsai = jsai, idx = idx)
}


#' @name set_ts_metadata
#' @export
set_metadata <- function(jsap, ref_jsai, idx) {
    jsai <- jsap_sai(jsap, idx = idx)
    jsai <- jsa$withInformations(ref_jsai$getMeta())
    replace_sa_item(jsap, jsai = jsai, idx = idx)
}

#' @name set_ts_metadata
#' @export
put_metadata <- function (jsap, idx, key, value) {
    jsai <- jsap_sai(jsap, idx = idx)

    meta <- .jcall(jsai, "Ljava/util/Map;", "getMeta")
    new_meta <- .jnew("java/util/HashMap", meta)

    jkey <- .jnew("java/lang/String", key)
    jvalue <- .jnew("java/lang/String", value)

    .jcall(
        obj = new_meta,
        returnSig = "Ljava/lang/Object;",
        method = "put",
        .jcast(jkey, "java/lang/Object"),
        .jcast(jvalue, "java/lang/Object")
    )

    jsai <- .jcall(
        obj = jsai,
        returnSig = "Ljdplus/sa/base/api/SaItem;",
        method = "withInformations",
        .jcast(new_meta, "java/util/Map")
    )

    replace_sa_item(jsap, jsai = jsai, idx = idx)
}



#' @title Get/Set SA-item Priority
#'
#' @inheritParams set_raw_data
#' @param priority integer containing the priority.
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#'
#' # Load a workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' my_jws <- jws_open(file)
#'
#' # Select the first SA-Processing and SA-Item
#' jsap <- jws_sap(my_jws, 1)
#' jsai <- jsap_sai(jsap, 1L)
#'
#' # Change priority
#' set_priority(jsap, idx = 1L, priority = 3L)
#'
#' # Retrieve priority
#' get_priority(jsai)
#'
#'
set_priority <- function(jsap, idx, priority = 0L) {
    jsai <- jsap_sai(jsap, idx = idx)
    jsai <- .jcall(
        jsai,
        "Ljdplus/sa/base/api/SaItem;",
        "withPriority",
        as.integer(priority)
    )
    replace_sa_item(jsap, jsai = jsai, idx = idx)
}

#' @name set_priority
#' @export
get_priority <- function(jsai) {
    .jcall(jsai, "I", "getPriority")
}
