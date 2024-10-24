#' @include saprocessing.R
NULL

#' Create a workspace or a multi-processing
#'
#' Functions to create a 'JDemetra+' workspace (\code{.jws_new()}) and
#' to add a new multi-processing (\code{.jws_sap_new()}).
#'
#' @param modelling_context The context (from [rjd3toolkit::modelling_context()]).
#' @param jws A workspace object.
#' @param name Character name of the new SAProcessing.
#'
#' @examples
#' # To create an empty 'JDemetra+' workspace
#' jwk <- .jws_new()
#' jsap <- .jws_sap_new(jwk, "sa1")
#'
#' @export
.jws_new <- function(modelling_context = NULL) {
    jws <- .jnew("jdplus/sa/base/workspace/Ws")
    if (!is.null(modelling_context)) {
        set_context(jws, modelling_context)
    }
    return(jws)
}
#' @name .jws_new
#' @export
.jws_sap_new <- function(jws, name) {
    return(.jcall(jws, "Ljdplus/sa/base/workspace/MultiProcessing;", "newMultiProcessing", name))
}

#' @name .jws_add
#' @export
.jws_add <- function(jws, jsap) {
    .jcall(jws, "V", "add", jsap)
}

#' Copy Workspace or a SAProcessing
#'
#' @name make_copy
#' @param jws,jsap Java Workspace or Multiprocessing
#' @export
.jws_make_copy <- function(jws) {
    return(.jcall(jws, "Ljdplus/sa/base/workspace/Ws;", "makeCopy"))
}

#' Refresh Workspace or SAProcessing
#'
#' @inheritParams make_copy
#' @param policy the refresh policy to apply (see details).
#' @param period,start,end to specify the span on which outliers will not be
#' re-identified (i.e.: re-detected) when `policy = "Outliers"` or
#' `policy = "Outliers_StochasticComponent"`.
#' Span definition: \code{period}: numeric, number of observations in a year
#' (12, 4...). \code{start} and \code{end}: first and last date from which
#' outliers will not be re-identfied, defined as arrays of two elements: year
#' and first period (for example, if `period = 12`, `c(1980, 1)` for January
#' 1980). If they are not specified, the outliers will be re-identified on the
#' whole series.
#' @param info information to refresh.
#' @details
#'
#' Available refresh policies are:
#'
#' \strong{Current}: applying the current pre-adjustment reg-arima model and
#' adding the new raw data points as Additive Outliers (defined as new
#' intervention variables)
#'
#' \strong{Fixed}: applying the current pre-adjustment reg-arima model and
#' replacing forecasts by new raw data points.
#'
#' \strong{FixedParameters}: pre-adjustment reg-arima model is partially
#' modified: regression coefficients will be re-estimated but regression
#' variables, Arima orders and coefficients are unchanged.
#'
#' \strong{FixedAutoRegressiveParameters}: same as FixedParameters but Arima
#' Moving Average coefficients (MA) are also re-estimated, Auto-regressive (AR)
#' coefficients are kept fixed.
#'
#' \strong{FreeParameters}: all regression and Arima model coefficients are
#' re-estimated, regression variables and Arima orders are kept fixed.
#'
#' \strong{Outliers}: regression variables and Arima orders are kept fixed, but
#' outliers will be re-detected on the defined span, thus all regression and
#' Arima model coefficients are re-estimated
#'
#' \strong{Outliers_StochasticComponent}: same as "Outliers" but Arima model
#' orders (p,d,q)(P,D,Q) can also be re-identified.
#'
#' @name refresh
#' @export
.jws_refresh <- function(jws,
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
    return(.jcall(jws, "V", "refreshAll", policy, jdom, info))
}

#' Set Context of a Workspace
#'
#' @inheritParams .jws_new
#' @inheritParams .jws_open
#' @export
set_context <- function(jws, modelling_context = NULL) {
    if (!is.null(set_context)) {
        jcontext <- rjd3toolkit::.r2jd_modellingcontext(modelling_context)
        .jcall(jws, "V", "setContext", jcontext)
    }
}
#' Get Context from Workspace
#'
#' @param jws the workspace.
#'
#' @export
get_context <- function(jws) {
    jcntxt <- .jcall(jws, "Ljdplus/toolkit/base/api/timeseries/regression/ModellingContext;", "getContext")
    rjd3toolkit::.jd2r_modellingcontext(jcntxt)
}

#' Count the number of objects inside a workspace or SAProcessing
#'
#' Functions to count the number of SAProcessing inside a workspace (`jws_sap_count`) or
#' the number of SaItem inside a SAProcessing (`jsap_sa_count`).
#'
#' @param jws,jsap the workspace or the SAProcessing.
#'
#' @export
.jws_sap_count <- function(jws) {
    return(.jcall(jws, "I", "getMultiProcessingCount"))
}



#' Extract a SAProcessing or a SaItem
#'
#' @param jws,jsap the workspace or the SAProcessing.
#' @param idx index of the object to extract.
#'
#' @export
.jws_sap <- function(jws, idx) {
    return(.jcall(jws, "Ljdplus/sa/base/workspace/MultiProcessing;", "getMultiProcessing", as.integer(idx - 1)))
}




#' Load a 'JDemetra+' workpace
#'
#' `.jws_open()` loads a workspace and `.jws_compute()` computes it (to be able to get all the models).
#'
#' @param file the path to the 'JDemetra+' workspace to load.
#' By default a dialog box opens.
#'
#' @seealso [read_workspace()] to import all the models of a workspace.
#'
#' @export
.jws_open <- function(file) {
    if (missing(file) || is.null(file)) {
        if (Sys.info()[["sysname"]] == "Windows") {
            file <- utils::choose.files(
                caption = "Select a workspace",
                filters = c("JDemetra+ workspace (.xml)", "*.xml")
            )
        } else {
            file <- base::file.choose()
        }
        if (length(file) == 0) {
            stop("You have to choose a file !")
        }
    }
    if (!file.exists(file) || tools::file_ext(file) != ".xml") {
        stop("The file doesn't exist or isn't a .xml file !")
    }
    full_file_name <- full_path(file)
    jws <- .jcall("jdplus/sa/base/workspace/Ws", "Ljdplus/sa/base/workspace/Ws;", "open", full_file_name)
    return(jws)
}

#' @export
.jws_compute <- function(jws) {
    .jcall(jws, "V", "computeAll")
}

#' @name .jws_open
#' @export
.jws_load <- function(file) {
    if (missing(file) || is.null(file)) {
        if (Sys.info()[["sysname"]] == "Windows") {
            file <- utils::choose.files(
                caption = "Select a workspace",
                filters = c("JDemetra+ workspace (.xml)", "*.xml")
            )
        } else {
            file <- base::file.choose()
        }
        if (length(file) == 0) {
            stop("You have to choose a file !")
        }
    }
    if (!file.exists(file) || tools::file_ext(file) != ".xml") {
        stop("The file doesn't exist or isn't a .xml file !")
    }

    jws <- .jws_open(file)

    return(jws)
}


#' Read all SaItems
#'
#' Functions to read all the SAItem of a SAProcessing (`read_sap()`)
#' or a workspace (`read_workspace()`).
#'
#' @param jws Java workspace.
#' @param jsap Java SAProcessing.
#' @param compute Compute the workspace.
#'
#' @export
#' @examples
#' file <- system.file("workspaces", "test.xml", package = "rjd3workspace")
#' jws <- .jws_load(file)
#' # We don't compute the workspace
#' rws <- read_workspace(jws, FALSE)
read_workspace <- function(jws, compute = TRUE) {
    if (compute) .jws_compute(jws)
    n <- .jws_sap_count(jws)
    jsaps <- lapply(1:n, function(i) {
        read_sap(.jws_sap(jws, i))
    })
    names <- lapply(1:n, function(i) {
        .jsap_name(.jws_sap(jws, i))
    })
    names(jsaps) <- names
    cntxt <- get_context(jws)
    return(list(processing = jsaps, context = cntxt))
}

#' Save Workspace
#'
#' @param jws the workspace object to export.
#' @param file the path where to export the 'JDemetra+' workspace (.xml file).
#' @param replace boolean indicating if the workspace should be replaced if it already exists.
#' @examples
#' dir <- tempdir()
#' jws <- .jws_new()
#' jsap1 <- .jws_sap_new(jws, "sa1")
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' add_sa_item(jsap1, name = "x13", x = y, rjd3x13::x13_spec())
#' save_workspace(jws, file.path(dir, "workspace.xml"))
#'
#' @export
save_workspace <- function(jws, file, replace = FALSE) {
    # version <- match.arg(tolower(version)[1], c("jd3", "jd2"))
    version <- "jd3"
    invisible(.jcall(jws, "Z", "saveAs", full_path(file), version, !replace))
}

full_path <- function(path) {
    base::file.path(
        base::normalizePath(dirname(path), mustWork = TRUE, winslash = "/"),
        base::basename(path),
        fsep = "/"
    )
}


#' Add Calendar to Workspace
#'
#' @inheritParams set_context
#' @param name the name of the calendar to add.
#' @param calendar the calendar to add.
#' @export
add_calendar <- function(jws, name, calendar) {
    pcal <- rjd3toolkit::.r2p_calendar(calendar)
    jcal <- rjd3toolkit::.p2jd_calendar(pcal)
    jcal <- .jcast(jcal, "jdplus/toolkit/base/api/timeseries/calendars/CalendarDefinition")

    .jcall(
        jws, "V", "addCalendar",
        name,
        jcal
    )
}

#' Add Variable to Workspace
#'
#' @inheritParams set_context
#' @param group,name the group and the name of the variable to add.
#' @param y the variable (a `ts` object).
#' @export
add_variable <- function(jws, group, name, y) {
    .jcall(
        jws, "V", "addVariable", group,
        name, rjd3toolkit::.r2jd_tsdata(y)
    )
}
