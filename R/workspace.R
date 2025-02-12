#' @include saprocessing.R
NULL

#' Create a Workspace or SA-Processing
#'
#' Functions for creating a 'JDemetra+' Workspace (\code{.jws_new()}) and
#' adding a new SA-Processing (\code{.jws_sap_new()}). A modelling context can be
#' added to a workspace, it will be valid for all its SA6processings.
#'
#' @details
#' A modelling context is a list of variables to be used as external regressors
#' in modelling processes (Reg-Arima or Tramo) or calendars to be used to generate calendar regressors.
#' It can be created with [rjd3toolkit::modelling_context()] function or retrieved from another
#' workspace (\code{(set_context)})
#'
#'
#' @param modelling_context a list of variables and calendars
#' @param jws Workspace object.
#' @param name name of the new SA-Processing (character).
#'
#' @return
#' Returns a java object workspace or SA-Processing.
#'
#' @examples
#' # Create an empty 'JDemetra+' Workspace
#' jws <- .jws_new()
#' # Add an empty SA-Processing
#' jsap <- .jws_sap_new(jws, "sap1")
#'
#' @seealso \code{\link{read_workspace}}, \code{\link{read_sap}}
#' @references
#' More information on workspaces in JDemetra+ Graphical User Interface:
#' \url{https://jdemetra-new-documentation.netlify.app/t-gui-sa-modelling-features/}
#'
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

#' Copy a Workspace or SA-Processing
#'
#' @name make_copy
#' @param jws,jsap Java Workspace or SA-Processing
#' @export
.jws_make_copy <- function(jws) {
    return(.jcall(jws, "Ljdplus/sa/base/workspace/Ws;", "makeCopy"))
}
#' @return
#' Returns a java object workspace or SA-Processing
#'
#' @details
#' The copy of a SA-processing will be made in the same workspace. The modelling context of the
#' workspace is also copied.
#'
#' @examples
#' # Create an empty 'JDemetra+' Workspace
#' jws <- .jws_new()
#' # Add an empty SA-Processing
#' jsap <- .jws_sap_new(jws, "sap1")
#' # Make a copy a the workspace
#' #jws2 <- .jws_make_copy(jws)
#' # Make a copy of sap1 in jws2
#' #jsap2 <- .jws_make_copy(jsap)
#'
#' @seealso \code{\link{read_workspace}}, \code{\link{read_sap}}
#' @references
#' More information on workspaces in JDemetra+ Graphical User Interface:
#' \url{https://jdemetra-new-documentation.netlify.app/t-gui-sa-modelling-features/}
#'

#' Refresh a Workspace or SA-Processing
#'
#' @inheritParams make_copy
#' @param policy refresh policy to apply (see details).
#' @param period,start,end to specify the span on which outliers will not be
#' re-identified (i.e.: re-detected) when `policy = "Outliers"` or
#' `policy = "Outliers_StochasticComponent"`.
#' Span definition: \code{period}: numeric, number of observations in a year
#' (12, 4...). \code{start} and \code{end}: first and last date from which
#' outliers will not be re-identified, defined as arrays of two elements: year
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
#' @param jws the Workspace.
#'
#' @export
get_context <- function(jws) {
    jcntxt <- .jcall(
        obj = jws,
        returnSig = "Ljdplus/toolkit/base/api/timeseries/regression/ModellingContext;",
        method = "getContext"
    )
    rjd3toolkit::.jd2r_modellingcontext(jcntxt)
}

#' Count the number of objects inside a Workspace or SA-Processing
#'
#' Functions counting the number of SA-Processings in a Workspace (`jws_sap_count`) or
#' the number of SA-Items in a SA-Processing (`jsap_sa_count`).
#'
#' @param jws,jsap Workspace or SA-Processing.
#'
#' @export
.jws_sap_count <- function(jws) {
    return(.jcall(jws, "I", "getMultiProcessingCount"))
}



#' Extract a SA-Processing or a SA-Item
#'
#' @param jws,jsap Workspace or SA-Processing.
#' @param idx index of the object to extract.
#'
#' @export
.jws_sap <- function(jws, idx) {
    jsap <- .jcall(
        obj = jws,
        returnSig = "Ljdplus/sa/base/workspace/MultiProcessing;",
        method = "getMultiProcessing",
        as.integer(idx - 1L)
    )
    return(jsap)
}




#' @title Load a 'JDemetra+' Workspace
#'
#' @description
#' `.jws_open()` loads a Workspace and `.jws_compute()` computes it (allowing
#' to extract all the SAitems).
#'
#' @param file path to the 'JDemetra+' Workspace to load.
#' By default a dialog box opens.
#'
#' @seealso [read_workspace()] to import all the models of a Workspace.
#'
#' @export
.jws_open <- function(file) {
    if (missing(file) || is.null(file)) {
        if (Sys.info()[["sysname"]] == "Windows") {
            file <- utils::choose.files(
                caption = "Select a Workspace",
                filters = c("JDemetra+ Workspace (.xml)", "*.xml")
            )
        } else {
            file <- base::file.choose()
        }
        if (length(file) == 0L) {
            stop("You have to choose a file !")
        }
    }
    if (!file.exists(file) || tools::file_ext(file) != "xml") {
        stop("The file doesn't exist or isn't a .xml file !")
    }
    full_file_name <- full_path(file)
    jws <- .jcall(
        obj = "jdplus/sa/base/workspace/Ws",
        returnSig = "Ljdplus/sa/base/workspace/Ws;",
        method = "open", full_file_name
    )
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
        if (length(file) == 0L) {
            stop("You have to choose a file !")
        }
    }
    if (!file.exists(file) || tools::file_ext(file) != "xml") {
        stop("The file doesn't exist or isn't a .xml file !")
    }

    jws <- .jws_open(file)

    return(jws)
}


#' Read all SA-Items
#'
#' Functions to read all SA-Items of a SA-Processing (`read_sap()`)
#' or a Workspace (`read_workspace()`).
#' The functions `.jread_sap()` and `.jread_workspace()` only return corresponding Java objects
#'
#' @param jws Java Workspace.
#' @param jsap Java SA-Processing.
#' @param compute Compute the Workspace.
#'
#' @export
#' @examples
#' file <- system.file("workspaces", "test.xml", package = "rjd3workspace")
#' jws <- .jws_load(file)
#' # We don't compute the Workspace
#' rws <- read_workspace(jws, FALSE)
read_workspace <- function(jws, compute = TRUE) {
    if (compute) .jws_compute(jws)
    n <- .jws_sap_count(jws)
    jsaps <- lapply(seq_len(n), function(i) {
        read_sap(.jws_sap(jws, i))
    })
    names <- lapply(seq_len(n), function(i) {
        .jsap_name(.jws_sap(jws, i))
    })
    names(jsaps) <- names
    cntxt <- get_context(jws)
    return(list(processing = jsaps, context = cntxt))
}
#' @name read_workspace
#' @export
.jread_workspace <- function(jws, compute = TRUE) {
    if (compute) .jws_compute(jws)
    n <- .jws_sap_count(jws)
    jsaps <- lapply(seq_len(n), function(i) {
        .jread_sap(.jws_sap(jws, i))
    })
    names <- lapply(seq_len(n), function(i) {
        .jsap_name(.jws_sap(jws, i))
    })
    names(jsaps) <- names
    return(jsaps)
}

#' Save Workspace
#'
#' @param jws Workspace object to export.
#' @param file path where to export the 'JDemetra+' Workspace (.xml file).
#' @param replace boolean indicating if the Workspace should be replaced if it already exists.
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
    file <- full_path(file)
    if (replace && file.exists(file)) {
        base::file.remove(file)
        base::unlink(
            gsub("\\.xml$", "", file),
            recursive = TRUE
        )
    }
    invisible(.jcall(jws, "Z", "saveAs", file, version, !replace))
}

full_path <- function(path) {
    base::file.path(
        base::normalizePath(dirname(path), mustWork = TRUE, winslash = "/"),
        base::basename(path),
        fsep = "/"
    )
}


#' Add a Calendar to a Workspace
#'
#' @inheritParams set_context
#' @param name  name of  calendar to add.
#' @param calendar  calendar to add.
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
#' @param group,name group and name of the variable to add.
#' @param y variable to add (a `ts` object).
#' @export
add_variable <- function(jws, group, name, y) {
    .jcall(
        jws, "V", "addVariable", group,
        name, rjd3toolkit::.r2jd_tsdata(y)
    )
}
