#' @include saprocessing.R
NULL

#' Create a Workspace or SA-Processing
#'
#' Functions creating a 'JDemetra+' Workspace (\code{.jws_new()}) and
#' adding a new SA-Processing (\code{.jws_sap_new()}). A modelling context can be
#' added to a workspace, it will be valid for all its SA-Processings.
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
#'
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

#' @export
.jws_make_copy <- function(jws) {
    return(.jcall(jws, "Ljdplus/sa/base/workspace/Ws;", "makeCopy"))
}

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

#' Count SA-Processings or SA-Items
#'
#' Functions counting the SA-Processings in a Workspace (`jws_sap_count`) or
#' the SA-Items in a SA-Processing (`jsap_sa_count`).
#'
#' @param jws,jsap Workspace or SA-Processing.
#'
#' @return
#' Returns an integer.
#' @examples
#' #' # Create a Workspace
#' jws <- .jws_new()
#' # Add an 2 SA-Processings
#' jsap1 <- .jws_sap_new(jws, "sap1")
#' jsap2 <- .jws_sap_new(jws, "sap2")
#' # Count the SA-Processings
#' .jws_sap_count(jws)
#'
#' @export
.jws_sap_count <- function(jws) {
    return(.jcall(jws, "I", "getMultiProcessingCount"))
}

#' Extract a SA-Processing or a SA-Item
#'
#' @description
#' Functions allowing to extract a SA-Processing from a Workspace using its order number (index) and a SA-Item from a
#' SA-Processing its order number (index). The original object is unaltered.
#'
#'
#' @param jws,jsap Workspace or SA-Processing.
#' @param idx index of the object to extract.
#'
#'
#'
#' @return
#' Returns a java object SA-Processing or SA-Item.
#'
#' @examples
#' # Load a Workspace
#' # jws <- .jws_load(file= my_workspace.xml)
#' # Compute the workspace to enable access to its components
#' # .jws_compute(jws)
#' # Extract 2nd SA-Processing
#' #jsap_2 <- .jws_sap(jws_ref,2)
#' # Extract 9th SA-item
#' #jsai_9 <- .jsap_sai(jsap_2,9)
#'
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
#' `.jws_open()` or `.jws_open()` load an existing Workspace and `.jws_compute()` computes it (allowing
#' to extract all the SA-Items as java objects).
#'
#' @param file path to Workspace xml master file
#' By default a dialog box opens.
#' @examples
#' # Load a Workspace
#' # jws <- .jws_load(file= my_workspace.xml)
#' # Compute the workspace to enable access to its components
#' # .jws_compute(jws)
#'
#' @seealso [read_workspace()] to transform the workspace in a R list.
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


#' Read all SA-Items from a Workspace or SA-Processing
#'
#' Functions reading all SA-Items of a SA-Processing (`read_sap()`)
#' or a Workspace (`read_workspace()`) and allowing to access them as R lists.
#' Whereas functions `.jread_sap()` and `.jread_workspace()` only return corresponding Java objects
#'
#' @param jws java Workspace.
#' @param jsap java SA-Processing.
#' @param compute compute or not the workspace.
#'
#' @value
#' Returns a list of lists
#' workspace$SA-Processing$SA-Item or SA-Processing$SA-Item
#'
#' @examples
#' file <- system.file("workspaces", "test.xml", package = "rjd3workspace")
#' jws <- .jws_load(file)
#' rws <- read_workspace(jws, FALSE)
#' # rws$Sap_3$SA-Item_43
#' @export
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
#' Function allowing to write a workspace as a collection of xml files readable by JDemetra+ Graphical
#' user interface.
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
#' @param name  name of calendar to add.
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
