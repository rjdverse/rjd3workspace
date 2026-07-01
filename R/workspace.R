#' @include saprocessing.R
NULL

#' @importFrom methods setClass
#' @importFrom methods new
methods::setClass("workspace", contains = "jobjRef")
methods::setClass("sa_item", contains = "jobjRef")
methods::setClass("sa_processing", contains = "jobjRef")

is.sa_processing <- function(x){
    inherits(x, "sa_processing")
}
is.sa_item <- function(x){
    inherits(x, "sa_item")
}
is.workspace <- function(x){
    inherits(x, "workspace")
}

#' Create a Workspace or SA-Processing
#'
#' Functions creating a 'JDemetra+' Workspace (\code{jws_new()}) and
#' adding a new SA-Processing (\code{jws_sap_new()}). A modelling context can be
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
#' @param jws a Javaworkspace object.
#' @param name name of the new SA-Processing to be added (character).
#'
#' @returns
#' Returns a Javaobject workspace or SA-Processing.
#'
#' @examplesIf rjd3jars::check_java_version()
#' # Create an empty 'JDemetra+' Workspace
#' jws <- jws_new()
#' # Add an empty SA-Processing
#' jsap <- jws_sap_new(jws, "sap1")
#'
#' @seealso [read_workspace()], [read_sap()]
#' @references
#' More information on workspaces in JDemetra+ Graphical User Interface:
#' \url{https://doc.jdemetra.org/t-gui-sa-modelling-features/}
#'
#'
#' @export
jws_new <- function(modelling_context = NULL) {
    jws <- .jnew("jdplus/sa/base/workspace/Ws")
    if (!is.null(modelling_context)) {
        set_context(jws, modelling_context)
    }
    # jws <- methods::new("workspace", jws)
    return(jws)
}

#' @rdname jws_new
#' @export
jws_sap_new <- function(jws, name) {
    return(.jcall(
        jws,
        "Ljdplus/sa/base/workspace/MultiProcessing;",
        "newMultiProcessing",
        name
    ))
}

#' @title Add a SA-Processing to a Workspace
#'
#' @inheritParams make_copy
#'
#' @returns Invisibly `NULL`
#'
#' @export
jws_add <- function(jws, jsap) {
    .jcall(jws, "V", "add", jsap)
    return(invisible(NULL))
}

#' @title Copy a Workspace or SA-Processing
#'
#' @name make_copy
#' @param jws,jsap Java Workspace or SA-Processing
#'
#' @returns
#' Returns a Java object workspace or SA-Processing
#'
#' @details
#' The copy of a SA-processing will be made in the same workspace. The modelling context of the
#' workspace is also copied.
#'
#' @examplesIf rjd3jars::check_java_version()
#' # Create an empty 'JDemetra+' Workspace
#' jws <- jws_new()
#'
#' # Add an empty SA-Processing
#' jsap <- jws_sap_new(jws, "sap1")
#'
#' # Make a copy of the workspace
#' jws2 <- jws_make_copy(jws)
#'
#' # Make a copy of sap1 in jws2
#' jsap2 <- jsap_make_copy(jsap)
#'
#'
#' @seealso [read_workspace()], [read_sap()]
#' @references
#' More information on workspaces in JDemetra+ Graphical User Interface:
#' \url{https://doc.jdemetra.org/t-gui-sa-modelling-features/}
#'
#' @export
jws_make_copy <- function(jws) {
    return(.jcall(jws, "Ljdplus/sa/base/workspace/Ws;", "makeCopy"))
}

#' Refresh a Workspace or SA-Processing
#'
#' @inheritParams make_copy
#'
#' @param policy refresh policy to apply (see details).
#'
#' @param period,start,end  additional parameters used to specify the span
#' When `policy = "Outliers"` or `policy = "Outliers_StochasticComponent"`
#' \code{period}: numeric, number of observations in a year (12, 4...), compulsory,
#' if mis-specified or missing, re-estimation with refreshed specification won't work.
#' \code{end} has to be specified as the date from which outliers will be re-identified
#'
#' @param info indication on how data should be refreshed
#' `All`: data and metadata will be refreshed (default)
#' `Data`: data will be refreshed, not metadata
#' `None`: nor data neither metadata will be refreshed, to be used for updating specifications only.
#'
#' @details
#'
#' A particular selection of parameters to be kept fixed or re-estimated is called a
#' revision policy.
#' Workspace has to be computed before refresh
#' When refreshing data, empty your cache by restarting your R session, before refreshing,
#' otherwise the specification will be refreshed but the new data will not be taken into account.
#'
#' Available refresh policies are:
#' \enumerate{
#' \item \strong{Fixed}: applying the current pre-adjustment reg-arima model
#' and replacing forecasts by new raw data points;
#' X11 (or SEATS) and Benchmarking part parameters are untouched.
#' \item \strong{FixedParameters}: pre-adjustment reg-arima model is partially
#' modified: regression coefficients will be re-estimated but regression
#' variables, Arima orders and coefficients are unchanged;
#' X11 (or SEATS) and Benchmarking part parameters are untouched.
#' \item \strong{FixedAutoRegressiveParameters}: same as FixedParameters but
#' Arima Moving Average coefficients (MA) are also re-estimated, Auto-regressive
#'  (AR) coefficients are kept fixed;
#' X11 (or SEATS) and Benchmarking part parameters are untouched.
#' \item \strong{FreeParameters}: all regression and Arima model coefficients
#' are re-estimated, regression variables and Arima orders are kept fixed;
#' X11 (or SEATS) and Benchmarking part parameters are untouched.
#' \item \strong{Outliers}: regression variables and Arima orders are kept
#' fixed, but outliers will be re-detected on the defined span, thus all
#' regression and Arima model coefficients are re-estimated;
#' X11 (or SEATS) and Benchmarking part parameters are untouched.
#' \item \strong{Outliers_StochasticComponent}: same as "Outliers" but Arima
#' model orders (p,d,q)(P,D,Q) can also be re-identified;
#' X11 (or SEATS) and Benchmarking part parameters are untouched.
#' \item \strong{Complete}: All the parameters are re-identified and
#' re-estimated, unless constrained in the reference spec.
#' X11 (or SEATS) and Benchmarking part parameters are entirely reset to values in the reference specification.
#' }
#' @references
#' More information on revision policies in JDemetra+ documentation:
#' \url{https://doc.jdemetra.org/a-rev-policies}
#'
#' @returns refreshed workspace or SAP
#'
#' @examplesIf rjd3jars::check_java_version()
#'
#' # Load workspace
#' file <- system.file("workspaces", "workspace_test_refresh.xml", package = "rjd3workspace")
#'
#' \donttest{
#' jws <- jws_open(file)
#' txt_update_path(
#'     jws = jws,
#'     new_path = system.file("extdata", "IPI_nace4.csv", package = "rjd3workspace")
#' )
#' jws_compute(jws)
#'
#' # Read current workspace: reference spec and estimation spec
#' rws <- read_workspace(jws, compute = TRUE)
#' rws$processing$`SAProcessing-1`$`RF0811`$referenceSpec
#' rws$processing$`SAProcessing-1`$`RF0811`$estimationSpec
#'
#' # Refresh workspace COMPLETE
#' jws_refresh(jws, policy = "Complete")
#'
#' # Read refreshed workspace: new estimation spec
#' rws2 <- read_workspace(jws, compute = TRUE)
#' rws2$processing$`SAProcessing-1`$`RF0811`$estimationSpec
#'
#' # Refresh workspace Outliers (like "lastoutliers in the GUI, but with custom start date)
#' jws <- jws_open(file)
#' jws_compute(jws)
#' jws_refresh(jws, policy = "Outliers", period = 12, end = c(2020, 4))
#'
#' # Read refreshed workspace: new estimation spec
#' rws3 <- read_workspace(jws, compute = TRUE)
#' rws3$processing$`SAProcessing-1`$`RF0811`$estimationSpec
#' }
#'
#' @name refresh
#' @export
jws_refresh <- function(
    jws,
    policy = c(
        "FreeParameters",
        "Complete",
        "Outliers_StochasticComponent",
        "Outliers",
        "FixedParameters",
        "FixedAutoRegressiveParameters",
        "Fixed",
        "Current"
    ),
    period = 0,
    start = NULL,
    end = NULL,
    info = c("All", "Data", "None")
) {
    policy <- match.arg(policy)
    info <- match.arg(info)
    jdom <- rjd3toolkit::.jdomain(period, start, end)
    return(.jcall(jws, "V", "refreshAll", policy, jdom, info))
}

#' Set Context of a Workspace
#'
#' @inheritParams jws_new
#' @inheritParams jws_open
#'
#' @returns Invisibly `NULL`
#'
#' @examplesIf rjd3jars::check_java_version()
#'
#' library("rjd3toolkit")
#'
#' # French calendar
#' french_calendar <- national_calendar(
#'     days = list(
#'         fixed_day(7, 14), # Bastille Day
#'         fixed_day(5, 8, validity = list(start = "1982-05-08")), # End of 2nd WW
#'         special_day("NEWYEAR"),
#'         special_day("CHRISTMAS"),
#'         special_day("MAYDAY"),
#'         special_day("EASTERMONDAY"),
#'         special_day("ASCENSION"),
#'         special_day("WHITMONDAY"),
#'         special_day("ASSUMPTION"),
#'         special_day("ALLSAINTSDAY"),
#'         special_day("ARMISTICE")
#'     )
#' )
#'
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' \donttest{
#' jws <- jws_open(file)
#'
#' # Creating a new context
#' new_context <- modelling_context(
#'     calendars = list(FR = french_calendar),
#'     variables = list(a = AirPassengers)
#' )
#'
#' # Set the context
#' set_context(jws, new_context)
#' }
#'
#' @export
set_context <- function(jws, modelling_context = NULL) {
    if (!is.null(set_context)) {
        jcontext <- rjd3toolkit::.r2jd_modellingcontext(modelling_context)
        .jcall(jws, "V", "setContext", jcontext)
    }
    return(invisible(NULL))
}

#' @title Get Context from Workspace
#'
#' @param jws the Workspace.
#'
#' @examplesIf rjd3jars::check_java_version()
#'
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' \donttest{
#' jws <- jws_open(file)
#'
#' # Get context
#' my_context <- get_context(jws)
#' }
#'
#' @returns The modelling context (list object with Calendars and Variables).
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
#' Functions counting the SA-Processings in a Workspace (`ws_sap_count`) or
#' the SA-Items in a SA-Processing (`sap_sai_count`).
#'
#' @param jws,jsap Workspace or SA-Processing.
#'
#' @returns
#' Returns an integer.
#'
#' @examplesIf rjd3jars::check_java_version()
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' \donttest{
#' jws <- jws_open(file)
#'
#' # Count the SA-Processings
#' ws_sap_count(jws)
#'
#' # Count the SA-Items
#' # In SAP 1
#' sap1 <- jws_sap(jws, 1)
#' sap_sai_count(sap1)
#' }
#'
#' @export
ws_sap_count <- function(jws) {
    return(.jcall(jws, "I", "getMultiProcessingCount"))
}

#' Extract a SA-Processing or a SA-Item
#'
#' @description
#' Functions allowing to extract a SA-Processing from a Workspace using its order number (index) and a SA-Item from a
#' SA-Processing its order number (index). The original object is unaltered.
#' @param jws,jsap Workspace or SA-Processing.
#' @param idx index of the object to extract.
#'
#' @returns
#' Returns a Javaobject SA-Processing or SA-Item.
#'
#' @examplesIf rjd3jars::check_java_version()
#'
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' \donttest{
#' jws <- jws_open(file)
#'
#' # Compute the workspace to enable accessing its components
#' jws_compute(jws)
#'
#' # Extract 2nd SA-Processing
#' jsap2 <- jws_sap(jws, 2)
#'
#' # Extract 3rd SA-item
#' jsai3 <- jsap_sai(jsap2, 3)
#' }
#'
#' @export
jws_sap <- function(jws, idx) {
    jsap <- .jcall(
        obj = jws,
        returnSig = "Ljdplus/sa/base/workspace/MultiProcessing;",
        method = "getMultiProcessing",
        as.integer(idx - 1L)
    )
    # jsap <- methods::new("sa_processing", jsap)
    return(jsap)
}

#' @title Open an existing 'JDemetra+' Workspace
#'
#' @description
#' `jws_open()` opens an existing Workspace (as a Java pointer) and `jws_compute()` computes it (allowing
#' to extract all the SA-Items as Java objects).
#'
#' @param file path to Workspace xml master file
#' By default a dialog box opens.
#' @returns a Javaworkspace
#'
#' @examplesIf rjd3jars::check_java_version()
#'
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' \donttest{
#' jws <- jws_open(file)
#'
#' # Compute the workspace to enable access its components
#' jws_compute(jws)
#' }
#'
#' @seealso [read_workspace()] to transform the workspace in a R list.
#'
#' @export
jws_open <- function(file) {
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
        method = "open",
        full_file_name
    )
    # jws <- methods::new("workspace", jws)
    return(jws)
}


#' @title Compute a Workspace
#'
#' @description
#' `jws_compute()` allows to extract all the SA-Items as Javaobject.
#'
#' @param jws a workspace
#'
#' @returns Invisibly `NULL`
#'
#' @examplesIf rjd3jars::check_java_version()
#'
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' \donttest{
#' jws <- jws_open(file)
#'
#' # Compute the workspace to access its components
#' jws_compute(jws)
#' }
#'
#' @export
jws_compute <- function(jws) {
    .jcall(jws, "V", "computeAll")
    return(invisible(NULL))
}

#' Read all SA-Items from a Workspace or SA-Processing
#'
#' Functions reading all SA-Items from a Workspace (`read_workspace()`) or a SA-Processing (`read_sap()`)
#' and allowing to access them as R lists.
#' Whereas functions `jread_sap()` and `jread_workspace()` only return corresponding Java objects
#'
#' @param jws Java Workspace.
#' @param jsap Java SA-Processing.
#' @param compute compute or not the workspace (to get the estimation results).
#' @returns list or Java object

#' @examplesIf rjd3jars::check_java_version()
#'
#' # Load workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' \donttest{
#' jws <- jws_open(file)
#'
#' # Read workspace
#' jread_workspace(jws, compute = FALSE)
#'
#' rws <- read_workspace(jws)
#'
#' # Read sap
#' sap <- jws_sap(jws,1)
#' jread_sap(sap)
#' read_sap(sap)
#' }
#'
#' @export
read_workspace <- function(jws, compute = TRUE) {
    if (compute) {
        jws_compute(jws)
    }
    n <- ws_sap_count(jws)
    jsaps <- lapply(seq_len(n), function(i) {
        read_sap(jws_sap(jws, i))
    })
    names <- lapply(seq_len(n), function(i) {
        sap_name(jws_sap(jws, i))
    })
    names(jsaps) <- names
    cntxt <- get_context(jws)
    return(list(processing = jsaps, context = cntxt))
}

#' @rdname read_workspace
#' @export
jread_workspace <- function(jws, compute = TRUE) {
    if (compute) {
        jws_compute(jws)
    }
    n <- ws_sap_count(jws)
    jsaps <- lapply(seq_len(n), function(i) {
        jread_sap(jws_sap(jws, i))
    })
    names <- lapply(seq_len(n), function(i) {
        sap_name(jws_sap(jws, i))
    })
    names(jsaps) <- names
    return(jsaps)
}

#' @title Save Workspace
#'
#' @description
#' Function allowing to write a workspace as a collection of xml files readable by JDemetra+ Graphical
#' User Interface.
#'
#' @param jws Workspace object to export.
#' @param file path where to export the 'JDemetra+' Workspace (.xml file).
#' @param replace boolean indicating if the Workspace should be replaced if it already exists.
#'
#' @returns A boolean indicating if the saving was successful.
#'
#' @examplesIf rjd3jars::check_java_version()
#' dir <- tempdir()
#' jws <- jws_new()
#' jsap1 <- jws_sap_new(jws, "sap1")
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#'
#' \donttest{
#' add_sa_item(jsap1, name = "serie_1", x = y, rjd3x13::x13_spec())
#' save_workspace(jws, file.path(dir, "workspace.xml"))
#' }
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


#' @title Add a Calendar to a Workspace
#'
#' @inheritParams set_context
#' @param name  character name of the calendar to add.
#' @param calendar  JDemetra+ calendar to add.
#'
#' @returns \code{NULL} returned invisibly
#'
#' @examplesIf rjd3jars::check_java_version()
#' # French calendar
#' french_calendar <- rjd3toolkit::national_calendar(
#'     days = list(
#'         rjd3toolkit::fixed_day(7, 14), # Bastille Day
#'         rjd3toolkit::fixed_day(5, 8, validity = list(start = "1982-05-08")), # End of 2nd WW
#'         rjd3toolkit::special_day("NEWYEAR"),
#'         rjd3toolkit::special_day("CHRISTMAS"),
#'         rjd3toolkit::special_day("MAYDAY"),
#'         rjd3toolkit::special_day("EASTERMONDAY"),
#'         rjd3toolkit::special_day("ASCENSION"),
#'         rjd3toolkit::special_day("WHITMONDAY"),
#'         rjd3toolkit::special_day("ASSUMPTION"),
#'         rjd3toolkit::special_day("ALLSAINTSDAY"),
#'         rjd3toolkit::special_day("ARMISTICE")
#'     )
#' )
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' \donttest{
#' jws <- jws_open(file)
#'
#' # Add calendar to the workspace
#' add_calendar(jws, "French Calendar", french_calendar)
#' get_context(jws) # The workspace already contained a Test Calendar
#' }
#'
#' @export
#'
add_calendar <- function(jws, name, calendar) {
    pcal <- rjd3toolkit::.r2p_calendar(calendar)
    jcal <- rjd3toolkit::.p2jd_calendar(pcal)
    jcal <- .jcast(
        jcal,
        "jdplus/toolkit/base/api/timeseries/calendars/CalendarDefinition"
    )

    .jcall(
        jws,
        "V",
        "addCalendar",
        name,
        jcal
    )
}


#' @title Add a Variable to a JD+ Workspace
#'
#' @description
#' Adds a single time series variable to a specified group within a JD+ workspace..
#'
#' @param jws A JD+ workspace object (Java pointer).
#' @param group A character string indicating the name of the group in which to store the variable.
#' @param y A \code{ts} object (R time series) to be added. Only a single time series can be added at a time.
#' @param name A character string naming the variable.
#' @param overwrite a Boolean to indicate whether a variable already present
#' should be replaced
#'
#' @returns No return value (\code{NULL} returned invisibly). This function is
#' used for its side effect of modifying the workspace.
#'
#' @details
#' For the time being, if the group does not already exist, a new group is
#' created, but the group will be named after \code{name}, not \code{group}.
#'
#' @section Limitations:
#' \itemize{
#'   \item Cannot add multiple variables at once.
#'   \item Does not support dynamic ts objects with metadata.
#'   \item If group does not exist, a new group is created but named after the variable name, not the intended group.
#' }
#'
#' @seealso [rjd3toolkit::modelling_context()] to create multiple variables and
#' groups at once, and [read_variables()], [write_variables()] to import/export
#' variables.
#'
#' @export
#'
#' @examplesIf rjd3jars::check_java_version()
#'
#' # Load a Workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' \donttest{
#' jws <- jws_open(file)
#' add_variables(jws = jws, group = "reg1", y = AirPassengers, name = "x1")
#' }
#'
add_variables <- function(jws, group, name, y, overwrite = FALSE) {
    if (inherits(y, what = c("JD3_DYNAMICTS", "JD3_TS"))) {
        context <- get_context(jws)
        vars <- context$variables
        if (!(is.null(vars[[group]][[name]]) || overwrite)) {
            message(
                "There is already a variable with the same name in the same group.",
                "Please change the name of the variable or the name of the group or set `overwrite` to `TRUE`."
            )
            return(invisible(NULL))
        }
        vars[[group]][[name]] <- y
        new_context <- rjd3toolkit::modelling_context(
            calendars = context$calendars,
            variables = vars
        )
        set_context(jws, modelling_context = new_context)
        return(invisible(NULL))
    }
    .jcall(
        jws,
        "V",
        "addVariable",
        group,
        name,
        rjd3toolkit::.r2jd_tsdata(y)
    )
}
