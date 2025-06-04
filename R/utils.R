#' @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass
#' @import rjd3x13 rjd3tramoseats rjd3providers
NULL


#' Read a Tramo specification file
#'
#'@description
#'
#' The specification file is a xml file like the one JDemetra+ would write when defining a specification in the
#' Graphical User Interface.
#'
#' @param file xml format,
#'
#' @return list
#'
#' @examples
#' file <- system.file("workspaces", "workspace_test","TramoSpec","TramoSpec-1.xml", package = "rjd3workspace")
#' my_spec<- tramo_read_spec(file)
#' class(my_spec)
#' str(my_spec)
#' @export
tramo_read_spec <- function(file) {
    jspec <- .jcall(
        "jdplus/tramoseats/base/workspace/Utility", "Ljdplus/tramoseats/base/api/tramo/TramoSpec;",
        "readTramoSpec", as.character((file))
    )
    if (is.jnull(jspec)) {
        return(NULL)
    }
    return(rjd3tramoseats::.jd2r_spec_tramo(jspec))
}

#' Write a Tramo specification file
#'
#' @description
#' The specification file is a xml file like the one JDemetra+ would write when defining a specification in the
#' Graphical User Interface.
#'
#' @param spec a specification created with `rjd3tramoseats::tramo_spec`
#' @param file xml format
#'
#'
#' @return \code{NULL} returned invisibly
#' @examples
#'
#' # creating a spec from default
#' tramo_spec <- rjd3tramoseats::tramo_spec("tr3")
#' # forcing multiplicative model
#' tramo_spec_d <- rjd3toolkit::set_transform(tramo_spec ,
#'                                           fun = "Log",
#'                                           outliers = TRUE)
#' # writing the specification in a xml file
#' tramo_write_spec(tramo_spec_d, file = normalizePath("~/tmp.xml", mustWork = FALSE))
#' @export
tramo_write_spec <- function(spec, file) {
    .jcall(
        "jdplus/tramoseats/base/workspace/Utility", "V", "writeTramoSpec",
        rjd3tramoseats::.r2jd_spec_tramo(spec),
        as.character((file))
    )
}
#' Read a Tramo-Seats specification file
#'
#' @description
#'
#' The specification file is a xml file like the one JDemetra+ would write when defining a specification in the
#' Graphical User Interface.
#'
#' @param file xml format,
#'
#' @return list
#'
#' @examples
#' file <- system.file("workspaces", "workspace_test","TramoSeatsSpec","TramoSeatsSpec-1.xml", package = "rjd3workspace")
#' my_spec<- tramoseats_read_spec(file)
#' class(my_spec)
#' str(my_spec)
#' @export
tramoseats_read_spec <- function(file) {
    jspec <- .jcall(
        obj = "jdplus/tramoseats/base/workspace/Utility",
        returnSig = "Ljdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec;",
        method = "readTramoSeatsSpec",
        as.character(file)
    )
    if (is.jnull(jspec)) {
        return(NULL)
    }
    return(rjd3tramoseats::.jd2r_spec_tramoseats(jspec))
}

#' Write a Tramo-Seats specification file
#'
#' @description
#' The specification file is a xml file like the one JDemetra+ would write when defining a specification in the
#' Graphical User Interface.
#' @param spec a specification created with `rjd3tramoseats::tramoseats_spec`
#' @param file xml format
#' @return \code{NULL} returned invisibly
#' @examples
#' # creating a spec from default
#' tramoseats_spec <- rjd3tramoseats::tramoseats_spec("rsa3")
#' # forcing multiplicative model
#'tramoseats_spec_d <- rjd3toolkit::set_transform(tramoseats_spec ,
#'                                                fun = "Log",
#'                                                outliers = TRUE)
#' # writing the specification in a xml file
#' tramoseats_write_spec(tramoseats_spec_d, file = normalizePath("~/tmp.xml", mustWork = FALSE))
#' @export
tramoseats_write_spec <- function(spec, file) {
    .jcall(
        "jdplus/tramoseats/base/workspace/Utility", "V", "writeTramoSeatsSpec",
        rjd3tramoseats::.r2jd_spec_tramoseats(spec),
        as.character(file)
    )
}

#' Read a Reg-Arima specification file
#'
#' @description
#'
#' The specification file is a xml file like the one JDemetra+ would write when defining a specification in the
#' Graphical User Interface.
#'
#' @param file xml format,
#'
#' @return list
#' @examples
#' file <- system.file("workspaces", "workspace_test","RegArimaSpec","RegArimaSpec-1.xml", package = "rjd3workspace")
#' my_spec<-regarima_read_spec(file)
#' class(my_spec)
#' str(my_spec)
#'
#' @export
regarima_read_spec <- function(file) {
    jspec <- .jcall(
        "jdplus/x13/base/workspace/Utility", "Ljdplus/x13/base/api/regarima/RegArimaSpec;",
        "readRegArimaSpec", as.character((file))
    )
    if (is.jnull(jspec)) {
        return(NULL)
    }
    return(rjd3x13::.jd2r_spec_regarima(jspec))
}

#' Write a Reg-Arima specification file
#'
#' @description
#' The specification file is a xml file like the one JDemetra+ would write when defining a specification in the
#' Graphical User Interface.
#'
#' @param spec a specification created with `rjd3x13::regarima_spec`
#' @param file xml format
#'
#' @return \code{NULL} returned invisibly
#' @examples
#' # creating a spec from default
#' reg_arima_spec <- rjd3x13::regarima_spec("rg3")
#' # forcing multiplicative model
#' reg_arima_spec_d <- rjd3toolkit::set_transform(reg_arima_spec ,
#'                                              fun = "Log",
#'                                             outliers = TRUE)
#' # writing the specification in a xml file
#' regarima_write_spec(reg_arima_spec_d, file = normalizePath("~/tmp.xml", mustWork = FALSE))
#'
#' @export
regarima_write_spec <- function(spec, file) {
    .jcall(
        "jdplus/x13/base/workspace/Utility", "V", "writeRegArimaSpec",
        rjd3x13::.r2jd_spec_regarima(spec),
        as.character(file)
    )
}

#' Read a X13 specification file
#'
#' @description
#' The specification file is a xml file like the one JDemetra+ would write when defining a specification in the
#' Graphical User Interface.
#'
#' @param file xml format,
#'
#' @return list
#' @examples
#' file <- system.file("workspaces", "workspace_test","X13Spec","X13Spec-1.xml", package = "rjd3workspace")
#' my_spec<-x13_read_spec(file)
#' class(my_spec)
#' str(my_spec)
#' @export
x13_read_spec <- function(file) {
    jspec <- .jcall(
        obj = "jdplus/x13/base/workspace/Utility",
        returnSig = "Ljdplus/x13/base/api/x13/X13Spec;",
        method = "readX13Spec",
        file
    )
    if (is.jnull(jspec)) {
        return(NULL)
    }
    return(rjd3x13::.jd2r_spec_x13(jspec))
}

#' Write a X13 specification file
#'
#' @description
#' The specification file is a xml file like the one JDemetra+ would write when defining a specification in the
#' Graphical User Interface.
#' @param spec a specification created with `rjd3x13::x13_spec`
#' @param file xml format
#' @return \code{NULL} returned invisibly
#' @examples
#' # creating a spec from default
#' x13_spec<- rjd3x13::x13_spec("rsa3")
#' # forcing multiplicative model
#' x13_spec_d <- rjd3toolkit::set_transform(x13_spec,
#'                                         fun = "Log",
#'                                         outliers = TRUE)
#' # writing the specification in a xml file
#' x13_write_spec(x13_spec_d, file = normalizePath("~/tmp.xml", mustWork = FALSE))
#' @export
x13_write_spec <- function(spec, file) {
    .jcall(
        "jdplus/x13/base/workspace/Utility", "V", "writeX13Spec",
        rjd3x13::.r2jd_spec_x13(spec),
        as.character(file)
    )
}

#' Read a Calendar file
#'
#' @param calendar a xml file
#' @description
#' The calendar file is a xml file like the one JDemetra+ would write when defining a calendar in the
#' Graphical User Interface.
#'
#' @return a list
#'
#' @examples
#'
#' file <- system.file("workspaces", "workspace_test", "Calendars", "Calendars.xml", package = "rjd3workspace")
#' my_calendar <- read_calendars(file)
#' my_calendar
#'
#' @export
read_calendars <- function(file) {
    jspec <- .jcall(
        obj = "jdplus/toolkit/base/workspace/file/Utility",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/calendars/CalendarManager;",
        method = "readCalendars",
        file
    )
    if (is.jnull(jspec)) {
        return(NULL)
    }
    return(rjd3toolkit::.jd2r_calendars(jspec))
}

#' Write a Calendar file
#'
#' @description
#' The calendar file is a xml file like the one JDemetra+ would write when defining a calendar in the
#' Graphical User Interface.
#' Calendars can be defined with `rjd3toolkit::national_calendar`
#'
#' @param list of calendars
#' @param file xml format
#'
#' @return \code{NULL} returned invisibly
#' @examples
#' library(rjd3toolkit)
#' BE <- national_calendar(list(
#'    fixed_day(7, 21),
#'    special_day("NEWYEAR"),
#'    special_day("CHRISTMAS"),
#'    special_day("MAYDAY"),
#'    special_day("EASTERMONDAY"),
#'    special_day("ASCENSION"),
#'    special_day("WHITMONDAY"),
#'    special_day("ASSUMPTION"),
#'    special_day("ALLSAINTSDAY"),
#'    special_day("ARMISTICE")
#' ))
#' write_calendars(list(BEL_cal = BE),
#'         file = normalizePath("~/tmp.xml", mustWork = FALSE))
#' @export
write_calendars <- function(calendars, file) {
    jcal <- rjd3toolkit::.r2jd_calendars(calendars)
    .jcall(
        "jdplus/toolkit/base/workspace/file/Utility", "V",
        "writeCalendars",
        jcal,
        as.character(file)
    )
}

#' Read auxiliary regressors file
#'
#' @description
#' The variables (regressors) file is a xml file like the one JDemetra+ would write when setting-up user defined regressors in the
#' Graphical User Interface.
#'
#' @param file xml format
#'
#' @return A named list of time series objects, grouped if applicable.
#'
#' @examples
#' file <- system.file("workspaces", "workspace_test", "Variables", "Vars-1.xml", package = "rjd3workspace")
#' my_regressors <- read_variables(file)
#' class(my_regressors)
#' str(my_regressors)
#'
#' @export
#'
read_variables <- function(file) {
    jspec <- .jcall(
        obj = "jdplus/toolkit/base/workspace/file/Utility",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/regression/TsDataSuppliers;",
        method = "readData",
        file
    )
    if (is.jnull(jspec)) {
        return(NULL)
    }
    return(rjd3toolkit::.jd2r_variables(jspec))
}

#' @title Write regressors file
#'
#' @param vars A named list of `ts` objects, possibly grouped (e.g., list(reg1 = list(x1 = AirPassengers))).
#' @param file Path to the output XML file.
#'
#' @return No return value (\code{NULL} returned invisibly). This function writes variables to file for use in JD+.
#'
#' @examples
#'
#' # Creating a list of regressors from a xml file
#' file <- system.file("workspaces", "workspace_test", "Variables", "Vars-1.xml", package = "rjd3workspace")
#' my_regressors <- read_variables(file)
#' class(my_regressors)
#' str(my_regressors)
#'
#' # Writing the regressors in a xml file
#' write_variables(my_regressors, file = normalizePath("tmp.xml", mustWork = FALSE))
#'
#' @export
write_variables <- function(vars, file) {
    jvars <- rjd3toolkit::.r2jd_variables(vars)
    .jcall(
        "jdplus/toolkit/base/workspace/file/Utility", "V",
        "writeData",
        jvars,
        as.character(file)
    )
}
