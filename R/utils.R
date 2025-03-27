#' @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass
#' @import rjd3x13 rjd3tramoseats rjd3providers
NULL


#' Read a Tramo specification file
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
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
#' @param spec
#' @param file
#'
#' @return
#' @export
#'
#' @examples
tramo_write_spec <- function(spec, file) {
    .jcall(
        "jdplus/tramoseats/base/workspace/Utility", "V", "writeTramoSpec",
        rjd3tramoseats::.r2jd_spec_tramo(spec),
        as.character((file))
    )
}
#' Read a Tramo-Seats specification file
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
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
#' @param file
#'
#' @return
#' @export
#'
#' @examples
tramoseats_write_spec <- function(spec, file) {
    .jcall(
        "jdplus/tramoseats/base/workspace/Utility", "V", "writeTramoSeatsSpec",
        rjd3tramoseats::.r2jd_spec_tramoseats(spec),
        as.character(file)
    )
}

#' Read a Reg-Arima specification file
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
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
#' @param file
#'
#' @return
#' @export
#'
#' @examples
regarima_write_spec <- function(spec, file) {
    .jcall(
        "jdplus/x13/base/workspace/Utility", "V", "writeRegArimaSpec",
        rjd3x13::.r2jd_spec_regarima(spec),
        as.character(file)
    )
}

#' Read a X13 specification file
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
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
#' @param spec
#' @param file
#'
#' @return
#' @export
#'
#' @examples
x13_write_spec <- function(spec, file) {
    .jcall(
        "jdplus/x13/base/workspace/Utility", "V", "writeX13Spec",
        rjd3x13::.r2jd_spec_x13(spec),
        as.character(file)
    )
}

#' Read a Calendar file
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
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
#' @param calendars
#' @param file
#'
#' @return
#' @export
#'
#' @examples
write_calendars <- function(calendars, file) {
    jcal <- rjd3toolkit::.r2jd_calendars(calendars)
    .jcall(
        "jdplus/toolkit/base/workspace/file/Utility", "V",
        "writeCalendars",
        jcal,
        as.character(file)
    )
}

#' Read auxiliary variables (regressors) file
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
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

#' Write auxiliary variables (regressors) file
#'
#' @param vars
#' @param file
#'
#' @return
#' @export
#'
#' @examples
write_variables <- function(vars, file) {
    jvars <- rjd3toolkit::.r2jd_variables(vars)
    .jcall(
        "jdplus/toolkit/base/workspace/file/Utility", "V",
        "writeData",
        jvars,
        as.character(file)
    )
}
