#' @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass
#' @import rjd3x13 rjd3tramoseats rjd3providers
NULL


#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
tramo_read_spec<-function(file){
  jspec<-.jcall("jdplus/tramoseats/base/workspace/Utility", "Ljdplus/tramoseats/base/api/tramo/TramoSpec;",
                "readTramoSpec", as.character((file)))
  if (is.jnull(jspec)) return (NULL)
  return (rjd3tramoseats::.jd2r_spec_tramo(jspec))
}

#' Title
#'
#' @param spec
#' @param file
#'
#' @return
#' @export
#'
#' @examples
tramo_write_spec<-function(spec, file){
  .jcall("jdplus/tramoseats/base/workspace/Utility", "V", "writeTramoSpec",
         rjd3tramoseats::.r2jd_spec_tramo(spec),
         as.character((file)))
}
#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
tramoseats_read_spec<-function(file){
  jspec<-.jcall("jdplus/tramoseats/base/workspace/Utility", "Ljdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec;",
                "readTramoSeatsSpec", as.character(file))
  if (is.jnull(jspec)) return (NULL)
  return (rjd3tramoseats::.jd2r_spec_tramoseats(jspec))
}

#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
tramoseats_write_spec<-function(spec, file){
  .jcall("jdplus/tramoseats/base/workspace/Utility", "V", "writeTramoSeatsSpec",
         rjd3tramoseats::.r2jd_spec_tramoseats(spec),
        as.character(file))
}

#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
regarima_read_spec<-function(file){
  jspec<-.jcall("jdplus/x13/base/workspace/Utility", "Ljdplus/x13/base/api/regarima/RegArimaSpec;",
                "readRegArimaSpec", as.character((file)))
  if (is.jnull(jspec)) return (NULL)
  return (rjd3x13::.jd2r_spec_regarima(jspec))
}

#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
regarima_write_spec<-function(spec, file){
  .jcall("jdplus/x13/base/workspace/Utility", "V", "writeRegArimaSpec",
         rjd3x13::.r2jd_spec_regarima(spec),
         as.character(file))
}

#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
x13_read_spec<-function(file){
  jspec<-.jcall("jdplus/x13/base/workspace/Utility", "Ljdplus/x13/base/api/x13/X13Spec;", "readX13Spec", file)
  if (is.jnull(jspec)) return (NULL)
  return (rjd3x13::.jd2r_spec_x13(jspec))
}

#' Title
#'
#' @param spec
#' @param file
#'
#' @return
#' @export
#'
#' @examples
x13_write_spec<-function(spec, file){
  .jcall("jdplus/x13/base/workspace/Utility", "V", "writeX13Spec",
         rjd3x13::.r2jd_spec_x13(spec),
         as.character(file))
}

#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
read_calendars<-function(file){
  jspec<-.jcall("jdplus/toolkit/base/workspace/file/Utility", "Ljdplus/toolkit/base/api/timeseries/calendars/CalendarManager;", "readCalendars", file)
  if (is.jnull(jspec)) return (NULL)
  return (rjd3toolkit::.jd2r_calendars(jspec))
}

#' Title
#'
#' @param calendars
#' @param file
#'
#' @return
#' @export
#'
#' @examples
write_calendars<-function(calendars, file){
  jcal<-rjd3toolkit::.r2jd_calendars(calendars)
  .jcall("jdplus/toolkit/base/workspace/file/Utility", "V",
                 "writeCalendars",
                 jcal,
                 as.character(file))
}

#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
read_variables<-function(file){
  jspec<-.jcall("jdplus/toolkit/base/workspace/file/Utility", "Ljdplus/toolkit/base/api/timeseries/regression/TsDataSuppliers;", "readData", file)
  if (is.jnull(jspec)) return (NULL)
  return (rjd3toolkit::.jd2r_variables(jspec))
}

#' Title
#'
#' @param vars
#' @param file
#'
#' @return
#' @export
#'
#' @examples
write_variables<-function(vars, file){
  jvars<-rjd3toolkit::.r2jd_variables(vars)
  .jcall("jdplus/toolkit/base/workspace/file/Utility", "V",
         "writeData",
         jvars,
         as.character(file))
}

