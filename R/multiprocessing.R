#' @include saitem.R
NULL

#' @export
.jmp_sa_count<-function(jmp){
  return (.jcall(jmp, "I", "size"))
}

#' Get the name of a multiprocessing or a SaItem
#'
#' Functions to retrieve the name of a multiprocessing (`.jmp_name()`) or SaItem (`.jsa_name()`).
#'
#' @param jmp,jsa the object to retrieve the name from.
#'
#' @export
.jmp_name<-function(jmp){
  return (.jcall(jmp, "S", "getName"))
}


#' @name .jws_multiprocessing
#' @export
.jmp_sa<-function(jmp, idx){
  if (idx < 1) return (NULL)
  return (.jcall(jmp, "Ljdplus/sa/base/api/SaItem;", "get", as.integer(idx-1)))
}
#' @name .jmp_name
#' @export
.jmp_sa_name <- function(jmp) {
  n <- .jcall(jmp, "I", "size")
  if (n == 0) {
    return (NULL)
  }
  names_sa <- vapply(X = seq_len(n),
                     FUN = function(i) {.jsa_name(.jmp_sa(jmp, i))},
                     FUN.VALUE = character(1))
  return(names_sa)
}

#' @name load_workspace
#' @export
.jmp_load<-function(jmp){
  n<-.jcall(jmp, "I", "size")
  if (n == 0){ return (NULL)}
  all<-lapply(1:n, function(i){.jsa_read(.jmp_sa(jmp, i))})
  names<-lapply(1:n, function(i){.jsa_name(.jmp_sa(jmp, i))})
  names(all)<-names
  return (all)
}

#' Add SAItem to Multiprocessing
#'
#' @param jmp the multiprocessing.
#' @param name the name of SAItem.
#' @param x either a seasonal adjustment model (from [rjd3x13::x13()] or [rjd3tramoseats::tramoseats()]) or a `"ts"` object.
#' @param spec the specification to use when `x` is a `"ts"` object.
#' @param ... other unused parameters.
#'
#' @examples
#' dir <- tempdir()
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' jws <- .jws_new()
#' jmp1 <- .jws_multiprocessing_new(jws, "sa1")
#' add_sa_item(jmp1, name = "x13", x = rjd3x13::x13(y))
#' add_sa_item(jmp1, name = "tramo", x = rjd3tramoseats::tramoseats(y))
#' add_sa_item(jmp1, name = "x13-2", x = y, rjd3x13::spec_x13())
#' add_sa_item(jmp1, name = "tramo-2", x = y, rjd3tramoseats::spec_tramoseats())
#' save_workspace(jws, file.path(dir, "workspace.xml"))
#' @export
add_sa_item <- function(jmp, name, x, spec, ...){
  UseMethod("add_sa_item", x)
}
#'@export
add_sa_item.ts <- function(jmp, name, x, spec, ...) {
  jts <- rjd3toolkit::.r2jd_ts(x)
  if (inherits(spec, "JD3_X13_SPEC")) {
    jspec <- rjd3x13::.r2jd_spec_x13(spec)
  } else if (inherits(spec, "JD3_TRAMOSEATS_SPEC")) {
    jspec <- rjd3tramoseats::.r2jd_spec_tramoseats(spec)
  } else {
    stop("wrong type of spec")
  }
  .jcall(jmp, "V", "add",
         name,
         jts,
         .jcast(jspec, "jdplus/sa/base/api/SaSpecification"))
}
#'@export
add_sa_item.default <- function(jmp, name, x, spec, ...) {
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
  add_sa_item.ts(jmp = jmp,
              x = y,
              spec = spec,
              name = name,
              ...)
}

#' Replace or Remove a SaItem
#' `replace_sa_item()` replaces a SaItem of a multiprocessing and `remove_sa_item()` removes a SaItem from a multiprocessing
#'
#' @param jmp the multiprocessing to modify.
#' @param jsa the new SaItem.
#' @param idx index of the target SaItem.
#' @export
replace_sa_item <- function(jmp, idx, jsa) {
.jcall(jmp, "V", "set", as.integer(idx-1), jsa)
}
#' @name replace_sa_item
#' @export
remove_sa_item <- function(jmp, idx) {
  .jcall(jmp, "V", "remove", as.integer(idx-1))
}
#' @name replace_sa_item
#' @export
remove_all_sa_item <- function(jmp) {
  .jcall(jmp, "V", "removeAll")
  return(invisible(TRUE))
}
#' @name replace_sa_item
#' @export
transfer_series <- function(jmp_from, jmp_to, selected_series,
                            print_indications = TRUE)
{
  mp_from_sa_name <- .jmp_sa_name(jmp_from)
  mp_to_sa_name <- .jmp_sa_name(jmp_to)

  if (missing(selected_series) || is.null(selected_series)) {
    selected_series <- mp_from_sa_name
  }

  if (!all(selected_series %in% mp_from_sa_name)) {
    missing_series <- selected_series[!selected_series %in% mp_from_sa_name]
    stop("The series ", paste0(missing_series, collapse = ", "), " are missing from the first SA Processing. The replacement wasn't performed.")
  }

  for (serie_name in selected_series) {
    index_from <- which(serie_name == mp_from_sa_name)
    if (length(index_from) > 1) {
      stop("Several series from first SA Processing have the same name : ", serie_name)
    }
    jsa1 <- .jmp_sa(jmp_from, idx = index_from)

    index_to <- which(serie_name == mp_to_sa_name)
    if (length(index_to) > 1) {
      stop("Several series from second SA Processing have the same name : ", serie_name)
    } else if (length(index_to) == 0) {
      rjdemetra3::add_sa_item(jmp = jmp_to, name = serie_name, x = .jsa_read(jsa1))
    } else {
      rjdemetra3::replace_sa_item(jmp = jmp_to, jsa = jsa1, idx = index_to)
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
#' @param y the new data.
#' @export
set_specification <- function(jmp, idx, spec) {
  if (inherits(spec, "JD3_X13_SPEC")) {
    jspec <- rjd3x13::.r2jd_spec_x13(spec)
  } else if (inherits(spec, "JD3_TRAMOSEATS_SPEC")) {
    jspec <- rjd3tramoseats::.r2jd_spec_tramoseats(spec)
  } else {
    stop("wrong type of spec")
  }
  jspec <- .jcast(jspec, "jdplus/sa/base/api/SaSpecification")
  .jcall(jmp, "V", "setSpecification", as.integer(idx-1), jspec)
}
#' @name set_specification
#' @export
set_domain_specification <- function(jmp, idx, spec) {
  if (inherits(spec, "JD3_X13_SPEC")) {
    jspec <- rjd3x13::.r2jd_spec_x13(spec)
  } else if (inherits(spec, "JD3_TRAMOSEATS_SPEC")) {
    jspec <- rjd3tramoseats::.r2jd_spec_tramoseats(spec)
  } else {
    stop("wrong type of spec")
  }
  jspec <- .jcast(jspec, "jdplus/sa/base/api/SaSpecification")
  .jcall(jmp, "V", "setDomainSpecification", as.integer(idx-1), jspec)
}
#' Get/Set the Raw Data of a SaItem
#'
#' @inheritParams replace_sa_item
#' @param y the new raw time serie.
#' @param jsa a SaItem.
#' @export
set_raw_data <- function(jmp, idx, y) {
  .jcall(jmp, "V", "setData", as.integer(idx-1), rjd3toolkit::.r2jd_ts(y))
}
#' @name set_raw_data
#' @export
get_raw_data <- function(jsa) {
  jts<-.jcall(.jcall(jsa, "Ljdplus/sa/base/api/SaDefinition;", "getDefinition")
              , "Ljdplus/toolkit/base/api/timeseries/Ts;", "getTs")
  rjd3toolkit::.jd2r_ts(.jcall(jts, "Ljdplus/toolkit/base/api/timeseries/TsData;", "getData"))
}
#' Get/Set SaItem Comment
#'
#' @inheritParams set_raw_data
#' @param comment char containing the comment.
#' @export
set_comment <- function(jmp, idx, comment) {
  jsa <- .jmp_sa(jmp, idx = idx)
  jsa <- .jcall(jsa,
                "Ljdplus/sa/base/api/SaItem;",
                "withComment",
                comment)
  replace_sa_item(jmp, jsa = jsa, idx = idx)
}
#' @name set_comment
#' @export
get_comment <- function(jsa) {
  .jcall(jsa, "S", "getComment")
}

#' Set the name associated to a SaItem Comment
#'
#' @inheritParams set_raw_data
#' @param name char containing the name of the SAItem.
#' @seealso [.jsa_name()]
#' @export
set_name <- function(jmp, idx, name) {
  jsa <- .jmp_sa(jmp, idx = idx)
  jsa <- .jcall(jsa,
                "Ljdplus/sa/base/api/SaItem;",
                "withName",
                name)
  replace_sa_item(jmp, jsa = jsa, idx = idx)
}

# set_metadata <- function(jmp, ref_jsa, idx) {
#   jsa <- .jmp_sa(jmp, idx = idx)
#   jsa <- jsa$withInformations(ref_jsa$getMeta())
#   replace_sa_item(jmp, jsa = jsa, idx = idx)
# }
#' Set Time Series Metadata of a SaItem
#'
#' Function to set the time series metadata of a SaItem (provider, source of the data...)
#'
#' @inheritParams set_raw_data
#' @param ref_jsa a reference SaItem containing the metadata.
#'
#' @export
set_ts_metadata <- function(jmp, idx, ref_jsa) {
  jsa <- .jmp_sa(jmp, idx = idx)
  jts<-.jcall(.jcall(jsa, "Ljdplus/sa/base/api/SaDefinition;", "getDefinition")
              , "Ljdplus/toolkit/base/api/timeseries/Ts;", "getTs")
  jts_ref<-.jcall(.jcall(ref_jsa, "Ljdplus/sa/base/api/SaDefinition;", "getDefinition")
              , "Ljdplus/toolkit/base/api/timeseries/Ts;", "getTs")
  jtsbuilder <- .jcall(jts, "Ljdplus/toolkit/base/api/timeseries/Ts$Builder;",
                       "toBuilder")
  # jts_ref$getMeta()$getClass()$descriptorString()
  # .jcall(jtsbuilder,  "Ljdplus/toolkit/base/api/timeseries/Ts$Builder;",
  #        "meta",
  #        .jcall(jts_ref, "Ljava/util/Map;", "getMeta"))
  jts <- jtsbuilder$
    meta(.jcall(jts_ref, "Ljava/util/Map;", "getMeta"))$
    moniker(.jcall(jts_ref, "Ljdplus/toolkit/base/api/timeseries/TsMoniker;", "getMoniker"))$
    build()
  jsa <- .jcall(jsa,
                "Ljdplus/sa/base/api/SaItem;",
                "withTs",
                jts)
  replace_sa_item(jmp, jsa = jsa, idx = idx)
}
#' Get/Set SaItem Priority
#'
#' @inheritParams set_raw_data
#' @param priority integer containing the priority.
#' @export
set_priority <- function(jmp, idx, priority = 0) {
  jsa <- .jmp_sa(jmp, idx = idx)
  jsa <- .jcall(jsa,
                "Ljdplus/sa/base/api/SaItem;",
                "withPriority",
                as.integer(priority))
  replace_sa_item(jmp, jsa = jsa, idx = idx)
}
#' @name set_priority
#' @export
get_priority <- function(jsa) {
  .jcall(jsa, "I", "getPriority")
}


