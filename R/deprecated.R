#' Deprecated functions
#'
#'
#' @param jmp,idx,jws,name Parameters.
#' @name deprecated-rjd3workspace
#' @export
.jmp_sa_count <- function(jmp) {
    .Deprecated(".jsap_sa_count")
    .jsap_sa_count(jmp)
}


#' @name deprecated-rjd3workspace
#' @export
.jmp_name <- function(jmp) {
    .Deprecated(".jsap_name")
    .jsap_name(jmp)
}

#' @name deprecated-rjd3workspace
#' @export
.jmp_sa <- function(jmp, idx) {
    .Deprecated(".jsap_sa")
    .jsap_sai(jmp, idx)
}
#' @name deprecated-rjd3workspace
#' @export
.jmp_sa_name <- function(jmp) {
    .Deprecated(".jsap_sa_name")
    .jsap_sa_name(jmp)
}

#' @name deprecated-rjd3workspace
#' @export
.jmp_load <- function(jmp) {
    .Deprecated("read_sap")
    read_sap(jmp)
}
#' @name deprecated-rjd3workspace
#' @export
.jws_multiprocessing <- function(jws, idx) {
    .Deprecated(".jws_sap")
    .jws_sap(jws, idx)
}
#' @name deprecated-rjd3workspace
#' @export
.jws_multiprocessing_new <- function(jws, name) {
    .Deprecated(".jws_sap_new")
    .jws_sap_new(jws, name)
}
#' @name deprecated-rjd3workspace
#' @export
.jws_multiprocessing_count <- function(jws) {
    .Deprecated(".jws_sap_count")
    .jws_sap_count(jws)
}
#' @name deprecated-rjd3workspace
#' @export
.jsap_sa <- function(jsap, idx) {
    .Deprecated(".jsap_sai")
    .jsap_sai(jsap, idx)
}
#' @name deprecated-rjd3workspace
#' @export
.jsa_read <- function(jsa) {
    .Deprecated("read_sai")
    read_sai(jsa)
}
#' @name deprecated-rjd3workspace
#' @export
.jsa_results <- function(jsa, items = NULL) {
    .Deprecated(".jsai_results")
    .jsai_results(jsa, items)
}
#' @name deprecated-rjd3workspace
#' @export
.jsa_jresults <- function(jsa) {
    .Deprecated(".jsai_jresults")
    .jsai_jresults(jsa)
}
#' @name deprecated-rjd3workspace
#' @export
.jsa_name <- function(jsa) {
    .Deprecated(".jsai_name")
    .jsai_name(jsa)
}
#' @name deprecated-rjd3workspace
#' @export
.jsa_metadata <- function(jsa, key) {
    .Deprecated(".jsai_metadata")
    .jsai_metadata(jsa, key)
}
#' @name deprecated-rjd3workspace
#' @export
.jsa_ts_metadata <- function(jsa, key) {
    .Deprecated(".jsai_ts_metadata")
    .jsai_ts_metadata(jsa, key)
}
