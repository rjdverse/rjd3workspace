#' Deprecated functions
#'
#'
#' @name deprecated-rjd3workspace
#' @export
.jmp_load <- function(jmp) {
    .Deprecated("read_sap")
    read_sap(jmp)
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
#' @name deprecated-rjd3workspace
#' @export
.jsap_sai_count <- function(jsap) {
    .Deprecated("sap_sai_count")
    sap_sai_count(jsap)
}
#' @name deprecated-rjd3workspace
#' @export
.jws_sap_count <- function(jws) {
    .Deprecated("ws_sap_count")
    ws_sap_count(jws)
}
#' @name deprecated-rjd3workspace
#' @export
.jws_open <- function(file) {
    .Deprecated("jws_open")
    jws_open(file)
}
#' @name deprecated-rjd3workspace
#' @export
.jread_workspace <- function(jws, compute = TRUE) {
    .Deprecated("jread_workspace")
    jread_workspace(jws, compute = TRUE)
}
#' @name deprecated-rjd3workspace
#' @export
.jread_sap <- function(jsap) {
    .Deprecated("jread_sap")
    jread_sap(jsap)
}
#' @name deprecated-rjd3workspace
#' @export
.jws_new <- function(modelling_context = NULL) {
    .Deprecated("jws_new")
    jws_new(modelling_context = NULL)
}
#' @name deprecated-rjd3workspace
#' @export
.jws_sap_new <- function(jws, name) {
    .Deprecated("jws_sap_new")
    jws_sap_new(jws, name)
}
#' @name deprecated-rjd3workspace
#' @export
.jws_make_copy <- function(jws) {
    .Deprecated("jws_make_copy")
    jws_make_copy(jws)
}
#' @name deprecated-rjd3workspace
#' @export
.jsap_make_copy <- function(jsap) {
    .Deprecated("jsap_make_copy")
    jsap_make_copy(jsap)
}
#' @name deprecated-rjd3workspace
#' @export
.jws_compute <- function(jws) {
    .Deprecated("jws_compute")
    jws_compute(jws)
}
#' @name deprecated-rjd3workspace
#' @export
.jsap_sai <- function(jsap, idx) {
    .Deprecated("jsap_sai")
    jws_sap_sai(jsap, idx)
}
#' @name deprecated-rjd3workspace
#' @export
.jws_sap <- function(jws, idx) {
    .Deprecated("jws_sap")
    jws_sap(jws, idx)
}
#' @name deprecated-rjd3workspace
#' @export
.jsai_name <- function(jsai) {
    .Deprecated("sai_name")
    sai_name(jsai)
}
#' @name deprecated-rjd3workspace
#' @export
.jsap_name <- function(jsap) {
    .Deprecated("sap_name")
    sap_name(jsap)
}
#' @name deprecated-rjd3workspace
#' @export
.jsap_sai_names <- function(jsap) {
    .Deprecated("sap_sai_names")
    sap_name(jsap)
}
#' @name deprecated-rjd3workspace
#' @export
.jsap_refresh <- function(jsap,
                          policy = c("FreeParameters", "Complete",
                                     "Outliers_StochasticComponent",
                                     "Outliers", "FixedParameters",
                                     "FixedAutoRegressiveParameters", "Fixed"),
                          period = 0,
                          start = NULL,
                          end = NULL,
                          info = c("All", "Data", "None")) {
    .Deprecated("jsap_refresh")
    jsap_refresh(jsap,
                 policy = c("FreeParameters", "Complete",
                            "Outliers_StochasticComponent",
                            "Outliers", "FixedParameters",
                            "FixedAutoRegressiveParameters", "Fixed"),
                 period = 0,
                 start = NULL,
                 end = NULL,
                 info = c("All", "Data", "None"))
}
#' @name deprecated-rjd3workspace
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
    .Deprecated("jws_refresh")
    jws_refresh(jws,
                 policy = c("FreeParameters", "Complete",
                            "Outliers_StochasticComponent",
                            "Outliers", "FixedParameters",
                            "FixedAutoRegressiveParameters", "Fixed"),
                 period = 0,
                 start = NULL,
                 end = NULL,
                 info = c("All", "Data", "None"))
}

#' @name deprecated-rjd3workspace
#' @export
transfer_series <- function(jsap_from, jsap_to, selected_sa_items,
                              print_indications = TRUE) {
    .Deprecated("transfer_sa_item")
    transfer_sa_item(jsap_from, jsap_to, selected_sa_items,
                     print_indications = TRUE)
}
