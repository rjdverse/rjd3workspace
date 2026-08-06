#' @title Deprecated functions
#'
#' @param jmp,idx,jws,name,jsa,jsai,jsap,items,key,file,compute,policy,period,start,end,info,modelling_context,jsap_from,jsap_to,selected_sa_items,print_indications,spec Parameters.
#'
#' @returns
#' The same value as returned by the corresponding non-deprecated function.
#' The returned object represents an encoded identifier for a spreadsheet
#' series or collection.
#'
#' @name deprecated-rjd3workspace
NULL

#' @rdname deprecated-rjd3workspace
#' @export
.jmp_sa_count <- function(jmp) {
    .Deprecated("sap_sai_count")
    sap_sai_count(jmp)
}
#' @rdname deprecated-rjd3workspace
#' @export
.jsap_sa_count <- function(jmp) {
    .Deprecated("sap_sai_count")
    sap_sai_count(jmp)
}
#' @rdname deprecated-rjd3workspace
#' @export
.jsap_sai_count <- function(jsap) {
    .Deprecated("sap_sai_count")
    sap_sai_count(jsap)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jmp_name <- function(jmp) {
    .Deprecated("sap_name")
    sap_name(jmp)
}
#' @rdname deprecated-rjd3workspace
#' @export
.jsap_name <- function(jsap) {
    .Deprecated("sap_name")
    sap_name(jsap)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jmp_sa <- function(jmp, idx) {
    .Deprecated("jsap_sai")
    jsap_sai(jmp, idx)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jsap_sa <- function(jsap, idx) {
    .Deprecated("jsap_sai")
    jsap_sai(jsap, idx)
}
#' @rdname deprecated-rjd3workspace
#' @export
.jsap_sai <- function(jsap, idx) {
    .Deprecated("jsap_sai")
    jsap_sai(jsap, idx)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jmp_sa_name <- function(jmp) {
    .Deprecated("sap_sai_names")
    sap_sai_names(jmp)
}
#' @rdname deprecated-rjd3workspace
#' @export
.jsap_sa_name <- function(jsap) {
    .Deprecated("sap_sai_names")
    sap_sai_names(jsap)
}
#' @rdname deprecated-rjd3workspace
#' @export
.jsap_sai_names <- function(jsap) {
    .Deprecated("sap_sai_names")
    sap_sai_names(jsap)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jmp_load <- function(jmp) {
    .Deprecated("read_sap")
    read_sap(jmp)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jsa_read <- function(jsa) {
    .Deprecated("read_sai")
    read_sai(jsa)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jsa_results <- function(jsa, items = NULL) {
    .Deprecated("jsai_results")
    jsai_results(jsa, items)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jsai_results <- function(jsa, items = NULL) {
    .Deprecated("jsai_results")
    jsai_results(jsa, items)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jsa_jresults <- function(jsa) {
    .Deprecated("jsai_jresults")
    jsai_jresults(jsa)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jsai_jresults <- function(jsa) {
    .Deprecated("jsai_jresults")
    jsai_jresults(jsa)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jsa_metadata <- function(jsa, key) {
    .Deprecated("get_metadata")
    get_metadata(jsa, key)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jsai_metadata <- function(jsai, key) {
    .Deprecated("get_metadata")
    get_metadata(jsai, key)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jsa_ts_metadata <- function(jsa, key) {
    .Deprecated("get_ts_metadata")
    get_ts_metadata(jsa, key)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jsai_ts_metadata <- function(jsa, key) {
    .Deprecated("get_ts_metadata")
    get_ts_metadata(jsa, key)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jws_sap_count <- function(jws) {
    .Deprecated("ws_sap_count")
    ws_sap_count(jws)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jws_open <- function(file) {
    .Deprecated("jws_open")
    jws_open(file)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jread_workspace <- function(jws, compute = TRUE) {
    .Deprecated("jread_workspace")
    jread_workspace(jws, compute = TRUE)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jread_sap <- function(jsap) {
    .Deprecated("jread_sap")
    jread_sap(jsap)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jws_new <- function(modelling_context = NULL) {
    .Deprecated("jws_new")
    jws_new(modelling_context = NULL)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jws_sap_new <- function(jws, name) {
    .Deprecated("jws_sap_new")
    jws_sap_new(jws, name)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jws_make_copy <- function(jws) {
    .Deprecated("jws_make_copy")
    jws_make_copy(jws)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jsap_make_copy <- function(jsap) {
    .Deprecated("jsap_make_copy")
    jsap_make_copy(jsap)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jws_compute <- function(jws) {
    .Deprecated("jws_compute")
    jws_compute(jws)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jws_sap <- function(jws, idx) {
    .Deprecated("jws_sap")
    jws_sap(jws, idx)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jsai_name <- function(jsai) {
    .Deprecated("sai_name")
    sai_name(jsai)
}

#' @rdname deprecated-rjd3workspace
#' @export
.jsap_refresh <- function(
    jsap,
    policy = c(
        "FreeParameters",
        "Complete",
        "Outliers_StochasticComponent",
        "Outliers",
        "FixedParameters",
        "FixedAutoRegressiveParameters",
        "Fixed"
    ),
    period = 0,
    start = NULL,
    end = NULL,
    info = c("All", "Data", "None")
) {
    .Deprecated("jsap_refresh")
    jsap_refresh(
        jsap,
        policy = c(
            "FreeParameters",
            "Complete",
            "Outliers_StochasticComponent",
            "Outliers",
            "FixedParameters",
            "FixedAutoRegressiveParameters",
            "Fixed"
        ),
        period = 0,
        start = NULL,
        end = NULL,
        info = c("All", "Data", "None")
    )
}

#' @rdname deprecated-rjd3workspace
#' @export
.jws_refresh <- function(
    jws,
    policy = c(
        "FreeParameters",
        "Complete",
        "Outliers_StochasticComponent",
        "Outliers",
        "FixedParameters",
        "FixedAutoRegressiveParameters",
        "Fixed"
    ),
    period = 0,
    start = NULL,
    end = NULL,
    info = c("All", "Data", "None")
) {
    .Deprecated("jws_refresh")
    jws_refresh(
        jws,
        policy = c(
            "FreeParameters",
            "Complete",
            "Outliers_StochasticComponent",
            "Outliers",
            "FixedParameters",
            "FixedAutoRegressiveParameters",
            "Fixed"
        ),
        period = 0,
        start = NULL,
        end = NULL,
        info = c("All", "Data", "None")
    )
}

#' @rdname deprecated-rjd3workspace
#' @export
transfer_series <- function(
    jsap_from,
    jsap_to,
    selected_sa_items,
    print_indications = TRUE
) {
    .Deprecated("transfer_sa_item")
    transfer_sa_item(
        jsap_from,
        jsap_to,
        selected_sa_items,
        print_indications = TRUE
    )
}

#' @rdname deprecated-rjd3workspace
#' @export
.jws_add <- function(jws, jsap) {
    .Deprecated("jws_add")
    jws_add(jws, jsap)
    .jcall(jws, "V", "add", jsap)
}

#' @rdname deprecated-rjd3workspace
#' @export
set_domain_specification <- function(jsap, idx, spec) {
    .Deprecated("set_reference_specification")
    set_reference_specification(jsap, idx, spec)
}

#' @rdname deprecated-rjd3workspace
#' @export
get_domain_specification <- function(jsai) {
    .Deprecated("get_reference_specification")
    get_reference_specification(jsai)
}

#' @rdname deprecated-rjd3workspace
#' @export
get_point_specification <- function(jsai) {
    .Deprecated("get_result_specification")
    get_result_specification(jsai)
}
