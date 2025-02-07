
#' @title Check existing JD+ object
#'
#' @param jws workspace object
#' @param idx_sap index (or indices) of the SA-Processing (s) to check
#' @param idx_sai index (or indices) of the SA-Item(s) to check.
#'
#' @return
#' This function returns either a boolean (TRUE) if the SAI and the SAP exist in
#' the WS, or an error specifying the not found object.
#'
#' @details
#'
#' If the object idx_sai is NULL then the function will only check if the
#' workspace contains a SA-Processing at the index idx_sap.
#' If the object idx_sap is NULL then the function will check if every
#' SA-Processing of the workspace contains a SA-Item at the index idx_sai.
#' If the object idx_sap is NULL and idx_sai is NULL then the function won't do any check.
#'
#' If the object idx_sap and / or idx_sai have a length > 1 then the checks are
#' iterated over all the indices.
#'
#' @examples
#'
#' # ws <- .jws_open(file = "ws_production.xml")
#' #
#' # # Check if the SA-Item 3 in the SA-Processing 1 exists
#' # check_information(jws = ws, idx_sap = 1, idx_sai = 3)
#'
#' # # Check if the SA-Items 1, 2 and 5 in the SA-Processing 1 exist
#' # check_information(jws = ws, idx_sap = 1, idx_sai = c(1, 2, 5))
#'
check_information <- function(jws, idx_sap = NULL, idx_sai = NULL) {

    if (!is.null(idx_sap) && max(idx_sap) > .jws_sap_count(jws)) {
        stop("The SAP n\u00b0", max(idx_sap), "doesn't exist")
    } else if (is.null(idx_sap)) {
        idx_sap <- seq_len(.jws_sap_count(jws))
    }

    for (id_sap in idx_sap) {
        jsap_i <- .jws_sap(jws, idx = id_sap)

        if (!is.null(idx_sai) && max(idx_sai) > .jsap_sa_count(jsap_i)) {
            stop("The SAI n\u00b0", max(idx_sai),
                 " doesn't exist in the SAP n\u00b0", id_sap)
        }
    }

    return(invisible(TRUE))
}

#' @title Update the path to a spreadsheet specified in a workspace
#' @inheritParams check_information
#' @param new_path new path to the spreadsheet containing raw data
#'
#' @return
#' This function returns either NULL if the update was successful, or an
#' error.
#'
#' @export
#'
#' @details
#' The spreadsheet file must be a .xlsx file. .xls file are not accepted in JDemetra+ v3.x.
#'
#' @examples
#'
#' # ws <- .jws_open(file = "ws_production.xml")
#' #
#' # # Update the entire second SA-Processing of the `ws` workspace with a new path
#' # spreadsheet_update_path(
#' #     jws = ws,
#' #     new_path = normalizePath("./data/IPI_nace4.xlsx", mustWork = TRUE),
#' #     idx_sap = 2L
#' # )
#'
spreadsheet_update_path <- function(jws, new_path, idx_sap = NULL, idx_sai = NULL) {

    new_path <- normalizePath(new_path, mustWork = TRUE)
    check_information(jws = jws, idx_sap, idx_sai)

    idx_sap <- unique(idx_sap)
    idx_sai <- unique(idx_sai)

    if (is.null(idx_sap)) {
        idx_sap <- seq_len(.jws_sap_count(jws))
    }

    for (id_sap in idx_sap) {
        jsap <- .jws_sap(jws, idx = id_sap)

        idx_sai_tmp <- idx_sai
        if (is.null(idx_sai)) {
            idx_sai_tmp <- seq_len(.jsap_sa_count(jsap))
        }

        for (id_sai in idx_sai_tmp) {
            jsai <- .jsap_sai(jsap, idx = id_sai)

            old_jd3_ts <- get_ts(jsai)
            properties <- rjd3providers::spreadsheet_id_properties(old_jd3_ts$metadata$`@id`)
            properties$file <- new_path
            new_id <- rjd3providers::spreadsheet_to_id(properties)

            new_jd3_ts <- old_jd3_ts
            new_jd3_ts$metadata$`@id` <- new_id
            new_jd3_ts$moniker$id <- new_id
            new_jd3_ts$moniker$source <- new_jd3_ts$metadata$`@source`
            set_ts(jsap = jsap, idx = id_sai, y = new_jd3_ts)
        }
    }

    return(invisible(NULL))
}

#' Update the path to a csv/txt file specified in a workspace
#' @inheritParams check_information
#' @param new_path new path to the csv/txt file containing raw data
#'
#' @return
#' This function returns either NULL if the update was successful, or an
#' error
#'
#' @export
#'
#' @examples
#'
#' # ws <- .jws_open(file = "ws_production.xml")
#' #
#' # # Update the entire second SA-Processing of the `ws` workspace with a new path
#' # txt_update_path(
#' #     jws = ws,
#' #     new_path = normalizePath("./data/IPI_nace4.csv", mustWork = TRUE),
#' #     idx_sap = 1L
#' # )
#'
txt_update_path <- function(jws, new_path, idx_sap = NULL, idx_sai = NULL) {

    new_path <- normalizePath(new_path, mustWork = TRUE)
    check_information(jws = jws, idx_sap, idx_sai)

    idx_sap <- unique(idx_sap)
    idx_sai <- unique(idx_sai)

    if (is.null(idx_sap)) {
        idx_sap <- seq_len(.jws_sap_count(jws))
    }

    for (id_sap in idx_sap) {
        jsap <- .jws_sap(jws, idx = id_sap)

        idx_sai_tmp <- idx_sai
        if (is.null(idx_sai)) {
            idx_sai_tmp <- seq_len(.jsap_sa_count(jsap))
        }

        for (id_sai in idx_sai_tmp) {
            jsai <- .jsap_sai(jsap, idx = id_sai)

            old_jd3_ts <- get_ts(jsai)
            properties <- rjd3providers::txt_id_properties(old_jd3_ts$metadata$`@id`)
            properties$file <- new_path
            new_id <- rjd3providers::txt_to_id(properties)

            new_jd3_ts <- old_jd3_ts
            new_jd3_ts$metadata$`@id` <- new_id
            new_jd3_ts$moniker$id <- new_id
            new_jd3_ts$moniker$source <- new_jd3_ts$metadata$`@source`
            set_ts(jsap = jsap, idx = id_sai, y = new_jd3_ts)
        }
    }

    return(invisible(NULL))
}
