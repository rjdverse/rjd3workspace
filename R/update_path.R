
#' @title Check if JD+ object exists
#'
#' @param jws workspace object
#' @param idx_sap index (or indices) of the SAProcessing(s)
#' @param idx_sai index (or indices) of the SA-item(s).
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
#' @examplesIf jversion >= 17
#'
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' jws <- jws_open(file)
#'
#' # Check if the SA-Item 3 in the SA-Processing 1 exists
#' rjd3workspace:::check_information(jws = jws, idx_sap = 1, idx_sai = 3)
#'
#' # Check if the SA-Items 1, 2 and 5 in the SA-Processing 1 exist
#' rjd3workspace:::check_information(jws = jws, idx_sap = 1, idx_sai = c(1, 2, 4))
#'
check_information <- function(jws, idx_sap = NULL, idx_sai = NULL) {

    if (!is.null(idx_sap) && max(idx_sap) > ws_sap_count(jws)) {
        stop("The SAP n\u00b0", max(idx_sap), "doesn't exist")
    } else if (is.null(idx_sap)) {
        idx_sap <- seq_len(ws_sap_count(jws))
    }

    for (id_sap in idx_sap) {
        jsap_i <- jws_sap(jws, idx = id_sap)

        if (!is.null(idx_sai) && max(idx_sai) > sap_sai_count(jsap_i)) {
            stop("The SAI n\u00b0", max(idx_sai),
                 " doesn't exist in the SAP n\u00b0", id_sap)
        }
    }

    return(TRUE)
}

#' @title Update the path to raw data in a workspace (spreadsheet)
#'
#' @inheritParams check_information
#'
#' @param new_path new path to the spreadsheet containing raw data
#'
#' @return
#' This function returns either NULL if the update was successful, or an
#' error.

#' @details
#' The spreadsheet file must be a .xlsx file. .xls files are not accepted in JDemetra+ v3.x.
#'
#' @examplesIf jversion >= 17
#'
#' # Load a workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' my_ws <- jws_open(file)
#'
#' # Update the entire second SA-Processing of the `my_ws` workspace with a new path to raw data
#' spreadsheet_update_path(
#'     jws = my_ws,
#'     new_path = system.file("data", "IPI_nace4.xlsx", package = "rjd3workspace"),
#'     idx_sap = 2
#' )
#'
#' # Select one (the 2nd) SA-item from second SA-Processing
#' sap2 <- jws_sap(my_ws, 2)
#' sai2 <- jsap_sai(sap2, 2)
#'
#' # Check path
#' get_ts_metadata(sai2, "@id")
#'
#' @export
spreadsheet_update_path <- function(jws, new_path, idx_sap = NULL, idx_sai = NULL) {

    new_path <- normalizePath(new_path, mustWork = TRUE)
    check_information(jws = jws, idx_sap, idx_sai)

    idx_sap <- unique(idx_sap)
    idx_sai <- unique(idx_sai)

    if (is.null(idx_sap)) {
        idx_sap <- seq_len(ws_sap_count(jws))
    }

    for (id_sap in idx_sap) {
        jsap <- jws_sap(jws, idx = id_sap)

        idx_sai_tmp <- idx_sai
        if (is.null(idx_sai)) {
            idx_sai_tmp <- seq_len(sap_sai_count(jsap))
        }

        for (id_sai in idx_sai_tmp) {
            jsai <- jsap_sai(jsap, idx = id_sai)
            jsai_id <- get_ts_metadata(jsai, "@id")
            nid <- rjd3providers::spreadsheet_change_file(id = jsai_id, nfile = new_path)
            put_ts_metadata(jsap, id_sai, "@id", nid)
        }
    }

    return(invisible(NULL))
}

#' @title Update the path to raw data in a workspace (txt/csv file)
#'
#' @inheritParams check_information
#'
#' @param new_path new path to the csv/txt file containing raw data
#'
#' @return
#' This function returns either NULL if the update was successful, or an
#' error
#' @examplesIf jversion >= 17
#'
#' # Load a workspace
#' file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#' my_ws <- jws_open(file)
#'
#' # Update the entire second SA-Processing of the `my_ws` workspace with a new path to raw data
#' txt_update_path(
#'     jws = my_ws,
#'     new_path = system.file("data", "IPI_nace4.csv", package = "rjd3workspace"),
#'     idx_sap = 1
#' )
#'
#' # Select one (the 2nd) SA-item from first SA-Processing
#' sap1 <- jws_sap(my_ws, 1)
#' sai2 <- jsap_sai(sap1, 2)
#'
#' # Check path
#' get_ts_metadata(sai2, "@id")
#'
#' @export
txt_update_path <- function(jws, new_path, idx_sap = NULL, idx_sai = NULL) {

    new_path <- normalizePath(new_path, mustWork = TRUE)
    check_information(jws = jws, idx_sap, idx_sai)

    idx_sap <- unique(idx_sap)
    idx_sai <- unique(idx_sai)

    if (is.null(idx_sap)) {
        idx_sap <- seq_len(ws_sap_count(jws))
    }

    for (id_sap in idx_sap) {
        jsap <- jws_sap(jws, idx = id_sap)

        idx_sai_tmp <- idx_sai
        if (is.null(idx_sai)) {
            idx_sai_tmp <- seq_len(sap_sai_count(jsap))
        }

        for (id_sai in idx_sai_tmp) {
            jsai <- jsap_sai(jsap, idx = id_sai)
            jsai_id <- get_ts_metadata(jsai, "@id")
            nid <- rjd3providers::txt_change_file(id = jsai_id, nfile = new_path)
            put_ts_metadata(jsap, id_sai, "@id", nid)
        }
    }

    return(invisible(NULL))
}
