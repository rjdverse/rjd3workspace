# Package index

## Workspaces: open, read, create, copy, save

Functions to open an existing JDemetra+ workspace, computing allows to
retrieve results generated from specifications and raw data

- [`jws_new()`](https://rjdverse.github.io/rjd3workspace/reference/jws_new.md)
  [`jws_sap_new()`](https://rjdverse.github.io/rjd3workspace/reference/jws_new.md)
  : Create a Workspace or SA-Processing
- [`jws_open()`](https://rjdverse.github.io/rjd3workspace/reference/jws_open.md)
  : Open an existing 'JDemetra+' Workspace
- [`jws_compute()`](https://rjdverse.github.io/rjd3workspace/reference/jws_compute.md)
  : Compute a Workspace
- [`read_sap()`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md)
  [`jread_sap()`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md)
  [`read_workspace()`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md)
  [`jread_workspace()`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md)
  : Read all SA-Items from a Workspace or SA-Processing
- [`jsap_refresh()`](https://rjdverse.github.io/rjd3workspace/reference/refresh.md)
  [`jws_refresh()`](https://rjdverse.github.io/rjd3workspace/reference/refresh.md)
  : Refresh a Workspace or SA-Processing
- [`set_context()`](https://rjdverse.github.io/rjd3workspace/reference/set_context.md)
  : Set Context of a Workspace
- [`jsap_make_copy()`](https://rjdverse.github.io/rjd3workspace/reference/make_copy.md)
  [`jws_make_copy()`](https://rjdverse.github.io/rjd3workspace/reference/make_copy.md)
  : Copy a Workspace or SA-Processing
- [`save_workspace()`](https://rjdverse.github.io/rjd3workspace/reference/save_workspace.md)
  : Save Workspace

## Workspaces: get information

Functions to get information on workspaces

- [`check_information()`](https://rjdverse.github.io/rjd3workspace/reference/check_information.md)
  : Check if JD+ object exists
- [`sap_sai_count()`](https://rjdverse.github.io/rjd3workspace/reference/ws_sap_count.md)
  [`ws_sap_count()`](https://rjdverse.github.io/rjd3workspace/reference/ws_sap_count.md)
  : Count SA-Processings or SA-Items
- [`get_context()`](https://rjdverse.github.io/rjd3workspace/reference/get_context.md)
  : Get Context from Workspace

## Update Path to Raw data

Functions to update path to raw data for a workspace, a SA-Processing or
SA-Item

- [`spreadsheet_update_path()`](https://rjdverse.github.io/rjd3workspace/reference/spreadsheet_update_path.md)
  : Update the path to raw data in a workspace (spreadsheet)
- [`txt_update_path()`](https://rjdverse.github.io/rjd3workspace/reference/txt_update_path.md)
  : Update the path to raw data in a workspace (txt/csv file)

## SA-Processings

Functions to create, read SA-Processings and wrangle SA-Items (add,
remove…)

- [`jsap_sai()`](https://rjdverse.github.io/rjd3workspace/reference/jws_sap.md)
  [`jws_sap()`](https://rjdverse.github.io/rjd3workspace/reference/jws_sap.md)
  : Extract a SA-Processing or a SA-Item

- [`jws_new()`](https://rjdverse.github.io/rjd3workspace/reference/jws_new.md)
  [`jws_sap_new()`](https://rjdverse.github.io/rjd3workspace/reference/jws_new.md)
  : Create a Workspace or SA-Processing

- [`jws_add()`](https://rjdverse.github.io/rjd3workspace/reference/jws_add.md)
  : Add a SA-Processing to a Workspace

- [`jsap_make_copy()`](https://rjdverse.github.io/rjd3workspace/reference/make_copy.md)
  [`jws_make_copy()`](https://rjdverse.github.io/rjd3workspace/reference/make_copy.md)
  : Copy a Workspace or SA-Processing

- [`add_sa_item()`](https://rjdverse.github.io/rjd3workspace/reference/add_sa_item.md)
  : Add a SA-item to a SAProcessing

- [`replace_sa_item()`](https://rjdverse.github.io/rjd3workspace/reference/replace_sa_item.md)
  [`remove_sa_item()`](https://rjdverse.github.io/rjd3workspace/reference/replace_sa_item.md)
  [`remove_all_sa_item()`](https://rjdverse.github.io/rjd3workspace/reference/replace_sa_item.md)
  : Replace or Remove a SA-item

- [`transfer_sa_item()`](https://rjdverse.github.io/rjd3workspace/reference/transfer_sa_item.md)
  :

  Copy & paste SA-items from one `SA-Processing` to another

- [`sai_name()`](https://rjdverse.github.io/rjd3workspace/reference/sap_name.md)
  [`sap_name()`](https://rjdverse.github.io/rjd3workspace/reference/sap_name.md)
  [`sap_sai_names()`](https://rjdverse.github.io/rjd3workspace/reference/sap_name.md)
  : Get the name of a SAProcessing or one (or all) Sa-item

- [`read_sap()`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md)
  [`jread_sap()`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md)
  [`read_workspace()`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md)
  [`jread_workspace()`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md)
  : Read all SA-Items from a Workspace or SA-Processing

- [`jsap_refresh()`](https://rjdverse.github.io/rjd3workspace/reference/refresh.md)
  [`jws_refresh()`](https://rjdverse.github.io/rjd3workspace/reference/refresh.md)
  : Refresh a Workspace or SA-Processing

- [`sap_sai_count()`](https://rjdverse.github.io/rjd3workspace/reference/ws_sap_count.md)
  [`ws_sap_count()`](https://rjdverse.github.io/rjd3workspace/reference/ws_sap_count.md)
  : Count SA-Processings or SA-Items

## SA-Items

Functions to extract, read SA-Items and set specifications

- [`jsap_sai()`](https://rjdverse.github.io/rjd3workspace/reference/jws_sap.md)
  [`jws_sap()`](https://rjdverse.github.io/rjd3workspace/reference/jws_sap.md)
  : Extract a SA-Processing or a SA-Item
- [`read_sai()`](https://rjdverse.github.io/rjd3workspace/reference/read_sai.md)
  : Read an SA-item
- [`get_results()`](https://rjdverse.github.io/rjd3workspace/reference/get-results.md)
  [`.jsai_results()`](https://rjdverse.github.io/rjd3workspace/reference/get-results.md)
  [`.jsai_jresults()`](https://rjdverse.github.io/rjd3workspace/reference/get-results.md)
  : Extract results from a SA-item
- [`set_specification()`](https://rjdverse.github.io/rjd3workspace/reference/set_specification.md)
  [`set_domain_specification()`](https://rjdverse.github.io/rjd3workspace/reference/set_specification.md)
  : Set Specification in a Sa-Item
- [`get_domain_specification()`](https://rjdverse.github.io/rjd3workspace/reference/get-specification.md)
  [`get_estimation_specification()`](https://rjdverse.github.io/rjd3workspace/reference/get-specification.md)
  [`get_point_specification()`](https://rjdverse.github.io/rjd3workspace/reference/get-specification.md)
  [`get_active_specification()`](https://rjdverse.github.io/rjd3workspace/reference/get-specification.md)
  : Get Specification in a Sa-Item

## SA-Items: metadata

Functions to get and set metadata from an SA-Item

- [`get_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/get_metadata.md)
  [`get_ts_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/get_metadata.md)
  : Extract Metadata from a SA-Item
- [`set_comment()`](https://rjdverse.github.io/rjd3workspace/reference/set_comment.md)
  [`get_comment()`](https://rjdverse.github.io/rjd3workspace/reference/set_comment.md)
  : Get/Set Comment from a SA-item
- [`set_name()`](https://rjdverse.github.io/rjd3workspace/reference/set_name.md)
  : Set the name of a SA-item
- [`set_priority()`](https://rjdverse.github.io/rjd3workspace/reference/set_priority.md)
  [`get_priority()`](https://rjdverse.github.io/rjd3workspace/reference/set_priority.md)
  : Get/Set SA-item Priority
- [`set_raw_data()`](https://rjdverse.github.io/rjd3workspace/reference/set_raw_data.md)
  [`get_raw_data()`](https://rjdverse.github.io/rjd3workspace/reference/set_raw_data.md)
  : Get/Set Raw Data in a SA-item
- [`set_ts()`](https://rjdverse.github.io/rjd3workspace/reference/set_ts.md)
  [`get_ts()`](https://rjdverse.github.io/rjd3workspace/reference/set_ts.md)
  : Get/Set the (JDemetra+) time series of a SA-item
- [`set_ts_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/set_ts_metadata.md)
  [`put_ts_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/set_ts_metadata.md)
  [`set_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/set_ts_metadata.md)
  [`put_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/set_ts_metadata.md)
  : Set (JDemetra+) Metadata of a SA-item

## Calendars and regressors

Functions to add calendars and regressors (variables) to a workspace

- [`add_calendar()`](https://rjdverse.github.io/rjd3workspace/reference/add_calendar.md)
  : Add a Calendar to a Workspace
- [`add_variables()`](https://rjdverse.github.io/rjd3workspace/reference/add_variables.md)
  : Add a Variable to a JD+ Workspace
- [`read_calendars()`](https://rjdverse.github.io/rjd3workspace/reference/read_calendars.md)
  : Read a Calendar file
- [`read_variables()`](https://rjdverse.github.io/rjd3workspace/reference/read_variables.md)
  : Read auxiliary regressors file
- [`write_calendars()`](https://rjdverse.github.io/rjd3workspace/reference/write_calendars.md)
  : Write a Calendar file
- [`write_variables()`](https://rjdverse.github.io/rjd3workspace/reference/write_variables.md)
  : Write regressors file

## Specification Files

Functions to read/write specification files

- [`x13_read_spec()`](https://rjdverse.github.io/rjd3workspace/reference/x13_read_spec.md)
  : Read a X13 specification file
- [`x13_write_spec()`](https://rjdverse.github.io/rjd3workspace/reference/x13_write_spec.md)
  : Write a X13 specification file
- [`regarima_read_spec()`](https://rjdverse.github.io/rjd3workspace/reference/regarima_read_spec.md)
  : Read a Reg-Arima specification file
- [`regarima_write_spec()`](https://rjdverse.github.io/rjd3workspace/reference/regarima_write_spec.md)
  : Write a Reg-Arima specification file
- [`tramoseats_read_spec()`](https://rjdverse.github.io/rjd3workspace/reference/tramoseats_read_spec.md)
  : Read a Tramo-Seats specification file
- [`tramoseats_write_spec()`](https://rjdverse.github.io/rjd3workspace/reference/tramoseats_write_spec.md)
  : Write a Tramo-Seats specification file
- [`tramo_read_spec()`](https://rjdverse.github.io/rjd3workspace/reference/tramo_read_spec.md)
  : Read a Tramo specification file
- [`tramo_write_spec()`](https://rjdverse.github.io/rjd3workspace/reference/tramo_write_spec.md)
  : Write a Tramo specification file

## Deprecated functions

Use new version

- [`.jmp_sa_count()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsap_sa_count()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsap_sai_count()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jmp_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsap_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jmp_sa()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsap_sa()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsap_sai()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jmp_sa_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsap_sa_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsap_sai_names()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jmp_load()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsa_read()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsa_results()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsa_jresults()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsa_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsai_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsa_ts_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsai_ts_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jws_sap_count()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jws_open()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jread_workspace()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jread_sap()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jws_new()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jws_sap_new()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jws_make_copy()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsap_make_copy()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jws_compute()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jws_sap()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsai_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jsap_refresh()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jws_refresh()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`transfer_series()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  [`.jws_add()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  : Deprecated functions
