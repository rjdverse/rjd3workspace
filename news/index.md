# Changelog

## rjd3workspace 3.7.1

CRAN release: 2026-04-02

All notable changes to this project will be documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.1.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

### [Unreleased](https://github.com/rjdverse/rjd3workspace/compare/v3.7.1...HEAD)

### [3.7.1](https://github.com/rjdverse/rjd3workspace/compare/v3.6.0...v3.7.1) - 2026-04-03

#### Changed

- Use
  [`get_java_version()`](https://rdrr.io/pkg/rjd3jars/man/jd3_utilities.html)
  instead of `.jversion`

#### Added

- First release on
  [CRAN](https://cran.r-project.org/web/packages/rjd3workspace/index.html)
- New function
  [`get_domain_specification()`](https://rjdverse.github.io/rjd3workspace/reference/get-specification.md),
  [`get_point_specification()`](https://rjdverse.github.io/rjd3workspace/reference/get-specification.md),
  [`get_active_specification()`](https://rjdverse.github.io/rjd3workspace/reference/get-specification.md)
  and
  [`get_estimation_specification()`](https://rjdverse.github.io/rjd3workspace/reference/get-specification.md)
  to extract the domain, point, active and estimation specfification.
- New function
  [`get_results()`](https://rjdverse.github.io/rjd3workspace/reference/get-results.md)
  to extract the results from a SA-Item

### [3.6.0](https://github.com/rjdverse/rjd3workspace/compare/v3.5.1...v3.6.0) - 2025-12-01

#### Added

- New function
  [`set_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/set_ts_metadata.md)
  to copy the metadata from another SAItem
- New function
  [`put_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/set_ts_metadata.md)
  to update a SAI with any metadata (key, value)
- Residuals are available
  [\#3](https://github.com/rjdverse/rjd3workspace/issues/3)

#### Deprecated

- Function
  [`.jws_add()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`jws_add()`](https://rjdverse.github.io/rjd3workspace/reference/jws_add.md)
  instead.

#### Changed

- [`write_calendars()`](https://rjdverse.github.io/rjd3workspace/reference/write_calendars.md)
  accepts now multiple calendars
  [\#95](https://github.com/rjdverse/rjd3workspace/issues/95)
- New JARS related to version
  [3.6.0](https://github.com/jdemetra/jdplus-main/releases/tag/v3.6.0)

### [3.5.1](https://github.com/rjdverse/rjd3workspace/compare/v3.5.0...v3.5.1) - 2025-06-19

#### Added

- For compatibility reasons, some functions have been added again
  ([`.jmp_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md),
  [`.jmp_sa()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md),
  [`.jmp_sa_count()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md),
  [`.jmp_sa_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md),
  [`.jsap_sa_count()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md),
  [`.jsap_sa_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md))
- New functions have deprecated old functions
  ([`get_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/get_metadata.md),
  [`get_ts_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/get_metadata.md),
  [`transfer_sa_item()`](https://rjdverse.github.io/rjd3workspace/reference/transfer_sa_item.md))

#### Deprecated

- Function
  [`.jmp_sa_count()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`sap_sai_count()`](https://rjdverse.github.io/rjd3workspace/reference/ws_sap_count.md)
  instead.
- Function
  [`.jsap_sa_count()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`sap_sai_count()`](https://rjdverse.github.io/rjd3workspace/reference/ws_sap_count.md)
  instead.
- Function
  [`.jmp_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`sap_name()`](https://rjdverse.github.io/rjd3workspace/reference/sap_name.md)
  instead.
- Function
  [`.jmp_sa()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`jsap_sai()`](https://rjdverse.github.io/rjd3workspace/reference/jws_sap.md)
  instead.
- Function
  [`.jmp_sa_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`sap_sai_names()`](https://rjdverse.github.io/rjd3workspace/reference/sap_name.md)
  instead.
- Function
  [`.jsap_sa_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`sap_sai_names()`](https://rjdverse.github.io/rjd3workspace/reference/sap_name.md)
  instead.
- Function
  [`.jsai_ts_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`get_ts_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/get_metadata.md)
  instead.
- Function
  [`transfer_series()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`transfer_sa_item()`](https://rjdverse.github.io/rjd3workspace/reference/transfer_sa_item.md)
  instead.
- Function
  [`.jsai_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`get_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/get_metadata.md)
  instead.

#### Changed

- Simplified the modification of path in `update_path()`
- New JARS related to version
  [3.5.1](https://github.com/jdemetra/jdplus-main/releases/tag/v3.5.1)

#### Removed

- `add_variable()` is removed. Please use now
  [`add_variables()`](https://rjdverse.github.io/rjd3workspace/reference/add_variables.md)

### [3.5.0](https://github.com/rjdverse/rjd3workspace/compare/v3.2.4...v3.5.0) - 2025-04-10

#### Added

- New functions to update the raw data path
  ([`spreadsheet_update_path()`](https://rjdverse.github.io/rjd3workspace/reference/spreadsheet_update_path.md)
  and
  [`txt_update_path()`](https://rjdverse.github.io/rjd3workspace/reference/txt_update_path.md))

#### Changed

- Rename the argument `jsa` in `jsai` in
  [`read_sai()`](https://rjdverse.github.io/rjd3workspace/reference/read_sai.md),
  [`.jsai_results()`](https://rjdverse.github.io/rjd3workspace/reference/get-results.md),
  [`.jsai_jresults()`](https://rjdverse.github.io/rjd3workspace/reference/get-results.md),
  [`.jsai_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md),
  [`.jsai_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md),
  [`.jsai_ts_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md),
  [`replace_sa_item()`](https://rjdverse.github.io/rjd3workspace/reference/replace_sa_item.md),
  [`get_raw_data()`](https://rjdverse.github.io/rjd3workspace/reference/set_raw_data.md),
  [`get_ts()`](https://rjdverse.github.io/rjd3workspace/reference/set_ts.md),
  [`get_comment()`](https://rjdverse.github.io/rjd3workspace/reference/set_comment.md)
  and
  [`get_priority()`](https://rjdverse.github.io/rjd3workspace/reference/set_priority.md)
- Rename the argument `ref_jsa` in `ref_jsai` in
  [`set_ts_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/set_ts_metadata.md)
- New JARS related to version
  [3.5.0](https://github.com/jdemetra/jdplus-main/releases/tag/v3.5.0)

#### Deprecated

- Function
  [`.jws_open()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`jws_open()`](https://rjdverse.github.io/rjd3workspace/reference/jws_open.md)
  instead.
- Function
  [`.jsap_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`sap_name()`](https://rjdverse.github.io/rjd3workspace/reference/sap_name.md)
  instead.
- Function
  [`.jsap_sai()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`jsap_sai()`](https://rjdverse.github.io/rjd3workspace/reference/jws_sap.md)
  instead.
- Function
  [`.jsap_sai_count()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`sap_sai_count()`](https://rjdverse.github.io/rjd3workspace/reference/ws_sap_count.md)
  instead.
- Function
  [`.jsap_sai_names()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function `sap_sai_names` instead.
- Function
  [`.jsai_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`sai_name()`](https://rjdverse.github.io/rjd3workspace/reference/sap_name.md)
  instead.
- Function
  [`.jws_sap()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`jws_sap()`](https://rjdverse.github.io/rjd3workspace/reference/jws_sap.md)
  instead.
- Function
  [`.jws_sap_count()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`ws_sap_count()`](https://rjdverse.github.io/rjd3workspace/reference/ws_sap_count.md)
  instead.
- Function
  [`.jws_sap_new()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`jws_sap_new()`](https://rjdverse.github.io/rjd3workspace/reference/jws_new.md)
  instead.
- Function
  [`.jmp_load()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`read_sap()`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md)
  instead.
- Function
  [`.jsa_read()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`read_sai()`](https://rjdverse.github.io/rjd3workspace/reference/read_sai.md)
  instead.
- Function
  [`.jsa_results()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`.jsai_results()`](https://rjdverse.github.io/rjd3workspace/reference/get-results.md)
  instead.
- Function
  [`.jsa_jresults()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`.jsai_jresults()`](https://rjdverse.github.io/rjd3workspace/reference/get-results.md)
  instead.
- Function
  [`.jsa_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`.jsai_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  instead.
- Function
  [`.jsa_ts_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`.jsai_ts_metadata()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  instead.
- Function
  [`.jread_workspace()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`jread_workspace()`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md)
  instead.
- Function
  [`.jread_sap()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`jread_sap()`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md)
  instead.
- Function
  [`.jws_new()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`jws_new()`](https://rjdverse.github.io/rjd3workspace/reference/jws_new.md)
  instead.
- Function
  [`.jws_make_copy()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`jws_make_copy()`](https://rjdverse.github.io/rjd3workspace/reference/make_copy.md)
  instead.
- Function
  [`.jsap_make_copy()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`jsap_make_copy()`](https://rjdverse.github.io/rjd3workspace/reference/make_copy.md)
  instead.
- Function
  [`.jws_compute()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is deprecated. Please use function
  [`jws_compute()`](https://rjdverse.github.io/rjd3workspace/reference/jws_compute.md)
  instead.
- Function `.jsap_refresh` is deprecated. Please use function
  `jsap_refresh` instead.
- Function `.jws_refresh` is deprecated. Please use function
  `jws_refresh` instead.

#### Removed

- [`.jmp_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is removed. Please use now
  [`sap_name()`](https://rjdverse.github.io/rjd3workspace/reference/sap_name.md)
- `.jws_load()` is removed. Please use now
  [`jws_open()`](https://rjdverse.github.io/rjd3workspace/reference/jws_open.md)
- [`.jmp_sa()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is removed. Please use now
  [`jsap_sai()`](https://rjdverse.github.io/rjd3workspace/reference/jws_sap.md)
- [`.jsap_sa()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is removed. Please use now
  [`jsap_sai()`](https://rjdverse.github.io/rjd3workspace/reference/jws_sap.md)
- [`.jmp_sa_count()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is removed. Please use now
  [`sap_sai_count()`](https://rjdverse.github.io/rjd3workspace/reference/ws_sap_count.md)
- [`.jsap_sa_count()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is removed. Please use now
  [`sap_sai_count()`](https://rjdverse.github.io/rjd3workspace/reference/ws_sap_count.md)
- [`.jmp_sa_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is removed. Please use now
  [`sap_sai_names()`](https://rjdverse.github.io/rjd3workspace/reference/sap_name.md)
- [`.jsap_sa_name()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  is removed. Please use now
  [`sap_sai_names()`](https://rjdverse.github.io/rjd3workspace/reference/sap_name.md)
- `.jsa_name()` is removed. Please use now
  [`sai_name()`](https://rjdverse.github.io/rjd3workspace/reference/sap_name.md)
- `.jws_multiprocessing()` is removed. Please use now
  [`jws_sap()`](https://rjdverse.github.io/rjd3workspace/reference/jws_sap.md)
- `.jws_multiprocessing_count()` is removed. Please use now
  [`ws_sap_count()`](https://rjdverse.github.io/rjd3workspace/reference/ws_sap_count.md)
- `.jws_multiprocessing_new()` is removed. Please use now
  [`jws_sap_new()`](https://rjdverse.github.io/rjd3workspace/reference/jws_new.md)

### [3.2.4](https://github.com/rjdverse/rjd3workspace/compare/v3.2.3...v3.2.4) - 2025-02-04

#### Fixed

- fixed
  [`set_specification()`](https://rjdverse.github.io/rjd3workspace/reference/set_specification.md)
  and
  [`set_domain_specification()`](https://rjdverse.github.io/rjd3workspace/reference/set_specification.md)
- Correction of imports when the workspace contains no SAP.
- `replace` parameter of
  [`save_workspace()`](https://rjdverse.github.io/rjd3workspace/reference/save_workspace.md)
  corrected in R (not in Java).

#### Added

- new function
  [`.jsa_jresults()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md),
  [`.jread_sap()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  and
  [`.jread_workspace()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
  to extract the Java object of the results of a SAItem.

#### Changed

- New JARS related to version
  [3.2.4](https://github.com/jdemetra/jdplus-main/releases/tag/v3.2.4)

### [3.2.3](https://github.com/rjdverse/rjd3workspace/compare/v3.2.2...v3.2.3) - 2024-07-12

#### Changed

- New JARS related to version
  [3.2.3](https://github.com/jdemetra/jdplus-main/releases/tag/v3.2.3)

### [3.2.2](https://github.com/rjdverse/rjd3workspace/compare/v3.2.1...v3.2.2) - 2024-03-15

#### Changed

- New JARS related to version
  [3.2.2](https://github.com/jdemetra/jdplus-main/releases/tag/v3.2.2)

### [3.2.1](https://github.com/rjdverse/rjd3workspace/compare/v3.2.0...v3.2.1) - 2023-12-12

#### Changed

- New JARS related to version
  [3.2.1](https://github.com/jdemetra/jdplus-main/releases/tag/v3.2.1)

### [3.2.0](https://github.com/rjdverse/rjd3workspace/compare/v3.1.0...v3.2.0) - 2023-11-24

#### Changed

- New JARS related to version
  [3.2.0](https://github.com/jdemetra/jdplus-main/releases/tag/v3.2.0)

### [3.1.0](https://github.com/rjdverse/rjd3workspace/releases/tag/v3.1.0) - 2023-08-11

#### Added

- New JARS related to version
  [3.1.0](https://github.com/jdemetra/jdplus-main/releases/tag/v3.1.0)
