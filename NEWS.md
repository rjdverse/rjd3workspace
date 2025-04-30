# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres
to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

* Rename `.jsai_ts_metadata` into `get_ts_metadata()`
* Rename `.jsai_metadata` into `get_metadata()`
* Simplified the modification of path in `update_path()`


## [3.5.0] - 2025-04-10

### Added

* New functions to update the raw data path

### Changed

* Rename `.jsa_XXX()` in `.jsai_XXX()` for SA-Item
* Rename `.jsap_sa()` in `.jsap_sai()`
* Rename `.jsap_sa_name()` in `.jsa_sai_names()`
* Rename `.jsap_sa_count()` in `.jsa_sai_count()`
* Rename the argument `jsa` in `jsai` in `read_sai()`, `.jsai_results()`, `.jsai_jresults()`, `.jsai_name()`, `.jsai_metadata()`, `.jsai_ts_metadata()`, `replace_sa_item()`, `get_raw_data()`, `get_ts()`, `get_comment()` and  `get_priority()`
* Rename the argument `ref_jsa` in `ref_jsai` in `set_ts_metadata()`
* Rename `.jsap_sai_count` in `sap_sai_count`

### Deprecated 

* `.jws_load()` in favour of `.jws_open()`
* `.jsap_sai_count` is now deprecated
* Rename `.jsap_sai_count()` in `sap_sai_count()`
* Rename `.jws_sap_count()` in `ws_sap_count()`
* Rename `.jws_open()` in `jws_open()`
* Rename `.jread_ws()` in `jread_ws()`
* Rename `.jread_sap()` in `jread_sap()`
* Rename `.jws_new()` in `jws_new()`
* Rename `.jws_sap_new()` in `.jws_sap_new()`
* Rename `.jws_make_copy()` in `jws_make_copy()`
* Rename `.jsap_make_copy()` in `jsap_make_copy()`
* Rename `.jws_compute()` in `jws_compute()`
* Rename `.jsap_sai` in `jsap_sai`
* Rename `.jws_sap` in `jws_sap`
* Rename `.jsai_name` in `sai_name`
* Rename `.jsap_name` in `sap_name`
* Rename `.jsap_sai_names` in `sap_sai_names`
* Rename `.jsap_refresh` in `jsap_refresh`
* Rename `.jws_refresh` in `jws_refresh`


## [3.2.4] - 2025-02-04

### Fixed

* fixed `set_specification()` and `set_domain_specification()`
* Correction of imports when the workspace contains no SAP.
* `replace` parameter of `save_workspace()` corrected in R (not in Java).

### Added

* new function `.jsa_jresults()`, `.jread_sap()` and `.jread_workspace()` to  extract the Java object of the results of a SAItem.


## [3.2.3] - 2024-07-12

### Changed

* new jars


## [3.2.2] - 2024-03-15


## [3.2.1] - 2023-12-12


## [3.2.0] - 2023-11-24


## [3.1.0] - 2023-08-11


[Unreleased]: https://github.com/rjdverse/rjd3workspace/compare/v3.5.0...HEAD
[3.5.0]: https://github.com/rjdverse/rjd3workspace/compare/v3.2.4...v3.5.0
[3.2.4]: https://github.com/rjdverse/rjd3workspace/compare/v3.2.3...v3.2.4
[3.2.3]: https://github.com/rjdverse/rjd3workspace/compare/v3.2.2...v3.2.3
[3.2.2]: https://github.com/rjdverse/rjd3workspace/compare/v3.2.1...v3.2.2
[3.2.1]: https://github.com/rjdverse/rjd3workspace/compare/v3.2.0...v3.2.1
[3.2.0]: https://github.com/rjdverse/rjd3workspace/compare/v3.1.0...v3.2.0
[3.1.0]: https://github.com/rjdverse/rjd3workspace/releases/tag/v3.1.0
