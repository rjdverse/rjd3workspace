# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres
to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

* New functions to update the raw data path

### Changed

* Rename `.jsa_XXX` in `.jsai_XXX` for SA-Item
* Rename `.jsap_sa` in `.jsap_sai`


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

[Unreleased]: https://github.com/rjdverse/rjd3workspace/compare/v3.2.4...HEAD
[3.2.4]: https://github.com/rjdverse/rjd3workspace/compare/v3.2.3...v3.2.4
[3.2.3]: https://github.com/rjdverse/rjd3workspace/compare/v3.2.2...v3.2.3
[3.2.2]: https://github.com/rjdverse/rjd3workspace/releases/tag/v3.2.2
