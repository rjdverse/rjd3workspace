# Save Workspace

Function allowing to write a workspace as a collection of xml files
readable by JDemetra+ Graphical User Interface.

## Usage

``` r
save_workspace(jws, file, replace = FALSE)
```

## Arguments

- jws:

  Workspace object to export.

- file:

  path where to export the 'JDemetra+' Workspace (.xml file).

- replace:

  boolean indicating if the Workspace should be replaced if it already
  exists.

## Examples

``` r
dir <- tempdir()
jws <- jws_new()
jsap1 <- jws_sap_new(jws, "sap1")
y <- rjd3toolkit::ABS$X0.2.09.10.M
add_sa_item(jsap1, name = "serie_1", x = y, rjd3x13::x13_spec())
save_workspace(jws, file.path(dir, "workspace.xml"))
```
