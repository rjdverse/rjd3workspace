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
#> Error in jws_new(): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws
jsap1 <- jws_sap_new(jws, "sap1")
#> Error: object 'jws' not found
y <- rjd3toolkit::ABS$X0.2.09.10.M
add_sa_item(jsap1, name = "serie_1", x = y, rjd3x13::x13_spec())
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsData;",     "of", as.integer(freq), as.integer(start[1]), as.integer(start[2]),     as.double(s)): RcallMethod: cannot determine object class
save_workspace(jws, file.path(dir, "workspace.xml"))
#> Error in .jcall(jws, "Z", "saveAs", file, version, !replace): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/r/timeseries/TsUtility has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0
```
