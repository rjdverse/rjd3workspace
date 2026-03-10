# Add a SA-item to a SAProcessing

Add a SA-item to a SAProcessing

## Usage

``` r
add_sa_item(jsap, name, x, spec, ...)
```

## Arguments

- jsap:

  SAProcessing.

- name:

  name of the SA-item to be added.

- x:

  either a seasonal adjustment model (from
  [`rjd3x13::x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.html)
  or
  [`rjd3tramoseats::tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html)),
  a SA-item object, `"ts"` object.

- spec:

  specification to use when `x` is a `"ts"` object.

## Value

`NULL` returned invisibly

## Examples

``` r
dir <- tempdir()

# Raw series
y <- rjd3toolkit::ABS$X0.2.09.10.M

# Creating an empty workspace and SAProcessing
jws <- jws_new()
#> Error in jws_new(): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws
jsap1 <- jws_sap_new(jws, "sap1")
#> Error: object 'jws' not found

# Adding SA-item as estimation result
# Estimation with rjd313
add_sa_item(jsap1, name = "series_1", x = rjd3x13::x13(y))
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsData;",     "of", as.integer(freq), as.integer(start[1]), as.integer(start[2]),     as.double(s)): RcallMethod: cannot determine object class

# Estimation with rjd3tramoseats
add_sa_item(jsap1, name = "series_2", x = rjd3tramoseats::tramoseats(y))
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsData;",     "of", as.integer(freq), as.integer(start[1]), as.integer(start[2]),     as.double(s)): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/r/timeseries/TsUtility has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0

# Adding SA-item as raw series + specification
add_sa_item(jsap1, name = "series_3", x = y, rjd3x13::x13_spec("RSA3"))
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsData;",     "of", as.integer(freq), as.integer(start[1]), as.integer(start[2]),     as.double(s)): RcallMethod: cannot determine object class
add_sa_item(jsap1, name = "series_4", x = y, rjd3tramoseats::tramoseats_spec("RSAFull"))
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsData;",     "of", as.integer(freq), as.integer(start[1]), as.integer(start[2]),     as.double(s)): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/r/timeseries/TsUtility has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0
rws <- read_workspace(jws)
#> Error: object 'jws' not found
rws$processing$sap1$series_4
#> Error: object 'rws' not found

# Writing the workspace
save_workspace(jws, file.path(dir, "workspace.xml"))
#> Error: object 'jws' not found
```
