# Set Specification in a Sa-Item

Set Specification in a Sa-Item

## Usage

``` r
set_specification(jsap, idx, spec)

set_domain_specification(jsap, idx, spec)
```

## Arguments

- jsap:

  SAProcessing to be modified.

- idx:

  index of the target SA-item.

- spec:

  new specification generated with
  [`rjd3x13::x13_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.html)
  or
  [`rjd3tramoseats::tramoseats_spec()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.html)

## Value

`NULL` returned invisibly

## Examples

``` r
# Create a (customized) spec) spec
library(rjd3x13)
#> Your java version is 17. 21 or higher is needed.
#> 
#> Attaching package: ‘rjd3x13’
#> The following object is masked from ‘package:grDevices’:
#> 
#>     x11

spec <- rjd3x13::x13_spec("rsa3") |>
    rjd3toolkit::set_basic(type = "From", d0 = "2012-01-01")
#> Error in .jcall("jdplus/x13/base/api/x13/X13Spec", "Ljdplus/x13/base/api/x13/X13Spec;",     "fromString", name): RcallMethod: cannot determine object class

# Load a Workspace to modify
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): java.lang.UnsupportedClassVersionError: jdplus/x13/base/api/x13/X13Spec has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0

# Select SAProcessing with the target SA-item
sap1 <- jws_sap(jws, 1)
#> Error: object 'jws' not found

# Set specification in targeted SA-item
set_specification(sap1, 2, spec)
#> Error: object 'spec' not found

# Set domain specification in selected SA-item
set_domain_specification(sap1, 3, spec)
#> Error: object 'spec' not found
```
