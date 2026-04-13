# Set Specification in a Sa-Item

Set Specification in a Sa-Item

## Usage

``` r
set_specification(jsap, idx, spec)

set_reference_specification(jsap, idx, spec)
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

## References

More information on different types of specifications in JDemetra+
documentation:
<https://doc.jdemetra.org/t-gui-sa-modelling-features#Spec-Def-App>

## Examples

``` r
# Create a (customized) spec) spec
library(rjd3x13)
#> 
#> Attaching package: ‘rjd3x13’
#> The following object is masked from ‘package:grDevices’:
#> 
#>     x11

spec <- rjd3x13::x13_spec("rsa3") |>
    rjd3toolkit::set_basic(type = "From", d0 = "2012-01-01")

# \donttest{
# Load a Workspace to modify
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)

# Select SAProcessing with the target SA-item
sap1 <- jws_sap(jws, 1)

# Set specification in targeted SA-item
set_specification(sap1, 2, spec)

# Set reference specification in selected SA-item
set_reference_specification(sap1, 3, spec)
# }
```
