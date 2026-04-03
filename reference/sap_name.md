# Get the name of a SAProcessing or one (or all) Sa-item

Functions to retrieve the name of a SAProcessing (`sap_name()`) or
Sa-item (`sai_name()`) or all SA-item (`sap_sai_names()`) .

## Usage

``` r
sai_name(jsai)

sap_name(jsap)

sap_sai_names(jsap)
```

## Arguments

- jsap, jsai:

  the object to retrieve the name from.

## Value

A vector `character`.

## Examples

``` r
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml",
                    package = "rjd3workspace")
# \donttest{
jws <- jws_open(file)

# Extract 2nd SA-Processing
jsap_2 <- jws_sap(jws, 2)

# Retrieve the name
sap_name(jsap_2)
#> [1] "SAProcessing-2"

# Retrieve all the SA-items names
sap_sai_names(jsap_2)
#> [1] "RF0893" "RF0899" "RF1011" "RF1012"
# }
```
