# Extract a SA-Processing or a SA-Item

Functions allowing to extract a SA-Processing from a Workspace using its
order number (index) and a SA-Item from a SA-Processing its order number
(index). The original object is unaltered.

## Usage

``` r
jsap_sai(jsap, idx)

jws_sap(jws, idx)
```

## Arguments

- idx:

  index of the object to extract.

- jws, jsap:

  Workspace or SA-Processing.

## Value

Returns a Javaobject SA-Processing or SA-Item.

## Examples

``` r

# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
# \donttest{
jws <- jws_open(file)

# Compute the workspace to enable accessing its components
jws_compute(jws)

# Extract 2nd SA-Processing
jsap2 <- jws_sap(jws, 2)

# Extract 3rd SA-item
jsai3 <- jsap_sai(jsap2, 3)
# }
```
