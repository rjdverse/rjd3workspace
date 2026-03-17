# Count SA-Processings or SA-Items

Functions counting the SA-Processings in a Workspace (`ws_sap_count`) or
the SA-Items in a SA-Processing (`sap_sai_count`).

## Usage

``` r
sap_sai_count(jsap)

ws_sap_count(jws)
```

## Arguments

- jws, jsap:

  Workspace or SA-Processing.

## Value

Returns an integer.

## Examples

``` r
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)

# Count the SA-Processings
ws_sap_count(jws)
#> [1] 2

# Count the SA-Items
# In SAP 1
sap1<-jws_sap(jws,1)
sap_sai_count(sap1)
#> [1] 5
```
