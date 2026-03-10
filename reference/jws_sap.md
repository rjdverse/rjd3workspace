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

Returns a java object SA-Processing or SA-Item.

## Examples

``` r
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class

# Compute the workspace to enable accessing its components
jws_compute(jws)
#> Error in .jcall(jws, "V", "computeAll"): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws

# Extract 2nd SA-Processing
jsap2 <- jws_sap(jws, 2)
#> Error: object 'jws' not found

# Extract 3rd SA-item
jsai3 <- jsap_sai(jsap2, 3)
#> Error: object 'jsap2' not found
```
