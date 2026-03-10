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
jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class

# Extract 2nd SA-Processing
jsap_2 <- jws_sap(jws, 2)
#> Error in .jcall(obj = jws, returnSig = "Ljdplus/sa/base/workspace/MultiProcessing;",     method = "getMultiProcessing", as.integer(idx - 1L)): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws

# Retrieve the name
sap_name(jsap_2)
#> Error: object 'jsap_2' not found

# Retrieve all the SA-items names
sap_sai_names(jsap_2)
#> Error: object 'jsap_2' not found
```
