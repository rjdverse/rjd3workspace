# Compute a Workspace

`jws_compute()` allows to extract all the SA-Items as java object.

## Usage

``` r
jws_compute(jws)
```

## Arguments

- jws:

  a workspace

## Examples

``` r
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class

# Compute the workspace to access its components
jws_compute(jws)
#> Error in .jcall(jws, "V", "computeAll"): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws
```
