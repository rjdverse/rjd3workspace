# Get Context from Workspace

Get Context from Workspace

## Usage

``` r
get_context(jws)
```

## Arguments

- jws:

  the Workspace.

## Examples

``` r
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class

# Get context
my_context <- get_context(jws)
#> Error in .jcall(obj = jws, returnSig = "Ljdplus/toolkit/base/api/timeseries/regression/ModellingContext;",     method = "getContext"): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws
```
