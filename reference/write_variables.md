# Write regressors file

Write regressors file

## Usage

``` r
write_variables(vars, file)
```

## Arguments

- vars:

  A named list of `ts` objects.

- file:

  Path to the output XML file.

## Value

No return value (`NULL` returned invisibly). This function writes
variables to file for use in JD+.

## Examples

``` r
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class

# Get context
my_context <- get_context(jws)
#> Error in .jcall(obj = jws, returnSig = "Ljdplus/toolkit/base/api/timeseries/regression/ModellingContext;",     method = "getContext"): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws
vars <- my_context$variables[[1L]]
#> Error: object 'my_context' not found

# Writing the regressors in a xml file
write_variables(vars, file = normalizePath("tmp.xml", mustWork = FALSE))
#> Error: object 'vars' not found
```
