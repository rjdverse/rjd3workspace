# Read all SA-Items from a Workspace or SA-Processing

Functions reading all SA-Items from a Workspace (`read_workspace()`) or
a SA-Processing (`read_sap()`) and allowing to access them as R lists.
Whereas functions `jread_sap()` and `jread_workspace()` only return
corresponding Java objects

## Usage

``` r
read_sap(jsap)

jread_sap(jsap)

read_workspace(jws, compute = TRUE)

jread_workspace(jws, compute = TRUE)
```

## Arguments

- jsap:

  java SA-Processing.

- jws:

  java Workspace.

- compute:

  compute or not the workspace (to get the estimation results).

## Value

list or java object

## Examples

``` r
# Load workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): java.lang.NoClassDefFoundError: Could not initialize class jdplus.toolkit.base.workspace.file.Utility

# Read workspace
jread_workspace(jws, compute = FALSE)
#> Error: object 'jws' not found
rws <- read_workspace(jws)
#> Error: object 'jws' not found

# Read sap
sap<-jws_sap(jws,1)
#> Error: object 'jws' not found
jread_sap(sap)
#> Error: object 'sap' not found
read_sap(sap)
#> Error: object 'sap' not found
```
