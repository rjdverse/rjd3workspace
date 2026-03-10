# Open an existing 'JDemetra+' Workspace

`jws_open()` opens an existing Workspace (as a Java pointer) and
[`jws_compute()`](https://rjdverse.github.io/rjd3workspace/reference/jws_compute.md)
computes it (allowing to extract all the SA-Items as java objects).

## Usage

``` r
jws_open(file)
```

## Arguments

- file:

  path to Workspace xml master file By default a dialog box opens.

## Value

a java workspace

## See also

[`read_workspace()`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md)
to transform the workspace in a R list.

## Examples

``` r
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class

# Compute the workspace to enable access its components
jws_compute(jws)
#> Error in .jcall(jws, "V", "computeAll"): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws
```
