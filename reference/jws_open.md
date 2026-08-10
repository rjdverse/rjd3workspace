# Open an existing 'JDemetra+' Workspace

`jws_open()` opens an existing Workspace (as a Java pointer) and
[`jws_compute()`](https://rjdverse.github.io/rjd3workspace/reference/jws_compute.md)
computes it (allowing to extract all the SA-Items as Java objects).

## Usage

``` r
jws_open(file)
```

## Arguments

- file:

  path to Workspace xml master file By default a dialog box opens.

## Value

a Javaworkspace

## See also

[`read_workspace()`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md)
to transform the workspace in a R list.

## Examples

``` r

# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml",
                    package = "rjd3workspace")
# \donttest{
jws <- jws_open(file)

# Compute the workspace to enable access its components
jws_compute(jws)
# }
```
