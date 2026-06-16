# Compute a Workspace

`jws_compute()` allows to extract all the SA-Items as Javaobject.

## Usage

``` r
jws_compute(jws)
```

## Arguments

- jws:

  a workspace

## Value

Invisibly `NULL`

## Examples

``` r

# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
# \donttest{
jws <- jws_open(file)

# Compute the workspace to access its components
jws_compute(jws)
# }
```
