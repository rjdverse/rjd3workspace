# Get Context from Workspace

Get Context from Workspace

## Usage

``` r
get_context(jws)
```

## Arguments

- jws:

  the Workspace.

## Value

The modelling context (list object with Calendars and Variables).

## Examples

``` r
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
# \donttest{
jws <- jws_open(file)

# Get context
my_context <- get_context(jws)
# }
```
