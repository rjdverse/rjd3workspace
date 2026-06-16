# Get/Set Comment from a SA-item

Get/Set Comment from a SA-item

## Usage

``` r
set_comment(jsap, idx, comment)

get_comment(jsai)
```

## Arguments

- jsap:

  SAProcessing to be modified.

- idx:

  index of the target SA-item.

- comment:

  character containing the comment.

- jsai:

  a SA-item.

## Value

`NULL` returned invisibly

## Examples

``` r

# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
# \donttest{
jws <- jws_open(file)

# Select SAProcessing
jsap1 <- jws_sap(jws, 1L)

# Add a comment
set_comment(jsap1, 2L, "data collection changed in 2012")

jsai2 <- jsap_sai(jsap1, 2L)
get_comment(jsai2)
#> [1] "data collection changed in 2012"
# }
```
