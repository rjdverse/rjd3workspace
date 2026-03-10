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
jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class

# Select SAProcessing
jsap1 <- jws_sap(jws, 1L)
#> Error in .jcall(obj = jws, returnSig = "Ljdplus/sa/base/workspace/MultiProcessing;",     method = "getMultiProcessing", as.integer(idx - 1L)): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws

# Add a comment
set_comment(jsap1, 2L, "data collection changed in 2012")
#> Error: object 'jsap1' not found

jsai2 <- jsap_sai(jsap1, 2L)
#> Error: object 'jsap1' not found
get_comment(jsai2)
#> Error: object 'jsai2' not found
```
