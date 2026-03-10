# Extract Metadata from a SA-Item

Extract specific metadata or time series metadata of a SA-item.

## Usage

``` r
get_metadata(jsai, key)

get_ts_metadata(jsai, key)
```

## Arguments

- jsai:

  Java SA-item object.

- key:

  key of the metadata.

## Examples

``` r
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class

# Select SAProcessing
jsap1 <- jws_sap(jws, 1)
#> Error in .jcall(obj = jws, returnSig = "Ljdplus/sa/base/workspace/MultiProcessing;",     method = "getMultiProcessing", as.integer(idx - 1L)): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws

# Select SA-item (as java object)
jsai1 <- jsap_sai(jsap1, 3)
#> Error: object 'jsap1' not found

# Extract the comment as metadata
get_metadata(jsai1, "comment")
#> Error: object 'jsai1' not found

# Extract the ts metadata
get_metadata(jsai1, "@id")
#> Error: object 'jsai1' not found
get_metadata(jsai1, "@source")
#> Error: object 'jsai1' not found
get_metadata(jsai1, "@timestamp")
#> Error: object 'jsai1' not found
```
