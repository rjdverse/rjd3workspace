# Get/Set SA-item Priority

Get/Set SA-item Priority

## Usage

``` r
set_priority(jsap, idx, priority = 0L)

get_priority(jsai)
```

## Arguments

- jsap:

  SAProcessing to be modified.

- idx:

  index of the target SA-item.

- priority:

  integer containing the priority.

- jsai:

  a SA-item.

## Examples

``` r
# Load a workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
my_jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class

# Select the first SA-Processing and SA-Item
jsap <- jws_sap(my_jws, 1)
#> Error in .jcall(obj = jws, returnSig = "Ljdplus/sa/base/workspace/MultiProcessing;",     method = "getMultiProcessing", as.integer(idx - 1L)): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws
jsai <- jsap_sai(jsap, 1L)
#> Error: object 'jsap' not found

# Change priority
set_priority(jsap, idx = 1L, priority = 3L)
#> Error: object 'jsap' not found

# Retrieve priority
get_priority(jsai)
#> Error: object 'jsai' not found
```
