# Get/Set Raw Data in a SA-item

Get/Set Raw Data in a SA-item

## Usage

``` r
set_raw_data(jsap, idx, y)

get_raw_data(jsai)
```

## Arguments

- jsap:

  SAProcessing to be modified.

- idx:

  index of the target SA-item.

- y:

  new raw time series.

- jsai:

  a SA-item.

## Value

`NULL` returned invisibly (set) or TS object (get)

## Examples

``` r
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class

# Select SAProcessing
sap1 <- jws_sap(jws, 1)
#> Error in .jcall(obj = jws, returnSig = "Ljdplus/sa/base/workspace/MultiProcessing;",     method = "getMultiProcessing", as.integer(idx - 1L)): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws

# Select SA-item
sai1 <- jsap_sai(sap1, 3) # java object sai
#> Error: object 'sap1' not found
tail(get_raw_data(sai1))
#> Error: object 'sai1' not found

new_raw_data <- rjd3toolkit::ABS$X0.2.15.10.M
set_raw_data(sap1,3,new_raw_data)
#> Error: object 'sap1' not found

sai1 <- jsap_sai(sap1,3) # reload SA-item
#> Error: object 'sap1' not found
tail(get_raw_data(sai1)) # get raw data
#> Error: object 'sai1' not found
```
