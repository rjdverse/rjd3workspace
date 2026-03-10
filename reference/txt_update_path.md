# Update the path to raw data in a workspace (txt/csv file)

Update the path to raw data in a workspace (txt/csv file)

## Usage

``` r
txt_update_path(jws, new_path, idx_sap = NULL, idx_sai = NULL)
```

## Arguments

- jws:

  workspace object

- new_path:

  new path to the csv/txt file containing raw data

- idx_sap:

  index (or indices) of the SAProcessing(s)

- idx_sai:

  index (or indices) of the SA-item(s).

## Value

This function returns either NULL if the update was successful, or an
error

## Examples

``` r
# Load a workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
my_ws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class

# Update the entire second SA-Processing of the `my_ws` workspace with a new path to raw data
txt_update_path(
    jws = my_ws,
    new_path = system.file("data", "IPI_nace4.csv", package = "rjd3workspace"),
    idx_sap = 1
)
#> Error in .jcall(jws, "I", "getMultiProcessingCount"): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws

# Select one (the 2nd) SA-item from first SA-Processing
sap1 <- jws_sap(my_ws, 1)
#> Error: object 'my_ws' not found
sai2 <- jsap_sai(sap1, 2)
#> Error: object 'sap1' not found

# Check path
get_ts_metadata(sai2, "@id")
#> Error: object 'sai2' not found
```
