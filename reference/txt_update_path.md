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

# Update the entire second SA-Processing of the `my_ws` workspace with a new path to raw data
txt_update_path(
    jws = my_ws,
    new_path = system.file("extdata", "IPI_nace4.csv", package = "rjd3workspace"),
    idx_sap = 1
)

# Select one (the 2nd) SA-item from first SA-Processing
sap1 <- jws_sap(my_ws, 1)
sai2 <- jsap_sai(sap1, 2)

# Check path
get_ts_metadata(sai2, "@id")
#> [1] "demetra://tsprovider/Txt/20111201/SERIES?datePattern=dd%2FMM%2Fyyyy&delimiter=SEMICOLON&file=%2Fhome%2Frunner%2Fwork%2F_temp%2FLibrary%2Frjd3workspace%2Fextdata%2FIPI_nace4.csv#seriesIndex=4"
```
