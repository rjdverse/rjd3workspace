# Update the path to raw data in a workspace (spreadsheet)

Update the path to raw data in a workspace (spreadsheet)

## Usage

``` r
spreadsheet_update_path(jws, new_path, idx_sap = NULL, idx_sai = NULL)
```

## Arguments

- jws:

  workspace object

- new_path:

  new path to the spreadsheet containing raw data

- idx_sap:

  index (or indices) of the SAProcessing(s)

- idx_sai:

  index (or indices) of the SA-item(s).

## Value

This function returns either NULL if the update was successful, or an
error.

## Details

The spreadsheet file must be a .xlsx file. .xls files are not accepted
in JDemetra+ v3.x.

## Examples

``` r
# Load a workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
my_ws <- jws_open(file)

# Update the entire second SA-Processing of the `my_ws` workspace with a new path to raw data
spreadsheet_update_path(
    jws = my_ws,
    new_path = system.file("extdata", "IPI_nace4.xlsx", package = "rjd3workspace"),
    idx_sap = 2
)

# Select one (the 2nd) SA-item from second SA-Processing
sap2 <- jws_sap(my_ws, 2)
sai2 <- jsap_sai(sap2, 2)

# Check path
get_ts_metadata(sai2, "@id")
#> [1] "demetra://tsprovider/XCLPRVDR/20111201/SERIES?file=%2Fhome%2Frunner%2Fwork%2F_temp%2FLibrary%2Frjd3workspace%2Fextdata%2FIPI_nace4.xlsx#seriesName=RF0899&sheetName=IPI"
```
