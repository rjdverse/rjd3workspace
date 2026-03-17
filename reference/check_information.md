# Check if JD+ object exists

Check if JD+ object exists

## Usage

``` r
check_information(jws, idx_sap = NULL, idx_sai = NULL)
```

## Arguments

- jws:

  workspace object

- idx_sap:

  index (or indices) of the SAProcessing(s)

- idx_sai:

  index (or indices) of the SA-item(s).

## Value

This function returns either a boolean (TRUE) if the SAI and the SAP
exist in the WS, or an error specifying the not found object.

## Details

If the object idx_sai is NULL then the function will only check if the
workspace contains a SA-Processing at the index idx_sap. If the object
idx_sap is NULL then the function will check if every SA-Processing of
the workspace contains a SA-Item at the index idx_sai. If the object
idx_sap is NULL and idx_sai is NULL then the function won't do any
check.

If the object idx_sap and / or idx_sai have a length \> 1 then the
checks are iterated over all the indices.

## Examples

``` r
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)

# Check if the SA-Item 3 in the SA-Processing 1 exists
rjd3workspace:::check_information(jws = jws, idx_sap = 1, idx_sai = 3)
#> [1] TRUE

# Check if the SA-Items 1, 2 and 5 in the SA-Processing 1 exist
rjd3workspace:::check_information(jws = jws, idx_sap = 1, idx_sai = c(1, 2, 4))
#> [1] TRUE
```
