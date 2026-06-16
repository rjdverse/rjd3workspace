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
# \donttest{
jws <- jws_open(file)

# Select SAProcessing
sap1 <- jws_sap(jws, 1)

# Select SA-item
sai1 <- jsap_sai(sap1, 3) # java object sai
tail(get_raw_data(sai1))
#>           Jan      Feb      Mar      Apr      May      Jun
#> 2024 88.21640 88.85806 96.89828 99.10996 95.52958 91.57847

new_raw_data <- rjd3toolkit::ABS$X0.2.15.10.M
set_raw_data(sap1,3,new_raw_data)

sai1 <- jsap_sai(sap1,3) # reload SA-item
tail(get_raw_data(sai1)) # get raw data
#>         Mar    Apr    May    Jun    Jul    Aug
#> 2017 1498.6 1490.7 1469.3 1462.5 1533.6 1538.4
# }
```
