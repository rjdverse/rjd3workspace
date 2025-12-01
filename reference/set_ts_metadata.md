# Set (JDemetra+) Metadata of a SA-item

Function to set the metadata of a SA-item.

`XXX_ts_metadata()` set the time series metadata of a SA-item (provider,
source of the data...). `XXX_metadata()` set any metadata to a SA-Item.

`set_XXX()` uses the metadata of another SA-item while `put_XXX()`
allows to update a specific key with a new information.

## Usage

``` r
set_ts_metadata(jsap, idx, ref_jsai)

put_ts_metadata(jsap, idx, key, value)

set_metadata(jsap, ref_jsai, idx)

put_metadata(jsap, idx, key, value)
```

## Arguments

- jsap:

  SAProcessing to be modified.

- idx:

  index of the target SA-item.

- ref_jsai:

  a reference SA-item containing the metadata.

- key:

  key of the metadata.

- value:

  value of the metadata.

## Examples

``` r
# Change the file of a given item
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
jsap <- jws_sap(jws, 1)
jsai <- jsap_sai(jsap, 1)
nid <- rjd3providers::txt_change_file(get_ts_metadata(jsai, "@id"), "test.csv")
put_ts_metadata(jsap, 1, "@id", nid)

jsai <- jsap_sai(jsap, 1)
get_ts_metadata(jsai, "@id")
#> [1] "demetra://tsprovider/Txt/20111201/SERIES?datePattern=dd%2FMM%2Fyyyy&delimiter=SEMICOLON&file=test.csv#seriesIndex=3"
```
