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
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class
jsap <- jws_sap(jws, 1)
#> Error in .jcall(obj = jws, returnSig = "Ljdplus/sa/base/workspace/MultiProcessing;",     method = "getMultiProcessing", as.integer(idx - 1L)): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws
jsai <- jsap_sai(jsap, 1)
#> Error: object 'jsap' not found
nid <- rjd3providers::txt_change_file(get_ts_metadata(jsai, "@id"), "test.csv")
#> Error: object 'jsai' not found
put_ts_metadata(jsap, 1, "@id", nid)
#> Error: object 'jsap' not found

jsai <- jsap_sai(jsap, 1)
#> Error: object 'jsap' not found
get_ts_metadata(jsai, "@id")
#> Error: object 'jsai' not found
```
