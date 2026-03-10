# Get/Set the (JDemetra+) time series of a SA-item

(JDemetra+) time series contains more information than raw data, which
can be manipulated with
[`set_raw_data()`](https://rjdverse.github.io/rjd3workspace/reference/set_raw_data.md)
and
[`get_raw_data()`](https://rjdverse.github.io/rjd3workspace/reference/set_raw_data.md)

## Usage

``` r
set_ts(jsap, idx, y)

get_ts(jsai)
```

## Arguments

- jsap:

  SAProcessing to be modified.

- idx:

  index of the target SA-item.

- y:

  a "full" time series (jd3-like).

- jsai:

  a SA-item.

## Examples

``` r
# Load a workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
my_jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class

library("rjd3providers")
#> Your java version is 17. 21 or higher is needed.
data_path <- system.file("data", "IPI_nace4.csv", package = "rjd3workspace")

ts_object <- txt_series(
    file = data_path,
    series = 1L,
    delimiter = "SEMICOLON",
    fmt.date = "dd/MM/yyyy"
)
#> Error in .jcall(obj = "jdplus/text/base/r/Utility", returnSig = "Ljdplus/toolkit/base/tsp/util/ObsFormat;",     method = "obsFormat", as.character(locale), as.character(dateFmt),     as.character(numberFmt), as.logical(ignoreNumberGrouping)): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws

# Select the first SA-Processing
jsap <- jws_sap(my_jws, 1L)
#> Error: object 'my_jws' not found

# Change the ts object
set_ts(jsap = jsap, idx = 1L, ts_object)
#> Error: object 'jsap' not found

jsai1 <- jsap_sai(jsap, 1L)
#> Error: object 'jsap' not found
jsai2 <- jsap_sai(jsap, 2L)
#> Error: object 'jsap' not found
jsai3 <- jsap_sai(jsap, 3L)
#> Error: object 'jsap' not found

# Get the ts object
get_ts(jsai1)
#> Error: object 'jsai1' not found
get_ts(jsai2)
#> Error: object 'jsai2' not found
get_ts(jsai3)
#> Error: object 'jsai3' not found
```
