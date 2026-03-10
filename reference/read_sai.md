# Read an SA-item

`read_sai()` extracts all the information of a SA-item (see details).

## Usage

``` r
read_sai(jsai)
```

## Arguments

- jsai:

  Java SA-item object.

## Value

a list

## Details

A SA-item contains more information than just the results of an
estimation. Full information is extracted with the `read_sai()` function
that returns a list of 5 objects:

- `ts`: raw time series.

- `domainSpec`: initial specification. Reference when refreshing and
  relaxing constraints.

- `estimationSpec`: specification used for the current estimation.

- `pointSpec`: specification corresponding to the results of the current
  estimation (fully identified model).

- `results`: results of the estimation.

## Examples

``` r
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/api/timeseries/calendars/CalendarManager has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0

# Select SAProcessing
jsap1 <- jws_sap(jws, 1)
#> Error: object 'jws' not found

# Select SA-item (as java object)
jsai1 <- jsap_sai(jsap1, 3)
#> Error: object 'jsap1' not found
```
