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
# \donttest{
jws <- jws_open(file)

# Select SAProcessing
jsap1 <- jws_sap(jws, 1)

# Select SA-item (as java object)
jsai1 <- jsap_sai(jsap1, 3)
# }
```
