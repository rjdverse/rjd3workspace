# Extract Metadata from a SA-Item

Extract specific metadata or time series metadata of a SA-item.

## Usage

``` r
get_metadata(jsai, key)

get_ts_metadata(jsai, key)
```

## Arguments

- jsai:

  Java SA-item object.

- key:

  key of the metadata.

## Examples

``` r
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)

# Select SAProcessing
jsap1 <- jws_sap(jws, 1)

# Select SA-item (as java object)
jsai1 <- jsap_sai(jsap1, 3)

# Extract the comment as metadata
get_metadata(jsai1, "comment")
#> NULL

# Extract the ts metadata
get_metadata(jsai1, "@id")
#> NULL
get_metadata(jsai1, "@source")
#> NULL
get_metadata(jsai1, "@timestamp")
#> NULL
```
