# Set the name of a SA-item

Set the name of a SA-item

## Usage

``` r
set_name(jsap, idx, name)
```

## Arguments

- jsap:

  SAProcessing to be modified.

- idx:

  index of the target SA-item.

- name:

  character corresponding to the new name

## Value

`NULL` returned invisibly

## See also

[`sai_name()`](https://rjdverse.github.io/rjd3workspace/reference/sap_name.md)

## Examples

``` r
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)

# Select SAProcessing
sap1 <- jws_sap(jws,1)

# Select SA-item
sai1 <- jsap_sai(sap1,3) # java object sai

# set name
set_name(sap1,3,"RF1011_1")

# check
sai1 <- jsap_sai(sap1,3) # reload sai
sai_name(sai1) #get name
#> [1] "RF1011_1"
```
