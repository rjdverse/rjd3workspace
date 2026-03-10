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
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class

# Select SAProcessing
sap1 <- jws_sap(jws,1)
#> Error in .jcall(obj = jws, returnSig = "Ljdplus/sa/base/workspace/MultiProcessing;",     method = "getMultiProcessing", as.integer(idx - 1L)): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws

# Select SA-item
sai1 <- jsap_sai(sap1,3) # java object sai
#> Error: object 'sap1' not found

# set name
set_name(sap1,3,"RF1011_1")
#> Error: object 'sap1' not found

# check
sai1 <- jsap_sai(sap1,3) # reload sai
#> Error: object 'sap1' not found
sai_name(sai1) #get name
#> Error: object 'sai1' not found
```
