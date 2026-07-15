# Copy a Workspace or SA-Processing

Copy a Workspace or SA-Processing

## Usage

``` r
jsap_make_copy(jsap)

jws_make_copy(jws)
```

## Arguments

- jws, jsap:

  Java Workspace or SA-Processing

## Value

Returns a Java object workspace or SA-Processing

## Details

The copy of a SA-processing will be made in the same workspace. The
modelling context of the workspace is also copied.

## References

More information on workspaces in JDemetra+ Graphical User Interface:
<https://doc.jdemetra.org/t-gui-sa-modelling-features/>

## See also

[`read_workspace()`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md),
[`read_sap()`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md)

## Examples

``` r
# Create an empty 'JDemetra+' Workspace
jws <- jws_new()

# Add an empty SA-Processing
jsap <- jws_sap_new(jws, "sap1")

# Make a copy of the workspace
jws2 <- jws_make_copy(jws)

# Make a copy of sap1 in jws2
jsap2 <- jsap_make_copy(jsap)
```
