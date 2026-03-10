# Create a Workspace or SA-Processing

Functions creating a 'JDemetra+' Workspace (`jws_new()`) and adding a
new SA-Processing (`jws_sap_new()`). A modelling context can be added to
a workspace, it will be valid for all its SA-Processings.

## Usage

``` r
jws_new(modelling_context = NULL)

jws_sap_new(jws, name)
```

## Arguments

- modelling_context:

  a list of variables and calendars

- jws:

  a java workspace object.

- name:

  name of the new SA-Processing to be added (character).

## Value

Returns a java object workspace or SA-Processing.

## Details

A modelling context is a list of variables to be used as external
regressors in modelling processes (Reg-Arima or Tramo) or calendars to
be used to generate calendar regressors. It can be created with
[`rjd3toolkit::modelling_context()`](https://rjdverse.github.io/rjd3toolkit/reference/modelling_context.html)
function or retrieved from another workspace (`(set_context)`)

## References

More information on workspaces in JDemetra+ Graphical User Interface:
<https://jdemetra-new-documentation.netlify.app/t-gui-sa-modelling-features/>

## See also

[`read_workspace`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md),
[`read_sap`](https://rjdverse.github.io/rjd3workspace/reference/read_workspace.md)

## Examples

``` r
# Create an empty 'JDemetra+' Workspace
jws <- jws_new()
#> Error in jws_new(): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws
# Add an empty SA-Processing
jsap <- jws_sap_new(jws, "sap1")
#> Error: object 'jws' not found
```
