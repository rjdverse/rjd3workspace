# Add a Variable to a JD+ Workspace

Adds a single time series variable to a specified group within a JD+
workspace..

## Usage

``` r
add_variables(jws, group, name, y, overwrite = FALSE)
```

## Arguments

- jws:

  A JD+ workspace object (Java pointer).

- group:

  A character string indicating the name of the group in which to store
  the variable.

- name:

  A character string naming the variable.

- y:

  A `ts` object (R time series) to be added. Only a single time series
  can be added at a time.

## Value

No return value (`NULL` returned invisibly). This function is used for
its side effect of modifying the workspace.

## Details

For the time being, if the group does not already exist, a new group is
created, but the group will be named after `name`, not `group`.

## Limitations

- Cannot add multiple variables at once.

- Does not support dynamic ts objects with metadata.

- If group does not exist, a new group is created but named after the
  variable name, not the intended group.

## See also

`modelling_context` to create multiple variables and groups at once, and
[`read_variables`](https://rjdverse.github.io/rjd3workspace/reference/read_variables.md),
[`write_variables`](https://rjdverse.github.io/rjd3workspace/reference/write_variables.md)
to import/export variables.

## Examples

``` r
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
add_variables(jws = jws, group = "reg1", y = AirPassengers, name = "x1")
```
