# Add a Variable to a JD+ Workspace

Adds a single time series variable to a specified group within a JD+
workspace..

## Usage

``` r
add_variables(jws, ...)
```

## Arguments

- jws:

  A JD+ workspace object (Java pointer).

- ...:

  Additional arguments passed to
  [`rjd3toolkit::complete_modelling_context()`](https://rjdverse.github.io/rjd3toolkit/reference/complete_modelling_context.html)
  as:

  - groupA character string indicating the name of the group in which to
    store the variable.

  - yA `ts` object (R time series) to be added. Only a single time
    series can be added at a time.

  - nameA character string naming the variable.

  - overwritea Boolean to indicate whether a variable already present
    should be replaced

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

[`rjd3toolkit::modelling_context()`](https://rjdverse.github.io/rjd3toolkit/reference/modelling_context.html)
to create multiple variables and groups at once, and
[`read_variables()`](https://rjdverse.github.io/rjd3workspace/reference/read_variables.md),
[`write_variables()`](https://rjdverse.github.io/rjd3workspace/reference/write_variables.md)
to import/export variables.

## Examples

``` r
# Create a Workspace
my_ws <- jws_new()

# \donttest{
# Add one variable
add_variables(
    jws = my_ws,
    group = "reg1",
    y = AirPassengers,
    name = "x1"
)

# Add 2 named variables
add_variables(
    jws = my_ws,
    group = "reg1",
    y = list(a = mdeaths, b = ldeaths),
    name = "DeathMale"
)

# Add a list of variables (or a MTS)
add_variables(
    jws = my_ws,
    group = "reg1",
    y = rjd3toolkit::ABS
)
#> Replaced forbidden character(s) in 22 name(s).
add_variables(
    jws = my_ws,
    group = "reg1",
    y = Seatbelts
)
# }
```
