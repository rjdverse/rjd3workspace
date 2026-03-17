# Write regressors file

Write regressors file

## Usage

``` r
write_variables(vars, file)
```

## Arguments

- vars:

  A named list of `ts` objects.

- file:

  Path to the output XML file.

## Value

No return value (`NULL` returned invisibly). This function writes
variables to file for use in JD+.

## Examples

``` r
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml",
                    package = "rjd3workspace")
jws <- jws_open(file)

# Get context
my_context <- get_context(jws)
vars <- my_context$variables[[1L]]

# Writing the regressors in a xml file
variable_path <- tempfile(fileext = ".xml")
write_variables(vars, file = variable_path)
```
