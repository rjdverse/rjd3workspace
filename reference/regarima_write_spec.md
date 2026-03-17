# Write a Reg-Arima specification file

The specification file is a xml file like the one JDemetra+ would write
when defining a specification in the Graphical User Interface.

## Usage

``` r
regarima_write_spec(spec, file)
```

## Arguments

- spec:

  a specification created with
  [`rjd3x13::regarima_spec`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.html)

- file:

  xml format

## Value

`NULL` returned invisibly

## Examples

``` r
# Creating a spec from default
regarima_spec <- rjd3x13::regarima_spec("rg3")

# Forcing multiplicative model
regarima_spec_d <- rjd3toolkit::set_transform(
    regarima_spec ,
    fun = "Log",
    outliers = TRUE
)

# Writing the specification in a xml file
spec_path <- tempfile(fileext = ".xml")
regarima_write_spec(regarima_spec_d, file = spec_path)
```
