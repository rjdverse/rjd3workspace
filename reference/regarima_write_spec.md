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
# creating a spec from default
reg_arima_spec <- rjd3x13::regarima_spec("rg3")
# forcing multiplicative model
reg_arima_spec_d <- rjd3toolkit::set_transform(reg_arima_spec ,
                                             fun = "Log",
                                            outliers = TRUE)
# writing the specification in a xml file
regarima_write_spec(reg_arima_spec_d, file = normalizePath("~/tmp.xml", mustWork = FALSE))
```
