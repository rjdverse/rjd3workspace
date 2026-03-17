# Write a Tramo specification file

The specification file is a xml file like the one JDemetra+ would write
when defining a specification in the Graphical User Interface.

## Usage

``` r
tramo_write_spec(spec, file)
```

## Arguments

- spec:

  a specification created with
  [`rjd3tramoseats::tramo_spec`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.html)

- file:

  xml format

## Value

`NULL` returned invisibly

## Examples

``` r
# Creating a spec from default
tramo_spec <- rjd3tramoseats::tramo_spec("tr3")

# Forcing multiplicative model
tramo_spec_d <- rjd3toolkit::set_transform(
    tramo_spec ,
    fun = "Log",
    outliers = TRUE
)

# Writing the specification in a xml file
spec_path <- tempfile(fileext = ".xml")
tramo_write_spec(tramo_spec_d, file = spec_path)
```
