# Write a Tramo-Seats specification file

The specification file is a xml file like the one JDemetra+ would write
when defining a specification in the Graphical User Interface.

## Usage

``` r
tramoseats_write_spec(spec, file)
```

## Arguments

- spec:

  a specification created with
  [`rjd3tramoseats::tramoseats_spec`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.html)

- file:

  xml format

## Value

`NULL` returned invisibly

## Examples

``` r
# creating a spec from default
tramoseats_spec <- rjd3tramoseats::tramoseats_spec("rsa3")
#> Error in .jcall("jdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec",     "Ljdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec;",     "fromString", name): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/api/information/InformationSetSerializer has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0
# forcing multiplicative model
tramoseats_spec_d <- rjd3toolkit::set_transform(tramoseats_spec ,
                                               fun = "Log",
                                               outliers = TRUE)
#> Error: object 'tramoseats_spec' not found
# writing the specification in a xml file
tramoseats_write_spec(tramoseats_spec_d, file = normalizePath("~/tmp.xml", mustWork = FALSE))
#> Error: object 'tramoseats_spec_d' not found
```
