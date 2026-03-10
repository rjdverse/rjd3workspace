# Write a X13 specification file

The specification file is a xml file like the one JDemetra+ would write
when defining a specification in the Graphical User Interface.

## Usage

``` r
x13_write_spec(spec, file)
```

## Arguments

- spec:

  a specification created with
  [`rjd3x13::x13_spec`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.html)

- file:

  xml format

## Value

`NULL` returned invisibly

## Examples

``` r
# creating a spec from default
x13_spec<- rjd3x13::x13_spec("rsa3")
#> Error in .jcall("jdplus/x13/base/r/X13", "[B", "toBuffer", jspec): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/api/information/InformationSetSerializer has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0
# forcing multiplicative model
x13_spec_d <- rjd3toolkit::set_transform(x13_spec,
                                        fun = "Log",
                                        outliers = TRUE)
#> Error in x$transform: object of type 'closure' is not subsettable
# writing the specification in a xml file
x13_write_spec(x13_spec_d, file = normalizePath("~/tmp.xml", mustWork = FALSE))
#> Error: object 'x13_spec_d' not found
```
