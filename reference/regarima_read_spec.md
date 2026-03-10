# Read a Reg-Arima specification file

The specification file is a xml file like the one JDemetra+ would write
when defining a specification in the Graphical User Interface.

## Usage

``` r
regarima_read_spec(file)
```

## Arguments

- file:

  xml format,

## Value

list

## Examples

``` r
file <- system.file("workspaces", "workspace_test","RegArimaSpec","RegArimaSpec-1.xml", package = "rjd3workspace")
my_spec<-regarima_read_spec(file)
#> Error in .jcall("jdplus/x13/base/workspace/Utility", "Ljdplus/x13/base/api/regarima/RegArimaSpec;",     "readRegArimaSpec", as.character((file))): RcallMethod: cannot determine object class
class(my_spec)
#> Error: object 'my_spec' not found
str(my_spec)
#> Error: object 'my_spec' not found
```
