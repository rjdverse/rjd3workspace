# Read a Tramo-Seats specification file

The specification file is a xml file like the one JDemetra+ would write
when defining a specification in the Graphical User Interface.

## Usage

``` r
tramoseats_read_spec(file)
```

## Arguments

- file:

  xml format,

## Value

list

## Examples

``` r
file <- system.file("workspaces", "workspace_test","TramoSeatsSpec","TramoSeatsSpec-1.xml", package = "rjd3workspace")
my_spec<- tramoseats_read_spec(file)
#> Error in .jcall(obj = "jdplus/tramoseats/base/workspace/Utility", returnSig = "Ljdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec;",     method = "readTramoSeatsSpec", as.character(file)): RcallMethod: cannot determine object class
class(my_spec)
#> Error: object 'my_spec' not found
str(my_spec)
#> Error: object 'my_spec' not found
```
