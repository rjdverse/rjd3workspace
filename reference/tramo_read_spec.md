# Read a Tramo specification file

The specification file is a xml file like the one JDemetra+ would write
when defining a specification in the Graphical User Interface.

## Usage

``` r
tramo_read_spec(file)
```

## Arguments

- file:

  xml format,

## Value

list

## Examples

``` r
file <- system.file("workspaces", "workspace_test","TramoSpec","TramoSpec-1.xml", package = "rjd3workspace")
my_spec<- tramo_read_spec(file)
#> Error in .jcall("jdplus/tramoseats/base/workspace/Utility", "Ljdplus/tramoseats/base/api/tramo/TramoSpec;",     "readTramoSpec", as.character((file))): RcallMethod: cannot determine object class
class(my_spec)
#> Error: object 'my_spec' not found
str(my_spec)
#> Error: object 'my_spec' not found
```
