# Read a X13 specification file

The specification file is a xml file like the one JDemetra+ would write
when defining a specification in the Graphical User Interface.

## Usage

``` r
x13_read_spec(file)
```

## Arguments

- file:

  xml format,

## Value

list

## Examples

``` r
file <- system.file("workspaces", "workspace_test","X13Spec","X13Spec-1.xml", package = "rjd3workspace")
my_spec<-x13_read_spec(file)
#> Error in .jcall(obj = "jdplus/x13/base/workspace/Utility", returnSig = "Ljdplus/x13/base/api/x13/X13Spec;",     method = "readX13Spec", file): RcallMethod: cannot determine object class
class(my_spec)
#> Error: object 'my_spec' not found
str(my_spec)
#> Error: object 'my_spec' not found
```
