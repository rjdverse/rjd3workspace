# Read auxiliary regressors file

The variables (regressors) file is a xml file like the one JDemetra+
would write when setting-up user defined regressors in the Graphical
User Interface.

## Usage

``` r
read_variables(file)
```

## Arguments

- file:

  xml format

## Value

A named list of time series objects.

## Examples

``` r
file <- system.file("workspaces", "workspace_test", "Variables", "Vars-1.xml", package = "rjd3workspace")
my_regressors <- read_variables(file)
#> Error in .jcall(obj = "jdplus/toolkit/base/workspace/file/Utility", returnSig = "Ljdplus/toolkit/base/api/timeseries/regression/TsDataSuppliers;",     method = "readData", file): RcallMethod: cannot determine object class
class(my_regressors)
#> Error: object 'my_regressors' not found
str(my_regressors)
#> Error: object 'my_regressors' not found
```
