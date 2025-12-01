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
class(my_regressors)
#> [1] "list"
str(my_regressors)
#> List of 2
#>  $ reg_ud_covid :List of 2
#>   ..$ moniker:List of 2
#>   .. ..$ source: chr "XCLPRVDR"
#>   .. ..$ id    : chr "demetra://tsprovider/XCLPRVDR/20111201/SERIES?file=C%3A%5CUsers%5CYWYD5I%5CDocuments%5C00_RJD3_Developpement%5C"| __truncated__
#>   .. ..- attr(*, "class")= chr "JD3_TSMONIKER"
#>   ..$ data   : Time-Series [1:444] from 1990 to 2027: 0 0 0 0 0 0 0 0 0 0 ...
#>   .. ..- attr(*, "name")= chr ""
#>   ..- attr(*, "class")= chr "JD3_DYNAMICTS"
#>  $ reg_ud_2022_1:List of 2
#>   ..$ moniker:List of 2
#>   .. ..$ source: chr "XCLPRVDR"
#>   .. ..$ id    : chr "demetra://tsprovider/XCLPRVDR/20111201/SERIES?file=C%3A%5CUsers%5CYWYD5I%5CDocuments%5C00_RJD3_Developpement%5C"| __truncated__
#>   .. ..- attr(*, "class")= chr "JD3_TSMONIKER"
#>   ..$ data   : Time-Series [1:444] from 1990 to 2027: 0 0 0 0 0 0 0 0 0 0 ...
#>   .. ..- attr(*, "name")= chr ""
#>   ..- attr(*, "class")= chr "JD3_DYNAMICTS"
```
