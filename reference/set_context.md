# Set Context of a Workspace

Set Context of a Workspace

## Usage

``` r
set_context(jws, modelling_context = NULL)
```

## Arguments

- jws:

  a java workspace object.

- modelling_context:

  a list of variables and calendars

## Examples

``` r
library("rjd3toolkit")
#> Your java version is 17. 21 or higher is needed.
#> 
#> Attaching package: ‘rjd3toolkit’
#> The following objects are masked from ‘package:stats’:
#> 
#>     aggregate, mad

# French calendar
french_calendar <- national_calendar(
    days = list(
        fixed_day(7, 14), # Bastille Day
        fixed_day(5, 8, validity = list(start = "1982-05-08")), # End of 2nd WW
        special_day("NEWYEAR"),
        special_day("CHRISTMAS"),
        special_day("MAYDAY"),
        special_day("EASTERMONDAY"),
        special_day("ASCENSION"),
        special_day("WHITMONDAY"),
        special_day("ASSUMPTION"),
        special_day("ALLSAINTSDAY"),
        special_day("ARMISTICE")
    )
)

# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class

# Creating a new context
new_context <- modelling_context(
    calendars = list(FR = french_calendar),
    variables = list(a = AirPassengers)
)

# Set the context
set_context(jws, new_context)
#> Error in .jcall("jdplus/toolkit/base/r/util/Modelling", "Ljdplus/toolkit/base/api/timeseries/regression/ModellingContext;",     "of", bytes): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws
```
