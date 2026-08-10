# Set Context of a Workspace

Set Context of a Workspace

## Usage

``` r
set_context(jws, modelling_context = NULL)
```

## Arguments

- jws:

  a Javaworkspace object.

- modelling_context:

  a list of variables and calendars

## Value

Invisibly `NULL`

## Examples

``` r

library("rjd3toolkit")
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
# \donttest{
jws <- jws_open(file)

# Creating a new context
new_context <- modelling_context(
    calendars = list(FR = french_calendar),
    variables = list(a = AirPassengers)
)
#> Replaced 1 duplicated or missing name(s).

# Set the context
set_context(jws, new_context)
# }
```
