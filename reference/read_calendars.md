# Read a Calendar file

The calendar file is a xml file like the one JDemetra+ would write when
defining a calendar in the Graphical User Interface.

## Usage

``` r
read_calendars(file)
```

## Arguments

- calendar:

  a xml file

## Value

a list of `JD3_CALENDAR` objects

## Examples

``` r
file <- system.file("workspaces", "workspace_test", "Calendars", "Calendars.xml", package = "rjd3workspace")
my_calendar <- read_calendars(file)
my_calendar
#> $Test_Calendar
#> Holiday:
#>  - Fixed day: month=1, day=1
#>  - Fixed day: month=5, day=1
#>  - Prespecified holiday: event=EASTERMONDAY
#>  - Prespecified holiday: event=ASCENSION
#>  - Prespecified holiday: event=WHITMONDAY
#> 
#> Mean correction: Yes
#> 
```
