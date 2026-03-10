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
#> Error in .jcall(obj = "jdplus/toolkit/base/workspace/file/Utility", returnSig = "Ljdplus/toolkit/base/api/timeseries/calendars/CalendarManager;",     method = "readCalendars", file): RcallMethod: cannot determine object class
my_calendar
#> Error: object 'my_calendar' not found
```
