# Add a Calendar to a Workspace

Add a Calendar to a Workspace

## Usage

``` r
add_calendar(jws, name, calendar)
```

## Arguments

- jws:

  a java workspace object.

- name:

  character name of the calendar to add.

- calendar:

  JDemetra+ calendar to add.

## Value

`NULL` returned invisibly

## Examples

``` r
# French calendar
french_calendar <- rjd3toolkit::national_calendar(
    days = list(
        rjd3toolkit::fixed_day(7, 14), # Bastille Day
        rjd3toolkit::fixed_day(5, 8, validity = list(start = "1982-05-08")), # End of 2nd WW
        rjd3toolkit::special_day("NEWYEAR"),
        rjd3toolkit::special_day("CHRISTMAS"),
        rjd3toolkit::special_day("MAYDAY"),
        rjd3toolkit::special_day("EASTERMONDAY"),
        rjd3toolkit::special_day("ASCENSION"),
        rjd3toolkit::special_day("WHITMONDAY"),
        rjd3toolkit::special_day("ASSUMPTION"),
        rjd3toolkit::special_day("ALLSAINTSDAY"),
        rjd3toolkit::special_day("ARMISTICE")
    )
)
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
#> Error in .jcall(obj = "jdplus/sa/base/workspace/Ws", returnSig = "Ljdplus/sa/base/workspace/Ws;",     method = "open", full_file_name): RcallMethod: cannot determine object class

# Add calendar to the workspace
add_calendar(jws, "French Calendar", french_calendar)
#> Error in .jcall("jdplus/toolkit/base/r/calendar/Calendars", "Ljdplus/toolkit/base/api/timeseries/calendars/Calendar;",     "calendarOf", bytes): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/api/util/Id has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0
get_context(jws) # The workspace already contained a Test Calendar
#> Error: object 'jws' not found
```
