# Write a Calendar file

The calendar file is a xml file like the one JDemetra+ would write when
defining a calendar in the Graphical User Interface. Calendars can be
defined with
[`rjd3toolkit::national_calendar`](https://rjdverse.github.io/rjd3toolkit/reference/national_calendar.html)

## Usage

``` r
write_calendars(calendars, file)
```

## Arguments

- calendars:

  list of calendars or a `JD3_CALENDAR` object

- file:

  xml format

## Value

`NULL` returned invisibly

## Examples

``` r
library(rjd3toolkit)
BE <- national_calendar(list(
   fixed_day(7, 21),
   special_day("NEWYEAR"),
   special_day("CHRISTMAS"),
   special_day("MAYDAY"),
   special_day("EASTERMONDAY"),
   special_day("ASCENSION"),
   special_day("WHITMONDAY"),
   special_day("ASSUMPTION"),
   special_day("ALLSAINTSDAY"),
   special_day("ARMISTICE")
))
write_calendars(BE,
        file = normalizePath("~/tmp.xml", mustWork = FALSE))
#> Error in .jcall("jdplus/toolkit/base/r/util/Modelling", "Ljdplus/toolkit/base/api/timeseries/calendars/CalendarManager;",     "calendarsOf", bytes): RcallMethod: cannot determine object class
write_calendars(list(BEL_cal = BE),
        file = normalizePath("~/tmp.xml", mustWork = FALSE))
#> Error in .jcall("jdplus/toolkit/base/r/util/Modelling", "Ljdplus/toolkit/base/api/timeseries/calendars/CalendarManager;",     "calendarsOf", bytes): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/r/util/Modelling has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0
```
