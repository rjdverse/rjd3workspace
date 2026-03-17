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
library("rjd3toolkit")
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

calendar_path <- tempfile(pattern = "calendar", fileext = ".xml")

write_calendars(BE, file = calendar_path)
write_calendars(list(BEL_cal = BE), file = calendar_path)
```
