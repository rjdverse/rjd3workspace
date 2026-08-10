# Write a Calendar file

The calendar file is a xml file like the one JDemetra+ would write when
defining a calendar in the Graphical User Interface. Calendars can be
defined with
[`rjd3toolkit::national_calendar`](https://rjdverse.github.io/rjd3toolkit/reference/national_calendar.html)

## Usage

``` r
write_calendars(calendars, file, verbose = TRUE)
```

## Arguments

- calendars:

  named list of calendars or a `JD3_CALENDAR` object

- file:

  xml format

- verbose:

  Boolean indicating whether to print additional information. Default is
  `TRUE`.

## Value

`NULL` returned invisibly

## Details

If `calendars` is a single calendar (`JD3_CALENDAR` object), it will be
named `cal` by default. If `calendars` is a list of calendars
(`JD3_CALENDAR` object). Then they all must be named. Else, a error is
risen.

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
#> The calendar will be renamed `cal`.
write_calendars(list(BEL_cal = BE), file = calendar_path)
```
