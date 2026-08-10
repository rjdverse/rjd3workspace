test_that("write_calendars works", {
    BE <- rjd3toolkit::national_calendar(list(
        rjd3toolkit::fixed_day(7, 21),
        rjd3toolkit::special_day("NEWYEAR"),
        rjd3toolkit::special_day("CHRISTMAS"),
        rjd3toolkit::special_day("MAYDAY"),
        rjd3toolkit::special_day("EASTERMONDAY"),
        rjd3toolkit::special_day("ASCENSION"),
        rjd3toolkit::special_day("WHITMONDAY"),
        rjd3toolkit::special_day("ASSUMPTION"),
        rjd3toolkit::special_day("ALLSAINTSDAY"),
        rjd3toolkit::special_day("ARMISTICE")
    ))

    calendar_path <- tempfile(pattern = "calendar", fileext = ".xml")

    expect_message(write_calendars(BE, file = calendar_path))
    expect_no_message(write_calendars(BE, file = calendar_path, verbose = FALSE))
    expect_no_message(write_calendars(list(BEL_cal = BE), file = calendar_path))
    expect_error(write_calendars(list(BE), file = calendar_path))
    expect_error(write_calendars(list(BE), file = calendar_path))
    expect_error(write_calendars(list(BE, BE), file = calendar_path))
    expect_error(write_calendars(list(BE, BE, my_cal = BE, BE), file = calendar_path))
})
