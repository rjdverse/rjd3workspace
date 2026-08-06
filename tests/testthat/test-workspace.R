test_that("add_variables works", {
    jws <- jws_new()
    add_variables(jws = jws, group = "reg1", y = AirPassengers, name = "x1")
    ctx <- get_context(jws)
    expect_identical(length(ctx$variables), 1L)
    expect_identical(names(ctx$variables), "reg1")
    expect_identical(length(ctx$variables$reg1), 1L)
    expect_identical(names(ctx$variables$reg1), "x1")
})
