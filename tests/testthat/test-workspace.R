test_that("save_workspace works", {
    path_ws <- tempfile(fileext = ".xml")

    jws <- jws_new()
    jsap1 <- jws_sap_new(jws, "sap1")
    add_sa_item(jsap1, name = "serie_1", x = rjd3toolkit::ABS$X0.2.09.10.M, rjd3x13::x13_spec())

    expect_message(save_workspace(jws, path_ws))

    jws2 <- jws_open(path_ws)
    jsap2 <- jws_sap(jws, idx = 1L)
    expect_identical(sap_sai_count(jsap2), 1L)
    expect_identical(sap_name(jsap2), "sap1")
    expect_identical(sap_sai_names(jsap2), "serie_1")

    jws <- jws_new()
    jsap1 <- jws_sap_new(jws, "sap1")
    add_sa_item(jsap1, name = "serie_1", x = rjd3toolkit::ABS$X0.2.09.10.M, rjd3x13::x13_spec())

    expect_warning(save_workspace(jws, path_ws, replace = FALSE))
    expect_no_warning(save_workspace(jws, path_ws, replace = FALSE, verbose = FALSE))

    jws2 <- jws_open(path_ws)
    jsap2 <- jws_sap(jws, idx = 1L)
    expect_identical(sap_sai_count(jsap2), 1L)
    expect_identical(sap_name(jsap2), "sap1")
    expect_identical(sap_sai_names(jsap2), "serie_1")

    jws <- jws_new()
    jsap1 <- jws_sap_new(jws, "sap1")
    add_sa_item(jsap1, name = "serie_2", x = rjd3toolkit::ABS$X0.2.09.10.M, rjd3x13::x13_spec())

    expect_message(expect_message(save_workspace(jws, path_ws, replace = TRUE)))
    expect_no_message(save_workspace(jws, path_ws, replace = TRUE, verbose = FALSE))

    jws2 <- jws_open(path_ws)
    jsap2 <- jws_sap(jws, idx = 1L)
    expect_identical(sap_sai_count(jsap2), 1L)
    expect_identical(sap_name(jsap2), "sap1")
    expect_identical(sap_sai_names(jsap2), "serie_2")
})
