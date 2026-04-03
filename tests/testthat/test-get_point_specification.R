library("rjd3toolkit")
library("rjd3x13")

test_that("get_point_specification works", {

    # Case 1
    jws <- jws_new()
    jsap <- jws_sap_new(jws, "sap1")
    add_sa_item(jsap, "test", ABS$X0.2.09.10.M, x13_spec())
    jsai <- jsap_sai(jsap, 1L)

    expect_no_error({res <- read_sai(jsai)})
})
