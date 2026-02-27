test_that("multiplication works", {
    file <- system.file(
        "workspaces",
        "workspace_test.xml",
        package = "rjd3workspace"
    )
    jws <- jws_open(file)

    # Check if the SA-Item 3 in the SA-Processing 1 exists
    expect_true(check_information(jws = jws, idx_sap = 1, idx_sai = 3))

    # Check if the SA-Items 1, 2 and 5 in the SA-Processing 1 exist
    expect_true(check_information(jws = jws, idx_sap = 1, idx_sai = c(1, 2, 4)))
})
