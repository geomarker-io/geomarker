test_that("check_installed works", {
  check_installed("not.a.real.package") |>
    expect_error("`not.a.real.package` is required")
  check_installed("tools") |>
    expect_invisible() |>
    expect_true()
})
