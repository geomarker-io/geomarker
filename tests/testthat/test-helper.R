test_that("check_installed works", {
  check_installed("not.a.real.package") |>
    expect_error("`not.a.real.package` is required")
  check_installed("not.a.real.package", "to test this function out") |>
    expect_error("`not.a.real.package` is required to test this function out")
  check_installed("tools") |>
    expect_invisible() |>
    expect_true()
})
