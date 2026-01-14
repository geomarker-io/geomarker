test_that("get_gridmet_data validates date range", {
  x_bad <- s2cd(
    s2::as_s2_cell("8841b39a7c46e25f"),
    dates = list(as.Date("1970-01-01"))
  )

  expect_error(
    get_gridmet_data(x_bad, "tmmx"),
    "after 1979-01-01"
  )
})

test_that("get_gridmet_data validates gridmet_var", {
  x_ok <- s2cd(
    s2::as_s2_cell("8841b39a7c46e25f"),
    dates = list(as.Date("1980-01-01"))
  )

  expect_error(
    get_gridmet_data(x_ok, "not_a_var"),
    "must be one of"
  )
})

test_that("get_gridmet_data works", {
  skip_if_offline()
  skip_if_not_installed("terra")

  x_ok <- s2cd(
    s2::as_s2_cell("8841b39a7c46e25f"),
    dates = list(c(as.Date("2022-01-01"), as.Date("2022-05-19")))
  )
  out <- get_gridmet_data(x_ok, "tmmx")

  expect_type(out, "list")
  expect_length(out, 1)
  expect_named(out, as.character(x_ok))
  expect_type(out[[1]], "double")
  expect_length(out[[1]], 2)
})
