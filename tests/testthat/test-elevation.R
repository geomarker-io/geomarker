test_that("get_elevation_summary validates inputs", {
  x_ok <- s2cd(
    s2::as_s2_cell("8841b39a7c46e25f"),
    dates = list(as.Date("2024-01-01"))
  )

  expect_error(
    get_elevation_summary(x_ok, buffer = c(800, 900)),
    "buffer must be length one"
  )
  expect_error(
    get_elevation_summary(x_ok, fun = "not_a_function"),
    "fun must be a function"
  )
})

test_that("get_elevation_summary integration path works", {
  skip_if_offline()
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  x_ok <- s2cd(
    s2::as_s2_cell("8841b39a7c46e25f"),
    dates = list(as.Date("2024-01-01"))
  )
  out <- get_elevation_summary(x_ok)

  expect_type(out, "double")
  expect_length(out, 1)
})
