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

test_that("get_elevation_summary works with fixture", {
  withr::local_envvar(
    R_USER_DATA_DIR = fs::path_package(
      "geomarker",
      "gmrkr--8841"
    ),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  set.seed(11)
  out <- s2cd_example_cincy(n_locations = 20L) |>
    get_elevation_summary(etag = FALSE)
  expect_type(out, "double")
  expect_length(out, 20)
})

test_that("get_elevation_summary() works", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  # TODO change to cincy example
  out <- s2cd_example_cincy(n_locations = 20L) |>
    get_elevation_summary()
  expect_type(out, "double")
  expect_length(out, 20)
})
