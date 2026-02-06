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
    "should be one of"
  )
})

test_that("get_gridmet_data works with fixture", {
  withr::local_envvar(
    R_USER_DATA_DIR = fs::path_package(
      "geomarker",
      "gmrkr--8841"
    ),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  set.seed(19)
  xx <- s2cd_example_cincy(n_locations = 20L)
  out <- get_gridmet_data(xx)
  expect_type(out, "list")
  expect_length(out, 20)
  expect_named(out, as.character(xx))
  expect_type(out[[1]], "double")
  expect_length(out[[1]], 3)
  expect_length(out[[2]], 3)
})

test_that("get_gridmet_data works", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  out <- get_gridmet_data(s2cd_example(), "tmmx")
  expect_type(out, "list")
  expect_length(out, 2)
  expect_named(out, as.character(s2cd_example()))
  expect_type(out[[1]], "double")
  expect_length(out[[1]], 1)
  expect_length(out[[2]], 2)
})
