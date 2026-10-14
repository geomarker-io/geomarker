test_that("get_narr_data validates date range", {
  x_bad <- s2cd(
    s2::as_s2_cell("8841b39a7c46e25f"),
    dates = list(as.Date("1970-01-01"))
  )

  expect_error(
    get_narr_data(x_bad, "air.2m"),
    "after 1979-01-01"
  )
})

test_that("get_narr_data validates narr_var", {
  x_ok <- s2cd(
    s2::as_s2_cell("8841b39a7c46e25f"),
    dates = list(as.Date("1980-01-01"))
  )
  expect_error(
    get_narr_data(x_ok, "not_a_var"),
    "should be one of"
  )
})

test_that("get_narr_data works with fixture", {
  withr::local_envvar(
    R_USER_DATA_DIR = fs::path_package(
      "geomarker",
      "gmrkr--8841"
    ),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  set.seed(19)
  xx <- s2cd_example_cincy(n_locations = 20L)
  out <- get_narr_data(xx)
  expect_type(out, "list")
  expect_length(out, 20)
  expect_named(out, as.character(xx))
  expect_type(out[[1]], "double")
  expect_length(out[[1]], 3)
  expect_length(out[[2]], 3)
})

test_that("NARR recovers from rate limits without extra header requests", {
  withr::local_envvar(R_GEOMARKER_NO_DOWNLOAD = NA)
  attempts <- 0L
  waits <- numeric()
  testthat::local_mocked_bindings(
    stow = function(url, package, subdir, etag, quiet) {
      expect_false(etag)
      attempts <<- attempts + 1L
      if (attempts < 3L) {
        stop("Download failed.\nThe requested URL returned error: 429")
      }
      "downloaded.nc"
    },
    .package = "stow"
  )
  testthat::local_mocked_bindings(
    Sys.sleep = function(time) waits <<- c(waits, time),
    .package = "base"
  )
  expect_identical(
    narr_stow("https://example.com/data.nc", quiet = TRUE),
    "downloaded.nc"
  )
  expect_identical(attempts, 3L)
  expect_equal(waits, c(5, 10))
})

test_that("NARR stops after bounded rate-limit retries", {
  withr::local_envvar(R_GEOMARKER_NO_DOWNLOAD = NA)
  attempts <- 0L
  waits <- numeric()
  testthat::local_mocked_bindings(
    stow = function(...) {
      attempts <<- attempts + 1L
      stop("Download failed.\nThe requested URL returned error: 429")
    },
    .package = "stow"
  )
  testthat::local_mocked_bindings(
    Sys.sleep = function(time) waits <<- c(waits, time),
    .package = "base"
  )
  expect_error(narr_stow("https://example.com/data.nc", quiet = TRUE), "429")
  expect_identical(attempts, 5L)
  expect_equal(waits, c(5, 10, 20, 40))
})

test_that("NARR does not retry missing files", {
  attempts <- 0L
  testthat::local_mocked_bindings(
    stow = function(...) {
      attempts <<- attempts + 1L
      stop("The requested URL returned error: 404")
    },
    .package = "stow"
  )
  testthat::local_mocked_bindings(
    Sys.sleep = function(time) stop("unexpected retry"),
    .package = "base"
  )
  expect_error(narr_stow("https://example.com/data.nc"), "404")
  expect_identical(attempts, 1L)
})

test_that("NARR preserves offline behavior without retries", {
  withr::local_envvar(c(
    R_USER_DATA_DIR = tempfile("narr-offline-"),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  ))
  testthat::local_mocked_bindings(
    Sys.sleep = function(time) stop("unexpected retry"),
    .package = "base"
  )
  expect_error(
    narr_stow("https://example.com/data.nc", quiet = TRUE),
    "No managed local copy"
  )
})
