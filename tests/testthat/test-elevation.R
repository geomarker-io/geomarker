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

elevation_test_url <- function() {
  paste0(
    "https://github.com/geomarker-io/geomarker/releases/download/",
    "v0.0.1/usgs_3dep_conus_800m.tif"
  )
}

elevation_test_fixture <- function() {
  file.path(
    fs::path_package("geomarker", "gmrkr--8841", "R", "geomarker"),
    "get_elevation_summary",
    geomarker_stow_filename(elevation_test_url())
  )
}

test_that("get_elevation_summary uses the versioned asset downloader", {
  fixture <- elevation_test_fixture()
  received <- NULL
  testthat::local_mocked_bindings(
    geomarker_stow = function(url, .subdir, ..., .etag = NULL) {
      received <<- c(
        list(url = url, subdir = .subdir, forced_etag = .etag),
        list(...)
      )
      fixture
    },
    .package = "geomarker"
  )

  out <- get_elevation_summary(
    s2cd_example_cincy(2L),
    quiet = TRUE,
    etag = TRUE
  )
  expect_type(out, "double")
  expect_match(received$url, "/releases/download/v0[.]0[.]1/")
  expect_match(received$url, "usgs_3dep_conus_800m[.]tif$")
  expect_identical(received$subdir, "get_elevation_summary")
  expect_identical(received$quiet, TRUE)
  expect_identical(received$forced_etag, FALSE)
})

test_that("get_elevation_summary works with the offline 3DEP fixture", {
  withr::local_envvar(
    R_USER_DATA_DIR = fs::path_package(
      "geomarker",
      "gmrkr--8841"
    ),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  set.seed(11)
  out <- s2cd_example_cincy(n_locations = 20L) |>
    get_elevation_summary()
  expect_type(out, "double")
  expect_length(out, 20)
})

test_that("elevation extraction uses the first value column", {
  fixture <- elevation_test_fixture()
  raster <- terra::rast(fixture)
  names(raster) <- "unexpected_layer_name"
  renamed <- tempfile(fileext = ".tif")
  withr::defer(unlink(renamed))
  terra::writeRaster(raster, renamed, overwrite = TRUE, datatype = "INT2S")
  testthat::local_mocked_bindings(
    geomarker_stow = function(...) renamed,
    .package = "geomarker"
  )

  expect_type(get_elevation_summary(s2cd_example_cincy(2L)), "double")
})

test_that("elevation fixture uses the downloader cache filename", {
  source <- elevation_test_fixture()
  output_dir <- tempfile("elevation-fixture")
  withr::defer(unlink(output_dir, recursive = TRUE))
  install_elevation_geomarker_fixture(
    s2::as_s2_cell("8841"),
    as.Date("2024-01-01"),
    output_dir,
    source_file = source
  )

  expect_true(file.exists(file.path(
    output_dir,
    "get_elevation_summary",
    geomarker_stow_filename(elevation_test_url())
  )))
})

test_that("get_elevation_summary() works with the published asset", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  out <- s2cd_example_cincy(n_locations = 20L) |>
    get_elevation_summary()
  expect_type(out, "double")
  expect_length(out, 20)
})
