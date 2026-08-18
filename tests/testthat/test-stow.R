test_that("geomarker stow fixes cache ownership and forwards arguments", {
  received <- NULL
  testthat::local_mocked_bindings(
    stow = function(...) {
      received <<- list(...)
      "cached-file"
    },
    .package = "stow"
  )
  withr::local_envvar(R_GEOMARKER_NO_DOWNLOAD = NA)

  expect_identical(
    geomarker_stow(
      "https://example.com/data/file.zip",
      "get_example_data",
      overwrite = TRUE,
      quiet = TRUE
    ),
    "cached-file"
  )
  expect_identical(received$url, "https://example.com/data/file.zip")
  expect_identical(received$package, "geomarker")
  expect_identical(received$subdir, "get_example_data")
  expect_identical(received$overwrite, TRUE)
  expect_identical(received$quiet, TRUE)
})

test_that("legacy cache helpers are not exported", {
  exports <- getNamespaceExports("geomarker")
  expect_false(any(c(
    "geomarker_download_file",
    "geomarker_data_dir",
    "geomarker_data_dir_info"
  ) %in% exports))
})

test_that("geomarker stow reserves package and subdir", {
  url <- "https://example.com/data/file.zip"
  expect_error(
    geomarker_stow(url, "get_example_data", package = "other"),
    "fixed by geomarker"
  )
  expect_error(
    geomarker_stow(url, "get_example_data", subdir = "other"),
    "fixed by geomarker"
  )
})

test_that("R_GEOMARKER_NO_DOWNLOAD forces stow offline", {
  received <- NULL
  testthat::local_mocked_bindings(
    stow = function(...) {
      received <<- list(...)
      "cached-file"
    },
    .package = "stow"
  )
  withr::local_envvar(R_GEOMARKER_NO_DOWNLOAD = "true")

  geomarker_stow(
    "https://example.com/data/file.zip",
    "get_example_data",
    offline = FALSE
  )
  expect_identical(received$offline, TRUE)
})

test_that("geomarker paths and fixture names match stow", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("geomarker-stow-data-"))
  path <- geomarker_stow_path("get_elevation_summary")
  expect_true(dir.exists(path))
  expect_true(endsWith(path, "/R/geomarker/get_elevation_summary"))

  url <- "https://example.com/data/file.zip"
  stow_filename <- getFromNamespace(".stow_url_to_filename", "stow")
  expect_identical(geomarker_stow_filename(url), stow_filename(url))
})

test_that("legacy flat cache entries are ignored", {
  withr::local_envvar(c(
    R_USER_DATA_DIR = tempfile("geomarker-stow-data-"),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  ))
  url <- "https://example.com/data/file.zip"
  legacy <- file.path(
    stow::stow_path(package = "geomarker"),
    geomarker_stow_filename(url)
  )
  file.create(legacy)

  expect_error(
    geomarker_stow(url, "get_example_data", quiet = TRUE),
    "No cached file"
  )
})
