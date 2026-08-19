test_that("geomarker stow fixes managed local copy ownership and forwards arguments", {
  received <- NULL
  testthat::local_mocked_bindings(
    stow = function(...) {
      received <<- list(...)
      "managed-file"
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
    "managed-file"
  )
  expect_identical(received$url, "https://example.com/data/file.zip")
  expect_identical(received$package, "geomarker")
  expect_identical(received$subdir, "get_example_data")
  expect_identical(received$overwrite, TRUE)
  expect_identical(received$quiet, TRUE)
})

test_that("legacy data helpers are not exported", {
  exports <- getNamespaceExports("geomarker")
  expect_false(any(
    c(
      "geomarker_download_file",
      "geomarker_data_dir",
      "geomarker_data_dir_info"
    ) %in%
      exports
  ))
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
      "managed-file"
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
  expect_identical(
    path,
    normalizePath(
      file.path(
        tools::R_user_dir("geomarker", "data"),
        "stow",
        "get_elevation_summary"
      ),
      winslash = "/",
      mustWork = TRUE
    )
  )

  url <- "https://example.com/data/file.zip"
  stow_filename <- getFromNamespace(".stow_url_to_filename", "stow")
  expect_identical(geomarker_stow_filename(url), stow_filename(url))
})

test_that("fixture paths reproduce the stow 0.3.0 hierarchy", {
  output_dir <- tempfile("geomarker-fixture-")
  withr::defer(unlink(output_dir, recursive = TRUE))
  path <- geomarker_fixture_stow_dir(output_dir, "get_evi_data")

  expect_identical(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    normalizePath(
      file.path(output_dir, "stow", "get_evi_data"),
      winslash = "/",
      mustWork = TRUE
    )
  )
  expect_false(dir.exists(file.path(output_dir, "get_evi_data")))
})

test_that("pre-0.3.0 stow paths are ignored", {
  withr::local_envvar(c(
    R_USER_DATA_DIR = tempfile("geomarker-stow-data-"),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  ))
  url <- "https://example.com/data/file.zip"
  legacy <- file.path(
    tools::R_user_dir("geomarker", "data"),
    "get_example_data",
    geomarker_stow_filename(url)
  )
  dir.create(dirname(legacy), recursive = TRUE)
  file.create(legacy)

  expect_error(
    geomarker_stow(url, "get_example_data", quiet = TRUE),
    "No managed local copy"
  )
})
