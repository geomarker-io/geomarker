test_that("historical EVI gaps warn, cache their provenance, and refresh online", {
  skip_if_not_installed("terra")
  directory <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = directory, R_GEOMARKER_NO_DOWNLOAD = "")
  items <- evi_test_items()
  items$xmin <- -85
  items$xmax <- -84
  items$ymin <- 39
  items$ymax <- 40
  listing <- items[-12, ]
  builds <- 0L
  queries <- 0L
  local_mocked_bindings(
    evi_planetary_computer_items = function(...) {
      queries <<- queries + 1L
      listing
    },
    evi_build_annual_composite = function(items, dest, ...) {
      builds <<- builds + 1L
      r <- terra::rast(nrows = 2, ncols = 2, xmin = -85, xmax = -84, ymin = 39, ymax = 40)
      terra::values(r) <- nrow(items) / 100
      terra::writeRaster(r, dest, overwrite = TRUE)
      evi_write_manifest(dest, items)
    }
  )
  x <- s2cd(s2::as_s2_cell(s2::s2_lnglat(-84.5, 39.5)), list(as.Date("2024-06-01")))
  expect_warning(out <- get_evi_summary(x, quiet = TRUE), "terra 2024-06-25")
  expect_equal(unname(out[[1]]), 0.45, tolerance = 1e-6)
  expect_identical(builds, 1L)
  path <- file.path(geomarker_stow_path("get_evi_summary"), evi_annual_composite_filename("2024", "h11v05"))
  record <- evi_read_manifest(path, "2024", "h11v05")
  expect_identical(record[["Coverage-Status"]], "incomplete")
  expect_identical(record[["Missing-Acquisitions"]], "terra 2024-06-25")
  expect_identical(record[["Missing-Count"]], "1")
  expect_identical(record[["Expected-Count"]], "46")
  expect_identical(record[["Source-Count"]], "45")
  expect_warning(get_evi_summary(x, quiet = TRUE), "incomplete")
  expect_identical(builds, 1L) # Same source set reuses the composite.

  Sys.setenv(R_GEOMARKER_NO_DOWNLOAD = "true")
  previous_queries <- queries
  expect_warning(offline <- get_evi_summary(x, quiet = TRUE), "terra 2024-06-25")
  expect_equal(offline, out)
  expect_identical(queries, previous_queries)
  expect_error(get_evi_summary(x, quiet = TRUE, require_complete = TRUE), "incomplete")
  expect_identical(queries, previous_queries)

  Sys.setenv(R_GEOMARKER_NO_DOWNLOAD = "")
  expect_error(get_evi_summary(x, quiet = TRUE, require_complete = TRUE), "incomplete")
  expect_identical(builds, 1L)
  # A newer processing revision must refresh even while the gap remains.
  listing$id[[1]] <- sub("2025001000000$", "2026001000000", listing$id[[1]])
  expect_warning(get_evi_summary(x, quiet = TRUE), "incomplete")
  expect_identical(builds, 2L)
  revised <- evi_read_manifest(path, "2024", "h11v05")
  expect_false(identical(revised[["Source-Signature"]], record[["Source-Signature"]]))
  listing$quality_href[[1]] <- paste0(listing$quality_href[[1]], "?revision=2")
  expect_warning(get_evi_summary(x, quiet = TRUE), "incomplete")
  expect_identical(builds, 3L)

  # Arrival of the missing observation promotes a new composite to complete.
  listing <- items
  expect_no_warning(complete <- get_evi_summary(x, quiet = TRUE, require_complete = TRUE))
  expect_equal(unname(complete[[1]]), 0.46, tolerance = 1e-6)
  expect_identical(builds, 4L)
  expect_identical(evi_read_manifest(path, "2024", "h11v05")[["Coverage-Status"]], "complete")
})

test_that("partial manifest coverage cannot conceal missing acquisitions", {
  directory <- withr::local_tempdir()
  path <- file.path(directory, "partial.tif")
  writeLines("partial composite", path)
  items <- evi_test_items()[-12, ]
  evi_write_manifest(path, items)
  expect_true(evi_cache_valid(path, "2024", "h11v05", items = items))
  expect_false(evi_cache_valid(path, "2024", "h11v05", require_complete = TRUE))
  expect_false(evi_cache_valid(path, "2024", "h11v05", items = evi_test_items()))
  record <- evi_read_manifest(path, "2024", "h11v05")
  record[["Missing-Acquisitions"]] <- "terra 2024-01-01"
  write.dcf(as.data.frame(record, check.names = FALSE), evi_manifest_path(path), keep.white = names(record))
  expect_false(evi_cache_valid(path, "2024", "h11v05"))
  evi_write_manifest(path, evi_test_items()[-c(12, 23, 46), ])
  record <- evi_read_manifest(path, "2024", "h11v05")
  expect_identical(record[["Missing-Count"]], "3")
  expect_length(strsplit(record[["Missing-Acquisitions"]], "\n", fixed = TRUE)[[1]], 3L)
})

test_that("source staging separates changed inputs while allowing exact resumption", {
  items <- evi_test_items()[-12, ]
  path <- tempfile(fileext = ".tif")
  original <- evi_source_staging_dir(path, items)
  expect_identical(evi_source_staging_dir(path, items[nrow(items):1, ]), original)
  items$href[[1]] <- paste0(items$href[[1]], "?revision=2")
  expect_false(identical(evi_source_staging_dir(path, items), original))
})

test_that("complete manifests from the previous format remain usable", {
  directory <- withr::local_tempdir()
  path <- file.path(directory, "complete.tif")
  writeLines("complete composite", path)
  evi_write_manifest(path, evi_test_items())
  record <- evi_read_manifest(path, "2024", "h11v05")
  record[c("Source-Signature", "Expected-Count", "Missing-Count")] <- NULL
  write.dcf(as.data.frame(record, check.names = FALSE), evi_manifest_path(path), keep.white = names(record))
  expect_true(evi_cache_valid(path, "2024", "h11v05", require_complete = TRUE))
})

test_that("EVI fixture builds support gaps and refresh when observations arrive", {
  skip_if_not_installed("terra")
  directory <- withr::local_tempdir()
  withr::local_envvar(R_GEOMARKER_NO_DOWNLOAD = "")
  items <- evi_test_items()
  listing <- items[-12, ]
  reads <- 0L
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2)
  terra::values(r) <- 0.42
  local_mocked_bindings(
    evi_planetary_computer_items = function(...) listing,
    evi_fixture_item_overlaps_cell = function(...) TRUE,
    evi_quality_filtered_fixture_raster = function(...) { reads <<- reads + 1L; r }
  )
  expect_warning(install_evi_geomarker_fixture("8841", as.Date("2024-01-01"), directory, quiet = TRUE), "incomplete")
  expect_identical(reads, 45L)
  expect_warning(install_evi_geomarker_fixture("8841", as.Date("2024-01-01"), directory, quiet = TRUE), "incomplete")
  expect_identical(reads, 45L)
  expect_error(install_evi_geomarker_fixture("8841", as.Date("2024-01-01"), directory, quiet = TRUE, require_complete = TRUE), "incomplete")
  listing <- items
  expect_no_warning(install_evi_geomarker_fixture("8841", as.Date("2024-01-01"), directory, quiet = TRUE, require_complete = TRUE))
  expect_identical(reads, 91L)
})

test_that("strict EVI rejects synthetic examples and validates its flag", {
  withr::local_envvar(
    R_USER_DATA_DIR = system.file("gmrkr--8841", package = "geomarker"),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  x <- s2cd_example()
  expect_error(get_evi_summary(x, require_complete = TRUE), "Synthetic EVI examples")
  expect_error(get_evi_summary(x, require_complete = NA), "require_complete must be TRUE or FALSE")
})
