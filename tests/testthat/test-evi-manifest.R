test_that("annual EVI rejects ongoing and future years before data lookup", {
  year <- format(Sys.Date(), "%Y")
  x <- s2cd(s2::as_s2_cell("8841b39a7c46e25f"), list(as.Date(paste0(year, "-01-01"))))
  local_mocked_bindings(
    evi_planetary_computer_items = function(...) stop("unexpected network"),
    evi_managed_annual_composite_files = function(...) stop("unexpected cache lookup")
  )
  for (offline in c("true", "")) {
    withr::local_envvar(R_GEOMARKER_NO_DOWNLOAD = offline)
    expect_error(get_evi_summary(x), "completed calendar years")
  }
  expect_error(evi_require_complete_years("2027", as.Date("2026-12-31")), "completed calendar years")
  expect_invisible(evi_require_complete_years("2025", as.Date("2026-01-01")))
})

test_that("complete EVI coverage requires each platform and acquisition", {
  items <- evi_test_items()
  expect_equal(nrow(evi_complete_source_items(items, "2024", "h11v05")), 46)
  expect_equal(nrow(evi_expected_observations("2023")), 46)
  expect_equal(nrow(evi_expected_observations("2000")), 20)
  expect_equal(nrow(evi_expected_observations("2002")), 35)
  expect_error(evi_complete_source_items(items[-12, ], "2024", "h11v05"), "missing: terra 2024-06-25")
  expect_error(evi_complete_source_items(items[1:23, ], "2024", "h11v05"), "missing: aqua")
  expect_error(evi_complete_source_items(items, "2024", "h12v05"), "do not match")
  expect_error(evi_annual_composite_files(items, c("2024", "2025"), quiet = TRUE), "year\\(s\\): 2025")

  revised <- items[12, ]
  revised$id <- sub("2025001000000$", "2026001000000", revised$id)
  revised$href <- paste0(revised$href, "-revised")
  out <- evi_complete_source_items(rbind(items, revised), "2024", "h11v05")
  expect_equal(nrow(out), 46)
  expect_true(revised$id %in% out$id)
  expect_false(items$id[[12]] %in% out$id)
})

test_that("EVI manifests validate source coverage, settings, and raster bytes", {
  directory <- withr::local_tempdir()
  path <- file.path(directory, evi_annual_composite_filename("2024", "h11v05"))
  writeLines("original", path)
  items <- evi_test_items()
  expect_false(evi_cache_valid(path, "2024", "h11v05"))
  expect_error(evi_read_manifest(path, "2024", "h11v05"), "Rebuild it online")
  evi_write_manifest(path, items)
  expect_true(evi_cache_valid(path, "2024", "h11v05"))
  record <- evi_read_manifest(path, "2024", "h11v05")
  expect_identical(record[["Source-Count"]], "46")
  expect_match(record[["Source-IDs"]], items$id[[12]], fixed = TRUE)

  changed <- record
  changed$Transformation <- "old-transform"
  write.dcf(as.data.frame(changed, check.names = FALSE), evi_manifest_path(path), keep.white = names(changed))
  expect_false(evi_cache_valid(path, "2024", "h11v05"))
  write.dcf(as.data.frame(record, check.names = FALSE), evi_manifest_path(path), keep.white = names(record))
  writeLines("modified", path) # same size, different SHA-256
  expect_false(evi_cache_valid(path, "2024", "h11v05"))
  writeLines("original", path)
  expect_true(evi_cache_valid(path, "2024", "h11v05"))

  changed <- record
  changed[["Source-Start-Dates"]] <- sub("2024-06-25", "2024-06-26", changed[["Source-Start-Dates"]], fixed = TRUE)
  write.dcf(as.data.frame(changed, check.names = FALSE), evi_manifest_path(path), keep.white = names(changed))
  expect_false(evi_cache_valid(path, "2024", "h11v05"))
})

test_that("legacy EVI caches rebuild online and completed caches stay stable", {
  directory <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = directory, R_GEOMARKER_NO_DOWNLOAD = "")
  items <- evi_test_items()
  path <- file.path(geomarker_stow_path("get_evi_summary"), evi_annual_composite_filename("2024", "h11v05"))
  writeLines("partial year", path)
  builds <- 0L
  overwrites <- logical()
  local_mocked_bindings(evi_build_annual_composite = function(items, dest, overwrite_sources, ...) {
    builds <<- builds + 1L
    overwrites <<- c(overwrites, overwrite_sources)
    writeLines(paste("complete build", builds), dest)
    evi_write_manifest(dest, items)
  })
  expect_error(evi_managed_annual_composite_files("2024"), "Rebuild it online")
  evi_annual_composite_files(items, "2024", quiet = TRUE)
  expect_identical(builds, 1L)
  original_hash <- digest::digest(path, algo = "sha256", file = TRUE)
  # A changed or incomplete live listing must not silently revise a validated cache.
  evi_annual_composite_files(items[-12, ], "2024", quiet = TRUE)
  expect_identical(builds, 1L)
  expect_identical(digest::digest(path, algo = "sha256", file = TRUE), original_hash)
  expect_error(evi_annual_composite_files(items[-12, ], "2024", overwrite = TRUE, quiet = TRUE), "incomplete")
  expect_identical(builds, 1L)
  evi_annual_composite_files(items, "2024", overwrite = TRUE, quiet = TRUE)
  expect_identical(builds, 2L)
  expect_identical(overwrites, c(FALSE, TRUE))
  expect_true(evi_cache_valid(path, "2024", "h11v05"))
})

test_that("annual builds publish their manifest only after successful processing", {
  skip_if_not_installed("terra")
  directory <- withr::local_tempdir()
  dest <- file.path(directory, evi_annual_composite_filename("2024", "h11v05"))
  items <- evi_test_items()
  local_mocked_bindings(evi_download_assets = function(hrefs, dest_dir, ...) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    paths <- file.path(dest_dir, paste0(seq_along(hrefs), ".tif"))
    r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2)
    for (i in seq_along(paths)) {
      terra::values(r) <- if (i <= 46) 2000 else 0
      terra::writeRaster(r, paths[[i]])
    }
    paths
  })
  evi_build_annual_composite(items, dest, 1, 1, quiet = TRUE)
  expect_true(evi_cache_valid(dest, "2024", "h11v05"))
  expect_false(dir.exists(evi_source_staging_dir(dest)))
  expect_equal(as.numeric(terra::values(terra::rast(dest))), rep(0.2, 4), tolerance = 1e-6)

  hash <- digest::digest(dest, algo = "sha256", file = TRUE)
  local_mocked_bindings(evi_write_annual_composite = function(...) stop("processing failed"))
  expect_error(evi_build_annual_composite(items, dest, 1, 1, quiet = TRUE), "processing failed")
  expect_identical(digest::digest(dest, algo = "sha256", file = TRUE), hash)
  expect_true(evi_cache_valid(dest, "2024", "h11v05"))
  expect_true(dir.exists(evi_source_staging_dir(dest)))
})

test_that("interrupted EVI publication cannot leave a trusted mixed pair", {
  directory <- withr::local_tempdir()
  dest <- file.path(directory, "old.tif")
  staged <- file.path(directory, "new.tif")
  writeLines("old", dest)
  evi_write_manifest(dest, evi_test_items())
  writeLines("new", staged)
  evi_write_manifest(staged, evi_test_items())
  local_mocked_bindings(evi_move_raster_file = function(...) stop("interrupted"))
  expect_error(evi_publish_composite(staged, dest), "interrupted")
  expect_false(evi_cache_valid(dest, "2024", "h11v05"))
})

test_that("EVI fixture builds retain coverage and spatial provenance", {
  skip_if_not_installed("terra")
  directory <- withr::local_tempdir()
  withr::local_envvar(R_GEOMARKER_NO_DOWNLOAD = "")
  items <- evi_test_items()
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2)
  terra::values(r) <- 0.42
  local_mocked_bindings(
    evi_planetary_computer_items = function(...) items,
    evi_fixture_item_overlaps_cell = function(...) TRUE,
    evi_quality_filtered_fixture_raster = function(...) r
  )
  install_evi_geomarker_fixture("8841", as.Date("2024-01-01"), directory, buffer = 1000, quiet = TRUE)
  path <- file.path(directory, "stow", "get_evi_summary", evi_annual_composite_filename("2024", "h11v05"))
  record <- evi_read_manifest(path, "2024", "h11v05")
  expect_identical(record[["Spatial-Scope"]], "fixture")
  expect_identical(record[["Fixture-Cell"]], "8841")
  expect_true(evi_cache_valid(path, "2024", "h11v05", s2::as_s2_cell("8841"), 800))
  expect_false(evi_cache_valid(path, "2024", "h11v05", s2::as_s2_cell("8841"), 2000))
})

test_that("the bundled EVI example is explicitly synthetic and checksum verified", {
  path <- system.file(
    "gmrkr--8841", "R", "geomarker", "stow", "get_evi_summary",
    evi_annual_composite_filename("2024", "h11v05"),
    package = "geomarker"
  )
  record <- evi_read_manifest(path, "2024", "h11v05")
  expect_identical(record[["Coverage-Status"]], "synthetic-example")
  expect_identical(record[["Collection"]], "synthetic")
  expect_match(record$Purpose, "not scientific data")
  expect_false(evi_cache_valid(path, "2024", "h11v05"))
})
