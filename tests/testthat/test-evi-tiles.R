evi_tile_test_data <- function(directory) {
  crs <- paste(
    "+proj=sinu +lon_0=0 +x_0=0 +y_0=0",
    "+R=6371007.181 +units=m +no_defs"
  )
  reference <- terra::rast(
    nrows = 20, ncols = 40,
    xmin = -2000, xmax = 2000, ymin = -1000, ymax = 1000,
    crs = crs
  )
  pixels <- matrix(
    rep(c(rep(0.2, 20), rep(0.8, 20)), 20),
    nrow = 20, byrow = TRUE
  )
  pixels[9:12, 18:19] <- NA_real_
  terra::values(reference) <- as.vector(t(pixels))
  tiles <- list(
    terra::crop(reference, terra::ext(-2000, 0, -1000, 1000)),
    terra::crop(reference, terra::ext(0, 2000, -1000, 1000))
  )
  files <- file.path(directory, evi_annual_composite_filename(
    "2024", c("h17v08", "h18v08")
  ))
  for (i in seq_along(tiles)) {
    terra::writeRaster(tiles[[i]], files[[i]])
  }
  bounds <- lapply(tiles, function(tile) {
    footprint <- terra::as.polygons(terra::ext(tile), crs = terra::crs(tile)) |>
      terra::project("EPSG:4326") |>
      terra::ext()
    c(terra::xmin(footprint), terra::ymin(footprint),
      terra::xmax(footprint), terra::ymax(footprint))
  })
  bounds <- as.data.frame(do.call(rbind, bounds))
  names(bounds) <- c("xmin", "ymin", "xmax", "ymax")
  records <- data.frame(
    year = "2024", tile = c("h17v08", "h18v08"), bounds, file = files
  )
  points <- terra::vect(
    data.frame(x = c(-100, -1500, -100, 1500, 3000), y = 0),
    geom = c("x", "y"), crs = crs
  ) |>
    terra::project("EPSG:4326") |>
    terra::crds()
  x <- s2cd(
    s2::as_s2_cell(s2::s2_lnglat(points[, 1], points[, 2])),
    rep(list(as.Date("2024-06-01")), nrow(points))
  )
  list(reference = reference, records = records, x = x)
}

test_that("EVI buffers across tiles match a single reference raster", {
  skip_if_not_installed("terra")
  directory <- withr::local_tempdir()
  data <- evi_tile_test_data(directory)
  buffers <- s2_cell_to_vect(data$x) |>
    terra::project(data$reference) |>
    terra::buffer(width = 400)
  expected <- terra::extract(
    data$reference, buffers, fun = mean, na.rm = TRUE, ID = FALSE
  )[[1]]
  expected[is.nan(expected)] <- NA_real_

  out <- evi_extract_annual_values(data$x, data$records, buffer = 400)
  expect_equal(as.numeric(out), expected, tolerance = 1e-6)
  expect_gt(out[1, 1], 0.2)
  expect_lt(out[1, 1], 0.5)
  expect_equal(out[1, 2], 0.2, tolerance = 1e-6)
  expect_equal(out[1, 4], 0.8, tolerance = 1e-6)
  expect_identical(out[1, 1], out[1, 3])
  expect_true(is.na(out[1, 5]))
  expect_identical(colnames(out), as.character(data$x))
  expect_equal(
    evi_extract_annual_values(data$x, data$records[2:1, ], buffer = 400),
    out
  )

  offline_records <- data$records
  offline_records[, c("xmin", "ymin", "xmax", "ymax")] <- NA_real_
  expect_equal(
    evi_extract_annual_values(data$x, offline_records, buffer = 400),
    out
  )
  expect_equal(
    evi_extract_annual_values(data$x[2], data$records, buffer = 0)[1, 1],
    0.2, tolerance = 1e-6
  )

  # An entirely missing neighboring tile must not become zero-valued pixels.
  right <- terra::rast(data$records$file[[2]])
  terra::values(right) <- NA_real_
  terra::writeRaster(right, data$records$file[[2]], overwrite = TRUE)
  missing <- evi_extract_annual_values(data$x, data$records, buffer = 400)
  expect_equal(missing[1, 1], 0.2, tolerance = 1e-6)
  expect_true(is.na(missing[1, 4]))
})

test_that("EVI combines tiles separately for each requested year", {
  skip_if_not_installed("terra")
  directory <- withr::local_tempdir()
  data <- evi_tile_test_data(directory)
  next_year <- data$reference
  terra::values(next_year) <- 0.6
  path <- file.path(directory, "2025.tif")
  terra::writeRaster(next_year, path)
  record <- data$records[1, ]
  record$year <- "2025"
  record$file <- path
  records <- rbind(data$records, record)
  x <- s2cd(
    s2::as_s2_cell(data$x[1]),
    list(as.Date(c("2024-06-01", "2025-06-01")))
  )

  out <- evi_extract_annual_values(x, records, buffer = 400)
  expect_identical(rownames(out), c("2024", "2025"))
  expect_lt(out[1, 1], 0.5)
  expect_equal(out[2, 1], 0.6, tolerance = 1e-6)
})

test_that("empty space between nonadjacent EVI tiles remains missing", {
  skip_if_not_installed("terra")
  directory <- withr::local_tempdir()
  data <- evi_tile_test_data(directory)
  extents <- list(
    terra::ext(-2000, -500, -1000, 1000),
    terra::ext(500, 2000, -1000, 1000)
  )
  for (i in seq_along(extents)) {
    terra::crop(data$reference, extents[[i]]) |>
      terra::writeRaster(data$records$file[[i]], overwrite = TRUE)
  }

  out <- evi_extract_annual_values(data$x[1], data$records, buffer = 300)
  expect_true(is.na(out[1, 1]))
})

test_that("online EVI discovery includes tiles crossed by the full buffer", {
  skip_if_not_installed("terra")
  withr::local_envvar(R_GEOMARKER_NO_DOWNLOAD = NA_character_)
  directory <- withr::local_tempdir()
  data <- evi_tile_test_data(directory)
  items <- data$records
  items$start_date <- as.Date("2024-01-01")
  distant <- items[1, ]
  distant$tile <- "h19v08"
  distant$xmin <- 1
  distant$xmax <- 2
  items <- rbind(items, distant)
  search_bbox <- NULL
  requested_tiles <- NULL
  local_mocked_bindings(
    evi_planetary_computer_items = function(bbox, date_range) {
      search_bbox <<- bbox
      items
    },
    evi_annual_composite_files = function(items, ...) {
      requested_tiles <<- items$tile
      data$records[match(items$tile, data$records$tile), ]
    }
  )

  out <- get_evi_summary(data$x[1], buffer = 400, quiet = TRUE)
  expect_gt(search_bbox[[3]], 0)
  expect_lt(search_bbox[[1]], -400 / 111000)
  expect_setequal(requested_tiles, c("h17v08", "h18v08"))
  expect_gt(unname(out[[1]]), 0.2)
  expect_lt(unname(out[[1]]), 0.5)
  expect_named(out[[1]], "2024")
})

test_that("offline EVI buffers include all available neighboring tiles", {
  skip_if_not_installed("terra")
  directory <- withr::local_tempdir()
  withr::local_envvar(
    R_USER_DATA_DIR = directory, R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  data <- evi_tile_test_data(geomarker_stow_path("get_evi_summary"))
  local_mocked_bindings(
    evi_planetary_computer_items = function(...) stop("unexpected network")
  )

  out <- get_evi_summary(data$x, buffer = 400, quiet = TRUE)
  expect_named(out, as.character(data$x))
  expect_gt(unname(out[[1]]), 0.2)
  expect_lt(unname(out[[1]]), 0.5)
  expect_equal(unname(out[[2]]), 0.2, tolerance = 1e-6)
  expect_identical(out[[1]], out[[3]])
  expect_equal(unname(out[[4]]), 0.8, tolerance = 1e-6)
  expect_true(is.na(out[[5]]))
})
