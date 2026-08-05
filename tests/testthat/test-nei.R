nei_test_data <- function() {
  data.frame(
    `site latitude` = c(
      39.1,
      39.1,
      39.1,
      39.1,
      999,
      NA,
      39.1,
      39.1,
      39.1
    ),
    `site longitude` = c(
      -84.5,
      -84.499,
      -84.4,
      -84.5,
      -84.5,
      -84.5,
      -84.5,
      999,
      -84.5
    ),
    `pollutant code` = c(
      "PM25-PRI",
      "PM25-PRI",
      "PM25-PRI",
      "EC",
      "PM25-PRI",
      "PM25-PRI",
      "PM25-PRI",
      "PM25-PRI",
      "PM25-PRI"
    ),
    `total emissions` = c(2, 1, 100, 50, 25, 25, NA, 25, Inf),
    check.names = FALSE
  )
}

nei_test_zip <- function(data, member = "nei-facility-summary.csv") {
  csv_file <- file.path(tempdir(), member)
  zip_file <- tempfile(fileext = ".zip")
  utils::write.csv(data, csv_file, row.names = FALSE, na = "")
  utils::zip(zipfile = zip_file, files = csv_file, flags = "-jq")
  unlink(csv_file)
  zip_file
}

nei_test_x <- function(dates = as.Date("2024-01-01")) {
  s2cd(
    s2::as_s2_cell(s2::s2_geog_point(-84.5, 39.1)),
    dates = list(dates)
  )
}

test_that("get_nei_point_summary validates inputs", {
  x <- nei_test_x()
  expect_error(
    get_nei_point_summary(s2::as_s2_cell(x)),
    "s2_cell_dates"
  )
  expect_error(
    get_nei_point_summary(x, pollutant_code = "NOPE"),
    "arg"
  )
  expect_error(
    get_nei_point_summary(x, fun = "sum"),
    "fun must be a function"
  )
  expect_error(
    get_nei_point_summary(x, buffer = c(100, 200)),
    "buffer must be length one"
  )
  expect_error(
    get_nei_point_summary(x, buffer = Inf),
    "buffer must be finite"
  )
  expect_error(
    get_nei_point_summary(x, buffer = -1),
    "buffer must not be negative"
  )
})

test_that("unavailable NEI cycles return missing without dependencies", {
  x <- nei_test_x(as.Date(c("2014-01-01", "2025-01-01", "2027-12-31")))
  testthat::local_mocked_bindings(
    check_installed = function(...) stop("check_installed was called"),
    geomarker_download_file = function(...) stop("download was called"),
    .package = "geomarker"
  )

  expect_identical(
    get_nei_point_summary(x),
    list(c(NA_real_, NA_real_, NA_real_))
  )
})

test_that("NEI point CSVs are read, filtered, and cleaned", {
  zip_file <- nei_test_zip(nei_test_data())
  withr::defer(unlink(zip_file))
  testthat::local_mocked_bindings(
    geomarker_download_file = function(...) zip_file,
    .package = "geomarker"
  )

  expect_equal(
    get_nei_point_summary(nei_test_x(), buffer = 500, quiet = TRUE),
    list(3)
  )
})

test_that("NEI point CSV structure is validated", {
  missing_column <- nei_test_data()[1, , drop = FALSE]
  missing_column[["total emissions"]] <- NULL
  column_zip <- nei_test_zip(missing_column, "missing-column.csv")
  withr::defer(unlink(column_zip))
  testthat::local_mocked_bindings(
    geomarker_download_file = function(...) column_zip,
    .package = "geomarker"
  )
  expect_error(
    get_nei_point_summary(nei_test_x(), quiet = TRUE),
    "total emissions|required columns|not found"
  )
})

test_that("nearby NEI emissions use the requested summary", {
  zip_file <- nei_test_zip(nei_test_data())
  withr::defer(unlink(zip_file))
  testthat::local_mocked_bindings(
    geomarker_download_file = function(...) zip_file,
    .package = "geomarker"
  )
  x <- nei_test_x(as.Date(c("2023-03-01", "2024-09-01")))

  expect_equal(
    get_nei_point_summary(x, buffer = 500, quiet = TRUE),
    list(c(3, 3))
  )
  expect_equal(
    get_nei_point_summary(x, fun = mean, buffer = 500, quiet = TRUE),
    list(c(1.5, 1.5))
  )
  expect_equal(
    get_nei_point_summary(
      x,
      pollutant_code = "EC",
      buffer = 500,
      quiet = TRUE
    ),
    list(c(50, 50))
  )
  expect_error(
    get_nei_point_summary(
      x,
      fun = \(values) c(length(values), sum(values)),
      buffer = 500,
      quiet = TRUE
    ),
    "fun must return one numeric value"
  )
})

test_that("empty NEI buffers use the function's empty-vector result", {
  zip_file <- nei_test_zip(nei_test_data())
  withr::defer(unlink(zip_file))
  testthat::local_mocked_bindings(
    geomarker_download_file = function(...) zip_file,
    .package = "geomarker"
  )
  x <- nei_test_x()

  expect_identical(
    get_nei_point_summary(x, buffer = 1, quiet = TRUE),
    list(2)
  )
  expect_identical(
    get_nei_point_summary(
      x,
      pollutant_code = "NO3",
      buffer = 1000,
      quiet = TRUE
    ),
    list(0)
  )
  expect_true(is.nan(get_nei_point_summary(
    x,
    pollutant_code = "NO3",
    fun = mean,
    buffer = 1000,
    quiet = TRUE
  )[[1]]))
})

test_that("calendar years request each NEI source once", {
  downloads <- character()
  zip_file <- nei_test_zip(nei_test_data()[1, , drop = FALSE])
  withr::defer(unlink(zip_file))
  testthat::local_mocked_bindings(
    geomarker_download_file = function(url, ...) {
      downloads <<- c(downloads, url)
      zip_file
    },
    .package = "geomarker"
  )
  x <- nei_test_x(as.Date(paste0(2016:2024, "-01-01")))

  expect_equal(
    get_nei_point_summary(x, buffer = 500, quiet = TRUE),
    list(rep(2, 9))
  )
  expect_length(downloads, 3)
  expect_setequal(
    downloads,
    c(
      paste0(
        "https://gaftp.epa.gov/Air/nei/nei_facility_summaries/",
        "2017_NEI_Facility_summary.zip"
      ),
      paste0(
        "https://gaftp.epa.gov/Air/nei/nei_facility_summaries/",
        "2020_NEI_Facility_summary.zip"
      ),
      paste0(
        "https://gaftp.epa.gov/Air/nei/2023/data_summaries/",
        "eis_report_38234_2023NEI_facility_summary_21jul2026.zip"
      )
    )
  )
})

test_that("NEI source ZIP is the only persistent cached artifact", {
  zip_file <- nei_test_zip(nei_test_data())
  withr::defer(unlink(zip_file))
  data_root <- tempfile("geomarker-nei-cache-")
  dir.create(data_root)
  withr::defer(unlink(data_root, recursive = TRUE, force = TRUE))
  withr::local_envvar(
    R_USER_DATA_DIR = data_root,
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  cache_dir <- geomarker_data_dir()
  cached_zip <- file.path(
    cache_dir,
    "nei-2023-facility-summary.zip"
  )
  file.copy(zip_file, cached_zip)
  before <- list.files(cache_dir, all.files = TRUE, no.. = TRUE)

  expect_equal(
    get_nei_point_summary(nei_test_x(), buffer = 500, quiet = TRUE),
    list(3)
  )
  expect_identical(
    list.files(cache_dir, all.files = TRUE, no.. = TRUE),
    before
  )
})

test_that("NEI fixtures retain point sources in the requested buffer halo", {
  cell <- s2::as_s2_cell("8841")
  outside <- s2::s2_cell_edge_neighbour(cell, 0)
  for (level in 7:30) {
    children <- s2::s2_cell_child(rep(outside, 4), 0:3)
    distances <- s2::s2_distance(
      s2::s2_cell_center(children),
      rep(s2::s2_cell_polygon(cell), 4)
    )
    outside <- children[[which.min(distances)]]
  }
  coordinates <- as.data.frame(s2::s2_cell_to_lnglat(outside))
  source_file <- nei_test_zip(data.frame(
    `site latitude` = coordinates$y,
    `site longitude` = coordinates$x,
    `pollutant code` = "PM25-PRI",
    `total emissions` = 1,
    check.names = FALSE
  ))
  withr::defer(unlink(source_file))
  source_files <- stats::setNames(source_file, "2023")
  no_halo_dir <- tempfile("nei-no-halo-")
  halo_dir <- tempfile("nei-halo-")
  withr::defer(unlink(c(no_halo_dir, halo_dir), recursive = TRUE))

  install_nei_point_geomarker_fixture(
    cell,
    as.Date("2024-01-01"),
    no_halo_dir,
    source_files = source_files,
    buffer = 0
  )
  install_nei_point_geomarker_fixture(
    cell,
    as.Date("2024-01-01"),
    halo_dir,
    source_files = source_files,
    buffer = 1000
  )
  read_fixture <- function(directory) {
    zip_file <- file.path(directory, "nei-2023-facility-summary.zip")
    member <- utils::unzip(zip_file, list = TRUE)$Name[[1]]
    utils::read.csv(unz(zip_file, member), check.names = FALSE)
  }

  expect_equal(nrow(read_fixture(no_halo_dir)), 0)
  expect_equal(nrow(read_fixture(halo_dir)), 1)
})

test_that("get_nei_point_summary works with the Cincinnati fixture", {
  withr::local_envvar(
    R_USER_DATA_DIR = fs::path_package("geomarker", "gmrkr--8841"),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  set.seed(816)
  x <- s2cd_example_cincy(n_locations = 20L)
  out <- get_nei_point_summary(
    x,
    buffer = 5000,
    quiet = TRUE
  )

  expect_type(out, "list")
  expect_length(out, 20)
  expect_true(all(vapply(out, is.numeric, logical(1))))
  expect_true(all(lengths(out) == lengths(s2cd_dates(x))))
})
