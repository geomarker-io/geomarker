test_merra_grid <- function() {
  as.character(s2::as_s2_cell(c(
    "8841b39a7c46e25f",
    "8841a45555555555"
  )))
}

test_merra_data <- function(dates, grid = test_merra_grid()) {
  data <- expand.grid(
    date = as.Date(dates),
    s2 = grid,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  value <- as.numeric(data$date - min(as.Date(dates))) +
    match(data$s2, grid) / 10
  data$merra_dust <- value
  data$merra_oc <- value + 1
  data$merra_bc <- value + 2
  data$merra_ss <- value + 3
  data$merra_so4 <- value + 4
  data$merra_pm25 <- with(
    data,
    merra_dust + merra_oc + merra_bc + merra_ss + merra_so4 * 132.14 / 96.06
  )
  data[, merra_data_columns]
}

test_write_merra <- function(path, data, fields = list()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(data, path, compress = "xz")
  dates <- sort(unique(data$date))
  record <- c(
    fields,
    list(
      `Coverage-Start` = as.character(min(dates)),
      `Coverage-End` = as.character(max(dates)),
      `Asset-Days` = as.character(length(dates)),
      `Asset-Name` = basename(path),
      `Asset-Bytes` = as.character(file.info(path)$size),
      `Asset-SHA256` = digest::digest(path, algo = "sha256", file = TRUE),
      `Asset-Rows` = as.character(nrow(data)),
      `Asset-Grid-Cells` = as.character(length(unique(data$s2))),
      `Asset-Schema` = merra_schema,
      `Asset-Compression` = "RDS-XZ"
    )
  )
  write.dcf(
    as.data.frame(record, check.names = FALSE),
    sub("[.]rds$", ".dcf", path)
  )
  record
}

test_release_merra <- function(year, half) {
  start <- as.Date(sprintf("%d-%s-01", year, if (half == "H1") "01" else "07"))
  end <- if (half == "H1") {
    as.Date(sprintf("%d-06-30", year))
  } else {
    as.Date(sprintf("%d-12-31", year))
  }
  name <- sprintf("merra2_%d_%s_pm25.rds", year, tolower(half))
  path <- tempfile(fileext = ".rds")
  data <- test_merra_data(seq(start, end, by = 1), test_merra_grid()[1])
  record <- test_write_merra(
    path,
    data,
    list(
      `MERRA-Year` = as.character(year),
      `MERRA-Half` = half
    )
  )
  record[["Asset-Name"]] <- name
  record[["Asset-URL"]] <- paste0("https://example.com/", name)
  list(path = path, record = record)
}

test_records <- function(x) {
  do.call(
    rbind,
    lapply(x, function(y) {
      as.data.frame(y$record, stringsAsFactors = FALSE, check.names = FALSE)
    })
  )
}

test_that("MERRA months and daily PM2.5 are calculated explicitly", {
  expect_identical(
    merra_month_dates("2024-02"),
    seq(as.Date("2024-02-01"), as.Date("2024-02-29"), by = 1)
  )
  expect_error(merra_months("2016-12"), "from 2017 onward")
  expect_error(merra_months("2024-13"), "YYYY-MM")

  hourly <- stats::setNames(
    lapply(1:5, function(x) {
      matrix(x * 1e-9, nrow = 2, ncol = 24)
    }),
    merra_variables
  )
  out <- merra_daily_values(hourly)
  expect_equal(out$merra_dust, c(1, 1))
  expect_equal(out$merra_so4, c(5, 5))
  expect_equal(out$merra_pm25, rep(1 + 2 + 3 + 4 + 5 * 132.14 / 96.06, 2))
  hourly[[1]] <- hourly[[1]][, -1, drop = FALSE]
  expect_error(merra_daily_values(hourly), "24 hourly slices")
})

test_that("MERRA validation accepts equivalent Date storage types", {
  path <- tempfile(fileext = ".rds")
  data <- test_merra_data(as.Date(c("2025-01-01", "2025-01-02")))
  data$date <- structure(as.integer(data$date), class = "Date")
  record <- test_write_merra(path, data)
  expect_equal(merra_read_data(path, record), data)
})

test_that("Earthdata credentials create private curl authentication files", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("merra-auth-"))
  withr::local_envvar(
    EARTHDATA_USER = "test-user",
    EARTHDATA_PASSWORD = "test-password"
  )
  auth <- merra_earthdata_auth()
  expect_true(file.exists(auth$netrc))
  expect_true(file.exists(auth$cookies))
  expect_match(readLines(auth$netrc), "test-user")
  if (.Platform$OS.type != "windows") {
    expect_identical(as.character(file.info(auth$netrc)$mode), "600")
    expect_identical(as.character(file.info(auth$cookies)$mode), "600")
  }
})

test_that("Earthdata builds respect no-download mode and reuse finished data", {
  withr::local_envvar(c(
    R_USER_DATA_DIR = tempfile("merra-offline-"),
    R_GEOMARKER_NO_DOWNLOAD = "true",
    EARTHDATA_USER = NA,
    EARTHDATA_PASSWORD = NA
  ))

  expect_error(
    merra_json("https://example.com"),
    "R_GEOMARKER_NO_DOWNLOAD"
  )
  expect_error(
    install_merra_data("2024-01", source = "earthdata", quiet = TRUE),
    "R_GEOMARKER_NO_DOWNLOAD"
  )

  month <- "2024-01"
  path <- merra_local_file(month)
  data <- test_merra_data(merra_month_dates(month))
  test_write_merra(
    path,
    data,
    list(`MERRA-Year` = "2024", `MERRA-Month` = month)
  )
  expect_identical(
    unname(install_merra_data(month, source = "earthdata", quiet = TRUE)),
    path
  )
})

test_that("local monthly data preserve dates, duplicates, and location order", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("merra-local-"))
  month <- "2024-01"
  path <- merra_local_file(month)
  test_write_merra(
    path,
    test_merra_data(merra_month_dates(month)),
    list(`MERRA-Year` = "2024", `MERRA-Month` = month)
  )
  testthat::local_mocked_bindings(
    merra_release_files = function(...) stop("release should not be used"),
    .package = "geomarker"
  )
  cell <- s2::as_s2_cell("8841b39a7c46e25f")
  x <- s2cd(
    c(cell, cell),
    list(
      as.Date(c("2024-01-01", "2024-01-02", "2024-01-02")),
      as.Date("2024-01-03")
    )
  )
  out <- get_merra_data(x)
  expect_named(out, as.character(x))
  expect_identical(names(out[[1]]), merra_columns)
  expect_equal(nrow(out[[1]]), 3)
  expect_equal(unname(unlist(out[[1]][2, ])), unname(unlist(out[[1]][3, ])))
  expect_false(any(vapply(out, anyNA, logical(1))))
})

test_that("unavailable periods are aligned once and CONUS is enforced", {
  withr::local_envvar(
    R_USER_DATA_DIR = tempfile("merra-missing-"),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  x <- s2cd(
    s2::as_s2_cell("8841b39a7c46e25f"),
    list(as.Date(c("2016-12-01", "2099-07-01", "2099-08-01")))
  )
  expect_warning(out <- get_merra_data(x), "2016-12, 2099-07, 2099-08")
  expect_true(all(is.na(out[[1]])))

  empty_dates <- s2cd(
    s2::as_s2_cell("8841b39a7c46e25f"),
    list(as.Date(character()))
  )
  expect_equal(nrow(get_merra_data(empty_dates)[[1]]), 0)
  expect_identical(
    get_merra_data(s2cd(s2::as_s2_cell(character()), list())),
    stats::setNames(list(), character())
  )
  outside <- s2::as_s2_cell(s2::s2_lnglat(0, 51.5))
  expect_error(
    get_merra_data(s2cd(outside, list(as.Date("2024-01-01")))),
    "CONUS"
  )
})

test_that("release DCF downloads each fixed half-year asset once", {
  h1 <- test_release_merra(2024, "H1")
  h2 <- test_release_merra(2024, "H2")
  records <- test_records(list(h1, h2))
  downloads <- character()
  arguments <- list()
  testthat::local_mocked_bindings(
    merra_release_manifest = function() records,
    geomarker_stow = function(url, .subdir, ..., .etag = NULL) {
      downloads <<- c(downloads, url)
      arguments[[url]] <<- c(
        list(subdir = .subdir, forced_etag = .etag),
        list(...)
      )
      if (identical(url, h1$record[["Asset-URL"]])) h1$path else h2$path
    },
    .package = "geomarker"
  )
  paths <- install_merra_data(c("2024-01", "2024-06", "2024-07", "2024-01"))
  expect_named(paths, c("2024-01", "2024-06", "2024-07", "2024-01"))
  expect_identical(paths[[1]], paths[[2]])
  expect_identical(paths[[1]], paths[[4]])
  expect_length(downloads, 2)
  expect_identical(arguments[[downloads[[1]]]]$forced_etag, FALSE)
  expect_identical(
    arguments[[downloads[[1]]]]$subdir,
    "get_merra_data"
  )
})

test_that("release DCF allows only one record per half-year", {
  release <- test_release_merra(2024, "H1")
  records <- test_records(list(release, release))
  testthat::local_mocked_bindings(
    merra_release_manifest = function() records,
    .package = "geomarker"
  )
  expect_error(install_merra_data("2024-01"), "duplicate records")
})

test_that("recorded release checksum and schema failures are errors", {
  release <- test_release_merra(2024, "H1")
  records <- test_records(list(release))
  records[1, "Asset-SHA256"] <- paste(rep("0", 64), collapse = "")
  testthat::local_mocked_bindings(
    merra_release_manifest = function() records,
    geomarker_stow = function(...) release$path,
    .package = "geomarker"
  )
  expect_error(install_merra_data("2024-01"), "SHA-256")

  records[1, "Coverage-End"] <- "2024-06-29"
  expect_error(install_merra_data("2024-01"), "complete half-year")
})

test_that("Earthdata builds complete months and records exact provenance", {
  withr::local_envvar(
    R_USER_DATA_DIR = tempfile("merra-earthdata-"),
    EARTHDATA_USER = "test-user",
    EARTHDATA_PASSWORD = "test-password",
    R_GEOMARKER_NO_DOWNLOAD = NA
  )
  dates <- merra_month_dates("2024-02")
  granules <- data.frame(
    date = dates,
    granule_ur = paste0("MERRA:", dates),
    concept_id = paste0("G", seq_along(dates)),
    revision_id = seq_along(dates),
    opendap_url = paste0("https://opendap.earthdata.nasa.gov/", dates)
  )
  builds <- 0L
  daily_overwrite <- logical()
  testthat::local_mocked_bindings(
    merra_granules = function(month) granules,
    create_daily_merra_data = function(granule, source_dir, auth, overwrite) {
      builds <<- builds + 1L
      daily_overwrite <<- c(daily_overwrite, overwrite)
      list(
        data = test_merra_data(
          as.Date(granule[["date"]]),
          test_merra_grid()[1]
        ),
        subset_url = paste0(
          "https://opendap.earthdata.nasa.gov/",
          granule[["concept_id"]]
        ),
        subset_sha256 = paste(rep("a", 64), collapse = "")
      )
    },
    .package = "geomarker"
  )
  paths <- install_merra_data(
    c("2024-02", "2024-02"),
    source = "earthdata",
    quiet = TRUE
  )
  expect_identical(paths[[1]], paths[[2]])
  expect_equal(builds, 29)
  record <- read.dcf(sub("[.]rds$", ".dcf", paths[[1]]))
  expect_identical(unname(record[1, "MERRA-Month"]), "2024-02")
  expect_identical(unname(record[1, "Asset-Days"]), "29")
  expect_match(record[1, "Source-Granule-Concept-Revisions"], "G1:1")
  expect_equal(nrow(merra_read_data(paths[[1]], record[1, ])), 29)

  install_merra_data(
    "2024-02",
    source = "earthdata",
    overwrite = TRUE,
    quiet = TRUE
  )
  expect_equal(builds, 58)
  expect_false(any(daily_overwrite))
})

test_that("CMR discovery requires exactly one granule per day", {
  item <- function(date) {
    list(
      meta = list(`concept-id` = paste0("G", date), `revision-id` = 1L),
      umm = list(
        TemporalExtent = list(
          RangeDateTime = list(
            BeginningDateTime = paste0(date, "T00:00:00Z")
          )
        ),
        GranuleUR = paste0("MERRA:", date),
        RelatedUrls = list(list(
          Subtype = "OPENDAP DATA",
          URL = "https://example.com"
        ))
      )
    )
  }
  dates <- merra_month_dates("2024-02")
  testthat::local_mocked_bindings(
    merra_json = function(...) list(items = lapply(as.character(dates), item)),
    .package = "geomarker"
  )
  expect_equal(merra_granules("2024-02")$date, dates)
  testthat::local_mocked_bindings(
    merra_json = function(...) {
      list(items = lapply(as.character(dates[-1]), item))
    },
    .package = "geomarker"
  )
  expect_error(merra_granules("2024-02"), "not complete")
})

test_that("staged daily sources resume only with matching hashes and revisions", {
  source_dir <- tempfile("merra-daily-")
  granule <- data.frame(
    date = as.Date("2024-01-01"),
    granule_ur = "MERRA:2024-01-01",
    concept_id = "G1",
    revision_id = 3L
  )
  stem <- "MERRA_2024-01-01-cmr-r3"
  subset <- file.path(source_dir, "subsets", paste0(stem, "--conus.nc4"))
  daily <- file.path(source_dir, "daily", paste0(stem, ".rds"))
  dir.create(dirname(subset), recursive = TRUE)
  dir.create(dirname(daily), recursive = TRUE)
  writeLines("cached subset", subset)
  saveRDS(
    test_merra_data(granule$date, test_merra_grid()[1]),
    daily,
    compress = "xz"
  )
  hash <- digest::digest(subset, algo = "sha256", file = TRUE)
  write.dcf(
    as.data.frame(
      list(
        `Granule-UR` = granule$granule_ur,
        `Granule-Concept-ID` = granule$concept_id,
        `Granule-Revision-ID` = "3",
        `Subset-URL` = "https://opendap.earthdata.nasa.gov/G1",
        `Subset-SHA256` = hash
      ),
      check.names = FALSE
    ),
    paste0(daily, ".dcf")
  )
  testthat::local_mocked_bindings(
    merra_json = function(...) stop("NASA should not be called"),
    .package = "geomarker"
  )
  out <- create_daily_merra_data(granule, source_dir, list())
  expect_equal(out$data, readRDS(daily))
  expect_identical(out$subset_sha256, hash)
})

test_that("unfinished months fail and bundled fixtures cover both halves", {
  expect_error(
    install_merra_data(format(Sys.Date(), "%Y-%m"), source = "earthdata"),
    "fully elapsed"
  )
  withr::local_envvar(
    R_USER_DATA_DIR = fs::path_package("geomarker", "gmrkr--8841"),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  out <- get_merra_data(s2cd_example())
  expect_length(out, 2)
  expect_equal(unname(vapply(out, nrow, integer(1))), c(1L, 2L))
  expect_false(any(vapply(out, anyNA, logical(1))))
})
