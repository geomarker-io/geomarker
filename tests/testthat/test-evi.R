test_that("get_evi_data validates arguments before querying source data", {
  x_bad <- s2cd(
    s2::as_s2_cell("8841b39a7c46e25f"),
    dates = list(as.Date("1999-01-01"))
  )
  x_ok <- s2cd(
    s2::as_s2_cell("8841b39a7c46e25f"),
    dates = list(as.Date("2024-01-01"))
  )

  expect_error(
    get_evi_data(x_bad),
    "on or after 2000-02-18"
  )
  expect_error(
    get_evi_data(x_ok, buffer = c(400, 800)),
    "buffer must be length one"
  )
})

test_that("EVI STAC parser extracts Terra and Aqua EVI assets", {
  json <- paste0(
    '{"features":[',
    '{"id":"MOD13Q1.A2024001.h11v05.061.test","bbox":[0,0,1,1],"assets":{"250m_16_days_EVI":',
    '{"href":"https:\\/\\/example.com\\/terra.tif"},',
    '"250m_16_days_pixel_reliability":',
    '{"href":"https:\\/\\/example.com\\/terra_quality.tif"}},',
    '"properties":{"platform":"terra","start_datetime":"2024-01-01T00:00:00Z",',
    '"end_datetime":"2024-01-16T23:59:59Z"}}',
    ',{"id":"MYD13Q1.A2024009.h12v05.061.test","bbox":[1,1,2,2],"assets":{"250m_16_days_EVI":',
    '{"href":"https:\\/\\/example.com\\/aqua.tif"},',
    '"250m_16_days_pixel_reliability":',
    '{"href":"https:\\/\\/example.com\\/aqua_quality.tif"}},',
    '"properties":{"platform":"aqua","start_datetime":"2024-01-09T00:00:00Z",',
    '"end_datetime":"2024-01-24T23:59:59Z"}}',
    '],"links":[{"rel":"next","href":"https:\\/\\/example.com\\/next?x=1\\u0026y=2"}]}'
  )

  out <- evi_parse_stac_items(json)
  expect_equal(nrow(out), 2)
  expect_equal(
    out$id,
    c("MOD13Q1.A2024001.h11v05.061.test", "MYD13Q1.A2024009.h12v05.061.test")
  )
  expect_equal(out$platform, c("terra", "aqua"))
  expect_equal(out$tile, c("h11v05", "h12v05"))
  expect_equal(
    out[, c("xmin", "ymin", "xmax", "ymax")],
    data.frame(
      xmin = c(0, 1),
      ymin = c(0, 1),
      xmax = c(1, 2),
      ymax = c(1, 2)
    )
  )
  expect_equal(out$start_date, as.Date(c("2024-01-01", "2024-01-09")))
  expect_equal(out$end_date, as.Date(c("2024-01-16", "2024-01-24")))
  expect_equal(
    out$href,
    c("https://example.com/terra.tif", "https://example.com/aqua.tif")
  )
  expect_equal(
    out$quality_href,
    c(
      "https://example.com/terra_quality.tif",
      "https://example.com/aqua_quality.tif"
    )
  )
  expect_equal(evi_parse_next_href(json), "https://example.com/next?x=1&y=2")
})

test_that("EVI item filter keeps tiles containing requested points", {
  items <- data.frame(
    id = c("hit", "miss", "unknown"),
    xmin = c(0, 10, NA),
    ymin = c(0, 10, NA),
    xmax = c(1, 11, NA),
    ymax = c(1, 11, NA)
  )
  points <- data.frame(x = c(0.5, 20), y = c(0.5, 20))

  out <- evi_filter_items_to_points(items, points)
  expect_equal(out$id, c("hit", "unknown"))
})

test_that("EVI download helper reports batch cache progress", {
  withr::local_envvar(R_USER_DATA_DIR = tempdir())
  hrefs <- c(
    "https://example.com/evi/a.tif",
    "https://example.com/evi/b.tif"
  )
  cached <- file.path(
    geomarker_data_dir("evi"),
    vapply(hrefs, url_to_filename, character(1), etag = FALSE)
  )
  file.create(cached)

  expect_message(
    out <- evi_download_assets(hrefs, quiet = FALSE),
    "EVI rasters: 2 found, 2 cached, 0 to download.",
    fixed = TRUE
  )
  expect_equal(out, cached)
})

test_that("EVI download progress updates use compact one-line text", {
  msg <- evi_download_progress_message(
    "EVI source files for 2024 h11v05",
    download_index = 2,
    download_total = 46
  )

  expect_equal(msg, "Downloading EVI source files for 2024 h11v05 2 of 46")
  expect_false(grepl("remaining|:", msg))
})

test_that("EVI signs Azure assets with cached container SAS tokens", {
  key <- "fakeaccount/fake-container"
  evi_sas_token_cache[[key]] <- list(
    token = "st=2024-01-01T00%3A00%3A00Z&sig=fake",
    expiry = Sys.time() + 3600
  )
  withr::defer(rm(list = key, envir = evi_sas_token_cache), teardown_env())
  href <- paste0(
    "https://fakeaccount.blob.core.windows.net/fake-container/",
    "MOD13Q1/file_250m_16_days_EVI.tif"
  )

  expect_equal(
    evi_asset_storage(href),
    list(account = "fakeaccount", container = "fake-container")
  )
  expect_equal(
    evi_sign_asset(href),
    paste0(href, "?st=2024-01-01T00%3A00%3A00Z&sig=fake")
  )
  expect_equal(
    evi_sign_asset(paste0(href, "?x=1")),
    paste0(href, "?x=1&st=2024-01-01T00%3A00%3A00Z&sig=fake")
  )
  expect_equal(
    evi_sign_asset("https://example.com/evi.tif"),
    "https://example.com/evi.tif"
  )
})

test_that("EVI parses Planetary Computer SAS token responses", {
  parsed <- evi_parse_sas_token_json(paste0(
    '{"msft:expiry":"2099-05-27T12:42:17Z",',
    '"token":"st=2024-01-01T00%3A00%3A00Z\\u0026sig=fake"}'
  ))

  expect_equal(parsed$token, "st=2024-01-01T00%3A00%3A00Z&sig=fake")
  expect_s3_class(parsed$expiry, "POSIXct")
  expect_false(evi_sas_token_expired(parsed))
})

test_that("EVI retry helpers use Retry-After headers", {
  headers <- "HTTP/2 429\r\nretry-after: 12\r\n"

  expect_equal(evi_retry_after(headers), 12)
  expect_equal(evi_retry_delay(headers, attempt = 1), 12)
  expect_equal(evi_retry_delay(character(0), attempt = 3), 4)
  expect_match(
    evi_http_error_message("https://example.com", 429L, headers, "slow down"),
    "rate limited"
  )
})

test_that("EVI annual composite files use cached derived rasters", {
  withr::local_envvar(c(
    R_USER_DATA_DIR = tempdir(),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  ))
  dest <- file.path(
    geomarker_data_dir("evi"),
    evi_annual_composite_filename("2024", "h11v05")
  )
  file.create(dest)
  items <- data.frame(
    id = "MOD13Q1.A2024001.h11v05.061.test",
    platform = "terra",
    xmin = 0,
    ymin = 0,
    xmax = 1,
    ymax = 1,
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-01-16"),
    tile = "h11v05",
    href = "https://example.com/evi.tif",
    quality_href = "https://example.com/quality.tif"
  )

  expect_message(
    out <- evi_annual_composite_files(items, years = "2024", quiet = FALSE),
    "Annual EVI composites: 1 required, 1 cached, 0 to build.",
    fixed = TRUE
  )
  expect_equal(out$file, dest)
})

test_that("EVI annual composite keeps staged sources after source errors", {
  withr::local_envvar(c(
    R_USER_DATA_DIR = tempfile(),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  ))
  href <- "https://example.com/evi/evi_2024_h11v05.tif"
  quality_href <- "https://example.com/evi/quality_2024_h11v05.tif"
  dest <- file.path(
    geomarker_data_dir("evi"),
    evi_annual_composite_filename("2024", "h11v05")
  )
  source_dir <- evi_source_staging_dir(dest)
  staged_evi <- file.path(source_dir, url_to_filename(href, etag = FALSE))
  dir.create(source_dir, recursive = TRUE)
  file.create(staged_evi)
  items <- data.frame(
    year = "2024",
    tile = "h11v05",
    href = href,
    quality_href = quality_href
  )

  expect_error(
    evi_build_annual_composite(items, dest, 1, 1, quiet = TRUE),
    "R_GEOMARKER_NO_DOWNLOAD"
  )
  expect_true(file.exists(staged_evi))
  expect_true(dir.exists(source_dir))
})

test_that("EVI annual composite reuses and cleans staged sources", {
  skip_if_not_installed("terra")
  withr::local_envvar(c(
    R_USER_DATA_DIR = tempfile(),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  ))
  href <- paste0(
    "https://example.com/evi/evi_2024_h11v05_",
    c("a", "b"),
    ".tif"
  )
  quality_href <- paste0(
    "https://example.com/evi/quality_2024_h11v05_",
    c("a", "b"),
    ".tif"
  )
  dest <- file.path(
    geomarker_data_dir("evi"),
    evi_annual_composite_filename("2024", "h11v05")
  )
  source_dir <- evi_source_staging_dir(dest)
  dir.create(source_dir, recursive = TRUE)
  evi_file <- file.path(
    source_dir,
    vapply(href, url_to_filename, character(1), etag = FALSE)
  )
  quality_file <- file.path(
    source_dir,
    vapply(quality_href, url_to_filename, character(1), etag = FALSE)
  )
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2)
  terra::values(r) <- c(1000, 2000, 3000, 4000)
  q <- r
  terra::values(q) <- c(0, 2, 0, 1)
  terra::writeRaster(r, evi_file[[1]], overwrite = TRUE)
  terra::writeRaster(q, quality_file[[1]], overwrite = TRUE)
  terra::values(r) <- c(3000, 4000, 5000, 6000)
  terra::values(q) <- c(0, 1, 2, 1)
  terra::writeRaster(r, evi_file[[2]], overwrite = TRUE)
  terra::writeRaster(q, quality_file[[2]], overwrite = TRUE)
  items <- data.frame(
    year = "2024",
    tile = "h11v05",
    href = href,
    quality_href = quality_href
  )

  evi_build_annual_composite(items, dest, 1, 1, quiet = TRUE)

  expect_true(file.exists(dest))
  expect_false(dir.exists(source_dir))
  expect_equal(
    as.vector(terra::values(terra::rast(dest))),
    c(0.2, 0.4, 0.3, 0.5),
    tolerance = 1e-6
  )
})

test_that("get_evi_data uses cached annual composites without downloads", {
  skip_if_not_installed("terra")
  withr::local_envvar(c(
    R_USER_DATA_DIR = tempdir(),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  ))
  cell <- s2::as_s2_cell("8841b39a7c46e25f")
  x <- s2cd(cell, dates = list(as.Date(c("2024-01-05", "2024-09-05"))))
  lnglat <- s2::s2_cell_to_lnglat(cell) |>
    as.data.frame()
  r <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = lnglat$x - 0.1,
    xmax = lnglat$x + 0.1,
    ymin = lnglat$y - 0.1,
    ymax = lnglat$y + 0.1,
    crs = "+proj=longlat +datum=WGS84"
  )
  terra::values(r) <- 0.42
  terra::writeRaster(
    r,
    file.path(
      geomarker_data_dir("evi"),
      evi_annual_composite_filename("2024", "h11v05")
    ),
    overwrite = TRUE
  )

  out <- get_evi_data(x, quiet = TRUE)
  expect_equal(unname(out[[1]]), 0.42, tolerance = 1e-6)
  expect_equal(names(out[[1]]), "2024")
})

test_that("get_evi_data works with fixture data", {
  withr::local_envvar(
    R_USER_DATA_DIR = fs::path_package(
      "geomarker",
      "gmrkr--8841"
    ),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  set.seed(923)
  xx <- s2cd_example_cincy(n_locations = 3L)
  out <- get_evi_data(xx, quiet = TRUE)
  expect_type(out, "list")
  expect_length(out, 3)
  expect_named(out, as.character(xx))
  expect_true(all(lengths(out) == 1L))
  expect_true(all(vapply(out, \(x) names(x), character(1)) == "2024"))
  expect_true(any(!is.na(unlist(out))))
})
