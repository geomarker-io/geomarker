evi_transformation <- "annual-evi-median-q01-v1"

evi_require_complete_years <- function(years, today = Sys.Date()) {
  if (any(as.integer(years) >= as.integer(format(today, "%Y")))) {
    stop(
      "Annual EVI summaries require completed calendar years; ",
      "ongoing and future years cannot be used.",
      call. = FALSE
    )
  }
  invisible(years)
}

evi_expected_observations <- function(year) {
  start <- as.Date(paste0(year, "-01-01"))
  end <- as.Date(paste0(year, "-12-31"))
  # MOD13Q1 and MYD13Q1 have 16-day cycles offset by eight days.
  # https://lpdaac.usgs.gov/documents/621/MOD13_User_Guide_V61.pdf
  terra_dates <- seq(start, end, by = 16)
  aqua_dates <- seq(start + 8, end, by = 16)
  terra_dates <- terra_dates[terra_dates >= as.Date("2000-02-18")]
  aqua_dates <- aqua_dates[aqua_dates >= as.Date("2002-07-04")]
  data.frame(
    platform = c(rep("terra", length(terra_dates)), rep("aqua", length(aqua_dates))),
    start_date = c(terra_dates, aqua_dates)
  )
}

evi_complete_source_items <- function(items, year, tile) {
  required <- c("id", "start_date", "end_date", "href", "quality_href")
  if (!all(required %in% names(items)) || anyNA(items[required])) {
    stop("EVI source metadata are incomplete.", call. = FALSE)
  }
  product <- substr(items$id, 1, 7)
  platform <- c(MOD13Q1 = "terra", MYD13Q1 = "aqua")[product]
  expected_prefix <- paste0(
    product, ".A", format(items$start_date, "%Y%j"), ".", tile, ".061."
  )
  if (
    anyNA(platform) || any(!grepl(
      "^M[OY]D13Q1[.]A[0-9]{7}[.]h[0-9]{2}v[0-9]{2}[.]061[.][0-9]{13}$", items$id
    )) || any(!startsWith(items$id, expected_prefix)) ||
      any(format(items$start_date, "%Y") != year) ||
      any(items$end_date < pmin(
        items$start_date + 15, as.Date(paste0(year, "-12-31"))
      )) || any(items$end_date > items$start_date + 15) ||
      any(!nzchar(items$href)) || any(!nzchar(items$quality_href))
  ) {
    stop("EVI source metadata do not match the requested tile and year.", call. = FALSE)
  }
  items$platform <- unname(platform)
  # A reprocessed acquisition can appear twice in STAC. Its production
  # timestamp is the final component of the item ID; use the latest version.
  items <- items[order(items$id, decreasing = TRUE), , drop = FALSE]
  keys <- paste(items$platform, items$start_date)
  items <- items[!duplicated(keys), , drop = FALSE]
  expected <- evi_expected_observations(year)
  expected_keys <- paste(expected$platform, expected$start_date)
  keys <- paste(items$platform, items$start_date)
  missing <- setdiff(expected_keys, keys)
  if (length(missing) || any(!keys %in% expected_keys)) {
    stop(
      "EVI source coverage is incomplete for ", year, " ", tile,
      if (length(missing)) paste0("; missing: ", paste(missing, collapse = ", ")) else "",
      ". A complete source schedule is required before building an annual composite.",
      call. = FALSE
    )
  }
  items[order(items$start_date, items$platform), , drop = FALSE]
}

evi_manifest_path <- function(path) sub("[.]tif$", ".dcf", path)

evi_write_manifest <- function(path, items, scope = "full-tile", cell = NULL, buffer = NULL) {
  year <- unique(format(items$start_date, "%Y"))
  tile <- unique(items$tile)
  stopifnot(length(year) == 1L, length(tile) == 1L)
  evi_require_complete_years(year)
  items <- evi_complete_source_items(items, year, tile)
  record <- list(
    `Manifest-Version` = "1",
    `EVI-Year` = year,
    `MODIS-Tile` = tile,
    Collection = "modis-13Q1-061",
    Transformation = evi_transformation,
    `Coverage-Status` = "complete",
    `Spatial-Scope` = scope,
    `Source-Count` = as.character(nrow(items)),
    `Source-IDs` = paste(items$id, collapse = "\n"),
    `Source-Start-Dates` = paste(items$start_date, collapse = "\n"),
    `Source-End-Dates` = paste(items$end_date, collapse = "\n"),
    `Source-EVI-URLs` = paste(items$href, collapse = "\n"),
    `Source-Quality-URLs` = paste(items$quality_href, collapse = "\n"),
    `Asset-Bytes` = as.character(file.info(path)$size),
    `Asset-SHA256` = digest::digest(path, algo = "sha256", file = TRUE),
    `Created-UTC` = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  if (!is.null(cell)) record[["Fixture-Cell"]] <- as.character(cell)
  if (!is.null(buffer)) record[["Fixture-Buffer-M"]] <- as.character(buffer)
  manifest <- evi_manifest_path(path)
  temporary <- tempfile(".evi-manifest-", dirname(manifest))
  on.exit(unlink(temporary), add = TRUE)
  write.dcf(as.data.frame(record, check.names = FALSE), temporary, keep.white = names(record))
  if (!file.rename(temporary, manifest)) {
    stop("Could not write EVI manifest: ", manifest, call. = FALSE)
  }
  invisible(manifest)
}

evi_read_manifest <- function(path, year, tile) {
  fail <- function() {
    stop(
      "EVI composite has no valid complete-year manifest: ", path,
      ". Rebuild it online with get_evi_summary(); ",
      "offline operation cannot verify or migrate legacy composites.",
      call. = FALSE
    )
  }
  manifest <- evi_manifest_path(path)
  if (!file.exists(path) || !file.exists(manifest)) fail()
  record <- tryCatch(read.dcf(manifest), error = function(e) NULL)
  required <- c(
    "Manifest-Version", "EVI-Year", "MODIS-Tile", "Collection",
    "Transformation", "Coverage-Status", "Spatial-Scope", "Asset-Bytes", "Asset-SHA256"
  )
  if (is.null(record) || nrow(record) != 1L || !all(required %in% colnames(record))) fail()
  record <- as.list(record[1, ])
  matches <- c(
    `Manifest-Version` = "1", `EVI-Year` = year, `MODIS-Tile` = tile
  )
  if (
    anyNA(unlist(record[required])) ||
      !all(vapply(names(matches), function(key) identical(record[[key]], matches[[key]]), logical(1))) ||
      !record[["Spatial-Scope"]] %in% c("full-tile", "fixture") ||
      !identical(record[["Asset-Bytes"]], as.character(file.info(path)$size)) ||
      !identical(record[["Asset-SHA256"]], digest::digest(path, algo = "sha256", file = TRUE))
  ) {
    fail()
  }
  if (identical(record[["Coverage-Status"]], "synthetic-example")) {
    if (!identical(record[["Collection"]], "synthetic") ||
      !identical(record[["Transformation"]], "synthetic-evi-example-v1")) {
      fail()
    }
    return(record)
  }
  source_fields <- c(
    "Source-Count", "Source-IDs", "Source-Start-Dates", "Source-End-Dates",
    "Source-EVI-URLs", "Source-Quality-URLs"
  )
  if (!all(source_fields %in% names(record)) ||
    anyNA(unlist(record[source_fields])) ||
    !identical(record[["Coverage-Status"]], "complete") ||
    !identical(record[["Collection"]], "modis-13Q1-061") ||
    !identical(record[["Transformation"]], evi_transformation)) {
    fail()
  }
  columns <- c(
    id = "Source-IDs", start_date = "Source-Start-Dates", end_date = "Source-End-Dates",
    href = "Source-EVI-URLs", quality_href = "Source-Quality-URLs"
  )
  sources <- lapply(columns, function(key) strsplit(record[[key]], "\n", fixed = TRUE)[[1]])
  if (length(unique(lengths(sources))) != 1L ||
    !identical(record[["Source-Count"]], as.character(length(sources$id)))) {
    fail()
  }
  valid <- tryCatch(
    {
      sources <- as.data.frame(sources, stringsAsFactors = FALSE)
      sources$start_date <- as.Date(sources$start_date)
      sources$end_date <- as.Date(sources$end_date)
      checked <- evi_complete_source_items(sources, year, tile)
      nrow(checked) == nrow(sources)
    },
    error = function(e) FALSE
  )
  if (!isTRUE(valid)) fail()
  record
}

evi_cache_valid <- function(path, year, tile, cell = NULL, buffer = NULL) {
  tryCatch(
    {
      record <- evi_read_manifest(path, year, tile)
      valid <- identical(record[["Coverage-Status"]], "complete")
      if (!is.null(cell)) {
        valid <- valid && identical(record[["Spatial-Scope"]], "fixture") &&
          identical(record[["Fixture-Cell"]], as.character(cell)) &&
          isTRUE(as.numeric(record[["Fixture-Buffer-M"]]) >= buffer)
      }
      valid
    },
    error = function(e) FALSE
  )
}

evi_publish_composite <- function(staged, dest) {
  # Invalidate the old manifest before replacing either member of the pair.
  # An interrupted replacement is unverified and will be rebuilt, never reused.
  unlink(evi_manifest_path(dest))
  evi_move_raster_file(staged, dest)
  if (!file.rename(evi_manifest_path(staged), evi_manifest_path(dest))) {
    stop("Could not publish EVI manifest: ", evi_manifest_path(dest), call. = FALSE)
  }
  invisible(dest)
}
