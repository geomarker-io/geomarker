s2_cell_to_vect <- function(x) {
  x <- s2::as_s2_cell(x)
  stopifnot(
    "x must be (coercible to) a s2_cell object" = inherits(x, "s2_cell")
  )
  x |>
    s2::s2_cell_to_lnglat() |>
    as.data.frame() |>
    terra::vect(geom = c("x", "y"), crs = "+proj=longlat +datum=WGS84")
}

check_installed <- function(pkg, reason = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- sprintf("The package `%s` is required", pkg)
    if (!is.null(reason)) {
      msg <- paste0(msg, " ", reason)
    }
    msg <- paste0(msg, ". Please install it first.")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

geomarker_no_download <- function() {
  nzchar(Sys.getenv("R_GEOMARKER_NO_DOWNLOAD", unset = ""))
}

geomarker_require_download <- function(action = "access the network") {
  if (geomarker_no_download()) {
    stop(
      "R_GEOMARKER_NO_DOWNLOAD is set; geomarker will not ",
      action,
      ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

is_non_decreasing <- function(x) {
  if (length(x) <= 1) {
    return(TRUE)
  }
  all(diff(x) >= 0)
}

url_to_filename <- function(url, etag = TRUE) {
  stopifnot(
    "etag must be length one" = length(etag) == 1,
    "etag must be logical" = inherits(etag, "logical"),
    "url must be length one" = length(url) == 1,
    "url must be character" = inherits(url, "character")
  )
  if (grepl("\\?", url)) {
    stop(
      "URLs with query parameters are not supported. ",
      "Provide a URL that ends with a filename.",
      call. = FALSE
    )
  }
  url_dir_hash <-
    digest::digest(
      dirname(url),
      algo = "xxhash32",
      serialize = FALSE,
    )
  f_name <- basename(url)
  if (etag) {
    url_etag <- url_etag(url)
    if (!is.na(url_etag)) {
      f_name <- paste0(
        tools::file_path_sans_ext(f_name),
        "--",
        url_etag,
        ".",
        tools::file_ext(f_name)
      )
    }
  }
  filepath <- paste(url_dir_hash, f_name, sep = "--")
  filepath
}

url_etag <- function(url) {
  stopifnot(
    "url must be length one" = length(url) == 1,
    "url must be character" = inherits(url, "character")
  )
  geomarker_require_download("request URL headers")
  check_installed("curl", "to check URL headers.")
  response <- curl::curl_fetch_memory(
    url = url,
    handle = curl::new_handle(nobody = TRUE, header = TRUE)
  )
  headers <- curl::parse_headers(response$headers)

  which_etag_header <- grep("^ETag:", headers, ignore.case = TRUE)
  if (length(which_etag_header) == 0) {
    return(NA_character_)
  }
  etag <- sub(
    "^ETag:\\s*",
    "",
    headers[[which_etag_header]],
    ignore.case = TRUE
  )
  if (!is.null(etag)) {
    etag <- gsub("\\\\\"", "\"", etag)
    etag <- gsub("^\"|\"$", "", etag)
  }

  if (!is.null(etag)) {
    return(etag)
  }
  NA_character_
}

geomarker_fixture_dates <- function(dates) {
  dates <- as.Date(dates)
  stopifnot(
    "dates must be a Date vector" = inherits(dates, "Date"),
    "dates must not be length zero" = length(dates) > 0
  )
  dates
}

geomarker_fixture_years <- function(dates) {
  unique(format(geomarker_fixture_dates(dates), "%Y"))
}

geomarker_fixture_output_dir <- function(output_dir) {
  stopifnot(
    "output_dir must be a character vector" = is.character(output_dir),
    "output_dir must be length one" = length(output_dir) == 1,
    "output_dir must be non-missing" = !is.na(output_dir)
  )
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  output_dir
}

geomarker_fixture_cell <- function(cell) {
  cell <- s2::as_s2_cell(cell)
  stopifnot(
    "fixture cell must be length one" = length(cell) == 1,
    "fixture cell must be a valid s2 cell" = s2::s2_cell_is_valid(cell),
    "fixture cell must be level 6" = s2::s2_cell_level(cell) == 6
  )
  cell
}

geomarker_fixture_buffer <- function(buffer) {
  stopifnot(
    "fixture buffer must be numeric" = is.numeric(buffer),
    "fixture buffer must be length one" = length(buffer) == 1,
    "fixture buffer must be finite" = is.finite(buffer),
    "fixture buffer must not be negative" = buffer >= 0
  )
  as.numeric(buffer)
}

geomarker_fixture_region <- function(cell, buffer = 0) {
  cell <- geomarker_fixture_cell(cell)
  buffer <- geomarker_fixture_buffer(buffer)
  region <- s2::s2_cell_polygon(cell)
  if (buffer > 0) {
    region <- s2::s2_buffer_cells(
      region,
      distance = buffer,
      max_cells = 1000,
      min_level = -1
    )
  }
  region
}

geomarker_fixture_crop_to_cell <- function(x, cell, buffer = 0) {
  check_installed("sf", "to crop geomarker fixture data.")
  check_installed("terra", "to crop geomarker fixture data.")
  cover <-
    geomarker_fixture_region(cell, buffer = buffer) |>
    sf::st_as_sfc() |>
    sf::st_transform(terra::crs(x)) |>
    terra::vect()
  out <- terra::crop(x, cover)
  if (inherits(x, "SpatRaster")) {
    out <- terra::mask(out, cover)
  }
  out
}

geomarker_fixture_cell_bbox <- function(cell, buffer = 0) {
  check_installed("sf", "to create geomarker fixture data.")
  bbox <-
    geomarker_fixture_region(cell, buffer = buffer) |>
    sf::st_as_sfc() |>
    sf::st_bbox()
  unname(c(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]))
}
